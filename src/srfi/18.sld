;;; SRFI 18: multithreading support.  Written for Pseudoscheme, on
;;; bordeaux-threads (its version 2 API, package BT2), which is loaded
;;; from Quicklisp or ASDF the first time this library is imported.
;;;
;;; Threads are native threads.  SRFI 18's mutexes are not native locks:
;;; a SRFI 18 mutex can be unlocked by a thread that doesn't own it, can
;;; be locked on behalf of another thread or of no thread, and becomes
;;; abandoned when its owner terminates.  So each mutex is a small state
;;; machine guarded by a native lock and condition variable.  A SRFI 18
;;; condition variable is a native one with a native lock of its own,
;;; which mutex-unlock! takes before it releases the mutex, so a signal
;;; sent after the release can't be missed.
;;;
;;; Each thread runs its thunk with the current input, output and error
;;; ports of the thread that made it.  Parameter objects made by
;;; make-parameter are NOT per-thread: R7RS parameterize (src/r7rs/
;;; rts.lisp) assigns the parameter's one global value for the extent of
;;; the body, so another thread sees the new value while the body runs.
;;;
;;; thread-terminate! makes the target thread escape to its top (by an
;;; interrupt, for another thread), so its dynamic-wind "after" thunks
;;; run.  Terminating a thread not made by make-thread (such as the
;;; primordial thread) from itself exits the program; from another
;;; thread, it is destroyed by the host.
;;;
;;; Time objects are seconds since the Unix epoch (with sub-second
;;; resolution, counted from the Lisp's internal real time).
;;;
;;; SRFI 18 also specifies raise and with-exception-handler; they are
;;; not exported here, because R7RS's and R6RS's own (which programs
;;; import anyway) would conflict with one of them.  current-exception-
;;; handler is the innermost handler installed by with-exception-handler.
(define-library (srfi 18)
  (export current-thread thread? make-thread thread-name
          thread-specific thread-specific-set! thread-start!
          thread-yield! thread-sleep! thread-terminate! thread-join!
          mutex? make-mutex mutex-name mutex-specific mutex-specific-set!
          mutex-state mutex-lock! mutex-unlock!
          condition-variable? make-condition-variable
          condition-variable-name condition-variable-specific
          condition-variable-specific-set! condition-variable-signal!
          condition-variable-broadcast!
          current-time time? time->seconds seconds->time
          current-exception-handler
          join-timeout-exception? abandoned-mutex-exception?
          terminated-thread-exception? uncaught-exception?
          uncaught-exception-reason)
  (import (scheme base) (scheme time) (scheme process-context)
          (pseudoscheme lisp)
          ;; Loads the system; BT2 is one of its packages.
          (only (prefix (cl bordeaux-threads) bt1:) bt1:make-thread)
          (prefix (cl bordeaux-threads-2) bt:)
          (prefix (only (cl trivial-garbage) make-weak-hash-table) tg:)
          (prefix (only (cl common-lisp) gethash remhash sleep
                        get-internal-real-time internal-time-units-per-second
                        handler-case serious-condition)
                  cl:))
  (begin
    ;; ------------------------------------------------------------
    ;; Native locks.  The body's result comes back through the Lisp
    ;; macro, which would turn #f into (), so callers return through
    ;; variables, never a boolean result.
    (define (with-lock lock thunk)
      (bt:with-lock-held (lock) (thunk)))

    ;; Wait on a native condition variable (its lock held) until
    ;; notified or until DEADLINE (internal seconds, or #f for none).
    ;; Returns #f if it timed out.  May wake spuriously.
    (define (wait cv lock deadline)
      (if deadline
          (let ((left (- deadline (now))))
            (and (> left 0)
                 (lisp-true? (bt:condition-wait cv lock #:timeout left))))
          (begin (bt:condition-wait cv lock) #t)))

    ;; ------------------------------------------------------------
    ;; Time

    (define jiffies-per-second cl:internal-time-units-per-second)
    (define (now)                       ; monotonic seconds
      (/ (cl:get-internal-real-time) jiffies-per-second))
    ;; The Unix time at (now) = 0.  current-second has whole-second
    ;; resolution, so this is accurate to a second; differences between
    ;; time objects are as accurate as the internal clock.
    (define epoch-offset (- (exact (current-second)) (now)))

    (define-record-type srfi-18-time (%make-time seconds) time?
      (seconds time-seconds))
    (define (current-time) (%make-time (+ epoch-offset (now))))
    (define (time->seconds t) (inexact (time-seconds t)))
    (define (seconds->time s) (%make-time (exact s)))

    ;; A timeout argument (#f, a time object, or relative seconds) as a
    ;; deadline in (now)'s terms, or #f.
    (define (deadline timeout)
      (cond ((not timeout) #f)
            ((time? timeout) (- (time-seconds timeout) epoch-offset))
            ((real? timeout) (+ (now) (exact timeout)))
            (else (error "not a timeout" timeout))))

    ;; ------------------------------------------------------------
    ;; Exception objects

    (define-record-type join-timeout-exception
      (make-join-timeout-exception) join-timeout-exception?)
    (define-record-type abandoned-mutex-exception
      (make-abandoned-mutex-exception) abandoned-mutex-exception?)
    (define-record-type terminated-thread-exception
      (make-terminated-thread-exception) terminated-thread-exception?)
    (define-record-type uncaught-exception
      (make-uncaught-exception reason) uncaught-exception?
      (reason uncaught-exception-reason))

    (define (current-exception-handler)
      (let ((handlers (lisp-value (lisp-symbol "*handlers*" "pseudoscheme-r7rs"))))
        (if (pair? handlers) (car handlers) raise)))

    ;; ------------------------------------------------------------
    ;; Threads

    (define-record-type srfi-18-thread
      (%make-thread name thunk specific state native result outcome
                    escape owned ports lock done)
      thread?
      (name thread-name)
      (thunk thread-thunk)
      (specific thread-specific thread-specific-set!)
      (state thread-state set-thread-state!)    ; new runnable terminated
      (native thread-native set-thread-native!) ; the BT2 thread
      (result thread-result set-thread-result!)
      (outcome thread-outcome set-thread-outcome!) ; normal uncaught terminated
      (escape thread-escape set-thread-escape!) ; to the thread's top, or #f
      (owned thread-owned set-thread-owned!)    ; mutexes it owns
      (ports thread-ports)                      ; (in out err) to run with
      (lock thread-lock)                        ; guards state and owned
      (done thread-done))                       ; notified on termination

    ;; BT2 thread -> SRFI 18 thread, for current-thread.
    (define threads (tg:make-weak-hash-table #:weakness #:key))
    (define threads-lock (bt:make-lock))
    (define (lookup native)
      (let ((t #f))
        (with-lock threads-lock
          (lambda ()
            (let ((found (cl:gethash native threads)))
              (set! t (if (lisp-true? found) found #f)))))
        t))
    (define (register! native thread)
      (with-lock threads-lock
        (lambda () (lisp-set! (cl:gethash native threads) thread))))
    (define (unregister! native)
      (with-lock threads-lock (lambda () (cl:remhash native threads))))

    (define (new-thread thunk name state native)
      (%make-thread name thunk #f state native #f #f #f '()
                    (list (current-input-port) (current-output-port)
                          (current-error-port))
                    (bt:make-lock) (bt:make-condition-variable)))

    (define (make-thread thunk . name)
      (new-thread thunk (if (pair? name) (car name) #f) 'new #f))

    ;; A thread not made by make-thread (the primordial thread, or one
    ;; the Lisp started) gets a thread object the first time it asks.
    (define (current-thread)
      (let ((native (bt:current-thread)))
        (or (lookup native)
            (let ((t (new-thread #f #f 'runnable native)))
              (register! native t)
              t))))

    (define (thread-start! t)
      (with-lock (thread-lock t)
        (lambda ()
          (unless (eq? (thread-state t) 'new)
            (error "thread-start!: thread already started" t))
          (set-thread-state! t 'runnable)))
      (set-thread-native! t (bt:make-thread (lambda () (run t))
                                            #:name (thread-label t)))
      t)

    (define (thread-label t)
      (let ((name (thread-name t)))
        (cond ((string? name) name)
              ((symbol? name) (symbol->string name))
              (else "SRFI 18 thread"))))

    (define (run t)
      (register! (bt:current-thread) t)
      (let ((ports (thread-ports t)))
        (parameterize ((current-input-port (car ports))
                       (current-output-port (cadr ports))
                       (current-error-port (car (cddr ports))))
          (call-with-current-continuation
           (lambda (k)
             (dynamic-wind
              (lambda () (set-thread-escape! t k))
              (lambda ()
                (let ((body (lambda ()
                              (guard (e (#t (finish! t 'uncaught e)))
                                (finish! t 'normal ((thread-thunk t))))))
                      (fail (lambda (c) (finish! t 'uncaught c))))
                  ;; Lisp's non-error serious conditions too.
                  (cl:handler-case (body)
                    (cl:serious-condition (c) (fail c)))))
              ;; Normal return, an uncaught exception, or termination.
              (lambda ()
                (set-thread-escape! t #f)
                (finish! t 'terminated #f)
                (unregister! (bt:current-thread))))))))
      #t)

    ;; Record how T ended (only the first call counts), mark it
    ;; terminated, abandon its mutexes and wake its joiners.
    (define (finish! t outcome result)
      (let ((owned '()))
        (with-lock (thread-lock t)
          (lambda ()
            (unless (thread-outcome t)
              (set-thread-outcome! t outcome)
              (set-thread-result! t result))
            (when (eq? outcome 'terminated)
              (set-thread-state! t 'terminated)
              (set! owned (thread-owned t))
              (set-thread-owned! t '())
              (bt:condition-broadcast (thread-done t)))))
        (for-each (lambda (m)
                    (with-lock (mutex-lock m)
                      (lambda ()
                        (when (eq? (mutex-owner m) t)
                          (set-mutex-owner! m #f)
                          (set-mutex-abandoned! m #t)
                          (bt:condition-broadcast (mutex-cv m))))))
                  owned)))

    (define (thread-yield!) (bt:thread-yield))

    (define (thread-sleep! timeout)
      (let ((d (deadline timeout)))
        (let loop ()
          (let ((left (- d (now))))
            (when (> left 0)
              (cl:sleep left)
              (loop))))))

    (define (thread-terminate! t)
      (let ((self? (eq? t (current-thread)))
            (escape #f)
            (state #f))
        (with-lock (thread-lock t)
          (lambda ()
            (set! state (thread-state t))
            (set! escape (thread-escape t))
            (case state
              ((new)
               (set-thread-state! t 'terminated)
               (set-thread-outcome! t 'terminated)
               (bt:condition-broadcast (thread-done t)))
              ((runnable)
               (unless (thread-outcome t)
                 (set-thread-outcome! t 'terminated))))))
        (cond ((not (eq? state 'runnable)))
              ((and self? escape) (escape #f))
              (self? (exit 0))
              (escape
               (guard (e (#t #f))       ; it may have finished meanwhile
                 (bt:interrupt-thread (thread-native t)
                                      (lambda ()
                                        (let ((k (thread-escape t)))
                                          (when k (k #f)))))))
              ((thread-native t)
               (guard (e (#t #f)) (bt:destroy-thread (thread-native t)))))
        (if #f #f)))

    (define (thread-join! t . opts)
      (let ((d (and (pair? opts) (deadline (car opts))))
            (timed-out? #f))
        (with-lock (thread-lock t)
          (lambda ()
            (let loop ()
              (unless (eq? (thread-state t) 'terminated)
                (if (wait (thread-done t) (thread-lock t) d)
                    (loop)
                    (set! timed-out? #t))))))
        (cond (timed-out?
               (if (and (pair? opts) (pair? (cdr opts)))
                   (cadr opts)
                   (raise (make-join-timeout-exception))))
              (else
               (case (thread-outcome t)
                 ((normal) (thread-result t))
                 ((uncaught) (raise (make-uncaught-exception (thread-result t))))
                 (else (raise (make-terminated-thread-exception))))))))

    ;; ------------------------------------------------------------
    ;; Mutexes

    (define-record-type srfi-18-mutex
      (%make-mutex name specific owner abandoned lock cv)
      mutex?
      (name mutex-name)
      (specific mutex-specific mutex-specific-set!)
      ;; a thread, not-owned (locked by no thread), or #f (unlocked)
      (owner mutex-owner set-mutex-owner!)
      (abandoned mutex-abandoned set-mutex-abandoned!)
      (lock mutex-lock)
      (cv mutex-cv))

    (define (make-mutex . name)
      (%make-mutex (if (pair? name) (car name) #f) #f #f #f
                   (bt:make-lock) (bt:make-condition-variable)))

    (define (mutex-state m)
      (let ((state #f))
        (with-lock (mutex-lock m)
          (lambda ()
            (set! state (cond ((mutex-owner m))
                              ((mutex-abandoned m) 'abandoned)
                              (else 'not-abandoned)))))
        state))

    (define (mutex-lock! m . opts)
      (let ((d (and (pair? opts) (deadline (car opts))))
            (owner (if (and (pair? opts) (pair? (cdr opts)))
                       (or (cadr opts) 'not-owned)
                       (current-thread)))
            (result #f)
            (abandoned? #f))
        (with-lock (mutex-lock m)
          (lambda ()
            (let loop ()
              (cond ((not (mutex-owner m))
                     (set! abandoned? (mutex-abandoned m))
                     (set-mutex-abandoned! m #f)
                     (set-mutex-owner! m owner)
                     (when (thread? owner)
                       (with-lock (thread-lock owner)
                         (lambda ()
                           (if (eq? (thread-state owner) 'terminated)
                               (begin (set-mutex-owner! m #f)
                                      (set-mutex-abandoned! m #t))
                               (set-thread-owned!
                                owner (cons m (thread-owned owner)))))))
                     (set! result #t))
                    ((wait (mutex-cv m) (mutex-lock m) d) (loop))))))
        (if abandoned?
            (raise (make-abandoned-mutex-exception))
            result)))

    (define (release! m)
      (with-lock (mutex-lock m)
        (lambda ()
          (let ((owner (mutex-owner m)))
            (when (thread? owner)
              (with-lock (thread-lock owner)
                (lambda ()
                  (set-thread-owned! owner (remove-mutex m (thread-owned owner)))))))
          (set-mutex-owner! m #f)
          (set-mutex-abandoned! m #f)
          (bt:condition-broadcast (mutex-cv m)))))

    (define (remove-mutex m ms)
      (cond ((null? ms) '())
            ((eq? (car ms) m) (cdr ms))
            (else (cons (car ms) (remove-mutex m (cdr ms))))))

    (define (mutex-unlock! m . opts)
      (if (and (pair? opts) (car opts))
          (let ((cv (car opts))
                (d (and (pair? (cdr opts)) (deadline (cadr opts))))
                (signalled? #f))
            (with-lock (cv-lock cv)
              (lambda ()
                (release! m)
                (set! signalled? (wait (cv-cv cv) (cv-lock cv) d))))
            signalled?)
          (begin (release! m) #t)))

    ;; ------------------------------------------------------------
    ;; Condition variables

    (define-record-type srfi-18-condition-variable
      (%make-condition-variable name specific lock cv)
      condition-variable?
      (name condition-variable-name)
      (specific condition-variable-specific condition-variable-specific-set!)
      (lock cv-lock)
      (cv cv-cv))

    (define (make-condition-variable . name)
      (%make-condition-variable (if (pair? name) (car name) #f) #f
                                (bt:make-lock) (bt:make-condition-variable)))

    (define (condition-variable-signal! cv)
      (with-lock (cv-lock cv) (lambda () (bt:condition-notify (cv-cv cv)))))

    (define (condition-variable-broadcast! cv)
      (with-lock (cv-lock cv) (lambda () (bt:condition-broadcast (cv-cv cv)))))))
