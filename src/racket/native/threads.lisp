; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

;;;; Racket's threads and synchronization, natively: threads, events and
;;;; sync, semaphores, channels, thread mailboxes, custodians, thread
;;;; groups, will executors, plumber flush handles, security guards,
;;;; subprocesses, and thread cells.  The semantics are those of Racket
;;;; CS's thread layer (racket/src/thread/): threads are green threads
;;;; on one OS thread, whose scheduler is not visible to the program.
;;;;
;;;; THE DESIGN.  A Racket thread is an SBCL thread, but only one runs
;;;; Racket code at a time: the one that holds the baton (*HOLDER*).  A
;;;; thread gives the baton up when it blocks (sync with nothing ready,
;;;; sleep, semaphore-wait, thread-wait, ...) and when it is preempted:
;;;; a ticker thread interrupts the holder every 10 ms (the interrupt
;;;; runs PREEMPT in the holder's own thread), which yields the baton
;;;; unless the thread is in an atomic section (its ATOMIC count is not
;;;; zero), is in the scheduler, or holds SBCL's world lock (a thread
;;;; that yielded inside COMPILE would deadlock the next one that
;;;; compiles).
;;;;
;;;; The scheduler (RESCHEDULE) is run by whichever thread gives the baton
;;;; up, under *SCHED-LOCK*: it finds the next thread to run, round
;;;; robin, by POLLING.  A blocked thread has a WAIT, which says what it
;;;; waits for (BRANCHES, one per event after choice-evt is flattened,
;;;; each with the wrappers to apply to its result) and until when; to
;;;; see whether it can go on, the scheduler polls the branches.  So a
;;;; semaphore that other code posts with INCF (ports.lisp does) wakes
;;;; its waiters without anyone telling the scheduler.  When no thread
;;;; can run, the giver-up sleeps (IDLE-WAIT) until the nearest deadline,
;;;; or two milliseconds if some event is external (sockets, ports, ...).
;;;; A new thread's SBCL thread starts waiting for the baton at once.
;;;;
;;;; Rendezvous (channels) are made by the second party to arrive, which
;;;; completes the first one's wait: each channel keeps queues of waiting
;;;; getters and putters, lazily pruned.  Semaphores keep a queue of
;;;; their waiters too, so that they're served first come, first served.
;;;;
;;;; Per thread: its Racket parameterization (a copy of every
;;;; parameter's current value, as thread cells are copied), its thread
;;;; cells, its mailbox, its break state; its continuation starts fresh
;;;; (CALL-WITH-CONTINUATION-BASE, the default prompt) with just the
;;;; creator's parameterization as a mark.  Killing a thread abandons its
;;;; continuation without running the posts of its dynamic-winds
;;;; (as Racket does): the killed thread unwinds, holding the baton, with
;;;; the winders marked as shared.
;;;;
;;;; Atomicity.  Racket's primitives are atomic with respect to other
;;;; Racket threads, ours are Lisp code that a preemption could interrupt
;;;; half way through; PROTECT-NATIVES (called once, at the end of this
;;;; file) wraps the natives of the shared mutable structures (hash
;;;; tables, ports, output) in WITH-ATOMIC.  Natives of mutable data that
;;;; other files add later should go through it too.
;;;;
;;;; What is approximate: see the notes on wills (no resurrection),
;;;; replace-evt (the other events of the sync are dropped once it
;;;; replaces), custodian memory limits (ignored), thread groups (no
;;;; scheduling weight), and breaks (delivered at preemption, blocking and
;;;; check points only).  A host call that blocks (a read from a
;;;; terminal) keeps the baton; our own ports should wait with WAIT-FOR.

(in-package "PSEUDOSCHEME-RACKET")

;;; ------------------------------------------------------------------
;;; Time

(defun epoch-ms ()
  "Milliseconds since the epoch, a double: current-inexact-milliseconds."
  (multiple-value-bind (s us) (sb-ext:get-time-of-day)
    (+ (* s 1000d0) (/ us 1000d0))))

(defun seconds->deadline (seconds)
  "The internal real time SECONDS from now."
  (+ (get-internal-real-time) (round (* seconds internal-time-units-per-second))))

(defun ms->deadline (ms)
  "The internal real time at epoch milliseconds MS."
  (+ (get-internal-real-time)
     (round (* (- ms (epoch-ms)) (/ internal-time-units-per-second 1000)))))

;;; ------------------------------------------------------------------
;;; Structures

(declaim (special *current-custodian* *current-thread-group*))

(defvar *sched-lock* (sb-thread:make-mutex :name "racket scheduler"))
(defvar *holder* *main-thread* "The thread that holds the baton.")
(defvar *threads* (list *main-thread*) "The live threads, in creation order.")
(defvar *in-sched* nil "True in a thread that is in the scheduler.")
(defvar *thread-ids* 0)
(defvar *ticker* nil)
(defvar *world-lock*
  (let ((s (find-symbol "**WORLD-LOCK**" "SB-C")))
    (and s (boundp s) (symbol-value s))))

(setf (rthread-state *main-thread*) :running)

(defstruct (rchannel (:constructor make-rchannel ()))
  (getters '())				; ((wait branch) ...), oldest first
  (putters '()))			; ((wait branch . value) ...)

(defstruct (custodian (:constructor make-custodian* (parent)))
  parent (children '()) (managed '()) (shutdown nil))

(defstruct (managed (:constructor make-managed (custodian object closer)))
  custodian object closer (live t))

(defstruct (custodian-box (:constructor make-custodian-box* (custodian value))) custodian value)
(defstruct (thread-group (:constructor make-thread-group* (parent))) parent (members '()))
(defstruct (security-guard (:constructor make-security-guard* (parent file network link)))
  parent file network link)
(defstruct (flush-handle (:constructor make-flush-handle (plumber proc weak))) plumber proc weak)
(defstruct (thread-cell-values (:constructor make-thread-cell-values (table))) table)

(defstruct (rwill-executor (:constructor make-rwill-executor ()))
  (ready '())				; entries finalized, newest first (atomic-push'd)
  (queue '()))				; entries to execute, oldest first

(defstruct (subprocess (:constructor make-subprocess* (process command)))
  process command (status nil))

;;; One synchronization: a thread's wait, and the branches of its events
(defstruct (branch (:constructor make-branch (leaf wraps groups))) leaf wraps groups)
(defstruct (wait (:constructor make-wait (thread branches)))
  thread branches (done nil) (chosen nil) (results nil)
  (deadline nil) (breakable nil) (enable-break nil) (external nil) (idle nil))

(defmethod print-object ((x rthread) stream) (write-string "#<thread>" stream))
(defmethod print-object ((x rsemaphore) stream) (write-string "#<semaphore>" stream))
(defmethod print-object ((x rchannel) stream) (write-string "#<channel>" stream))
(defmethod print-object ((x custodian) stream) (write-string "#<custodian>" stream))
(defmethod print-object ((x custodian-box) stream) (write-string "#<custodian-box>" stream))
(defmethod print-object ((x thread-group) stream) (write-string "#<thread-group>" stream))
(defmethod print-object ((x security-guard) stream) (write-string "#<security-guard>" stream))
(defmethod print-object ((x rwill-executor) stream) (write-string "#<will-executor>" stream))
(defmethod print-object ((x subprocess) stream)
  (format stream "#<subprocess:~A>" (subprocess-command x)))
(defmethod print-object ((x revt) stream)
  (format stream "#<~A>"
	  (case (revt-kind x)
	    (:always "always-evt") (:never "never-evt") (:peek "semaphore-peek-evt")
	    (:put "channel-put-evt") (:alarm "alarm-evt") (:dead "thread-dead-evt")
	    (:suspend "thread-suspend-evt") (:resume "thread-resume-evt")
	    (:receive "thread-receive-evt") (:idle "system-idle-evt")
	    (t "evt"))))

;;; ------------------------------------------------------------------
;;; Atomic sections

(defmacro with-atomic (&body body)
  "BODY without preemption."
  `(let ((self *self*))
     (incf (rthread-atomic self))
     (unwind-protect (progn ,@body)
       (decf (rthread-atomic self)))))

(defnative "unsafe-start-atomic" () (:instance "#%unsafe" :leaf t)
  (incf (rthread-atomic *self*)) *void*)
(defnative "unsafe-end-atomic" () (:instance "#%unsafe" :leaf t)
  (when (plusp (rthread-atomic *self*)) (decf (rthread-atomic *self*))) *void*)
(defnative "unsafe-start-breakable-atomic" () (:instance "#%unsafe" :leaf t)
  (incf (rthread-atomic *self*)) *void*)
(defnative "unsafe-end-breakable-atomic" () (:instance "#%unsafe" :leaf t)
  (when (plusp (rthread-atomic *self*)) (decf (rthread-atomic *self*))) *void*)
(defnative "unsafe-start-uninterruptible" () (:instance "#%unsafe" :leaf t)
  (incf (rthread-atomic *self*)) *void*)
(defnative "unsafe-end-uninterruptible" () (:instance "#%unsafe" :leaf t)
  (when (plusp (rthread-atomic *self*)) (decf (rthread-atomic *self*))) *void*)
(defnative "unsafe-in-atomic?" () (:instance "#%unsafe" :leaf t)
  (bool (plusp (rthread-atomic *self*))))

;;; The uninterruptible locks: holding one is being atomic
(defstruct (ulock (:constructor make-ulock ())) (holder nil))
(defnative "unsafe-make-uninterruptible-lock" () (:instance "#%unsafe" :leaf t) (make-ulock))
(defnative "unsafe-uninterruptible-lock-acquire" (l) (:instance "#%unsafe" :leaf t)
  (unless (eq (ulock-holder l) *self*)
    (incf (rthread-atomic *self*))
    (setf (ulock-holder l) *self*))
  *void*)
(defnative "unsafe-uninterruptible-lock-release" (l) (:instance "#%unsafe" :leaf t)
  (when (eq (ulock-holder l) *self*)
    (setf (ulock-holder l) nil)
    (when (plusp (rthread-atomic *self*)) (decf (rthread-atomic *self*))))
  *void*)
(defnative "unsafe-uninterruptible-custodian-lock-acquire" () (:instance "#%unsafe" :leaf t) *void*)
(defnative "unsafe-uninterruptible-custodian-lock-release" () (:instance "#%unsafe" :leaf t) *void*)

;;; ------------------------------------------------------------------
;;; Thread cells (the structure is control.lisp's): each thread has its
;;; own value of a cell, once it sets one; until then the cell's own

(defun cell-table (th)
  (or (rthread-cells th) (setf (rthread-cells th) (make-hash-table :test 'eq :weakness :key))))

(defun thread-cell-get (c)
  (let ((table (rthread-cells *self*)))
    (if table
	(multiple-value-bind (v found) (gethash c table)
	  (if found v (thread-cell-value c)))
	(thread-cell-value c))))

(defun thread-cell-put (c v)
  (setf (gethash c (cell-table *self*)) v))

(defun preserved-cell-values (th)
  "A table of TH's values of the preserved cells it has set."
  (let ((new (make-hash-table :test 'eq :weakness :key)))
    (when (rthread-cells th)
      (maphash (lambda (c v) (when (thread-cell-preserved c) (setf (gethash c new) v)))
	       (rthread-cells th)))
    new))

(defnative "make-thread-cell" (v &optional (preserved? ps:false)) (:leaf t)
  (make-thread-cell* v (truthy preserved?)))
(defnative "thread-cell?" (x) (:leaf t) (bool (thread-cell-p x)))
(defnative "thread-cell-ref" (c) (:leaf t)
  (unless (thread-cell-p c) (arg-error "thread-cell-ref" "thread-cell?" c))
  (thread-cell-get c))
(defnative "thread-cell-set!" (c v) (:leaf t)
  (unless (thread-cell-p c) (arg-error "thread-cell-set!" "thread-cell?" c))
  (thread-cell-put c v)
  *void*)
(defnative "thread-cell-values?" (x) (:leaf t) (bool (thread-cell-values-p x)))
(defnative "current-preserved-thread-cell-values" (&optional (v nil v-p)) (:leaf t)
  (cond ((not v-p) (make-thread-cell-values (preserved-cell-values *self*)))
	(t (unless (thread-cell-values-p v)
	     (arg-error "current-preserved-thread-cell-values" "thread-cell-values?" v))
	   (let ((table (cell-table *self*)))
	     ;; the preserved cells take V's values, or their initial ones
	     (let ((drop '()))
	       (maphash (lambda (c x) (declare (ignore x)) (when (thread-cell-preserved c) (push c drop))) table)
	       (dolist (c drop) (remhash c table)))
	     (maphash (lambda (c x) (setf (gethash c table) x)) (thread-cell-values-table v)))
	   *void*)))

;;; ------------------------------------------------------------------
;;; Breaks

(defvar *break-default-cell* (make-thread-cell* ps:true nil))

(defun current-break-cell ()
  (let ((c (first-mark *break-enabled-key* (current-marks) *break-default-cell*)))
    (if (thread-cell-p c) c *break-default-cell*)))

(defun breaks-enabled-p () (truthy (thread-cell-get (current-break-cell))))

(defun raise-break ()
  "Raise the break the current thread has pending."
  (let* ((self *self*) (kind (rthread-pending-break self)))
    (setf (rthread-pending-break self) nil)
    (do-raise (make-exn (case kind (:hang-up "exn:break:hang-up") (:terminate "exn:break:terminate") (t "exn:break"))
			(case kind (:hang-up "hang-up") (:terminate "terminate break") (t "user break"))
			(lambda (&rest args) (declare (ignore args)) *void*)))))

(defun check-pending-break ()
  (let ((self *self*))
    (when (and (rthread-pending-break self) (zerop (rthread-atomic self)) (breaks-enabled-p))
      (raise-break))))

(defnative "check-for-break" () () (check-pending-break) *void*)

(defnative "break-enabled" (&optional (on? nil on-p)) ()
  (cond ((not on-p) (bool (breaks-enabled-p)))
	(t (thread-cell-put (current-break-cell) (bool (truthy on?)))
	   (when (truthy on?) (check-pending-break))
	   *void*)))

(defnative "break-thread" (th &optional (kind ps:false)) ()
  (unless (rthread-p th) (arg-error "break-thread" "thread?" th))
  (let ((k (cond ((not (truthy kind)) :break)
		 ((and (racket-symbol-p kind) (string= (rname kind) "hang-up")) :hang-up)
		 ((and (racket-symbol-p kind) (string= (rname kind) "terminate")) :terminate)
		 (t (arg-error "break-thread" "(or/c #f 'hang-up 'terminate)" kind)))))
    (unless (rthread-dead th)
      (setf (rthread-pending-break th) k)
      (when (eq th *self*) (check-pending-break)))
    *void*))

;;; ------------------------------------------------------------------
;;; Waits and events

(defvar *prop-evt* (primitive "prop:evt"))

(defun evt-value-p (x)
  "Whether X is an event."
  (typecase x
    ((or rsemaphore revt rthread rchannel rwill-executor custodian-box subprocess rinport routport) t)
    (t (and (struct-instance-p x) (nth-value 1 (property-value x *prop-evt*))))))

(defnative "evt?" (x) (:leaf t) (bool (evt-value-p x)))

(defvar *user-peek-evts* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "The semaphore-peek-evts the program made: the others are port progress events.")

(defnative "semaphore-peek-evt" (s) (:leaf t)
  (unless (rsemaphore-p s) (arg-error "semaphore-peek-evt" "semaphore?" s))
  (let ((e (make-revt :peek s)))
    (setf (gethash e *user-peek-evts*) t)
    e))
(defnative "semaphore-peek-evt?" (x) (:leaf t) (bool (gethash x *user-peek-evts*)))
(defnative "progress-evt?" (x) (:leaf t)
  (bool (and (revt-p x) (eq (revt-kind x) :peek) (not (gethash x *user-peek-evts*)))))

(defun live-waiters (s)
  "S's waiters that still wait, pruned."
  (setf (rsemaphore-waiters s) (delete-if #'wait-done (rsemaphore-waiters s))))

(defun sema-poll (s wait)
  (when (plusp (rsemaphore-count s))
    (let ((first (car (live-waiters s))))
      (when (or (null first) (eq first wait))
	(decf (rsemaphore-count s))
	(return-from sema-poll (values (list s) t)))))
  (values nil nil))

(defun prune-queue (queue)
  (delete-if (lambda (entry) (wait-done (car entry))) queue))

(defun channel-get-poll (ch wait)
  "A getter: the oldest putter that isn't WAIT's own takes its value."
  (setf (rchannel-putters ch) (prune-queue (rchannel-putters ch)))
  (let ((entry (find-if (lambda (e) (and (not (eq (car e) wait))
					 (not (rthread-suspended (wait-thread (car e))))))
			(rchannel-putters ch))))
    (if entry
	(destructuring-bind (pwait pbranch . value) entry
	  (complete-wait pwait pbranch (list (branch-leaf pbranch)))
	  (values (list value) t))
	(values nil nil))))

(defun channel-put-poll (ch value wait)
  "A putter: the oldest getter that isn't WAIT's own gets the value."
  (setf (rchannel-getters ch) (prune-queue (rchannel-getters ch)))
  (let ((entry (find-if (lambda (e) (and (not (eq (car e) wait))
					 (not (rthread-suspended (wait-thread (car e))))))
			(rchannel-getters ch))))
    (if entry
	(destructuring-bind (gwait gbranch) entry
	  (complete-wait gwait gbranch (list value))
	  (values :put t))
	(values nil nil))))

(defvar *phase-two* nil "True while the scheduler asks the idle events.")

(defun revt-poll (e wait)
  "Whether the revt E is ready: its results (a list) and T."
  (flet ((ready (v) (values (list v) t)))
    (ecase (revt-kind e)
      (:always (ready e))
      (:never (values nil nil))
      (:peek (if (plusp (rsemaphore-count (revt-target e))) (ready e) (values nil nil)))
      (:thunk (ready (funcall (revt-target e))))
      (:log-receiver (multiple-value-bind (v r) (log-receiver-poll e)
		       (if r (ready v) (values nil nil))))
      (:poll (multiple-value-bind (v r) (funcall (revt-target e))
	       (if r (ready v) (values nil nil))))
      (:alarm (if (>= (epoch-ms) (revt-target e)) (ready e) (values nil nil)))
      (:dead (if (rthread-dead (revt-target e)) (ready e) (values nil nil)))
      (:suspend (let ((th (revt-target e)))
		  (if (and (rthread-suspended th) (not (rthread-dead th))) (ready th) (values nil nil))))
      (:resume (let ((th (revt-target e)))
		 (if (and (not (rthread-suspended th)) (not (rthread-dead th))) (ready th) (values nil nil))))
      (:receive (if (rthread-mailbox (revt-target e)) (ready e) (values nil nil)))
      (:idle (if (and *phase-two* (not (eq (wait-thread wait) nil))) (ready e) (values nil nil)))
      (:put (multiple-value-bind (v r) (channel-put-poll (car (revt-target e)) (cdr (revt-target e)) wait)
	      (declare (ignore v))
	      (if r (ready e) (values nil nil)))))))

(defun leaf-poll (leaf wait)
  "Whether LEAF is ready for WAIT: its results (a list) and T; the result of a
poll that succeeds is committed."
  (typecase leaf
    (rsemaphore (sema-poll leaf wait))
    (revt (revt-poll leaf wait))
    (rthread (if (rthread-dead leaf) (values (list leaf) t) (values nil nil)))
    (rchannel (multiple-value-bind (v r) (channel-get-poll leaf wait)
		(if r (values v t) (values nil nil))))
    (rwill-executor (if (poll-wills leaf) (values (list leaf) t) (values nil nil)))
    (custodian-box (if (custodian-shutdown (custodian-box-custodian leaf))
		       (values (list leaf) t) (values nil nil)))
    (subprocess (if (subprocess-done-p leaf) (values (list leaf) t) (values nil nil)))
    (rinport (if (ip-ready leaf) (values (list leaf) t) (values nil nil)))
    (t (values (list leaf) t))))

(defun external-leaf-p (leaf)
  (typecase leaf
    (revt (member (revt-kind leaf) '(:poll :thunk :log-receiver)))
    ((or rinport subprocess rwill-executor) t)
    (t nil)))

(defun complete-wait (wait branch results)
  "WAIT is over: BRANCH (or :timeout, :break, :killed) was chosen."
  (setf (wait-done wait) t (wait-chosen wait) branch (wait-results wait) results)
  (let ((th (wait-thread wait)))
    (when (eq (rthread-state th) :blocked) (setf (rthread-state th) :runnable))))

(defun poll-wait (wait)
  "Try WAIT's branches, from a random one: complete the wait with the first
ready one.  True if it's complete."
  (let* ((branches (wait-branches wait)) (n (length branches)))
    (when (plusp n)
      (let ((start (random n)))
	(dotimes (i n nil)
	  (let ((b (svref branches (mod (+ start i) n))))
	    (multiple-value-bind (results ready) (leaf-poll (branch-leaf b) wait)
	      (when ready
		(complete-wait wait b results)
		(return t)))))))))

(defun poll-blocked (th)
  "Whether the blocked thread TH can go on; completes its wait if so."
  (let ((w (rthread-wait th)))
    (cond ((null w) t)
	  ((wait-done w) (setf (rthread-state th) :runnable) t)
	  ((rthread-suspended th) nil)
	  ((and (wait-breakable w) (rthread-pending-break th) (zerop (rthread-atomic th)))
	   (complete-wait w :break nil) t)
	  ((poll-wait w) t)
	  ((and (wait-deadline w) (>= (get-internal-real-time) (wait-deadline w)))
	   (complete-wait w :timeout nil) t)
	  (t nil))))

;;; ------------------------------------------------------------------
;;; The scheduler

(defun candidate-p (th)
  (case (rthread-state th)
    (:runnable (or (rthread-zombie th) (not (rthread-suspended th))))
    (:blocked (poll-blocked th))
    (:running t)
    (t nil)))

(defun pick-next (self)
  "The next thread to run after SELF, round robin, or NIL if none can."
  (let* ((tail (member self *threads*))
	 (ring (if tail
		   (append (cdr tail) (ldiff *threads* tail) (list self))
		   *threads*)))
    (or (find-if #'candidate-p ring)
	;; nothing else can run: the idle events are ready
	(let ((*phase-two* t))
	  (find-if (lambda (th)
		     (and (eq (rthread-state th) :blocked)
			  (rthread-wait th)
			  (not (rthread-suspended th))
			  (poll-wait (rthread-wait th))))
		   ring)))))

(defun next-wakeup ()
  "Seconds the scheduler may sleep when no thread can run."
  (let ((now (get-internal-real-time)) (best 0.25d0))
    (dolist (th *threads*)
      (let ((w (rthread-wait th)))
	(when (and w (eq (rthread-state th) :blocked) (not (rthread-suspended th)))
	  (when (wait-external w) (setq best (min best 0.002d0)))
	  (when (wait-deadline w)
	    (setq best (min best (max 0.0002d0 (/ (- (wait-deadline w) now) internal-time-units-per-second))))))))
    best))

(defun idle-wait ()
  "No thread can run: sleep (the lock is released meanwhile), then try again."
  (let ((dt (next-wakeup)))
    (sb-thread:release-mutex *sched-lock*)
    (sleep dt)
    (sb-thread:grab-mutex *sched-lock*)))

(defun hand-baton (next)
  (setf (rthread-state next) :running
	*holder* next)
  (sb-thread:condition-broadcast (rthread-cv next)))

(defun exit-thread-unwinding ()
  "Leave the current thread without running its dynamic-winds' posts."
  (setf psx::*shared-winders* psx::*winders*)
  (throw 'rthread-exit nil))

(defun reschedule (self)
  "SELF has made itself :runnable or :blocked: run others until it's chosen."
  (let ((*in-sched* t))
    (sb-thread:with-mutex (*sched-lock*)
      (loop
	(let ((next (pick-next self)))
	  (cond (next
		 (hand-baton next)
		 (unless (eq next self)
		   (loop until (eq *holder* self)
			 do (sb-thread:condition-wait (rthread-cv self) *sched-lock*)))
		 (return))
		(t (idle-wait)))))))
  (when (rthread-zombie self) (exit-thread-unwinding)))

(defun yield-thread ()
  "Let the other threads that can run, run."
  (let ((self *self*))
    (when (cdr *threads*)
      (setf (rthread-state self) :runnable)
      (reschedule self))))

(defun preempt ()
  "Run in the holder's thread by the ticker's interrupt."
  (let ((self *self*))
    (setf (rthread-preempt-queued self) nil)
    (when (and (eq *holder* self) (not *in-sched*) (zerop (rthread-atomic self))
	       (not (and *world-lock* (sb-thread:holding-mutex-p *world-lock*)))
	       (cdr *threads*))
      ;; the interrupt runs with interrupts off; the thread may wait a long time
      (sb-sys:with-interrupts
	(yield-thread)
	(check-pending-break)))))

(defun ticker-main ()
  (loop
    (sleep 0.01)
    (let ((h *holder*))
      (when (and h (rthread-lisp h) (cdr *threads*)
		 (not (rthread-preempt-queued h))
		 (zerop (rthread-atomic h)))
	(setf (rthread-preempt-queued h) t)
	(handler-case (sb-thread:interrupt-thread (rthread-lisp h) #'preempt)
	  (error () (setf (rthread-preempt-queued h) nil)))))))

(defun exit-hook ()
  "The process is exiting: the other threads die without unwinding."
  (when (cdr *threads*)
    (ignore-errors (finish-output *standard-output*))
    (ignore-errors (finish-output *error-output*))
    (let* ((s (find-symbol "*EXIT-IN-PROGRESS*" "SB-IMPL"))
	   (code (if (and s (boundp s) (integerp (symbol-value s))) (symbol-value s) 0)))
      (sb-ext:exit :code code :abort t))))

(defvar *scheduler-started* nil)

(defun ensure-scheduler ()
  "At the first thread: note the main thread's SBCL thread, start the ticker."
  (unless *scheduler-started*
    (setq *scheduler-started* t)
    (setf (rthread-lisp *main-thread*) sb-thread:*current-thread*)
    ;; the stacks of threads: the main one's is huge
    (ignore-errors
     (setf (sb-alien:extern-alien "thread_control_stack_size" sb-alien:unsigned-long)
	   (* 64 1024 1024)))
    (pushnew 'exit-hook sb-ext:*exit-hooks*)
    (setq *ticker* (sb-thread:make-thread #'ticker-main :name "racket ticker"))))

;;; ------------------------------------------------------------------
;;; Creating and finishing threads

(defparameter *thread-specials*
  '(*standard-input* *standard-output* *error-output* *terminal-io* *query-io*
    *default-pathname-defaults* *package* *readtable* *read-default-float-format*
    *print-pretty*)
  "The Lisp specials a new thread starts with the creator's values of.")

(defun parameterization-snapshot ()
  "A parameterization with a fresh cell for every parameter, holding its value now."
  (let ((cells '()))
    (maphash (lambda (p data)
	       (declare (ignore p))
	       (push (cons data (list (car (parameter-cell data)))) cells))
	     *parameters*)
    (make-parameterization cells)))

(defun acquire-baton (me)
  (sb-thread:with-mutex (*sched-lock*)
    (loop until (eq *holder* me)
	  do (sb-thread:condition-wait (rthread-cv me) *sched-lock*))))

(defun thread-finished (me)
  "ME is done: it leaves the threads, and the baton goes on."
  (setf (rthread-dead me) t (rthread-state me) :done (rthread-mailbox me) '())
  (dolist (m (rthread-custodians me))
    (setf (managed-live m) nil)
    (setf (custodian-managed (managed-custodian m)) (delete m (custodian-managed (managed-custodian m)))))
  (setf (rthread-custodians me) '())
  (let ((g (rthread-group me)))
    (when g (setf (thread-group-members g) (delete me (thread-group-members g)))))
  (setq *threads* (delete me *threads*))
  (let ((*in-sched* t))
    (sb-thread:with-mutex (*sched-lock*)
      (loop
	(let ((next (pick-next me)))
	  (cond (next (hand-baton next) (return))
		(t (idle-wait))))))))

(defun run-thread (me thunk values marks)
  "The body of a Racket thread's SBCL thread."
  (ps:disable-float-traps)
  (progv *thread-specials* values
    (let ((*self* me) (*in-sched* nil) (*uncaught* nil) (ps-r6rs::*marks* marks))
      (let ((*in-sched* t)) (acquire-baton me))
      (unwind-protect
	   (catch 'rthread-exit
	     (unless (rthread-zombie me)
	       (psx::with-thread-state
		 (with-racket-errors
		   (psx::call-with-continuation-base
		    (lambda ()
		      (call-with-continuation-prompt
		       (lambda () (check-pending-break) (funcall thunk))
		       *default-prompt-tag* nil)))))))
	(thread-finished me)))))

(defun custodian-register (c object closer)
  "A record that C manages OBJECT, which CLOSER (called with it) closes at
the custodian's shutdown.  Raises if C is shut down."
  (let ((m (make-managed c object closer)))
    (push m (custodian-managed c))
    m))

(defun custodian-unregister (m)
  (when m
    (setf (managed-live m) nil)
    (let ((c (managed-custodian m)))
      (setf (custodian-managed c) (delete m (custodian-managed c))))))

(defun check-custodian-live (who c)
  (when (custodian-shutdown c)
    (raise-exn "exn:fail:contract"
	       (format nil "~A: the custodian has been shut down~%  custodian: #<custodian>" who))))

(defun create-thread (who thunk &optional suspend-to-kill)
  (let ((c (funcall *current-custodian*)))
    (check-custodian-live who c)
    (ensure-scheduler)
    (let* ((creator *self*)
	   (me (make-rthread nil))
	   (group (funcall *current-thread-group*))
	   (param (parameterization-snapshot)))
      (setf (rthread-id me) (incf *thread-ids*)
	    (rthread-cells me) (preserved-cell-values creator)
	    (rthread-suspend-to-kill me) suspend-to-kill
	    (rthread-group me) (and (thread-group-p group) group))
      ;; it starts with the creator's break state
      (setf (gethash *break-default-cell* (cell-table me)) (bool (breaks-enabled-p)))
      (when (rthread-group me) (push me (thread-group-members group)))
      (push (custodian-register c me nil) (rthread-custodians me))
      (let ((values (mapcar #'symbol-value *thread-specials*))
	    (marks (list (list* -1 *parameterization-key* param))))
	(setf (rthread-lisp me)
	      (sb-thread:make-thread (lambda () (run-thread me thunk values marks))
				     :name "racket thread")))
      (setq *threads* (append *threads* (list me)))
      me)))

;; the second argument: a parallel-thread pool (racket/base's #:pool), or #f;
;; threads here are never parallel
(defnative "thread" (thunk &optional pool) ()
  (declare (ignore pool))
  (unless (accepts-arity-p thunk 0) (arg-error "thread" "(procedure-arity-includes/c 0)" thunk))
  (create-thread "thread" thunk))

(defnative "thread/suspend-to-kill" (thunk) ()
  (unless (accepts-arity-p thunk 0)
    (arg-error "thread/suspend-to-kill" "(procedure-arity-includes/c 0)" thunk))
  (create-thread "thread/suspend-to-kill" thunk t))

(defnative "current-thread" () (:leaf t) *self*)
(defnative "thread?" (x) (:leaf t) (bool (rthread-p x)))
(defnative "get-thread-id" (th) (:leaf t)
  (unless (rthread-p th) (arg-error "get-thread-id" "thread?" th))
  (rthread-id th))

(defun check-thread (who x)
  (unless (rthread-p x) (arg-error who "thread?" x))
  x)

(defnative "thread-dead?" (th) (:leaf t)
  (check-thread "thread-dead?" th)
  (bool (rthread-dead th)))
(defnative "thread-running?" (th) (:leaf t)
  (check-thread "thread-running?" th)
  (bool (and (not (rthread-dead th)) (not (rthread-suspended th)))))

(defnative "thread-wait" (th) ()
  (check-thread "thread-wait" th)
  (do-sync "thread-wait" (list th) ps:false nil)
  *void*)

(defun kill-thread* (th)
  "Kill TH (not running Racket code of its own)."
  (cond ((rthread-dead th) nil)
	((rthread-suspend-to-kill th)
	 (setf (rthread-suspended th) t)
	 (when (eq th *self*) (yield-thread)))
	((eq th *main-thread*)
	 (ignore-errors (finish-output *standard-output*))
	 (ignore-errors (finish-output *error-output*))
	 (uiop:quit 0))
	((eq th *self*)
	 (setf (rthread-dead th) t (rthread-zombie th) t)
	 (exit-thread-unwinding))
	(t
	 (setf (rthread-dead th) t (rthread-zombie th) t (rthread-suspended th) nil)
	 (let ((w (rthread-wait th)))
	   (when (and w (not (wait-done w))) (complete-wait w :killed nil)))
	 (when (eq (rthread-state th) :blocked) (setf (rthread-state th) :runnable)))))

(defnative "kill-thread" (th) ()
  (check-thread "kill-thread" th)
  (kill-thread* th)
  *void*)

(defnative "thread-suspend" (th) ()
  (check-thread "thread-suspend" th)
  (unless (rthread-dead th)
    (setf (rthread-suspended th) t)
    (when (eq th *self*) (yield-thread)))
  *void*)

(defnative "thread-resume" (th &optional (benefactor ps:false)) ()
  (check-thread "thread-resume" th)
  (unless (or (rthread-dead th) (not (rthread-suspended th)))
    (setf (rthread-suspended th) nil))
  (when (and (custodian-p benefactor) (not (rthread-dead th))
	     (notany #'managed-live (rthread-custodians th)))
    (push (custodian-register benefactor th nil) (rthread-custodians th)))
  *void*)

(defnative "thread-suspend-evt" (th) (:leaf t)
  (check-thread "thread-suspend-evt" th) (make-revt :suspend th))
(defnative "thread-resume-evt" (th) (:leaf t)
  (check-thread "thread-resume-evt" th) (make-revt :resume th))
(defnative "thread-dead-evt" (th) (:leaf t)
  (check-thread "thread-dead-evt" th) (make-revt :dead th))

;;; Mailboxes
(defnative "thread-send" (th v &optional (fail ps:false)) ()
  (check-thread "thread-send" th)
  (cond ((rthread-dead th)
	 (cond ((truthy fail) (if (functionp fail) (funcall fail) fail))
	       (t (raise-exn "exn:fail:contract" "thread-send: target thread is not running"))))
	(t (setf (rthread-mailbox th) (append (rthread-mailbox th) (list v)))
	   *void*)))

(defnative "thread-receive" () ()
  (loop
    (let ((self *self*))
      (when (rthread-mailbox self) (return (pop (rthread-mailbox self))))
      (do-sync "thread-receive" (list (make-revt :receive self)) ps:false nil))))

(defnative "thread-try-receive" () (:leaf t)
  (let ((self *self*))
    (if (rthread-mailbox self) (pop (rthread-mailbox self)) ps:false)))

(defnative "thread-rewind-receive" (lst) (:leaf t)
  (unless (listp lst) (arg-error "thread-rewind-receive" "list?" lst))
  (dolist (x lst) (push x (rthread-mailbox *self*)))
  *void*)

(defnative "thread-receive-evt" () (:leaf t) (make-revt :receive *self*))

(defnative "call-in-nested-thread" (thunk &optional (cust ps:false)) ()
  (unless (accepts-arity-p thunk 0)
    (arg-error "call-in-nested-thread" "(procedure-arity-includes/c 0)" thunk))
  (let* ((result nil) (finished nil)
	 (th (let ((*current-custodian* (if (custodian-p cust)
					    (lambda (&rest args) (declare (ignore args)) cust)
					    *current-custodian*)))
	       (create-thread "call-in-nested-thread"
			      (lambda ()
				(setq result (multiple-value-list (funcall thunk)) finished t))))))
    (do-sync "call-in-nested-thread" (list th) ps:false nil)
    (unless finished
      (raise-exn "exn:fail:contract"
		 "call-in-nested-thread: the thread was killed, or it exited via the default error escape handler"))
    (values-list result)))

(defnative "sleep" (&optional (secs 0)) ()
  (unless (and (realp secs) (>= secs 0)) (arg-error "sleep" "(>=/c 0)" secs))
  (if (zerop secs)
      (progn (yield-thread) (check-pending-break))
      (do-sync "sleep" '() secs nil))
  *void*)

(defnative "threaded?" () (:leaf t) ps:true)

;;; ------------------------------------------------------------------
;;; Sync

(defun expand-evt (e who poll groups wraps)
  "E's branches: choice-evts flattened, guards called, wrappers noted."
  (typecase e
    (revt
     (case (revt-kind e)
       (:choice (loop for x in (revt-target e) append (expand-evt x who poll groups wraps)))
       (:wrap (destructuring-bind (inner proc . handle) (revt-target e)
		(expand-evt inner who poll groups (cons (cons (if handle :handle :wrap) proc) wraps))))
       (:replace (destructuring-bind (inner . proc) (revt-target e)
		   (expand-evt inner who poll groups (cons (cons :replace proc) wraps))))
       (:guard (expand-guard (funcall (revt-target e)) who poll groups wraps))
       (:poll-guard (expand-guard (funcall (revt-target e) (bool poll)) who poll groups wraps))
       (:nack-guard
	(let* ((sema (make-rsemaphore 0))
	       (nack (make-revt :peek sema)))
	  (expand-guard (funcall (revt-target e) nack) who poll (cons sema groups) wraps)))
       (t (list (make-branch e wraps groups)))))
    (t (cond ((not (evt-value-p e)) (arg-error who "evt?" e))
	     ((struct-instance-p e)
	      (let ((v (property-value e *prop-evt*)))
		(cond ((evt-value-p v) (expand-evt v who poll groups wraps))
		      ((functionp v) (expand-guard (funcall v e) who poll groups wraps e))
		      ;; a field: an event, or a procedure of the struct (racket/base's
		      ;; guard-evt is such a struct)
		      ((integerp v)
		       (let ((f (svref (struct-fields e) v)))
			 (cond ((evt-value-p f) (expand-evt f who poll groups wraps))
			       ((functionp f) (expand-guard (funcall f e) who poll groups wraps e))
			       (t (list (make-branch (make-revt :never) wraps groups))))))
		      (t (list (make-branch (make-revt :never) wraps groups))))))
	     (t (list (make-branch e wraps groups)))))))

(defun expand-guard (r who poll groups wraps &optional default)
  "The branches of what a guard produced: an event, or else a value that is ready."
  (if (evt-value-p r)
      (expand-evt r who poll groups wraps)
      (let ((v (if default default r)))
	(list (make-branch (make-revt :thunk (lambda () v)) wraps groups)))))

(defun check-sync-timeout (who timeout)
  (unless (or (not (truthy timeout)) (functionp timeout) (and (realp timeout) (>= timeout 0)))
    (arg-error who "(or/c #f (and/c real? (not/c negative?)) (-> any))" timeout)))

(defun enroll (wait)
  "Put WAIT in the queues of the semaphores and channels it waits for."
  (let ((external nil) (deadline (wait-deadline wait)))
    (loop for b across (wait-branches wait)
	  for leaf = (branch-leaf b)
	  do (typecase leaf
	       (rsemaphore (live-waiters leaf)
			   (setf (rsemaphore-waiters leaf) (append (rsemaphore-waiters leaf) (list wait))))
	       (rchannel (setf (rchannel-getters leaf)
			       (append (prune-queue (rchannel-getters leaf)) (list (list wait b)))))
	       (revt (case (revt-kind leaf)
		       (:put (let ((ch (car (revt-target leaf))))
			       (setf (rchannel-putters ch)
				     (append (prune-queue (rchannel-putters ch))
					     (list (list* wait b (cdr (revt-target leaf))))))))
		       (:alarm (let ((d (ms->deadline (revt-target leaf))))
				 (setq deadline (if deadline (min deadline d) d))))
		       (:idle (setf (wait-idle wait) t))))
	       (t nil))
	     (when (external-leaf-p leaf) (setq external t)))
    (setf (wait-external wait) external
	  (wait-deadline wait) deadline)))

(defun block-on-wait (wait)
  (let ((self *self*))
    (setf (wait-breakable wait) (or (wait-enable-break wait) (breaks-enabled-p)))
    (enroll wait)
    (setf (rthread-wait self) wait
	  (rthread-state self) :blocked)
    (reschedule self)
    (setf (rthread-wait self) nil)))

(defun post-nacks (wait chosen)
  "Post the nack of each guard whose events weren't chosen."
  (let ((keep (and chosen (branch-groups chosen))) (done '()))
    (loop for b across (wait-branches wait)
	  do (dolist (g (branch-groups b))
	       (unless (or (member g keep :test #'eq) (member g done :test #'eq))
		 (push g done)
		 (incf (rsemaphore-count g)))))))

(defun apply-wraps (wraps results)
  (loop
    (when (null wraps) (return (values-list results)))
    (let ((w (pop wraps)))
      (ecase (car w)
	((:wrap :handle)
	 (if (null wraps)
	     (return (apply (cdr w) results))
	     (setq results (multiple-value-list (apply (cdr w) results)))))
	(:replace
	 (let ((new (apply (cdr w) results)))
	   (setq results (multiple-value-list (do-sync "sync" (list new) ps:false nil)))))))))

(defun finish-sync (wait chosen results)
  (post-nacks wait chosen)
  (apply-wraps (branch-wraps chosen) results))

(defun finish-timeout (wait timeout)
  (post-nacks wait nil)
  (if (functionp timeout) (funcall timeout) ps:false))

(defun do-sync (who evts timeout enable-break)
  "sync on EVTS (a list), with TIMEOUT (#f, seconds, or a thunk)."
  (check-sync-timeout who timeout)
  ;; a thunk as the timeout: poll once, and call it if nothing is ready
  (let* ((poll (or (and (realp timeout) (zerop timeout)) (functionp timeout)))
	 (branches (loop for e in evts append (expand-evt e who poll '() '())))
	 (wait (make-wait *self* (coerce branches 'simple-vector))))
    (setf (wait-enable-break wait) enable-break)
    (when enable-break (check-pending-break*))
    (cond ((poll-wait wait)
	   (finish-sync wait (wait-chosen wait) (wait-results wait)))
	  (poll (finish-timeout wait timeout))
	  (t
	   (when (realp timeout) (setf (wait-deadline wait) (seconds->deadline timeout)))
	   (block-on-wait wait)
	   (let ((chosen (wait-chosen wait)))
	     (case chosen
	       (:timeout (finish-timeout wait timeout))
	       (:break (post-nacks wait nil) (raise-break))
	       (:killed (exit-thread-unwinding))
	       (t (finish-sync wait chosen (wait-results wait)))))))))

(defun check-pending-break* ()
  "A pending break, in a sync that enables them."
  (let ((self *self*))
    (when (and (rthread-pending-break self) (zerop (rthread-atomic self)))
      (raise-break))))

(defun sync-evts (evts &optional timeout)
  "sync on the Lisp list EVTS: for the Lisp code that waits."
  (do-sync "sync" evts (or timeout ps:false) nil))

(defun wait-for (poll-thunk &optional timeout)
  "Block the thread until POLL-THUNK returns true, which the scheduler asks
while other threads run.  With TIMEOUT (seconds), true, or NIL if it passes."
  (let ((r (do-sync "sync"
		    (list (make-revt :poll (lambda () (if (funcall poll-thunk) (values t t) (values nil nil)))))
		    (or timeout ps:false) nil)))
    (and (truthy r) t)))

(defun evt-ready (e)
  "E's sync result (its first value), and whether it's ready: a poll."
  (let* ((wait (make-wait *self* (coerce (expand-evt e "sync" t '() '()) 'simple-vector))))
    (if (poll-wait wait)
	(let ((v (multiple-value-list (finish-sync wait (wait-chosen wait) (wait-results wait)))))
	  (values (car v) t))
	(values nil nil))))

(defnative "sync" (&rest evts) () (do-sync "sync" evts ps:false nil))
(defnative "sync/timeout" (timeout &rest evts) () (do-sync "sync/timeout" evts timeout nil))
(defnative "sync/enable-break" (&rest evts) () (do-sync "sync/enable-break" evts ps:false t))
(defnative "sync/timeout/enable-break" (timeout &rest evts) ()
  (do-sync "sync/timeout/enable-break" evts timeout t))

;;; Event combinators
(defun check-evt (who e) (unless (evt-value-p e) (arg-error who "evt?" e)) e)

(defnative "choice-evt" (&rest evts) (:leaf t)
  (dolist (e evts) (check-evt "choice-evt" e))
  (make-revt :choice evts))
(defnative "wrap-evt" (e proc) (:leaf t)
  (check-evt "wrap-evt" e)
  (unless (functionp proc) (arg-error "wrap-evt" "procedure?" proc))
  (make-revt :wrap (list* e proc nil)))
(defnative "handle-evt" (e proc) (:leaf t)
  (check-evt "handle-evt" e)
  (unless (functionp proc) (arg-error "handle-evt" "procedure?" proc))
  (make-revt :wrap (list* e proc t)))
(defnative "handle-evt?" (x) (:leaf t)
  (bool (and (revt-p x) (eq (revt-kind x) :wrap) (cddr (revt-target x)))))
(defnative "replace-evt" (e proc) (:leaf t)
  (check-evt "replace-evt" e)
  (unless (functionp proc) (arg-error "replace-evt" "procedure?" proc))
  (make-revt :replace (cons e proc)))
(defnative "nack-guard-evt" (proc) (:leaf t)
  (unless (accepts-arity-p proc 1) (arg-error "nack-guard-evt" "(procedure-arity-includes/c 1)" proc))
  (make-revt :nack-guard proc))
(defnative "poll-guard-evt" (proc) (:leaf t)
  (unless (accepts-arity-p proc 1) (arg-error "poll-guard-evt" "(procedure-arity-includes/c 1)" proc))
  (make-revt :poll-guard proc))
(defnative "guard-evt" (proc) (:leaf t)
  (unless (accepts-arity-p proc 0) (arg-error "guard-evt" "(procedure-arity-includes/c 0)" proc))
  (make-revt :guard proc))
(defnative "alarm-evt" (ms) (:leaf t)
  (unless (realp ms) (arg-error "create-alarm-evt" "real?" ms))
  (make-revt :alarm ms))
(defvar *idle-evt* (make-revt :idle))
(defnative "system-idle-evt" () (:leaf t) *idle-evt*)

;;; ------------------------------------------------------------------
;;; Semaphores

(defun check-sema (who s) (unless (rsemaphore-p s) (arg-error who "semaphore?" s)) s)

(defnative "make-semaphore" (&optional (n 0)) (:leaf t)
  (unless (and (integerp n) (>= n 0)) (arg-error "make-semaphore" "exact-nonnegative-integer?" n))
  (make-rsemaphore n))
(defnative "semaphore?" (x) (:leaf t) (bool (rsemaphore-p x)))
(defnative "semaphore-post" (s) (:leaf t)
  (check-sema "semaphore-post" s)
  (incf (rsemaphore-count s))
  *void*)
(defnative "semaphore-try-wait?" (s) (:leaf t)
  (check-sema "semaphore-try-wait?" s)
  (if (and (plusp (rsemaphore-count s)) (null (live-waiters s)))
      (progn (decf (rsemaphore-count s)) ps:true)
      ps:false))

(defun semaphore-wait* (who s enable-break)
  (check-sema who s)
  (if (and (plusp (rsemaphore-count s)) (null (live-waiters s)) (not enable-break))
      (decf (rsemaphore-count s))
      (do-sync who (list s) ps:false enable-break))
  *void*)

(defnative "semaphore-wait" (s) () (semaphore-wait* "semaphore-wait" s nil))
(defnative "semaphore-wait/enable-break" (s) () (semaphore-wait* "semaphore-wait/enable-break" s t))

(defun do-call-with-semaphore (who s proc try-fail args enable-break)
  (check-sema who s)
  (unless (functionp proc) (arg-error who "procedure?" proc))
  (let ((got nil) (results nil))
    (funcall (psx:host-ref "dynamic-wind")
	     (lambda ()
	       (if (truthy try-fail)
		   (when (and (plusp (rsemaphore-count s)) (null (live-waiters s)))
		     (decf (rsemaphore-count s))
		     (setq got t))
		   (progn (semaphore-wait* who s enable-break) (setq got t))))
	     (lambda ()
	       (when got (setq results (multiple-value-list (apply proc args)))))
	     (lambda () (when got (incf (rsemaphore-count s)))))
    (if got (values-list results) (funcall try-fail))))

(defnative "call-with-semaphore" (s proc &optional (try-fail ps:false) &rest args) ()
  (do-call-with-semaphore "call-with-semaphore" s proc try-fail args nil))
(defnative "call-with-semaphore/enable-break" (s proc &optional (try-fail ps:false) &rest args) ()
  (do-call-with-semaphore "call-with-semaphore/enable-break" s proc try-fail args t))

;;; ------------------------------------------------------------------
;;; Channels

(defnative "make-channel" () (:leaf t) (make-rchannel))
(defnative "channel?" (x) (:leaf t) (bool (rchannel-p x)))
(defnative "channel-put-evt" (ch v) (:leaf t)
  (unless (rchannel-p ch) (arg-error "channel-put-evt" "channel?" ch))
  (make-revt :put (cons ch v)))
(defnative "channel-put-evt?" (x) (:leaf t) (bool (and (revt-p x) (eq (revt-kind x) :put))))

;;; ------------------------------------------------------------------
;;; Custodians

(defvar *root-custodian* (make-custodian* nil))

(defvar *current-custodian*
  (make-parameter* *root-custodian*
		   (guard-check "current-custodian" #'custodian-p "custodian?")
		   (rsym "current-custodian")))
(define-primitive "#%kernel" "current-custodian" *current-custodian*)

(defnative "make-custodian" (&optional (parent nil parent-p)) ()
  (let ((parent (if parent-p parent (funcall *current-custodian*))))
    (unless (custodian-p parent) (arg-error "make-custodian" "custodian?" parent))
    (check-custodian-live "make-custodian" parent)
    (let ((c (make-custodian* parent)))
      (push c (custodian-children parent))
      c)))
(defnative "custodian?" (x) (:leaf t) (bool (custodian-p x)))
(defnative "custodian-shut-down?" (c) (:leaf t)
  (unless (custodian-p c) (arg-error "custodian-shut-down?" "custodian?" c))
  (bool (custodian-shutdown c)))

(defun shutdown-custodian (c)
  (unless (custodian-shutdown c)
    (setf (custodian-shutdown c) t)
    (dolist (child (reverse (custodian-children c))) (shutdown-custodian child))
    (setf (custodian-children c) '())
    (let ((kill-self nil))
      (dolist (m (reverse (custodian-managed c)))
	(when (managed-live m)
	  (setf (managed-live m) nil)
	  (let ((o (managed-object m)))
	    (cond ((rthread-p o)
		   (setf (rthread-custodians o) (delete m (rthread-custodians o)))
		   (when (notany #'managed-live (rthread-custodians o))
		     (if (eq o *self*) (setq kill-self t) (kill-thread* o))))
		  ((managed-closer m)
		   (let ((cb (managed-closer m)))
		     (if (accepts-arity-p cb 2) (funcall cb o c) (funcall cb o))))))))
      (setf (custodian-managed c) '())
      (when kill-self (kill-thread* *self*)))
    (when (custodian-parent c)
      (setf (custodian-children (custodian-parent c)) (delete c (custodian-children (custodian-parent c)))))))

(defnative "custodian-shutdown-all" (c) ()
  (unless (custodian-p c) (arg-error "custodian-shutdown-all" "custodian?" c))
  (shutdown-custodian c)
  *void*)

(defun custodian-ancestor-p (super c)
  (loop for x = (custodian-parent c) then (custodian-parent x)
	while x thereis (eq x super)))

(defnative "custodian-managed-list" (c super) (:leaf t)
  (unless (custodian-p c) (arg-error "custodian-managed-list" "custodian?" c))
  (unless (custodian-p super) (arg-error "custodian-managed-list" "custodian?" super))
  (unless (custodian-ancestor-p super c)
    (raise-exn "exn:fail:contract"
	       (format nil "custodian-managed-list: the second custodian does not manage the first custodian~%  first custodian: #<custodian>~%  second custodian: #<custodian>")))
  (append (copy-list (custodian-children c))
	  (loop for m in (custodian-managed c)
		when (managed-live m) collect (managed-object m))))

(defnative "custodian-limit-memory" (c limit &optional (stop ps:false)) (:leaf t)
  (declare (ignore limit stop))
  (unless (custodian-p c) (arg-error "custodian-limit-memory" "custodian?" c))
  *void*)
(defnative "custodian-require-memory" (limit-cust need stop-cust) (:leaf t)
  (declare (ignore limit-cust need stop-cust))
  *void*)
(defnative "custodian-memory-accounting-available?" () (:leaf t) ps:false)

(defnative "make-custodian-box" (c v) (:leaf t)
  (unless (custodian-p c) (arg-error "make-custodian-box" "custodian?" c))
  (make-custodian-box* c v))
(defnative "custodian-box?" (x) (:leaf t) (bool (custodian-box-p x)))
(defnative "custodian-box-value" (b) (:leaf t)
  (unless (custodian-box-p b) (arg-error "custodian-box-value" "custodian-box?" b))
  (if (custodian-shutdown (custodian-box-custodian b)) ps:false (custodian-box-value b)))

(defnative "unsafe-custodian-register" (c obj callback &optional at-exit? weak? late?) (:instance "#%unsafe" :leaf t)
  (declare (ignore at-exit? weak? late?))
  (if (custodian-shutdown c) ps:false (custodian-register c obj callback)))
(defnative "unsafe-custodian-unregister" (obj ref) (:instance "#%unsafe" :leaf t)
  (declare (ignore obj))
  (when (managed-p ref) (custodian-unregister ref))
  *void*)
(defnative "unsafe-add-post-custodian-shutdown" (&rest args) (:instance "#%unsafe" :leaf t)
  (declare (ignore args)) *void*)
(defnative "unsafe-make-custodian-at-root" () (:instance "#%unsafe" :leaf t)
  (let ((c (make-custodian* *root-custodian*)))
    (push c (custodian-children *root-custodian*))
    c))
(defnative "unsafe-thread-at-root" (thunk) (:instance "#%unsafe")
  (let ((*current-custodian* (lambda (&rest args) (declare (ignore args)) *root-custodian*)))
    (create-thread "thread" thunk)))

;;; ------------------------------------------------------------------
;;; Thread groups

(defvar *root-thread-group* (make-thread-group* nil))
(defvar *current-thread-group*
  (make-parameter* *root-thread-group*
		   (guard-check "current-thread-group" #'thread-group-p "thread-group?")
		   (rsym "current-thread-group")))
(define-primitive "#%kernel" "current-thread-group" *current-thread-group*)

(defnative "make-thread-group" (&optional (parent nil parent-p)) (:leaf t)
  (let ((parent (if parent-p parent (funcall *current-thread-group*))))
    (unless (thread-group-p parent) (arg-error "make-thread-group" "thread-group?" parent))
    (make-thread-group* parent)))
(defnative "thread-group?" (x) (:leaf t) (bool (thread-group-p x)))

;;; ------------------------------------------------------------------
;;; Plumbers: flush handles (the plumber is control.lisp's)

(defnative "plumber?" (x) (:leaf t) (bool (plumber-p x)))
(defnative "plumber-add-flush!" (p proc &optional (weak? ps:false)) (:leaf t)
  (unless (plumber-p p) (arg-error "plumber-add-flush!" "plumber?" p))
  (unless (accepts-arity-p proc 1) (arg-error "plumber-add-flush!" "(procedure-arity-includes/c 1)" proc))
  (let ((h (make-flush-handle p proc (truthy weak?))))
    (push h (plumber-flushes p))
    h))
(defnative "plumber-flush-all" (p) ()
  (unless (plumber-p p) (arg-error "plumber-flush-all" "plumber?" p))
  (dolist (h (reverse (plumber-flushes p))) (funcall (flush-handle-proc h) h))
  *void*)
(defnative "plumber-flush-handle?" (x) (:leaf t) (bool (flush-handle-p x)))
(defnative "plumber-flush-handle-remove!" (h) (:leaf t)
  (unless (flush-handle-p h) (arg-error "plumber-flush-handle-remove!" "plumber-flush-handle?" h))
  (let ((p (flush-handle-plumber h)))
    (setf (plumber-flushes p) (delete h (plumber-flushes p))))
  *void*)

;;; ------------------------------------------------------------------
;;; Security guards: permissive

(defvar *root-security-guard* (make-security-guard* nil nil nil nil))
(defvar *current-security-guard*
  (make-parameter* *root-security-guard*
		   (guard-check "current-security-guard" #'security-guard-p "security-guard?")
		   (rsym "current-security-guard")))
(define-primitive "#%kernel" "current-security-guard" *current-security-guard*)

(defnative "make-security-guard" (parent file network &optional (link ps:false)) (:leaf t)
  (unless (security-guard-p parent) (arg-error "make-security-guard" "security-guard?" parent))
  (unless (accepts-arity-p file 3) (arg-error "make-security-guard" "(procedure-arity-includes/c 3)" file))
  (unless (accepts-arity-p network 4) (arg-error "make-security-guard" "(procedure-arity-includes/c 4)" network))
  (unless (or (not (truthy link)) (accepts-arity-p link 3))
    (arg-error "make-security-guard" "(or/c (procedure-arity-includes/c 3) #f)" link))
  (make-security-guard* parent file network (and (truthy link) link)))
(defnative "security-guard?" (x) (:leaf t) (bool (security-guard-p x)))

(defun security-guards ()
  (loop for g = (funcall *current-security-guard*) then (security-guard-parent g)
	while g collect g))

(defnative "security-guard-check-file" (who path modes) ()
  (dolist (g (security-guards))
    (when (security-guard-file g) (funcall (security-guard-file g) who path modes)))
  *void*)
(defnative "security-guard-check-file-link" (who path dest) ()
  (dolist (g (security-guards))
    (when (security-guard-link g) (funcall (security-guard-link g) who path dest)))
  *void*)
(defnative "security-guard-check-network" (who host port mode) ()
  (dolist (g (security-guards))
    (when (security-guard-network g) (funcall (security-guard-network g) who host port mode)))
  *void*)
(defnative "unsafe-make-security-guard-at-root" (&rest args) (:instance "#%unsafe" :leaf t)
  (declare (ignore args))
  (make-security-guard* nil nil nil nil))

;;; ------------------------------------------------------------------
;;; Wills.  SBCL's finalizers can't resurrect their object, so a will
;;; procedure can't be given the very value that was registered.  What
;;; is achievable: when the registered value is unreachable (an SBCL
;;; finalizer notices), the will becomes ready, and the procedure is
;;; given a TWIN of the value -- for a struct instance a new instance
;;; that shares the fields (so its state is the original's, eq? aside);
;;; for a box, pair, vector, string or bytes a copy of the value as it
;;; was at registration.  A value that can't be copied (a procedure, a
;;; port, ...) never has its will ready.  The registration's own
;;; references (the twin) must not reach the value, or it never dies.
;;; Racket promises nothing about when a will is ready.

(defun twin-maker (v)
  "A function that makes V's twin when V is gone, holding nothing of V's
identity; or NIL."
  (typecase v
    (rstruct (let ((type (rstruct-type v)) (fields (rstruct-fields v)))
	       (lambda () (make-rstruct type fields))))
    (rbox (let ((value (rbox-value v))) (lambda () (make-rbox :value value))))
    (mpair (let ((a (mpair-car v)) (d (mpair-cdr v))) (lambda () (make-mpair :car a :cdr d))))
    (cons (let ((a (car v)) (d (cdr v))) (lambda () (cons a d))))
    (simple-vector (let ((c (copy-seq v))) (lambda () (copy-seq c))))
    (string (let ((c (copy-seq v))) (lambda () (copy-seq c))))
    ((simple-array (unsigned-byte 8) (*)) (let ((c (copy-seq v))) (lambda () (copy-seq c))))
    (t nil)))

(defun run-finalizers-now ()
  (let ((f (find-symbol "RUN-PENDING-FINALIZERS" "SB-IMPL")))
    (when (and f (fboundp f)) (ignore-errors (funcall f)))))

(defun poll-wills (e)
  "Whether executor E has a will ready."
  (run-finalizers-now)
  (let ((new (loop for x = (sb-ext:atomic-pop (rwill-executor-ready e)) while x collect x)))
    (when new
      (setf (rwill-executor-queue e) (append (rwill-executor-queue e) (nreverse new)))))
  (and (rwill-executor-queue e) t))

(defun check-will-executor (who e)
  (unless (rwill-executor-p e) (arg-error who "will-executor?" e)))

(defnative "make-will-executor" () (:leaf t) (make-rwill-executor))
(defnative "will-executor?" (x) (:leaf t) (bool (rwill-executor-p x)))
(defnative "will-register" (e v proc) (:leaf t)
  (check-will-executor "will-register" e)
  (unless (accepts-arity-p proc 1) (arg-error "will-register" "(procedure-arity-includes/c 1)" proc))
  (let ((maker (twin-maker v)))
    (when maker
      (let ((entry (cons maker proc)))
	(ignore-errors
	 (sb-ext:finalize v (lambda () (sb-ext:atomic-push entry (rwill-executor-ready e)))
			  :dont-save t)))))
  *void*)

(defun execute-will (e)
  (destructuring-bind (maker . proc) (pop (rwill-executor-queue e))
    (funcall proc (funcall maker))))

(defnative "will-try-execute" (e &optional (default ps:false)) ()
  (check-will-executor "will-try-execute" e)
  (if (poll-wills e) (execute-will e) default))
(defnative "will-execute" (e) ()
  (check-will-executor "will-execute" e)
  (loop
    (when (poll-wills e) (return (execute-will e)))
    (do-sync "will-execute" (list e) ps:false nil)))

;;; ------------------------------------------------------------------
;;; Subprocesses

(defun subprocess-done-p (s)
  (let ((p (subprocess-process s)))
    (or (subprocess-status s)
	(not (eq (sb-ext:process-status p) :running)))))

(defun subprocess-exit-status (s)
  (or (subprocess-status s)
      (let ((p (subprocess-process s)))
	(if (eq (sb-ext:process-status p) :running)
	    nil
	    (setf (subprocess-status s)
		  (if (eq (sb-ext:process-status p) :signaled)
		      (+ 128 (sb-ext:process-exit-code p))
		      (sb-ext:process-exit-code p)))))))

(defun fd-poll-readable (fd)
  (sb-unix:unix-simple-poll fd :input 0))

(defun fd-filler (fd stream)
  "The fill function of a lazy input port that reads descriptor FD."
  (lambda (p need blocking)
    (declare (ignore need))
    (loop
      (cond ((fd-poll-readable fd)
	     (let* ((buf (make-octets 4096))
		    (n (sb-sys:with-pinned-objects (buf)
			 (sb-unix:unix-read fd (sb-sys:vector-sap buf) 4096))))
	       (cond ((or (null n) (zerop n)) (ignore-errors (close stream)) (return :eof))
		     (t (ip-append p buf 0 n) (return :data)))))
	    (blocking (wait-for (lambda () (fd-poll-readable fd))))
	    (t (return :block))))))

(defun subprocess-arg (who x)
  (cond ((stringp x) x)
	((typep x '(simple-array (unsigned-byte 8) (*))) (decode-utf-8 x))
	((rpath-p x) (path-string who x))
	(t (arg-error who "(or/c path? string-no-nuls? bytes-no-nuls?)" x))))

(defun subprocess-stream-arg (who x direction)
  "The Lisp stream (or T) to hand run-program for the port X (a file-stream port), or :stream if X is #f."
  (cond ((not (truthy x)) :stream)
	((and (routport-p x) (eq (routport-kind x) :file)) (routport-stream x))
	((and (rinport-p x) (eq (rinport-kind x) :host)) t)
	((and (eq direction :output) (not (routport-p x)) (not (rinport-p x))) t)
	(t (racket-error who "contract violation~%  expected: (or/c (and/c output-port? file-stream-port?) #f)~%  given: ~A" (written x)))))

(defnative "subprocess" (stdout stdin stderr &rest more) ()
  ;; an optional group argument, #f or 'new, comes before the command
  (when (and more (or (eq (car more) ps:false)
		      (and (racket-symbol-p (car more)) (string= (rname (car more)) "new"))))
    (pop more))
  (unless more
    (racket-arity-error "subprocess" 3))
  (let* ((command (car more))
	 (args (cdr more))
	 (cmd (subprocess-arg "subprocess" command))
	 (err-is-out (and (racket-symbol-p stderr) (string= (rname stderr) "stdout")))
	 (out-s (subprocess-stream-arg "subprocess" stdout :output))
	 (in-s (subprocess-stream-arg "subprocess" stdin :input))
	 (err-s (if err-is-out :output (subprocess-stream-arg "subprocess" stderr :output)))
	 (process
	   (handler-case
	       (sb-ext:run-program cmd (mapcar (lambda (a) (subprocess-arg "subprocess" a)) args)
				   :search nil :wait nil :input in-s :output out-s :error err-s
				   :directory (let ((d (namestring *default-pathname-defaults*)))
						(and (plusp (length d)) d)))
	     (error (e)
	       (racket-error "subprocess" "process creation failed~%  system error: ~A" e)))))
    (let ((s (make-subprocess* process cmd)))
      (values s
	      (and (eq out-s :stream)
		   (let ((stream (sb-ext:process-output process)))
		     (new-lazy-inport (rsym "subprocess") :host
				      (fd-filler (sb-sys:fd-stream-fd stream) stream))))
	      (and (eq in-s :stream)
		   (make-file-outport cmd (sb-sys:fd-stream-fd (sb-ext:process-input process))))
	      (and (eq err-s :stream)
		   (let ((stream (sb-ext:process-error process)))
		     (new-lazy-inport (rsym "subprocess") :host
				      (fd-filler (sb-sys:fd-stream-fd stream) stream))))))))

(defnative "subprocess?" (x) (:leaf t) (bool (subprocess-p x)))
(defnative "subprocess-pid" (s) (:leaf t)
  (unless (subprocess-p s) (arg-error "subprocess-pid" "subprocess?" s))
  (sb-ext:process-pid (subprocess-process s)))
(defnative "subprocess-status" (s) (:leaf t)
  (unless (subprocess-p s) (arg-error "subprocess-status" "subprocess?" s))
  (or (subprocess-exit-status s) (rsym "running")))
(defnative "subprocess-wait" (s) ()
  (unless (subprocess-p s) (arg-error "subprocess-wait" "subprocess?" s))
  (do-sync "subprocess-wait" (list s) ps:false nil)
  *void*)
(defnative "subprocess-kill" (s force?) (:leaf t)
  (unless (subprocess-p s) (arg-error "subprocess-kill" "subprocess?" s))
  (unless (subprocess-done-p s)
    (sb-ext:process-kill (subprocess-process s) (if (truthy force?) 9 2)))
  *void*)
(defvar *subprocess-group-enabled*
  (make-parameter* ps:false nil (rsym "subprocess-group-enabled")))
(define-primitive "#%kernel" "subprocess-group-enabled" *subprocess-group-enabled*)

(defnative "shell-execute" (&rest args) ()
  (declare (ignore args))
  (racket-error "shell-execute" "failed~%  system error: unsupported; rkt_err=1"))

;;; ------------------------------------------------------------------
;;; Odds and ends

(defnative "memory-order-acquire" () (:leaf t) *void*)
(defnative "memory-order-release" () (:leaf t) *void*)

(defnative "exn-classify-errno" (v) (:leaf t)
  (let ((errno (cond ((and (consp v) (integerp (car v)) (racket-symbol-p (cdr v))) (car v))
		     ((and (exn-p v) (typep (ignore-errors (svref (struct-fields v) 2)) 'cons))
		      (car (svref (struct-fields v) 2)))
		     (t (arg-error "exn-classify-errno" "(or/c exn? (cons/c exact-integer? (or/c 'posix 'windows 'gai)))" v)))))
    (let ((entry (find errno #+darwin *macos-errnos* #-darwin *linux-errnos* :key #'cdr)))
      (if entry (rsym (car entry)) ps:false))))

(defnative "impersonator-ephemeron" (v) (:leaf t)
  (funcall (native-function (gethash "make-ephemeron" *natives*)) v v))

;;; ------------------------------------------------------------------
;;; Atomicity of the natives of shared mutable structures

;;; Only the natives that change a shared structure: reads of hash tables
;;; stay unwrapped (the expander is made of them).
(defparameter *atomic-native-names*
  '("hash-set!" "hash-remove!" "hash-clear!" "hash-update!" "hash-ref!" "hash-set*!" "hash-remove*!"
    "hash-copy-clear" "vector-set!" "vector-fill!" "vector-copy!" "string-set!" "string-fill!"
    "string-copy!" "bytes-set!" "bytes-fill!" "bytes-copy!" "set-box!" "set-mcar!" "set-mcdr!"
    "make-pipe" "flush-output" "file-position" "make-input-port" "make-output-port"
    "get-output-bytes" "get-output-string")
  "Natives that run without preemption, by name.")

(defparameter *atomic-native-prefixes*
  '("write" "display" "print" "newline" "read-" "peek-" "close-" "port-" "open-" "set-port-"
    "fprintf" "printf" "eprintf")
  "And natives whose names start with one of these.")

(defvar *wrapped-natives* (make-hash-table :test 'eq))

(defun atomic-wrapper (f)
  "F, without preemption (cheaply skipped while there's one thread)."
  (lambda (&rest args)
    (declare (dynamic-extent args))
    (if (null (cdr *threads*))
	(apply f args)
	(let ((self *self*))
	  (incf (rthread-atomic self))
	  (unwind-protect (apply f args)
	    (decf (rthread-atomic self)))))))

(defun protect-natives ()
  "Wrap the natives that change shared structures in atomic sections,
keeping their arities.  Idempotent."
  (maphash (lambda (name n)
	     (when (and (not (gethash n *wrapped-natives*))
			(or (member name *atomic-native-names* :test #'string=)
			    (some (lambda (prefix)
				    (and (>= (length name) (length prefix))
					 (string= prefix name :end2 (length prefix))))
				  *atomic-native-prefixes*)))
	       (let* ((f (native-function n))
		      (mask (ignore-errors (arity-mask f)))
		      (w (atomic-wrapper f)))
		 (when mask (setf (gethash w *arity-masks*) mask))
		 (setf (native-function n) w
		       (gethash n *wrapped-natives*) t))))
	   *natives*))

(protect-natives)
