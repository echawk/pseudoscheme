; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-GUILE -*-

;;;; The C half of (ice-9 threads): threads are SBCL's threads; mutexes
;;;; and condition variables are Guile's, made of SBCL's locks and wait
;;;; queues.

(in-package "PSEUDOSCHEME-GUILE")

;;; A Guile mutex is its own state (an owner, a level) under an SBCL
;;; lock, with a queue of the threads waiting for it: Guile's kinds lock
;;; again or unlock across threads in ways SBCL's mutexes don't.  KIND is
;;; :recursive (the owner locks again), :unowned (SRFI 18's
;;; allow-external-unlock: the owner locking again waits, any thread
;;; unlocks) or :plain.

(defstruct (gmutex (:constructor make-gmutex (kind)) (:copier nil))
  (lock (sb-thread:make-mutex :name "guile mutex"))
  (queue (sb-thread:make-waitqueue))
  kind
  (owner nil)
  (level 0))

(defmethod print-object ((m gmutex) stream) (write-string "#<mutex>" stream))

(defstruct (gcondvar (:constructor make-gcondvar ()) (:copier nil))
  (lock (sb-thread:make-mutex :name "guile condition variable"))
  (queue (sb-thread:make-waitqueue)))

(defmethod print-object ((c gcondvar) stream) (write-string "#<condition-variable>" stream))

(defun timeout-seconds (timeout)
  "A Guile absolute timeout (seconds since the epoch, or (seconds .
microseconds)) as seconds from now, or NIL for none."
  (cond ((or (null timeout) (eq timeout ps:false)) nil)
	(t (let* ((abs (if (consp timeout) (+ (car timeout) (/ (cdr timeout) 1000000)) timeout))
		  (now (multiple-value-bind (s us) (sb-ext:get-time-of-day) (+ s (/ us 1000000)))))
	     (max 0 (float (- abs now) 1d0))))))

(defun deadline (seconds)
  (and seconds (+ (get-internal-real-time) (* seconds internal-time-units-per-second))))

(defun seconds-left (deadline)
  (and deadline (max 0 (/ (- deadline (get-internal-real-time)) internal-time-units-per-second))))

(defun lock-gmutex (m &optional timeout)
  (let ((self sb-thread:*current-thread*)
	(deadline (deadline (timeout-seconds timeout))))
    (sb-thread:with-mutex ((gmutex-lock m))
      (loop
	(cond ((null (gmutex-owner m))
	       (setf (gmutex-owner m) self (gmutex-level m) 1)
	       (return ps:true))
	      ((and (eq (gmutex-owner m) self) (eq (gmutex-kind m) :recursive))
	       (incf (gmutex-level m))
	       (return ps:true))
	      ((and (eq (gmutex-owner m) self) (eq (gmutex-kind m) :plain))
	       (guile-error (ssym "misc-error") "lock-mutex" "mutex already locked by thread" '()))
	      ((and deadline (zerop (seconds-left deadline)))
	       (return ps:false))
	      (t (sb-thread:condition-wait (gmutex-queue m) (gmutex-lock m)
					   :timeout (seconds-left deadline))))))))

(defun release-gmutex (m)
  "Unlock M once (its lock held)."
  (when (zerop (gmutex-level m))
    (guile-error (ssym "misc-error") "unlock-mutex" "mutex not locked" '()))
  (unless (or (eq (gmutex-owner m) sb-thread:*current-thread*) (eq (gmutex-kind m) :unowned))
    (guile-error (ssym "misc-error") "unlock-mutex" "mutex not locked by current thread" '()))
  (when (zerop (decf (gmutex-level m)))
    (setf (gmutex-owner m) nil)
    (sb-thread:condition-notify (gmutex-queue m))))

(defun unlock-gmutex (m)
  (sb-thread:with-mutex ((gmutex-lock m)) (release-gmutex m))
  ps:true)

(defun wait-gcondvar (c m &optional timeout)
  ;; C's lock is taken before M is released, and a signal takes it, so a
  ;; signal after the release isn't lost
  (let ((seconds (timeout-seconds timeout)) level woken)
    (sb-thread:with-mutex ((gcondvar-lock c))
      (sb-thread:with-mutex ((gmutex-lock m))
	(setq level (gmutex-level m))
	(setf (gmutex-level m) 1)
	(release-gmutex m))
      (setq woken (sb-thread:condition-wait (gcondvar-queue c) (gcondvar-lock c) :timeout seconds)))
    (lock-gmutex m)
    (setf (gmutex-level m) level)
    (bool woken)))

(defun signal-gcondvar (c broadcast)
  (sb-thread:with-mutex ((gcondvar-lock c))
    (if broadcast
	(sb-thread:condition-broadcast (gcondvar-queue c))
	(sb-thread:condition-notify (gcondvar-queue c))))
  ps:true)

(defvar *vm*)				; vm.lisp: each thread's VM

(defparameter *guile-thread-stack-size* (* 256 1024 1024)
  "The control stack of a thread Guile code makes: Guile's grow, so its
code may recurse deeply in a thread (par-map's futures nest one per
element).")

(defun new-thread (thunk)
  ;; SBCL gives each new thread a control stack of thread_control_stack_size
  (setf (sb-alien:extern-alien "thread_control_stack_size" sb-alien:unsigned)
	(max *guile-thread-stack-size*
	     (sb-alien:extern-alien "thread_control_stack_size" sb-alien:unsigned)))
  (let ((out *standard-output*) (in *standard-input*) (err *error-output*)
	(module *current-module*)
	(params (ps-r7rs::thread-parameters-snapshot)))
    (sb-thread:make-thread
     (lambda ()
       (psx::with-thread-state
       (let ((*standard-output* out) (*standard-input* in) (*error-output* err)
	     (*current-module* module)
	     (ps-r7rs::*thread-parameters* params)
	     ;; a VM of its own (vm.lisp), made when first needed
	     (*vm* nil))
	 ;; an exhausted stack (SBCL's binding stack is of fixed size) is a
	 ;; Guile stack-overflow, once unwound, rather than the end of the
	 ;; process
	 ;; an exception the thread doesn't handle ends it, not the process
	 (flet ((uncaught (c)
		  (ignore-errors
		   (format *error-output* "~&In thread:~%~A~%" (error-text c))
		   (finish-output *error-output*))
		  ps:false))
	   (handler-case
	       (handler-case (with-guile-errors (funcall thunk))
		 (storage-condition ()
		   (with-guile-errors
		     (guile-error (ssym "stack-overflow") ps:false "Stack overflow" '()))))
	     (error (c) (uncaught c)))))))
     :name "guile thread")))

(defextension "scm_init_ice_9_threads"
  (list
   (cons "%call-with-new-thread" #'new-thread)
   (cons "yield" (lambda () (sb-thread:thread-yield) ps:true))
   (cons "thread?" (lambda (x) (bool (typep x 'sb-thread:thread))))
   (cons "current-thread" (lambda () sb-thread:*current-thread*))
   (cons "all-threads" (lambda () (sb-thread:list-all-threads)))
   (cons "thread-exited?" (lambda (th) (bool (not (sb-thread:thread-alive-p th)))))
   (cons "make-mutex" (lambda (&optional kind)
			(let ((name (and kind (symbolp kind) (ps:scheme-symbol-name kind))))
			  (make-gmutex (cond ((equal name "recursive") :recursive)
					     ((equal name "allow-external-unlock") :unowned)
					     (t :plain))))))
   (cons "make-recursive-mutex" (lambda () (make-gmutex :recursive)))
   (cons "lock-mutex" (lambda (m &optional timeout) (lock-gmutex m timeout)))
   (cons "unlock-mutex" (lambda (m &optional cv timeout)
			  (if (and cv (gcondvar-p cv))
			      (wait-gcondvar cv m timeout)
			      (unlock-gmutex m))))
   (cons "mutex?" (lambda (x) (bool (gmutex-p x))))
   (cons "mutex-owner" (lambda (m) (or (gmutex-owner m) ps:false)))
   (cons "mutex-level" (lambda (m) (gmutex-level m)))
   (cons "mutex-locked?" (lambda (m) (bool (plusp (gmutex-level m)))))
   (cons "make-condition-variable" #'make-gcondvar)
   (cons "condition-variable?" (lambda (x) (bool (gcondvar-p x))))
   (cons "wait-condition-variable" (lambda (c m &optional timeout) (wait-gcondvar c m timeout)))
   (cons "signal-condition-variable" (lambda (c) (signal-gcondvar c nil)))
   (cons "broadcast-condition-variable" (lambda (c) (signal-gcondvar c t)))
   (cons "total-processor-count" (lambda () (or (ignore-errors (parse-integer (uiop:run-program '("sysctl" "-n" "hw.ncpu") :output :string) :junk-allowed t)) 1)))
   (cons "current-processor-count" (lambda () (or (ignore-errors (parse-integer (uiop:run-program '("sysctl" "-n" "hw.ncpu") :output :string) :junk-allowed t)) 1)))
   (cons "%make-transcoded-port" (lambda (port) port))))

;;; Atomic boxes ((ice-9 atomic)'s C half): a cell whose updates are
;;; atomic, compared by eq?.

(defstruct (atomic-box (:constructor make-atomic-box (value)) (:copier nil))
  (value nil :type t))

(defmethod print-object ((b atomic-box) stream)
  (format stream "#<atomic-box ~(~X~) value: " (logand (sb-kernel:get-lisp-obj-address b) #xffffffffff))
  (guile-write (atomic-box-value b) stream)
  (write-char #\> stream))

(defun atomic-box-cas (box expected desired)
  "Set BOX to DESIRED if it holds EXPECTED; its value before, either way."
  (loop (let ((old (atomic-box-value box)))
	  (unless (eq old expected) (return old))
	  (when (eq (sb-ext:compare-and-swap (atomic-box-value box) old desired) old)
	    (return old)))))

(defun atomic-box-swap (box value)
  (loop (let ((old (atomic-box-value box)))
	  (when (eq (sb-ext:compare-and-swap (atomic-box-value box) old value) old)
	    (return old)))))

(defun check-atomic-box (who b) (unless (atomic-box-p b) (wrong-type who 1 b)))

(defextension "scm_init_atomic"
  (list (cons "make-atomic-box" #'make-atomic-box)
	(cons "atomic-box?" (lambda (x) (bool (atomic-box-p x))))
	(cons "atomic-box-ref" (lambda (b) (check-atomic-box "atomic-box-ref" b)
				 (sb-thread:barrier (:read)) (atomic-box-value b)))
	(cons "atomic-box-set!" (lambda (b v) (check-atomic-box "atomic-box-set!" b)
				  (setf (atomic-box-value b) v) (sb-thread:barrier (:write)) *unspecified*))
	(cons "atomic-box-swap!" (lambda (b v) (check-atomic-box "atomic-box-swap!" b) (atomic-box-swap b v)))
	(cons "atomic-box-compare-and-swap!"
	      (lambda (b expected desired)
		(check-atomic-box "atomic-box-compare-and-swap!" b)
		(atomic-box-cas b expected desired)))))
