; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-GUILE -*-

;;;; The C half of (ice-9 threads): threads are SBCL's threads, mutexes
;;;; and condition variables SBCL's, with Guile's recursive mutexes
;;;; counted by hand (as src/chez/host.lisp does for Chez's).

(in-package "PSEUDOSCHEME-GUILE")

(defstruct (gmutex (:constructor make-gmutex (kind)) (:copier nil))
  (lock (sb-thread:make-mutex :name "guile mutex"))
  kind
  (owner nil)
  (level 0))

(defmethod print-object ((m gmutex) stream) (write-string "#<mutex>" stream))

(defstruct (gcondvar (:constructor make-gcondvar ()) (:copier nil))
  (queue (sb-thread:make-waitqueue)))

(defmethod print-object ((c gcondvar) stream) (write-string "#<condition-variable>" stream))

(defun timeout-seconds (timeout)
  "A Guile absolute timeout (seconds since the epoch, or (seconds .
microseconds)) as seconds from now, or NIL for none."
  (cond ((or (null timeout) (eq timeout ps:false)) nil)
	(t (let* ((abs (if (consp timeout) (+ (car timeout) (/ (cdr timeout) 1000000)) timeout))
		  (now (multiple-value-bind (s us) (sb-ext:get-time-of-day) (+ s (/ us 1000000)))))
	     (max 0 (float (- abs now) 1d0))))))

(defun lock-gmutex (m &optional timeout)
  (let ((self sb-thread:*current-thread*))
    (cond ((and (eq (gmutex-owner m) self) (eq (gmutex-kind m) :recursive))
	   (incf (gmutex-level m)) ps:true)
	  ((let ((seconds (timeout-seconds timeout)))
	     (if seconds
		 (sb-thread:grab-mutex (gmutex-lock m) :timeout seconds)
		 (sb-thread:grab-mutex (gmutex-lock m))))
	   (setf (gmutex-owner m) self (gmutex-level m) 1)
	   ps:true)
	  (t ps:false))))

(defun unlock-gmutex (m)
  (when (> (decf (gmutex-level m)) 0)
    (return-from unlock-gmutex ps:true))
  (setf (gmutex-owner m) nil (gmutex-level m) 0)
  (sb-thread:release-mutex (gmutex-lock m) :if-not-owner :punt)
  ps:true)

(defun wait-gcondvar (c m &optional timeout)
  (let ((level (gmutex-level m)) (seconds (timeout-seconds timeout)))
    (setf (gmutex-owner m) nil (gmutex-level m) 0)
    (let ((woken (if seconds
		     (sb-thread:condition-wait (gcondvar-queue c) (gmutex-lock m) :timeout seconds)
		     (sb-thread:condition-wait (gcondvar-queue c) (gmutex-lock m)))))
      (unless (sb-thread:holding-mutex-p (gmutex-lock m))
	(sb-thread:grab-mutex (gmutex-lock m)))
      (setf (gmutex-owner m) sb-thread:*current-thread* (gmutex-level m) level)
      (bool woken))))

(defun new-thread (thunk)
  (let ((out *standard-output*) (in *standard-input*) (err *error-output*)
	(module *current-module*)
	(params (ps-r7rs::thread-parameters-snapshot)))
    (sb-thread:make-thread
     (lambda ()
       (let ((*standard-output* out) (*standard-input* in) (*error-output* err)
	     (*current-module* module)
	     (ps-r7rs::*thread-parameters* params))
	 (with-guile-errors (funcall thunk))))
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
			(make-gmutex (if (and kind (symbolp kind)
					      (string= (ps:scheme-symbol-name kind) "recursive"))
					 :recursive :plain))))
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
   (cons "signal-condition-variable" (lambda (c) (sb-thread:condition-notify (gcondvar-queue c)) ps:true))
   (cons "broadcast-condition-variable" (lambda (c) (sb-thread:condition-broadcast (gcondvar-queue c)) ps:true))
   (cons "total-processor-count" (lambda () (or (ignore-errors (parse-integer (uiop:run-program '("sysctl" "-n" "hw.ncpu") :output :string) :junk-allowed t)) 1)))
   (cons "current-processor-count" (lambda () (or (ignore-errors (parse-integer (uiop:run-program '("sysctl" "-n" "hw.ncpu") :output :string) :junk-allowed t)) 1)))
   (cons "%make-transcoded-port" (lambda (port) port))))
