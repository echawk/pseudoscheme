; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

;;;; Control, natively: continuation marks, prompts with tags, raise and
;;;; exception handlers, parameters and parameterizations (#%paramz).
;;;;
;;;; Marks are the Chez mode's (src/chez/host.lisp): a mark belongs to a
;;;; frame, and one set in tail position replaces its frame's mark of the
;;;; same key; with-continuation-mark compiles to CALL-WITH-MARK.
;;;; Prompts are the host's (src/continuations.lisp), whose handler gets
;;;; the composable continuation and the values; Racket's gets the
;;;; values.
;;;;
;;;; Exceptions, as in Racket CS's rumble: raise calls the handler that
;;;; the innermost exception-handler-key mark holds, with the marks
;;;; outside that one current, so that a raise inside it goes to the
;;;; next handler out.  With none, the uncaught-exception handler runs.
;;;; Not yet: marks delimited by a prompt tag, composable continuations
;;;; (call-with-composable-continuation), barriers, breaks.

(in-package "PSEUDOSCHEME-RACKET")

;;; ------------------------------------------------------------------
;;; Continuation marks

(defvar *exception-handler-key* (make-symbol "exception-handler-key"))
(defvar *parameterization-key* (make-symbol "parameterization-key"))
(defvar *break-enabled-key* (make-symbol "break-enabled-key"))

(define-primitive "#%paramz" "exception-handler-key" *exception-handler-key*)
(define-primitive "#%paramz" "parameterization-key" *parameterization-key*)
(define-primitive "#%paramz" "break-enabled-key" *break-enabled-key*)

(defun current-marks () ps-r6rs::*marks*)

(defun marks-of (set)
  (if (eq set ps:false) (current-marks) (ps-r6rs::mark-set-marks set)))

(defun first-mark (key &optional (marks (current-marks)) (none ps:false))
  (let ((m (find key marks :key #'cadr :test #'eq)))
    (if m (cddr m) none)))

(defnative "current-continuation-marks" (&optional prompt-tag) (:rumble "current-continuation-marks" :leaf t)
  (declare (ignore prompt-tag))
  (ps-r6rs::make-mark-set (current-marks)))

(defnative "continuation-mark-set?" (x) (:leaf t) (bool (ps-r6rs::mark-set-p x)))

(defnative "continuation-mark-set-first" (marks key &optional (none ps:false) prompt-tag)
    (:rumble "continuation-mark-set-first" :leaf t)
  (declare (ignore prompt-tag))
  ;; a thread's continuation starts with the root parameterization marked
  (first-mark key (marks-of marks)
	      (if (eq key *parameterization-key*) *root-parameterization* none)))

(defnative "continuation-mark-set->list" (marks key &optional prompt-tag) (:leaf t)
  (declare (ignore prompt-tag))
  (loop for m in (marks-of marks) when (eq (cadr m) key) collect (cddr m)))

(defnative "call-with-immediate-continuation-mark" (key proc &optional (default ps:false)) ()
  (ps-r6rs::call-with-immediate-mark key proc default))

;;; ------------------------------------------------------------------
;;; Prompts

(defstruct (prompt-tag (:constructor make-prompt-tag (name))) name)

(defmethod print-object ((x prompt-tag) stream)
  (format stream "#<continuation-prompt-tag~@[:~A~]>" (and (prompt-tag-name x) (rname (prompt-tag-name x)))))

(defvar *default-prompt-tag* (make-prompt-tag (rsym "default")))

(defnative "default-continuation-prompt-tag" () (:leaf t) *default-prompt-tag*)
(defnative "make-continuation-prompt-tag" (&optional (name nil)) (:leaf t)
  (make-prompt-tag (and name (truthy name) name)))
(defnative "continuation-prompt-tag?" (x) (:leaf t) (bool (prompt-tag-p x)))
(defnative "continuation-prompt-available?" (tag &optional k) (:leaf t)
  (declare (ignore k))
  (bool (psx::find-prompt tag)))

(defun default-prompt-handler (tag)
  (if (eq tag *default-prompt-tag*)
      ;; the default tag's: call the thunk aborted with, under the prompt again
      (lambda (thunk) (call-with-continuation-prompt thunk tag nil))
      #'values))

(defun call-with-continuation-prompt (proc tag handler &rest args)
  (let ((handler (or handler (default-prompt-handler tag))))
    (psx::call-with-prompt tag
			   (lambda () (apply proc args))
			   (lambda (k &rest values)
			     (declare (ignore k))
			     (apply handler values)))))

(defnative "call-with-continuation-prompt" (proc &optional (tag *default-prompt-tag*) (handler ps:false) &rest args)
    (:rumble "call-with-continuation-prompt")
  (apply #'call-with-continuation-prompt proc tag (and (truthy handler) handler) args))

(defnative "abort-current-continuation" (tag &rest vals) (:rumble "abort-current-continuation")
  (unless (psx::find-prompt tag)
    (racket-error "abort-current-continuation" "continuation includes no prompt with the given tag~%  tag: ~A" tag))
  (apply #'psx::abort-to-prompt tag vals))

;;; ------------------------------------------------------------------
;;; Raise

(defvar *uncaught-exception-handler* nil
  "The uncaught-exception-handler parameter's root value (exn.lisp sets it).")

(defun do-raise (v &optional (barrier t))
  "raise: the innermost handler, in the marks outside its own."
  (declare (ignore barrier))
  (let* ((marks (current-marks))
	 (entry (find *exception-handler-key* marks :key #'cadr :test #'eq)))
    (if entry
	(let ((handler (cddr entry)))
	  (let ((ps-r6rs::*marks* (cdr (member entry marks :test #'eq))))
	    (funcall handler v))
	  ;; a handler that returns, for raise
	  (let ((ps-r6rs::*marks* (cdr (member entry marks :test #'eq))))
	    (do-raise (make-exn "exn:fail:contract"
				(format nil "exception handler returned~%  exception: ~A"
					(with-output-to-string (s) (funcall (psx:host-ref "write") v s)))))))
	(funcall *uncaught-exception-handler* v))))

(defnative "raise" (v &optional (barrier? ps:true)) (:rumble "raise")
  (do-raise v (truthy barrier?)))

;;; ------------------------------------------------------------------
;;; Parameters
;;;
;;; A parameter is a procedure (a closure, which *PARAMETERS* maps to its
;;; data): its value is in a cell, the current parameterization's for it
;;; (the innermost parameterization-key mark), else the parameter's own.

(defstruct (parameter-data (:constructor make-parameter-data (cell guard name))) cell guard name)
(defstruct (parameterization (:constructor make-parameterization (cells))) cells) ; alist: data -> cell

(defvar *parameters* (make-hash-table :test 'eq :weakness :key :synchronized t))
(defvar *root-parameterization* (make-parameterization '()))

(defun current-parameterization ()
  (first-mark *parameterization-key* (current-marks) *root-parameterization*))

(defun parameter-cell (data)
  (or (cdr (assoc data (parameterization-cells (current-parameterization)) :test #'eq))
      (parameter-data-cell data)))

(defun make-parameter* (value &optional guard name)
  (let* ((data (make-parameter-data (list value) guard name))
	 (p (lambda (&optional (new nil new-p))
	      (if new-p
		  (progn (setf (car (parameter-cell data))
			       (if (parameter-data-guard data) (funcall (parameter-data-guard data) new) new))
			 *void*)
		  (car (parameter-cell data))))))
    (setf (gethash p *parameters*) data)
    p))

(defnative "make-parameter" (&optional (value ps:false) (guard ps:false) (name ps:false) realm) ()
  (declare (ignore realm))
  (make-parameter* value (and (truthy guard) guard) name))

(defun parameter-data-of (p)
  "P's parameter data: P's own, or what P chaperones' (procedures.lisp)."
  (loop for x = p then (impersonation-of i)
	for data = (gethash x *parameters*)
	for i = (and (not data) (impersonation x))
	when data return data
	while i))

(defnative "parameter?" (x) (:leaf t) (bool (parameter-data-of x)))
(defnative "parameterization?" (x) (:leaf t) (bool (parameterization-p x)))
(defnative "current-parameterization" () (:leaf t) (current-parameterization))

;;; The extension parameterization (#%paramz): PARAMZ with each parameter
;;; given a fresh cell holding its value (after its guard)
(defnative "extend-parameterization" (paramz &rest params-and-values)
    (:instance "#%paramz" :rumble "extend-parameterization")
  (let ((cells (parameterization-cells paramz)))
    (loop for (p v) on params-and-values by #'cddr
	  do (let ((data (or (parameter-data-of p)
			     (racket-error "parameterize" "contract violation~%  expected: parameter?~%  given: ~S" p))))
	       (push (cons data (list (if (parameter-data-guard data) (funcall (parameter-data-guard data) v) v)))
		     cells)))
    (make-parameterization cells)))

(defnative "call-with-parameterization" (paramz thunk) ()
  (ps-r6rs::call-with-mark *parameterization-key* paramz thunk))

;;; The port parameters.  Until a program sets or parameterizes one,
;;; its value is the host's (*standard-output* and so on).  Our natives
;;; that take an optional port ask them (CURRENT-PORT); the host's own
;;; procedures don't.

(defun make-port-parameter (variable name)
  (let* ((data (make-parameter-data (list nil) nil (rsym name)))
	 (p (lambda (&optional (new nil new-p))
	      (if new-p
		  (progn (setf (car (parameter-cell data)) new) *void*)
		  (or (car (parameter-cell data)) (symbol-value variable))))))
    (setf (gethash p *parameters*) data)
    (define-primitive "#%kernel" name p)
    p))

(defvar *current-output-port* (make-port-parameter '*standard-output* "current-output-port"))
(defvar *current-input-port* (make-port-parameter '*standard-input* "current-input-port"))
(defvar *current-error-port* (make-port-parameter '*error-output* "current-error-port"))

(defun current-port (which)
  "The current :OUTPUT, :INPUT or :ERROR port."
  (funcall (ecase which
	     (:output *current-output-port*)
	     (:input *current-input-port*)
	     (:error *current-error-port*))))

;;; ------------------------------------------------------------------
;;; Thread cells.  The cell holds its initial value (and whether it's
;;; preserved: copied to new threads); each thread's own values, once it
;;; sets them, are the thread layer's (threads.lisp).

(defstruct (thread-cell (:constructor make-thread-cell* (value preserved))) value preserved)

(defnative "call-with-continuation-barrier" (thunk) () (funcall thunk))
(defnative "call-with-escape-continuation" (proc) ()
  (funcall (psx:host-ref "call-with-current-continuation") proc))
(defnative "unsafe-root-continuation-prompt-tag" () (:instance "#%unsafe" :leaf t) *default-prompt-tag*)
(defnative "reparameterize" (paramz) (:instance "#%paramz" :leaf t) paramz)

;;; ------------------------------------------------------------------
;;; The thread layer's structures (threads.lisp has the rest): semaphores
;;; (a count, and a queue of the waits that wait for it), and threads

(defstruct (rsemaphore (:constructor make-rsemaphore (count))) (count 0) (waiters '()))

(defstruct (rthread (:constructor make-rthread (name)))
  name
  (id 0)
  (lisp nil)				; its SBCL thread
  (state :runnable)			; :running :runnable :blocked :done
  (cv (sb-thread:make-waitqueue))	; where it waits for the baton
  (wait nil)				; what it waits for, while blocked
  (cells nil)				; its values of thread cells
  (mailbox '())
  (dead nil)
  (zombie nil)				; killed, and still to unwind
  (suspended nil)
  (suspend-to-kill nil)
  (custodians '())			; the records that custodians manage it by
  (group nil)
  (pending-break nil)			; NIL, :break, :hang-up or :terminate
  (atomic 0)				; depth of atomic sections: no preemption
  (preempt-queued nil))

(defvar *main-thread* (make-rthread (rsym "main")))
(defvar *self* *main-thread*
  "The Racket thread the current SBCL thread runs (bound in each new one).")

(defstruct (plumber (:constructor make-plumber* ())) (flushes '()))
(defvar *current-plumber* (make-parameter* (make-plumber*)))
(define-primitive "#%kernel" "current-plumber" *current-plumber*)
(defnative "make-plumber" () (:leaf t) (make-plumber*))
;; plumber?, flush handles and plumber-flush-all: threads.lisp

(defstruct (logger (:constructor make-logger* (name parent))) name parent)
(defvar *current-logger* (make-parameter* (make-logger* ps:false nil)))
(define-primitive "#%kernel" "current-logger" *current-logger*)
(defnative "make-logger" (&optional (name ps:false) parent &rest more) (:leaf t)
  (declare (ignore more))
  (make-logger* name (and parent (truthy parent) parent)))
(defnative "logger?" (x) (:leaf t) (bool (logger-p x)))
(defnative "logger-name" (l) (:leaf t) (logger-name l))
;; no log receivers yet: nothing is ever logged
(defnative "log-level?" (l level &optional topic) (:leaf t) (declare (ignore l level topic)) ps:false)
(defnative "log-max-level" (l &optional topic) (:leaf t) (declare (ignore l topic)) ps:false)
(defnative "log-message" (l level &rest more) (:leaf t) (declare (ignore l level more)) *void*)

;;; Events (threads.lisp has sync and the combinators): a REVT is a
;;; primitive event of a KIND, with a TARGET; the kinds are polled
;;; (:always :never :peek :thunk :log-receiver :poll) or made by the
;;; thread layer (:put :alarm :dead :suspend :resume :receive :idle :choice
;;; :wrap :replace :guard :nack-guard :poll-guard).
(defstruct (revt (:constructor make-revt (kind &optional target))) kind target)
(defvar *always-evt* (make-revt :always))
(defvar *never-evt* (make-revt :never))
(define-primitive "#%kernel" "always-evt" *always-evt*)
(define-primitive "#%kernel" "never-evt" *never-evt*)

;;; Continuation mark keys: any value is a key; these are unique ones
(defstruct (mark-key (:constructor make-mark-key (name))) name)
(defnative "make-continuation-mark-key" (&optional (name nil)) (:leaf t) (make-mark-key name))
(defnative "continuation-mark-key?" (x) (:leaf t) (bool (mark-key-p x)))
(defnative "continuation-mark-set->list*" (marks keys &optional (none ps:false) prompt-tag) (:leaf t)
  (declare (ignore prompt-tag))
  ;; per frame with any of KEYS: a vector of their values (NONE if absent)
  (let ((keys (coerce keys 'list)))
    (loop for (nil . entries) in (ps-r6rs::mark-frames (marks-of marks))
	  when (some (lambda (k) (assoc k entries :test #'eq)) keys)
	    collect (coerce (mapcar (lambda (k) (let ((e (assoc k entries :test #'eq))) (if e (cdr e) none))) keys)
			    'simple-vector))))
