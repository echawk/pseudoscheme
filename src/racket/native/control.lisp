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
  (first-mark key (marks-of marks) none))

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

(defnative "parameter?" (x) (:leaf t) (bool (gethash x *parameters*)))
(defnative "parameterization?" (x) (:leaf t) (bool (parameterization-p x)))
(defnative "current-parameterization" () (:leaf t) (current-parameterization))

;;; The extension parameterization (#%paramz): PARAMZ with each parameter
;;; given a fresh cell holding its value (after its guard)
(defnative "extend-parameterization" (paramz &rest params-and-values)
    (:instance "#%paramz" :rumble "extend-parameterization")
  (let ((cells (parameterization-cells paramz)))
    (loop for (p v) on params-and-values by #'cddr
	  do (let ((data (or (gethash p *parameters*)
			     (racket-error "parameterize" "contract violation~%  expected: parameter?~%  given: ~S" p))))
	       (push (cons data (list (if (parameter-data-guard data) (funcall (parameter-data-guard data) v) v)))
		     cells)))
    (make-parameterization cells)))

(defnative "call-with-parameterization" (paramz thunk) ()
  (ps-r6rs::call-with-mark *parameterization-key* paramz thunk))
