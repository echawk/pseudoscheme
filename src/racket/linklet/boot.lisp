; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

;;;; Strategy B's start-up: the native runtime as the primitive instances,
;;;; then the extracted thread, io, regexp and expander linklets, compiled
;;;; by compiler.lisp (docs/racket-linklet-compiler.md, "Milestones").
;;;;
;;;; Until the expander runs (milestone 6), a file of linklet forms runs:
;;;; each is compiled and instantiated in turn, into one target instance,
;;;; and its result written.  An instance made by an earlier one is
;;;; imported by naming it: (linklet ((NAME x y)) ...) imports x and y
;;;; from the instance of the linklet that `(define-linklet NAME ...)'
;;;; made.  That's milestone 1's test harness (tests/racket/linklets/).

(in-package "PSEUDOSCHEME-RACKET")

(defun linklet-boot ()
  (unless psx::*host* (psx::boot))
  (install-natives)
  (install-linklet-primitives)
  (install-stubs)
  (install-host-globals))

(defun linklet-eval (text)
  "TEXT's forms, each evaluated by Racket's expander in a namespace with
#%kernel (as racket -I '#%kernel -e would), under the default prompt;
the last one's values.  The first use compiles and instantiates the
expander (src/racket/linklet/sources.lisp), about half a minute."
  (let ((forms (with-input-from-string (in text)
		 (loop for form = (read-racket in) until (eq form *eof*) collect form)))
	(values (list *void*)))
    (setq *uncaught* nil)
    (unless *expander-instance*
      (unless (instantiate-expander :stream nil)
	(racket-error "racket" "the expander didn't instantiate")))
    (dolist (form forms)
      (call-with-continuation-prompt
       (lambda () (with-racket-errors (setq values (multiple-value-list (expander-eval form)))))
       *default-prompt-tag* nil))
    (when *uncaught* (finish-output) (uiop:quit 1))
    (values-list values)))

(defun run-linklet-forms (forms &key (output *standard-output*))
  "FORMS: (linklet ...) or (define-linklet NAME (linklet ...)).  Each
linklet is instantiated: a named one into a new instance, kept for later
ones to import; the others into one target instance, their results
written to OUTPUT."
  (let ((named (make-hash-table :test 'eq))
	(target (make-linklet-instance (rsym "top"))))
    (dolist (form forms)
      (let ((name (and (form-p form "define-linklet") (cadr form)))
	    (form (if (form-p form "define-linklet") (caddr form) form)))
	;; an import set (NAME x ...) names the instance; the compiler
	;; sees just the variables
	(let* ((sets (cadr form))
	       (imports (loop for set in sets
			      collect (or (and (consp set) (symbolp (car set)) (gethash (car set) named))
					  (racket-error "linklet" "no instance named by ~S" set))))
	       (compiled (compile-linklet (list* (car form) (mapcar #'cdr sets) (cddr form)) name)))
	  ;; each under the default prompt, as a module body's forms are
	  (call-with-continuation-prompt
	   (lambda ()
	     (with-racket-errors
	       (if name
		   (setf (gethash name named) (instantiate-linklet compiled imports))
		   (let ((values (multiple-value-list (instantiate-linklet compiled imports target))))
		     (dolist (v values)
		       (unless (eq v *void*)
			 (funcall (psx:host-ref "write") v output)
			 (terpri output)))))))
	   *default-prompt-tag* nil))))))

(defun linklet-load (path)
  (unless (equal (pathname-type path) "rktl")
    (not-yet "strategy linklet: running a module (milestone 7)"))
  (setq *uncaught* nil)
  (run-linklet-forms (read-racket-file path))
  ;; as racket's: an uncaught exception makes the exit status 1
  (when *uncaught* (finish-output) (uiop:quit 1)))
