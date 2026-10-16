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
  (install-host-globals)
  (prune-extra-primitives))

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
    (print-stats)
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
	  ;; each under the default prompt, as a module body's forms are,
	  ;; inside the base of continuations (which, made by the
	  ;; instantiation inside, would hide the prompt from it)
	  (with-racket-errors
	  (psx::call-with-continuation-base
	   (lambda ()
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
	   *default-prompt-tag* nil)))))))))

(defun linklet-load (path)
  "PATH: a file of linklets (.rktl, the test harness above), or a module,
which is required (as racket FILE runs one: its #lang, its body, and
its main submodule if it has one)."
  (setq *uncaught* nil)
  (if (equal (pathname-type path) "rktl")
      (run-linklet-forms (read-racket-file path))
      (let ((file (namestring (merge-pathnames path (uiop:getcwd)))))
	(unless *expander-instance*
	  (unless (instantiate-expander :stream nil)
	    (racket-error "racket" "the expander didn't instantiate")))
	;; the base of continuations outside the default prompt, as in
	;; RUN-LINKLET-FORMS, and Lisp errors raised as Racket's outside the
	;; base: a continuation re-entered runs the rest under the base
	(with-racket-errors
	(psx::call-with-continuation-base
	 (lambda ()
	(call-with-continuation-prompt
	 (lambda ()
	   (with-racket-errors
	     (expander-eval (list (rsym "#%require") (list (rsym "file") file)))
	     ;; racket runs a main submodule if the module has one
	     (expander-eval
	      (read-racket (make-string-input-stream
			    (format nil "(if (module-declared? '(submod (file ~S) main) #t) (dynamic-require '(submod (file ~S) main) #f) (void))"
				    file file))))))
	 *default-prompt-tag* nil))))))
  (print-stats)
  ;; as racket's: an uncaught exception makes the exit status 1
  (when *uncaught* (finish-output) (uiop:quit 1)))
