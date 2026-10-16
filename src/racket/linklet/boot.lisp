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

;;; ------------------------------------------------------------------
;;; Start-up from a saved image
;;;
;;; Instantiating the expander (loading its compiled code, running its
;;; 2,000 top-level forms) takes a second, most of what a `racket/base'
;;; program's start-up costs.  So with PSEUDOSCHEME_RACKET_CORE=1, the
;;; first --racket=linklet run saves an image of this program with the
;;; expander instantiated, in the Racket cache (its generation,
;;; cache.lisp), and each run after replaces itself with that image
;;; (exec), given the same arguments.  The image is made by a run of this
;;; program with PSEUDOSCHEME_RACKET_SAVE_CORE set to its path.  It is
;;; opt-in because Racket's threads crash in it (tests/racket/programs/
;;; threads*.rkt: a fault at the bottom of a new thread's stack, under the
;;; ticker's interrupts), which runs without the image don't.  The expander
;;; is only instantiated, not booted: that, and the collection paths,
;;; are the run's own (EXPANDER-EVAL).

(defun racket-core-path ()
  "Where this program's image with the expander instantiated is kept: by the
program file's date and size, as well as the cache's own key."
  (let* ((exe (and sb-ext:*runtime-pathname* (probe-file sb-ext:*runtime-pathname*)))
	 (stamp (and exe (list (file-write-date exe)
			       (with-open-file (in exe :element-type '(unsigned-byte 8)) (file-length in))))))
    (and stamp
	 (merge-pathnames (format nil "core-~36R" (psx::fnv-1a (prin1-to-string stamp)))
			  (racket-cache-directory)))))

(defun exec-program (path arguments)
  "Replace this process with PATH, run with ARGUMENTS (its argv, the
first the program's name).  Returns only if that fails."
  (let* ((n (length arguments))
	 (argv (sb-alien:make-alien sb-alien:c-string (1+ n))))
    (loop for i from 0 for a in arguments do (setf (sb-alien:deref argv i) a))
    (setf (sb-alien:deref argv n) nil)
    (finish-output *standard-output*)
    (finish-output *error-output*)
    (sb-alien:alien-funcall
     (sb-alien:extern-alien "execv" (function sb-alien:int sb-alien:c-string (* sb-alien:c-string)))
     path argv)))

(defun start-from-racket-core ()
  "Called as --racket=linklet is selected: if this program isn't the saved
image, run it instead, making it first if need be."
  (unless (or (uiop:getenv "PSEUDOSCHEME_RACKET_SAVE_CORE")
	      (uiop:getenv "PSEUDOSCHEME_RACKET_CORE_ACTIVE")
	      ;; opt-in for now: Racket's threads crash in the saved image
	      ;; (a fault in a new thread's stack under the ticker's interrupts)
	      (not (member (uiop:getenv "PSEUDOSCHEME_RACKET_CORE") '("1" "yes" "on") :test #'equal))
	      (null sb-ext:*runtime-pathname*))
    (ignore-errors
     (let ((core (racket-core-path)))
       (when core
	 (unless (probe-file core)
	   (let ((temp (format nil "~A.~36R" (sb-ext:native-namestring core)
			       (random (expt 36 8) (make-random-state t)))))
	     (ensure-directories-exist core)
	     (unwind-protect
		  (when (zerop (nth-value 2 (uiop:run-program
					     (list "env" (format nil "PSEUDOSCHEME_RACKET_SAVE_CORE=~A" temp)
						   (sb-ext:native-namestring sb-ext:*runtime-pathname*) "--no-userinit" "--racket=linklet"
						   "-e" "(void)")
					     :ignore-error-status t :output nil :error-output nil :input nil)))
		    (when (probe-file temp)
		      (sb-posix:rename temp (sb-ext:native-namestring core))))
	       (ignore-errors (when (probe-file temp) (delete-file temp))))))
	 (when (probe-file core)
	   (sb-posix:setenv "PSEUDOSCHEME_RACKET_CORE_ACTIVE" "1" 1)
	   (exec-program (sb-ext:native-namestring core) sb-ext:*posix-argv*)))))))

(defun save-racket-core (path)
  "Instantiate the expander and save this program, so, as PATH, which
doesn't return."
  (unless *expander-instance*
    (unless (instantiate-expander :stream nil)
      (uiop:quit 1)))
  (sb-posix:unsetenv "PSEUDOSCHEME_RACKET_SAVE_CORE")
  (uiop:dump-image path :executable t))

(defun linklet-eval (text)
  "TEXT's forms, each evaluated by Racket's expander in a namespace with
#%kernel (as racket -I '#%kernel -e would), under the default prompt;
the last one's values.  The first use compiles and instantiates the
expander (src/racket/linklet/sources.lisp), about half a minute."
  (let ((save (uiop:getenv "PSEUDOSCHEME_RACKET_SAVE_CORE")))
    (when save (save-racket-core save)))
  (let ((forms (with-input-from-string (in text)
		 (loop for form = (read-racket in) until (eq form *eof*) collect form)))
	(values (list *void*)))
    (setq *uncaught* nil)
    (unless *expander-instance*
      (unless (instantiate-expander :stream nil)
	(racket-error "racket" "the expander didn't instantiate")))
    ;; as LINKLET-LOAD: Lisp errors raised as Racket's outside the base
    ;; of continuations, which is outside each form's default prompt
    (dolist (form forms)
      (with-racket-errors
	(psx::call-with-continuation-base
	 (lambda ()
	   (call-with-continuation-prompt
	    (lambda () (with-racket-errors (setq values (multiple-value-list (expander-eval form)))))
	    *default-prompt-tag* nil)))))
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
