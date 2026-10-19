; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-RACKET -*-

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

(in-package "QUASISCHEME-RACKET")

(defun linklet-boot ()
  (unless qsx::*host* (qsx::boot))
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
;;; program's start-up costs.  So the first --racket=linklet run saves an
;;; image of this program with the expander instantiated, in the Racket
;;; cache (its generation, cache.lisp), and each run after replaces itself
;;; with that image (exec), given the same arguments.  The image is made by
;;; a run of this program with QUASISCHEME_RACKET_SAVE_CORE set to its
;;; path; QUASISCHEME_RACKET_CORE=0 turns it off.  It goes by the program
;;; file's date and size, and the images of other builds in the generation
;;; (190 MB each) are deleted as one is made.  Racket's threads once
;;; crashed in it, a fault at the bottom of a new thread's stack: the
;;; image's thread stacks are the command line's 32 MB (LIMIT-THREAD-
;;; STACKS, saved with the image), and the scheduler raised the size to
;;; 64 MB, while SBCL gives a new thread the memory of a finished one
;;; (the finalizer thread's) whatever the size now is (qs:LIMIT-THREAD-
;;; STACK-SIZE only lowers it).  The expander
;;; is only instantiated, not booted: that, and the collection paths,
;;; are the run's own (EXPANDER-EVAL).

(defun racket-core-path ()
  "Where this program's image with the expander instantiated is kept: by the
program file's date and size, as well as the cache's own key."
  (let* ((exe (and sb-ext:*runtime-pathname* (probe-file sb-ext:*runtime-pathname*)))
	 (stamp (and exe (list (file-write-date exe)
			       (with-open-file (in exe :element-type '(unsigned-byte 8)) (file-length in))))))
    (and stamp
	 (merge-pathnames (format nil "core-~36R" (qsx::fnv-1a (prin1-to-string stamp)))
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
  (unless (or (uiop:getenv "QUASISCHEME_RACKET_SAVE_CORE")
	      (uiop:getenv "QUASISCHEME_RACKET_CORE_ACTIVE")
	      (member (uiop:getenv "QUASISCHEME_RACKET_CORE") '("0" "no" "off") :test #'equal)
	      (null sb-ext:*runtime-pathname*))
    (ignore-errors
     (let ((core (racket-core-path)))
       (when core
	 (unless (probe-file core)
	   ;; the finished images of other builds, not a run's partial one
	   (dolist (old (directory (merge-pathnames (make-pathname :name :wild :type nil) core)))
	     (when (and (uiop:string-prefix-p "core-" (pathname-name old)) (null (pathname-type old)))
	       (ignore-errors (delete-file old))))
	   (let ((temp (format nil "~A.~36R" (sb-ext:native-namestring core)
			       (random (expt 36 8) (make-random-state t)))))
	     (ensure-directories-exist core)
	     (unwind-protect
		  (when (zerop (nth-value 2 (uiop:run-program
					     (list "env" (format nil "QUASISCHEME_RACKET_SAVE_CORE=~A" temp)
						   (sb-ext:native-namestring sb-ext:*runtime-pathname*) "--no-userinit" "--racket=linklet"
						   "-e" "(void)")
					     :ignore-error-status t :output nil :error-output nil :input nil)))
		    (when (probe-file temp)
		      (sb-posix:rename temp (sb-ext:native-namestring core))))
	       (ignore-errors (when (probe-file temp) (delete-file temp))))))
	 (when (probe-file core)
	   (sb-posix:setenv "QUASISCHEME_RACKET_CORE_ACTIVE" "1" 1)
	   (exec-program (sb-ext:native-namestring core) sb-ext:*posix-argv*)))))))

(defun save-racket-core (path)
  "Instantiate the expander and save this program, so, as PATH, which
doesn't return."
  (unless *expander-instance*
    (unless (instantiate-expander :stream nil)
      (uiop:quit 1)))
  (sb-posix:unsetenv "QUASISCHEME_RACKET_SAVE_CORE")
  (uiop:dump-image path :executable t))

(defun linklet-eval (text)
  "TEXT's forms, each evaluated by Racket's expander in a namespace with
#%kernel (as racket -I '#%kernel -e would), under the default prompt;
the last one's values.  The first use compiles and instantiates the
expander (src/racket/linklet/sources.lisp), about half a minute."
  (let ((save (uiop:getenv "QUASISCHEME_RACKET_SAVE_CORE")))
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
	(qsx::call-with-continuation-base
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
	  (qsx::call-with-continuation-base
	   (lambda ()
	  (call-with-continuation-prompt
	   (lambda ()
	     (with-racket-errors
	       (if name
		   (setf (gethash name named) (instantiate-linklet compiled imports))
		   (let ((values (multiple-value-list (instantiate-linklet compiled imports target))))
		     (dolist (v values)
		       (unless (eq v *void*)
			 (funcall (qsx:host-ref "write") v output)
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
	(qsx::call-with-continuation-base
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

;;; ------------------------------------------------------------------
;;; Executables (contrib/cli: --racket --compile)

(defun racket-eval-at-base (text)
  "TEXT's forms evaluated by the expander as LINKLET-LOAD does its own, and
the last one's values."
  (let ((forms (read-racket-forms-from-string text))
	(values (list *void*)))
    (setq *uncaught* nil)
    (unless *expander-instance*
      (unless (instantiate-expander :stream nil)
	(racket-error "racket" "the expander didn't instantiate")))
    (with-racket-errors
      (qsx::call-with-continuation-base
       (lambda ()
	 (call-with-continuation-prompt
	  (lambda ()
	    (with-racket-errors
	      (dolist (form forms)
		(setq values (multiple-value-list (expander-eval form))))))
	  *default-prompt-tag* nil))))
    (when *uncaught* (finish-output) (uiop:quit 1))
    (values-list values)))

(defun read-racket-forms-from-string (text)
  (with-input-from-string (in text)
    (loop for form = (read-racket in) until (eq form *eof*) collect form)))

(defun linklet-prepare-module (path)
  "Make PATH's module ready to run from a saved image: declare it (which
compiles it, and declares what it requires), and instantiate the modules
of the collections that it, and the modules of its own that it requires,
require, so that the program's start-up has only its own modules to run.
Where the program is the module's own business (a module of the user's
is not instantiated) and the collections are where the Racket's are
(find-system-path 'collects-dir)."
  ;; relative requires are resolved against the module's directory
  (let ((cwd (funcall *current-directory*)))
    (funcall *current-directory* (make-rpath (directory-namestring path)))
    (unwind-protect (prepare-module-1 path)
      (funcall *current-directory* cwd))))

(defun prepare-module-1 (path)
  (racket-eval-at-base
   (format nil "
(let-values ([(main) (list 'file ~S)]
             [(collects) (path->string (path->complete-path (find-system-path 'collects-dir)))])
  (letrec-values
      ([(seen) (make-hash)]
       [(under?) (lambda (p)
                   (if (path? p)
                       (let-values ([(s) (path->string p)])
                         (if (>= (string-length s) (string-length collects))
                             (string=? (substring s 0 (string-length collects)) collects)
                             #f))
                       #f))]
       [(walk) (lambda (mod)
                 (if (hash-ref seen mod #f)
                     (void)
                     (begin
                       (hash-set! seen mod #t)
                       (module-declared? mod #t)
                       (for-each
                        (lambda (phase+mpis)
                          (if (eqv? (car phase+mpis) 0)
                              (for-each
                               (lambda (mpi)
                                 (let-values ([(name) (resolved-module-path-name
                                                       (module-path-index-resolve mpi #t))])
                                   (if (path? name)
                                       (if (under? name)
                                           (dynamic-require name #f)
                                           (walk name))
                                       (void))))
                               (cdr phase+mpis))
                              (void)))
                        (module->imports mod)))))])
    (walk main)))" path)))
