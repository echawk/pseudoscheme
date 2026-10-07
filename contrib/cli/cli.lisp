; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-CLI -*-

;;;; pseudoscheme: a Scheme you can run from the shell
;;;;
;;;;   pseudoscheme [option ...] [file [argument ...]]
;;;;
;;;; With a file, runs it as a program (and exits with its EXIT status,
;;;; or 0); with no file and no -e/-p, starts a REPL.  See USAGE below.
;;;;
;;;; Built as an executable with ASDF's PROGRAM-OP (contrib/cli/Makefile:
;;;; `make', or `make LISP=ccl'); everything -- the translator, psyntax,
;;;; the R6RS and R7RS libraries -- is loaded and initialized when the
;;;; image is built, so startup is immediate.

(defpackage "PSEUDOSCHEME-CLI"
  (:use "COMMON-LISP")
  (:export "MAIN"))

(in-package "PSEUDOSCHEME-CLI")

(defparameter *version*
  ;; "3.0", from the translator's "Pseudoscheme 3.0" (src/version.scm)
  (let ((v (uiop:symbol-call "SCHEME-TRANSLATOR" "TRANSLATOR-VERSION")))
    (subseq v (1+ (position #\Space v :from-end t)))))

(defparameter *usage* "Usage: pseudoscheme [option ...] [file [argument ...]]

  --r7rs           R7RS-small (the default)
  --r6rs           R6RS, expanded by psyntax
  --r5rs           R5RS: (scheme r5rs), with a case-folding reader
  -e, --eval EXPR  evaluate EXPR (Scheme text); may be repeated
  -p, --print EXPR evaluate EXPR and write its value
  -L, --library-path DIR
                   look for libraries in DIR too (foo/bar.sld etc.
                   for (foo bar)); may be repeated
  --akku           look for libraries in the Akku project's .akku/lib
                   (in the current directory or the nearest one above)
  -l, --lisp-system SYSTEM
                   load the Common Lisp system SYSTEM (with ASDF, or
                   Quicklisp if loaded) first; may be repeated
  --quicklisp      load Quicklisp (~/quicklisp/setup.lisp), so that
                   importing (cl <package>) can fetch missing systems
  --no-userinit    don't load the Lisp's init file (~/.sbclrc for SBCL)
  --continuations=escape
                   compile with escape-only continuations (a
                   continuation can't be called after its call/cc has
                   returned), which is a little faster in code that
                   calls unknown procedures in loops; the default,
                   --continuations=full, makes them re-entrant
  --precompile-srfi
                   expand and compile every bundled SRFI library into
                   the library cache (~/.cache/pseudoscheme/libraries),
                   for the continuation mode given before it, so that
                   later programs load them compiled
  --precompile DIR likewise for the libraries (.sld files) under DIR
  -i, --interactive
                   start a REPL after running the file or expressions
  --version        print the version and exit
  -V               print the version and more as SRFI 176's alist
  -h, --help       print this and exit
  --               end of options: the next argument is the file

PSEUDOSCHEME_LIBRARY_PATH, a list of directories separated by colons,
is searched before the -L directories.  PSEUDOSCHEME_THREAD_STACK_SIZE
is the control stack of each thread a program makes, in megabytes
(default 32; the main thread's is 256).

The Lisp's init file (~/.sbclrc for SBCL) is loaded first, as the Lisp
itself would, so it can set up Quicklisp dists such as Ultralisp, ASDF's
search paths, and so on.

With a FILE, it is run as a program: for R6RS/R7RS, any (library ...) /
(define-library ...) forms are installed and the (import ...) program
after them is run; for R5RS the file is loaded.  (command-line) is
(FILE ARGUMENT ...).  Without a file or -e/-p, the REPL starts.
In the REPL, ,q quits.

Scheme code can use Common Lisp packages as libraries:
  (import (prefix (cl common-lisp) cl:) (pseudoscheme lisp))
A (cl <package>) whose package isn't loaded loads the system of that
name first (see -l and --quicklisp; docs/interop.md).
")

(defvar *standard* :r7rs)
(defvar *precompile-failed* nil "Did a library fail to precompile?")
(defvar *script* nil "Running as one of SRFI 22's script interpreters?")

(defun evaluate-string (text)
  "-e and -p: TEXT's forms, one by one, as at the REPL (so an import or a
definition holds for the next -e too); the last one's values."
  (ecase *standard*
    (:r7rs (pseudoscheme-api::r7rs-repl-eval (pseudoscheme-api:read-scheme-forms text)))
    (:r6rs (pseudoscheme-api::r6rs-repl-eval (pseudoscheme-api:read-scheme-forms text)))
    (:r5rs (r5rs:eval text))))

(defun run-file (path)
  (ecase *standard*
    (:r7rs (r7rs:load path))
    (:r6rs (r6rs:load path))
    (:r5rs (r5rs:load path))))

(defun program-with-main (forms args)
  "FORMS, an R6RS or R7RS program after any libraries, with (main ARGS)
after its body, and what that needs imported, under a prefix."
  (let* ((libraries (loop while (or (psl:keyword-head-p (car forms) "library")
				    (psl:keyword-head-p (car forms) "define-library"))
			  collect (pop forms)))
	 (s (lambda (name) (ps:intern-scheme-symbol name)))
	 (q (funcall s "%srfi-22:quote")))
    (unless (psl:keyword-head-p (car forms) "import")
      (die "~A: not a program" (car ps-r7rs:*command-line*)))
    (append libraries
	    (list (append (car forms)
			  (list (list (funcall s "prefix")
				      (list (funcall s "only") (list (funcall s "rnrs") (funcall s "base"))
					    (funcall s "quote"))
				      (funcall s "%srfi-22:")))))
	    (cdr forms)
	    (list (list (funcall s "main") (list q args))))))

(defun run-script (file)
  "SRFI 22: run FILE, then call its main with the command line; main's
value, if an exit status, is the process's.  Its script prelude is
skipped when FILE is read."
  (let* ((args (copy-list ps-r7rs:*command-line*))
	 (status
	   (ecase *standard*
	     (:r5rs (r5rs:load file)
		    (r5rs:eval (format nil "(main '~A)" (pseudoscheme-api:write-to-string args))))
	     (:r7rs (pseudoscheme-api::ensure-psyntax)
		    (let ((ps-r7rs::*include-directory*
			    (make-pathname :name nil :type nil :defaults (pathname file))))
		      (ps-r7rs::eval-forms (program-with-main (ps-r7rs::read-forms file) args))))
	     (:r6rs (pseudoscheme-api::ensure-psyntax)
		    (psx:eval-forms (program-with-main (psx::read-file-forms file) args))))))
    (finish-output)
    (if (and (integerp status) (<= 0 status 255))
	(uiop:quit status)
	(progn
	  (format *error-output* "~&pseudoscheme: main returned ~A, not an exit status~%"
		  (pseudoscheme-api:write-to-string status))
	  (finish-output *error-output*)
	  (uiop:quit 70)))))

;;; SRFI 22's script interpreters: this program, run as one of these
;;; names (a link to it, say), is FILE ARG ... and calls FILE's main.
(defparameter *script-interpreters*
  '(("scheme-r4rs" . :r5rs) ("scheme-r5rs" . :r5rs) ("scheme-ieee-1178-1990" . :r5rs)
    ("scheme-srfi-0" . :r5rs) ("scheme-r6rs" . :r6rs) ("scheme-r7rs" . :r7rs)))

(defun script-interpreter-standard ()
  (let ((argv0 (car (uiop:raw-command-line-arguments))))
    (and argv0
	 (cdr (assoc (file-namestring argv0) *script-interpreters* :test #'string=)))))

;;; SRFI 138: run as compile-r7rs (a link to this program), it compiles
;;; an R7RS program to an executable, an image of this program with the
;;; program's libraries installed -- expanded and compiled, not yet run --
;;; and the program itself read, to be expanded and run when the
;;; executable starts.  Libraries' bodies run then too, as they would
;;; from source.

(defvar *compiled-program* nil
  "The program forms (import declarations first) a compiled executable runs.")

(defun compile-r7rs (args)
  (let ((output nil) (file nil))
    (loop while args do
      (let ((a (pop args)))
	(flet ((value () (or (pop args) (die "~A needs an argument" a))))
	  (cond ((string= a "-I")
		 (setf psx:*library-path*
		       (cons (namestring (uiop:ensure-directory-pathname
					  (uiop:parse-native-namestring (value))))
			     psx:*library-path*)))
		((string= a "-A") (add-library-path (value)))
		((string= a "-D") (push (value) psl:*scheme-features*))
		((string= a "-o") (setq output (value)))
		((string= a "-V") (print-version-alist) (uiop:quit 0))
		((or file (and (plusp (length a)) (char= (char a 0) #\-)))
		 (die "usage: compile-r7rs [-I dir] [-A dir] [-D feature] [-o output] file"))
		(t (setq file a))))))
    (unless file
      (die "usage: compile-r7rs [-I dir] [-A dir] [-D feature] [-o output] file"))
    (unless (probe-file file) (die "no such file: ~A" file))
    (pseudoscheme-api::ensure-psyntax)
    (handler-bind ((serious-condition #'report-and-exit))
      (let ((ps-r7rs::*include-directory*
	      (make-pathname :name nil :type nil :defaults (truename file))))
	(multiple-value-bind (libraries imports body)
	    (ps-r7rs::split-program (ps-r7rs::read-forms file))
	  (dolist (l libraries) (psx:eval-library (ps-r7rs::library-form l)))
	  (let ((imports (mapcar #'ps-r7rs::translate-import-set imports)))
	    ;; installs the libraries
	    (apply (psx:host-ref "psyntax:environment") imports)
	    (setq *compiled-program* (cons (cons (ps:intern-scheme-symbol "import") imports) body))))))
    ;; for include in the body, when it runs
    (setf ps-r7rs::*include-directory*
	  (make-pathname :name nil :type nil :defaults (truename file)))
    (setq uiop:*image-entry-point* #'run-compiled-program)
    (uiop:dump-image (or output (namestring (make-pathname :type nil :defaults file)))
		     :executable t)))

(defun limit-thread-stacks ()
  "Threads' control stacks: PSEUDOSCHEME_THREAD_STACK_SIZE megabytes, or
32 (the main thread's is 256), since making a thread takes time in
proportion (ps:limit-thread-stack-size)."
  (let ((mb (or (ignore-errors (parse-integer (uiop:getenv "PSEUDOSCHEME_THREAD_STACK_SIZE"))) 32)))
    (ps:limit-thread-stack-size (* mb 1024 1024))))

(defun run-compiled-program ()
  (ps:disable-float-traps)
  (limit-thread-stacks)
  (setf ps-r7rs:*command-line*
	(cons (car (uiop:raw-command-line-arguments)) (uiop:command-line-arguments)))
  (handler-bind ((serious-condition
		   (lambda (e) (if (interrupt-p e) (uiop:quit 130) (report-and-exit e)))))
    (ps-r7rs::eval-forms *compiled-program*))
  (finish-output)
  (uiop:quit 0))

(defun repl ()
  (format t "Pseudoscheme ~A (~(~A~)) on ~A ~A~%" *version* *standard*
	  (lisp-implementation-type) (lisp-implementation-version))
  (ecase *standard*
    (:r7rs (r7rs:repl))
    (:r6rs (r6rs:repl))
    (:r5rs (r5rs:repl))))

(defun print-version-alist ()
  "SRFI 176's version output: one property to a line.  If the output goes
away (pseudoscheme -V | head -1), exit quietly, with failure, as SRFI 176
asks when not all of it was written."
  (handler-case
      (progn
	(dolist (property (ps-r7rs:version-alist))
	  (funcall ps:*scheme-write* property *standard-output*)
	  (terpri))
	(finish-output))
    (stream-error () (uiop:quit 1 nil))))

(defun die (control &rest args)
  (format *error-output* "~&pseudoscheme: ~?~%" control args)
  (finish-output *error-output*)
  (uiop:quit 64))

(defun add-library-path (dir)
  (let ((dir (namestring (uiop:ensure-directory-pathname (uiop:parse-native-namestring dir)))))
    (setf psx:*library-path* (append psx:*library-path* (list dir)))))

(defun akku-library-directory ()
  "The .akku/lib of the Akku project the current directory is in."
  (loop for dir = (uiop:getcwd) then (uiop:pathname-parent-directory-pathname dir)
	for lib = (merge-pathnames ".akku/lib/" dir)
	when (uiop:directory-exists-p lib) return (namestring lib)
	when (equal dir (uiop:pathname-parent-directory-pathname dir))
	  do (die "--akku: no .akku/lib here or above (run akku install first)")))

(defun parse-arguments (args)
  "Returns (values actions file file-args interactive), ACTIONS being a
list of (:eval text) / (:print text)."
  (let ((actions '()) (interactive nil))
    (loop
      (when (null args) (return))
      (let ((a (pop args)))
	(flet ((value ()
		 (or (pop args) (die "~A needs an argument" a))))
	  (cond ((string= a "--") (return))
		((string= a "--r7rs") (setq *standard* :r7rs))
		((string= a "--r6rs") (setq *standard* :r6rs))
		((string= a "--r5rs") (setq *standard* :r5rs))
		((member a '("-e" "--eval") :test #'string=) (push (list :eval (value)) actions))
		((member a '("-p" "--print") :test #'string=) (push (list :print (value)) actions))
		((member a '("-L" "--library-path") :test #'string=) (add-library-path (value)))
		((string= a "--akku") (add-library-path (akku-library-directory)))
		((member a '("-l" "--lisp-system") :test #'string=)
		 (push (list :lisp-system (value)) actions))
		((string= a "--quicklisp") (push (list :quicklisp nil) actions))
		((string= a "--precompile-srfi") (push (list :precompile nil) actions))
		((string= a "--precompile") (push (list :precompile (value)) actions))
		((string= a "--no-userinit") (setq *userinit* nil))
		((string= a "--continuations=full")
		 (setf (symbol-value (find-symbol "*FULL-CONTINUATIONS*" "PSEUDOSCHEME-PSYNTAX")) t))
		((string= a "--continuations=escape")
		 (setf (symbol-value (find-symbol "*FULL-CONTINUATIONS*" "PSEUDOSCHEME-PSYNTAX")) nil))
		((member a '("-i" "--interactive") :test #'string=) (setq interactive t))
		((member a '("-h" "--help") :test #'string=)
		 (write-string *usage*) (uiop:quit 0))
		((string= a "--version")
		 (format t "Pseudoscheme ~A~%" *version*) (uiop:quit 0))
		((string= a "-V")
		 (print-version-alist) (uiop:quit 0))
		((and (> (length a) 1) (char= (char a 0) #\-))
		 (die "unknown option ~A (try --help)" a))
		(t (push a args) (return))))))
    (values (nreverse actions) (car args) (cdr args) interactive)))

#+sbcl
(defparameter *build-sbcl-home* (sb-int:sbcl-homedir-pathname)
  "Where SBCL's contrib modules were when the image was built.  A saved
executable doesn't know (it isn't the sbcl runtime), and Quicklisp, or a
system being loaded, may REQUIRE contribs such as SB-POSIX.")

(defun find-sbcl-contribs ()
  #+sbcl
  (flet ((has-contribs-p (home)
	   (and home (eq (car (pathname-directory home)) :absolute)
		(directory (merge-pathnames "contrib/sb-posix.*" home)))))
    (unless (has-contribs-p (sb-int:sbcl-homedir-pathname))
      (when (has-contribs-p *build-sbcl-home*)
	(setf sb-sys::*sbcl-homedir-pathname* *build-sbcl-home*)))))

(defvar *userinit* t
  "Whether to load the Lisp's init file; --no-userinit clears it.")

(defun user-init-file ()
  "The init file the host Lisp itself would load, if there is one."
  (let ((home (user-homedir-pathname)))
    (probe-file
     (merge-pathnames
      #+sbcl ".sbclrc" #+ccl "ccl-init.lisp" #+ecl ".eclrc" #+clisp ".clisprc.lisp"
      #-(or sbcl ccl ecl clisp) ".lisprc"
      home))))

(defun load-user-init-file ()
  "Load the user's init file, in CL-USER as the Lisp would; an error in it
is reported, and the program runs anyway."
  (let ((file (user-init-file)))
    (when file
      (handler-case
	  (let ((*package* (find-package "CL-USER")))
	    (load file))
	(error (e)
	  (format *error-output* "~&pseudoscheme: error loading ~A: ~A~%"
		  (namestring file) e)
	  (finish-output *error-output*))))))

(defun load-quicklisp ()
  (let ((setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (unless (probe-file setup)
      (die "--quicklisp: no ~A" (namestring setup)))
    (load setup)))

(defun report-and-exit (e)
  (format *error-output* "~&Error: ~A~%" (pseudoscheme-api:error-message e))
  (finish-output *error-output*)
  (uiop:quit 70))

(defun interrupt-p (condition)
  "Control-C?"
  #+sbcl (typep condition 'sb-sys:interactive-interrupt)
  #+ccl (typep condition 'ccl:interrupt-signal-condition)
  #-(or sbcl ccl) (progn condition nil))

(defun main ()
  (ps:disable-float-traps)
  (limit-thread-stacks)
  (find-sbcl-contribs)
  (dolist (dir (uiop:split-string (or (uiop:getenv "PSEUDOSCHEME_LIBRARY_PATH") "") :separator ":"))
    (when (plusp (length dir)) (add-library-path dir)))
  (when (equal (file-namestring (car (uiop:raw-command-line-arguments))) "compile-r7rs")
    (compile-r7rs (uiop:command-line-arguments)))
  (multiple-value-bind (actions file file-args interactive)
      (let ((standard (script-interpreter-standard))
	    (args (uiop:command-line-arguments)))
	(cond ((null standard) (parse-arguments args))
	      ((null args) (die "usage: ~A FILE [ARGUMENT ...]"
				(file-namestring (car (uiop:raw-command-line-arguments)))))
	      (t (setq *standard* standard *script* t)
		 (values '() (car args) (cdr args) nil))))
    (when *userinit* (load-user-init-file))
    ;; With no program, SRFI 193's ("") as in Chez.  A script's name is
    ;; made absolute before it runs, in case it changes directory.
    (setf ps-r7rs:*command-line* (cons (or file "") file-args))
    (when file
      (setf ps-r7rs:*script-file*
            (namestring (merge-pathnames (uiop:parse-native-namestring file) (uiop:getcwd)))))
    (when (eq *standard* :r5rs) (setf ps:*fold-case* t))
    (handler-bind ((serious-condition
		     (lambda (e)
		       (if (interrupt-p e)
			   (uiop:quit 130)
			   (report-and-exit e)))))
      (dolist (action actions)
	(destructuring-bind (kind text) action
	  (case kind
	    (:lisp-system (pseudoscheme-interop:load-lisp-system text))
	    (:quicklisp (load-quicklisp))
	    (:precompile
	     (multiple-value-bind (ok failed)
		 (if text
		     (pseudoscheme-api:precompile-libraries
		      :directory (uiop:ensure-directory-pathname (uiop:parse-native-namestring text)))
		     (pseudoscheme-api:precompile-libraries))
	       (declare (ignore ok))
	       (when (plusp failed) (setq *precompile-failed* t))))
	    (t
	     (let ((values (multiple-value-list (evaluate-string text))))
	       (when (eq kind :print)
		 (dolist (v values)
		   (funcall ps:*scheme-write* v *standard-output*)
		   (terpri))))))))
      (when file
	(unless (probe-file file) (die "no such file: ~A" file))
	(if *script* (run-script file) (run-file file))))
    (finish-output)
    (when (or interactive (and (null file) (null actions)))
      (repl))
    (finish-output)
    (uiop:quit (if *precompile-failed* 1 0))))

;;; Initialize everything now, at build time, so it's in the saved image.
(pseudoscheme-interop:boot)
