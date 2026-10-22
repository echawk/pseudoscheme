; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-RACKET -*-

;;;; racket's command line, for `quasischeme --racket ARG ...' (and this
;;;; program run as `racket'): the switches of `racket -h', parsed as
;;;; Racket CS's main.sps parses them (racket/src/cs/main.sps of the
;;;; pinned release, its flags-loop), and run as it runs them:
;;;;
;;;;   - the init library (racket/init, or -I's) required, unless -n, or
;;;;     a -t/-l/-p/-u came before any -i/-e/-f/-r;
;;;;   - the loads, in order, under the default prompt: the first that
;;;;     escapes (an uncaught exception) makes the exit status 1 and
;;;;     stops the rest;
;;;;   - the REPL, for -i, or when only configuration switches were given.
;;;;
;;;; The first argument that isn't a switch, with only configuration
;;;; switches before it, is -u's: `racket file arg ...' requires FILE
;;;; with (current-command-line-arguments) the args.  Single-letter
;;;; switches combine, their arguments after them: -ifve file expr.
;;;;
;;;; What runs is in a Lisp function per load, which calls the expander's
;;;; own procedures, so that nothing of it is evaluated in the namespace
;;;; (where -I's language, Typed Racket say, would check it).  Switches
;;;; for what Quasischeme hasn't (-k and -Y's embedded code, -C and the
;;;; cross-compiler's) are errors; -y, -d, -b, -j, -M, -V and the logging
;;;; levels are accepted and change nothing here.

(in-package "QUASISCHEME-RACKET")

(defparameter *racket-help* "usage: racket [<option> ...] <argument> ...

File and expression options:

  -e <exprs>, --eval <exprs>
     Evaluate <exprs>, print results
  -f <file>, --load <file>
     Like -e '(load \"<file>\")' without printing, or
     evaluates all from input when <file> is \"-\"
  -t <file>, --require <file>
     Like -e '(require (file \"<file>\"))' [*]
  -l <path>, --lib <path>
     Like -e '(require (lib \"<path>\"))' [*]
  -p <package>
     Like -e '(require (planet \"<package>\"))' [*]
  -r <file>, --script <file>
     Same as -f <file> -N <file> --
  -u <file>, --require-script <file>
     Same as -t <file> -N <file> --
  -m, --main
     Call `main` with command-line arguments, print results

 [*] Also `require`s a `main` submodule, if any

Interaction options:

  -i, --repl
     Run interactive read-eval-print loop; implies -v
  -n, --no-lib
     Skip `(require (lib \"<init-lib>\"))` for -i/-e/-f/-r
  -v, --version
     Show version
  -V, --no-yield
     Same as -v here

Configuration options:

  -c, --no-compiled
     Disable loading of compiled files
  -q, --no-init-file
     Skip load of racketrc.rktl for -i
  -I <path>
     Set <init-lib> to <path> (sets language)
  -X <dir>, --collects <dir>
     Main collects at <dir> (or \"\" disables all)
  -S <dir>, --search <dir>
     More collects at <dir> (after main collects)
  -G <dir>, --config <dir>
     Main configuration directory at <dir>
  -A <dir>, --addon <dir>
     Addon directory at <dir>
  -U, --no-user-path
     Ignore user-specific collects, etc.
  -N <file>, --name <file>
     Sets `(find-system-path 'run-file)` to <file>
  -E <file>, --exec <file>
     Sets `(find-system-path 'exec-file)` to <file>
  -y, -d, -b, -j, -M, -R <paths>, -W <levels>, -O <levels>, -L <levels>
     Accepted; no effect in Quasischeme
  (-k, -Y, -C and the cross-compiling switches aren't supported)

Meta options:

  --
     No argument following this switch is used as a switch
  -Z
     Ignore the argument following this switch
  -h, --help
     Show this information and exits, ignoring other options

Default options:

  * If only configuration options are provided, -i is added
  * If only configuration options are before the first
    argument, -u is added
  * If -t/-l/-p/-u appears before the first -i/-e/-f/-r,
    -n is added
  * <init-lib> defaults to racket/init

Switch syntax:

  Multiple single-letter switches can be collapsed, with
  arguments placed after the collapsed switches; the first
  collapsed switch cannot be --

  For example,

      -ifve file expr

  is the same as

      -i -f file -v -e expr

Quasischeme runs this as `quasischeme --racket <option> ...' (its own
options before --racket; docs/racket.md).
")

(defstruct (racket-command-line (:conc-name rcl-))
  (init-library (list (rsym "lib") "racket/init")) ; NIL for -n
  (loads '())				; reversed: (:require PATH) (:load FILE) (:eval TEXT) (:main)
  (repl nil)
  (repl-init t)				; racket/interactive, the init file (-q: not)
  (version nil)				; the banner (-v, -i)
  (no-compiled nil)			; -c
  (arguments '()))			; current-command-line-arguments

(defun racket-startup-error (format &rest args)
  "racket's startup-error: the message, after the program's name, and
exit status 1."
  (format *error-output* "~&racket: ~?~%" format args)
  (finish-output *error-output*)
  (uiop:quit 1))

(defun parse-racket-command-line (arguments)
  "ARGUMENTS (strings) as racket's command line: a RACKET-COMMAND-LINE.
-h prints the help and exits; a bad switch is a startup error.  Sets the
system paths -N, -E, -X, -A and -G name, and the collection options."
  (let ((cl (make-racket-command-line))
	(saw '()))
    (labels ((saw-p (tag) (member tag saw))
	     (see (&rest tags) (dolist (tag tags) (pushnew tag saw)))
	     (no-init () (unless (saw-p :top) (setf (rcl-init-library cl) nil)))
	     (add-load (load) (push load (rcl-loads cl)))
	     (set-path (kind path) (push (cons kind path) *system-paths*))
	     (next-arg (what flag within args)
	       ;; the first plain argument after ARGS' head; switches split
	       ;; from the same combined one stay before the rest
	       (loop for tail on (cdr args)
		     for a = (car tail)
		     if (consp a) collect a into accum
		     else do (return-from next-arg (values a (append accum (cdr tail)))))
	       (racket-startup-error "missing ~A after ~A switch" what (or within flag)))
	     (check-path (path what flag within)
	       (when (equal path "")
		 (racket-startup-error "empty ~A after ~A switch" what (or within flag))))
	     (finish (args)
	       (cond ((and args (not (saw-p :non-config)))
		      (flags (cons "-u" args)))
		     (t (setf (rcl-arguments cl) args)
			(if (and (null args) (not (saw-p :non-config)))
			    (setf (rcl-repl cl) t (rcl-version cl) t)
			    (no-init)))))
	     (flags (args)
	       (when (null args) (return-from flags (finish args)))
	       (let* ((arg (car args))
		      (within (and (consp arg) (cdr arg)))
		      (arg (if (consp arg) (car arg) arg)))
		 (macrolet ((with-arg ((var what) &body body)
			      `(multiple-value-bind (,var rest) (next-arg ,what arg within args)
				 ,@body)))
		   (flet ((is (&rest names) (member arg names :test #'string=))
			  (bad () (racket-startup-error "bad switch: ~A~@[ within: ~A~]" arg within)))
		     (cond
		       ((is "-l" "--lib")
			(with-arg (lib "library name")
			  (add-load (list :require (list (rsym "lib") lib)))
			  (no-init) (see :non-config :lib) (flags rest)))
		       ((is "-t" "--require")
			(with-arg (file "file name")
			  (add-load (list :require (list (rsym "file") file)))
			  (no-init) (see :non-config :lib) (flags rest)))
		       ((is "-p")
			(with-arg (package "package")
			  (add-load (list :require (list (rsym "planet") package)))
			  (no-init) (see :non-config :lib) (flags rest)))
		       ((is "-u" "--require-script")
			(with-arg (file "file name")
			  (add-load (list :require (list (rsym "file") file)))
			  (no-init) (check-path file "file name" arg within)
			  (set-path "run-file" file)
			  (see :non-config :lib) (flags (cons "--" rest))))
		       ((is "-f" "--load")
			(with-arg (file "file name")
			  (add-load (list :load file))
			  (see :non-config :top) (flags rest)))
		       ((is "-r" "--script")
			(with-arg (file "file name")
			  (add-load (list :load file))
			  (check-path file "file name" arg within)
			  (set-path "run-file" file)
			  (see :non-config :top) (flags (cons "--" rest))))
		       ((is "-e" "--eval")
			(with-arg (text "expression")
			  (add-load (list :eval text))
			  (see :non-config :top) (flags rest)))
		       ((is "-m" "--main")
			(add-load (list :main))
			(see :non-config :top) (flags (cdr args)))
		       ((is "-i" "--repl")
			(setf (rcl-repl cl) t (rcl-version cl) t)
			(see :non-config :top) (flags (cdr args)))
		       ((is "-n" "--no-lib")
			(setf (rcl-init-library cl) nil)
			(see :non-config) (flags (cdr args)))
		       ((is "-v" "--version" "-V" "--no-yield")
			(setf (rcl-version cl) t)
			(see :non-config) (flags (cdr args)))
		       ((is "-I")
			(with-arg (lib "library name")
			  (when (rcl-init-library cl)
			    (setf (rcl-init-library cl) (list (rsym "lib") lib)))
			  (see :something) (flags rest)))
		       ((is "-q" "--no-init-file")
			(setf (rcl-repl-init cl) nil) (see :something) (flags (cdr args)))
		       ((is "-c" "--no-compiled")
			(setf (rcl-no-compiled cl) t) (see :something) (flags (cdr args)))
		       ((is "-y" "--make" "-d" "--no-delay" "-b" "--binary" "-j" "--no-jit"
			    "-M" "--compile-any")
			(see :something) (flags (cdr args)))
		       ((is "-X" "--collects")
			(with-arg (dir "collects path")
			  (if (equal dir "")
			      (setf (getf *collection-options* :no-collects) t)
			      (set-path "collects-dir" (namestring (uiop:ensure-directory-pathname
								     (merge-pathnames dir (uiop:getcwd))))))
			  (see :something) (flags rest)))
		       ((is "-S" "--search")
			(with-arg (dir "path")
			  (check-path dir "collects path" dir within)
			  (setf (getf *collection-options* :search)
				(append (getf *collection-options* :search)
					(list (namestring (merge-pathnames dir (uiop:getcwd))))))
			  (see :something) (flags rest)))
		       ((is "-G" "--config")
			(with-arg (dir "config path")
			  (check-path dir "config path" dir within)
			  (set-path "config-dir" (namestring (merge-pathnames dir (uiop:getcwd))))
			  (see :something) (flags rest)))
		       ((is "-A" "--addon")
			(with-arg (dir "addon directory")
			  (set-path "addon-dir" (namestring (merge-pathnames dir (uiop:getcwd))))
			  (see :something) (flags rest)))
		       ((is "-U" "--no-user-path")
			(setf (getf *collection-options* :no-user-paths) t)
			(see :something) (flags (cdr args)))
		       ((is "-R" "--compiled" "-W" "--stderr" "-O" "--stdout" "-L" "--syslog")
			(with-arg (value "argument")
			  (declare (ignore value))
			  (see :something) (flags rest)))
		       ((is "-N" "--name")
			(with-arg (name "name")
			  (check-path name "name" arg within)
			  (set-path "run-file" name)
			  (see :something) (flags rest)))
		       ((is "-E" "--exec")
			(with-arg (name "name")
			  (check-path name "name" arg within)
			  (set-path "exec-file" name)
			  (see :something) (flags rest)))
		       ((is "-Z")
			(with-arg (ignored "argument to ignore")
			  (declare (ignore ignored))
			  (flags rest)))
		       ((is "-k" "-Y" "-C" "--cross" "--compile-machine" "--cross-compiler" "--cross-server")
			(racket-startup-error "~A: not supported by Quasischeme (docs/racket.md)" arg))
		       ((is "-h" "--help")
			(write-string *racket-help*)
			(finish-output)
			(uiop:quit 0))
		       ((is "--")
			(if (or (null (cdr args)) (not (consp (cadr args))))
			    (finish (cdr args))
			    ;; more switches split from a combined one
			    (flags (list* (cadr args) (car args) (cddr args)))))
		       ((and (> (length arg) 1) (char= (char arg 0) #\-))
			(if (and (> (length arg) 2) (char/= (char arg 1) #\-))
			    ;; -ifve: one switch per letter, each "within" ARG
			    (flags (append (map 'list (lambda (c) (cons (format nil "-~C" c) arg))
						(subseq arg 1))
					   (cdr args)))
			    (bad)))
		       (t (finish args))))))))
      (flags arguments))
    cl))

;;; ------------------------------------------------------------------
;;; Running it

(defun expander-call (name &rest args)
  (apply (expander-export name) args))

(defun kernel-ref (name)
  "#%kernel's NAME (the expander's own, re-exported)."
  (expander-call "dynamic-require" (list (rsym "quote") (rsym "#%kernel")) (rsym name)))

(defvar *need-runtime-configure* t
  "Whether NAMESPACE-REQUIRE+ is still to configure the runtime, as for the
first module the command line requires.")

(defun configure-runtime (mpi)
  "main.sps's configure-runtime: MPI's configure-runtime submodule, if it
has one, and its module language info's configure-runtime entries."
  (let ((config (expander-call "module-path-index-join"
			       (list (rsym "submod") "." (rsym "configure-runtime")) mpi)))
    (when (truthy (expander-call "module-declared?" config qs:true))
      (expander-call "dynamic-require" config qs:false)))
  (let ((info (expander-call "module->language-info" mpi qs:true)))
    (when (and (simple-vector-p info) (= (length info) 3))
      (flet ((info-load (info)
	       (funcall (expander-call "dynamic-require" (svref info 0) (svref info 1)) (svref info 2))))
	(let ((infos (funcall (info-load info) (rsym "configure-runtime") '())))
	  (unless (and (listp infos)
		       (every (lambda (i) (and (simple-vector-p i) (= (length i) 3))) infos))
	    (racket-error "runtime-configure" "contract violation~%  expected: (listof (vector any any any))"))
	  (mapc #'info-load infos))))))

(defun namespace-require+ (module-path)
  "main.sps's namespace-require+: MODULE-PATH required into the namespace
(the first one configuring the runtime), then its main submodule run, if
it has one."
  (let ((mpi (expander-call "module-path-index-join" module-path qs:false)))
    (when *need-runtime-configure*
      (configure-runtime mpi)
      (setq *need-runtime-configure* nil))
    (expander-call "namespace-require" mpi)
    (let ((main (expander-call "module-path-index-join" (list (rsym "submod") "." (rsym "main")) mpi)))
      (when (truthy (expander-call "module-declared?" main qs:true))
	(expander-call "dynamic-require" main qs:false)))))

(defun print-results (values)
  (dolist (v values)
    (funcall (funcall (primitive "current-print")) v)
    (funcall (primitive "flush-output"))))

(defun eval-all (in)
  "main.sps's eval-all: each datum read from port IN evaluated as
(#%top-interaction . datum), its results printed."
  (let ((read (kernel-ref "read")))
    (loop for datum = (funcall read in)
	  until (eq datum qs:eof-object)
	  do (print-results (multiple-value-list
			     (expander-call "eval" (cons (rsym "#%top-interaction") datum)))))))

(defun call-main (arguments)
  "-m: main applied to the command-line arguments, its results printed."
  (let ((m (expander-call "namespace-datum-introduce" (rsym "main"))))
    (unless (truthy (expander-call "identifier-binding" m))
      (expander-call "namespace-variable-value" (rsym "main") qs:false
		     (lambda () (racket-error "main" "not defined or required into the top-level environment"))))
    (print-results (multiple-value-list (expander-call "eval" (cons (rsym "main") arguments))))))

(defun run-load (load arguments)
  (ecase (car load)
    (:require (namespace-require+ (second load)))
    (:load (if (equal (second load) "-")
	       (eval-all (funcall (primitive "current-input-port")))
	       (funcall (kernel-ref "load") (second load))))
    (:eval (eval-all (make-input-string-port (second load) "string")))
    (:main (call-main arguments))))

(defun call-at-racket-base (thunk)
  "THUNK under the base of continuations and the default prompt, as
LINKLET-LOAD runs a module; whether it went without an uncaught
exception."
  (setq *uncaught* nil)
  (with-racket-errors
    (qsx::call-with-continuation-base
     (lambda ()
       (call-with-continuation-prompt
	(lambda () (with-racket-errors (funcall thunk)))
	*default-prompt-tag* nil))))
  (not *uncaught*))

(defun run-racket-command-line (arguments &key (repl (lambda (&key init-file)
						       (linklet-repl :banner nil :init-library nil
								     :init-file init-file))))
  "racket's command line ARGUMENTS, run as racket runs it; the exit
status.  REPL runs the loop, given whether to load the init file."
  (boot)
  (let ((save (uiop:getenv "QUASISCHEME_RACKET_SAVE_CORE")))
    (when save (save-racket-core save)))
  (let ((cl (parse-racket-command-line arguments))
	(status 0))
    (boot-expander)
    (let ((arguments (rcl-arguments cl)))
      (funcall *current-command-line-arguments* (immutable! (coerce arguments 'simple-vector)))
      (when (rcl-no-compiled cl)
	(funcall (kernel-ref "use-compiled-file-paths") '()))
      (when (rcl-version cl) (call-at-racket-base #'write-banner))
      (unless (call-at-racket-base
	       (lambda ()
		 (when (rcl-init-library cl) (namespace-require+ (rcl-init-library cl)))
		 (dolist (load (reverse (rcl-loads cl)))
		   (run-load load arguments))))
	(setq status 1))
      (when (rcl-repl cl)
	(setq status (funcall repl :init-file (rcl-repl-init cl)))))
    (print-stats)
    (finish-output)
    status))
