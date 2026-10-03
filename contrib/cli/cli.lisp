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

(defparameter *version* "0.2")

(defparameter *usage* "Usage: pseudoscheme [option ...] [file [argument ...]]

  --r7rs           R7RS-small (the default)
  --r6rs           R6RS, expanded by psyntax
  --r5rs           R5RS: case-folding reader, the classic translator
  -e, --eval EXPR  evaluate EXPR (Scheme text); may be repeated
  -p, --print EXPR evaluate EXPR and write its value
  -L, --library-path DIR
                   look for libraries in DIR too (foo/bar.sld etc.
                   for (foo bar)); may be repeated
  -i, --interactive
                   start a REPL after running the file or expressions
  --version        print the version and exit
  -h, --help       print this and exit
  --               end of options: the next argument is the file

With a FILE, it is run as a program: for R6RS/R7RS, any (library ...) /
(define-library ...) forms are installed and the (import ...) program
after them is run; for R5RS the file is loaded.  (command-line) is
(FILE ARGUMENT ...).  Without a file or -e/-p, the REPL starts.
In the REPL, ,q quits.
")

(defvar *standard* :r7rs)

(defun evaluate-string (text)
  (ecase *standard*
    (:r7rs (r7rs:eval text))
    (:r6rs (r6rs:eval text))
    (:r5rs (r5rs:eval text))))

(defun run-file (path)
  (ecase *standard*
    (:r7rs (r7rs:load path))
    (:r6rs (r6rs:load path))
    (:r5rs (r5rs:load path))))

(defun repl ()
  (format t "Pseudoscheme ~A (~(~A~)) on ~A ~A~%" *version* *standard*
	  (lisp-implementation-type) (lisp-implementation-version))
  (ecase *standard*
    (:r7rs (r7rs:repl))
    (:r6rs (r6rs:repl))
    (:r5rs (r5rs:repl))))

(defun die (control &rest args)
  (format *error-output* "~&pseudoscheme: ~?~%" control args)
  (finish-output *error-output*)
  (uiop:quit 64))

(defun add-library-path (dir)
  (let ((dir (namestring (uiop:ensure-directory-pathname (uiop:parse-native-namestring dir)))))
    (setf psx:*library-path* (append psx:*library-path* (list dir)))))

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
		((member a '("-i" "--interactive") :test #'string=) (setq interactive t))
		((member a '("-h" "--help") :test #'string=)
		 (write-string *usage*) (uiop:quit 0))
		((string= a "--version")
		 (format t "Pseudoscheme ~A~%" *version*) (uiop:quit 0))
		((and (> (length a) 1) (char= (char a 0) #\-))
		 (die "unknown option ~A (try --help)" a))
		(t (push a args) (return))))))
    (values (nreverse actions) (car args) (cdr args) interactive)))

(defun report-and-exit (e)
  (format *error-output* "~&Error: ~A~%" (string-trim '(#\Newline #\Space) (princ-to-string e)))
  (finish-output *error-output*)
  (uiop:quit 70))

(defun interrupt-p (condition)
  "Control-C?"
  #+sbcl (typep condition 'sb-sys:interactive-interrupt)
  #+ccl (typep condition 'ccl:interrupt-signal-condition)
  #-(or sbcl ccl) (progn condition nil))

(defun main ()
  (ps:disable-float-traps)
  (multiple-value-bind (actions file file-args interactive)
      (parse-arguments (uiop:command-line-arguments))
    (setf ps-r7rs:*command-line* (cons (or file "pseudoscheme") file-args))
    (when (eq *standard* :r5rs) (setf ps:*fold-case* t))
    (handler-bind ((serious-condition
		     (lambda (e)
		       (if (interrupt-p e)
			   (uiop:quit 130)
			   (report-and-exit e)))))
      (dolist (action actions)
	(destructuring-bind (kind text) action
	  (let ((values (multiple-value-list (evaluate-string text))))
	    (when (eq kind :print)
	      (dolist (v values)
		(funcall ps:*scheme-write* v *standard-output*)
		(terpri))))))
      (when file
	(unless (probe-file file) (die "no such file: ~A" file))
	(run-file file)))
    (finish-output)
    (when (or interactive (and (null file) (null actions)))
      (repl))
    (finish-output)
    (uiop:quit 0)))

;;; Initialize everything now, at build time, so it's in the saved image.
(ps-r7rs::boot)
