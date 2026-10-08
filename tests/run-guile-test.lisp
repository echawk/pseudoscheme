;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER -*-

;;;; One file of Guile's test suite (vendor/guile-test-suite/) on the
;;;; Guile mode (src/guile/), counted with the suite's own reporters.
;;;; Prints one line, "<file> pass=N fail=N ...", and with -v the
;;;; failures as Guile's user-reporter prints them.
;;;;
;;;; Usage:  sbcl --dynamic-space-size 4GB --control-stack-size 500MB \
;;;;              --script tests/run-guile-test.lisp FILE.test [-v]
;;;; tests/run-guile-tests.sh runs every file.

(require :asdf)

(let ((setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file setup) (load setup)))

(let* ((here (make-pathname :name nil :type nil
			    :defaults (or *load-truename* *load-pathname*)))
       (root (merge-pathnames (make-pathname :directory '(:relative :up)) here)))
  (pushnew (truename root) asdf:*central-registry* :test #'equal))

(let ((*standard-output* (make-broadcast-stream))
      (*error-output* (make-broadcast-stream)))
  (handler-bind ((warning #'muffle-warning))
    (if (find-package "QL")
	(uiop:symbol-call "QL" "QUICKLOAD" :pseudoscheme/guile :silent t)
	(asdf:load-system :pseudoscheme/guile))))

(defparameter *args* (uiop:command-line-arguments))
(defparameter *file* (find-if (lambda (a) (search ".test" a)) *args*))
(defparameter *verbose* (member "-v" *args* :test #'string=))
(defparameter *suite* (asdf:system-relative-pathname :pseudoscheme "vendor/guile-test-suite/"))
;; as Guile's check-guile sets it: popen.test runs tests/popen-child.scm
(sb-posix:setenv "TEST_SUITE_DIR" (string-right-trim "/" (namestring *suite*)) 1)

(setq psx::*full-continuations* t)
(psg:boot)
;; --script implies --lose-on-corruption, which makes a stack overflow
;; fatal; Guile's tests overflow on purpose (call-with-stack-overflow-handler)
(setf (sb-alien:extern-alien "lose_on_corruption_p" sb-alien:int) 0)
;; GUILE_TRACE=1: a backtrace for each Lisp error that becomes a Guile exception
(when (uiop:getenv "GUILE_TRACE") (setq psg::*trace-lisp-errors* t))
;; GUILE_TRACE_CACHE=1: which modules load from the compiled-file cache
(when (uiop:getenv "GUILE_TRACE_CACHE") (setq psg::*trace-cache* t))

(defun guile-string (x)
  (with-output-to-string (s) (write-string (substitute #\/ #\\ (namestring x)) s)))

(let* ((path (merge-pathnames *file*))
       (name (pathname-name path))
       (result
	 (handler-case
	     (psg:eval-string
	      (format nil "(set! %load-path (cons ~S %load-path))
(use-modules (test-suite lib))
;; what (test-suite guile-test)'s main sets: the tests' directory, and
;; where they may write files
(let ((m (resolve-module '(test-suite guile-test))))
  (module-set! m 'test-suite ~S)
  (module-set! m 'tmp-dir (mkdtemp (string-copy \"/tmp/guile-test-XXXXXX\"))))
(define %counter (make-count-reporter))
(register-reporter (car %counter))
~A
(catch #t
  (lambda ()
    (with-test-prefix ~S (load ~S)))
  (lambda (key . args)
    ((car %counter) 'error (list ~S \"file aborted\") (cons key args))
    ~A))
(apply string-append
  (map (lambda (r) (if (zero? (cdr r)) \"\" (string-append \" \" (symbol->string (car r)) \"=\" (number->string (cdr r)))))
       ((cadr %counter))))"
		      (guile-string *suite*)
		      (guile-string (merge-pathnames "tests/" *suite*))
		      (if *verbose* "(register-reporter user-reporter)" "")
		      (concatenate 'string name ".test") (guile-string path)
		      (concatenate 'string name ".test")
		      "(format (current-error-port) \"file aborted: ~s ~s~%\" key args)"))
	   (error (e) (format nil " error=1 (~A)" (remove #\Newline (princ-to-string e)))))))
  (format t "~&~A~A~%" name result)
  (finish-output))
