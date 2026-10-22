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
	(uiop:symbol-call "QL" "QUICKLOAD" :quasischeme/guile :silent t)
	(asdf:load-system :quasischeme/guile))))

(defparameter *args* (uiop:command-line-arguments))
(defparameter *file* (find-if (lambda (a) (search ".test" a)) *args*))
(defparameter *verbose* (member "-v" *args* :test #'string=))
(defparameter *suite* (asdf:system-relative-pathname :quasischeme "vendor/guile-test-suite/"))
;; as Guile's check-guile sets it: popen.test runs tests/popen-child.scm
(sb-posix:setenv "TEST_SUITE_DIR" (string-right-trim "/" (namestring *suite*)) 1)

(setq qsx::*full-continuations* t)
(qsg:boot)
;; --script implies --lose-on-corruption, which makes a stack overflow
;; fatal; Guile's tests overflow on purpose (call-with-stack-overflow-handler)
(setf (sb-alien:extern-alien "lose_on_corruption_p" sb-alien:int) 0)
;; GUILE_TRACE=1: a backtrace for each Lisp error that becomes a Guile exception
(when (uiop:getenv "GUILE_TRACE") (setq qsg::*trace-lisp-errors* t))
;; GUILE_TRACE_CACHE=1: which modules load from the compiled-file cache
(when (uiop:getenv "GUILE_TRACE_CACHE") (setq qsg::*trace-cache* t))
;; GUILE_VM_PRELOAD="srfi/srfi-1 ice-9/q": load those modules from the
;; installed Guile's compiled .go files, on the VM (src/guile/vm.lisp),
;; before the tests: the tests then call the VM's code
(let ((preload (uiop:getenv "GUILE_VM_PRELOAD")))
  (when preload
    (dolist (name (uiop:split-string preload :separator " "))
      (when (plusp (length name))
	(qsg::call-with-guile-catch
	 (lambda ()
	   (funcall (qsg::load-go-file (qsg::installed-go-file name)))))))))

;; A stand-in for Guile's source tree, which %guile-build-info's
;; top_srcdir names and ftw.test, ports.test and c-api.test look into:
;; test-suite/ (its tests and its library, linked to the vendored ones),
;; module/ (the installed Guile's Scheme sources), libguile/ (no C here)
(defparameter *quasischeme-command*
  (asdf:system-relative-pathname :quasischeme "bin/quasischeme"))

(defun make-source-tree ()
  (let ((top (sb-posix:mkdtemp "/tmp/guile-srcdir-XXXXXX")))
    (flet ((dir (name) (ensure-directories-exist (format nil "~A/~A/" top name)))
	   (file (name) (with-open-file (s (format nil "~A/~A" top name) :direction :output)))
	   (link (from name) (sb-posix:symlink (string-right-trim "/" (namestring from))
					       (format nil "~A/~A" top name))))
      (mapc #'dir '("libguile" "test-suite/tests" "test-suite/test-suite" "test-suite/standalone"))
      (mapc #'file '("README" "COPYING" "Makefile.am" "test-suite/Makefile.am"))
      (link qsg::*guile-source-directory* "module")
      (dolist (f (directory (merge-pathnames "tests/*.*" *suite*)))
	(link f (format nil "test-suite/tests/~A.~A" (pathname-name f) (pathname-type f))))
      (dolist (f '("lib.scm" "guile-test" "lib"))
	(link (merge-pathnames (format nil "test-suite/~A" f) *suite*) (format nil "test-suite/test-suite/~A" f))))
    top))

(defun guile-string (x)
  (with-output-to-string (s) (write-string (substitute #\/ #\\ (namestring x)) s)))

(let* ((path (merge-pathnames *file*))
       (name (pathname-name path))
       (top (make-source-tree))
       (result
	 (handler-case
	     (qsg:eval-string
	      (format nil "(set! %load-path (cons ~S %load-path))
(use-modules (test-suite lib))
;; what (test-suite guile-test)'s main sets: the tests' directory, and
;; where they may write files, which is also the working directory
(let ((m (resolve-module '(test-suite guile-test)))
      (tmp (mkdtemp (string-copy \"/tmp/guile-test-XXXXXX\"))))
  (module-set! m 'test-suite ~S)
  (module-set! m 'tmp-dir tmp)
  (chdir tmp)
  ;; meta/guile, Guile's build tree's script (popen.test runs it): here
  ;; bin/quasischeme --guile, if it's built
  (when (file-exists? ~S)
    (mkdir \"meta\")
    (call-with-output-file \"meta/guile\" (lambda (p) (display ~S p)))
    (chmod \"meta/guile\" #o755)))
(set! %guile-build-info
      (append '((top_srcdir . ~S) (srcdir . ~S)) %guile-build-info))
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
		      (guile-string *quasischeme-command*)
		      (format nil "#!/bin/sh
exec ~A --guile \"$@\"
" (guile-string *quasischeme-command*))
		      top (concatenate 'string top "/libguile")
		      (if *verbose* "(register-reporter user-reporter)" "")
		      (concatenate 'string name ".test") (guile-string path)
		      (concatenate 'string name ".test")
		      "(format (current-error-port) \"file aborted: ~s ~s~%\" key args)"))
	   (error (e) (format nil " error=1 (~A)" (remove #\Newline (princ-to-string e)))))))
  (format t "~&~A~A~%" name result)
  (finish-output)
  ;; rm doesn't follow the links
  (uiop:run-program (list "rm" "-rf" top) :ignore-error-status t))
