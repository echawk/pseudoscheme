; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER -*-

;;;; Runs Racket's R6RS test suite (tests/r6rs/, from
;;;; github.com/racket/r6rs, MIT/Apache-2.0 -- see LICENSE-racket.txt)
;;;; under psyntax + Pseudoscheme.  Each tests/r6rs/run/**/*.sps program
;;;; runs in a freshly booted host, and the "N tests passed" / "F of N
;;;; tests failed" line it prints is tallied.
;;;;
;;;; Usage:  sbcl --dynamic-space-size 4GB --control-stack-size 500MB \
;;;;              --script tests/run-r6rs-tests.lisp [-v] [--continuations=escape] [name ...]
;;;; e.g. ... --script tests/run-r6rs-tests.lisp lists sorting

(require :asdf)

;; The dependencies (float-features, cl-unicode, ...) come from
;; Quicklisp when it's installed; QUICKLOAD fetches any that are missing.
(let ((setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file setup) (load setup)))

(defun load-system (system)
  (if (find-package "QL")
      (uiop:symbol-call "QL" "QUICKLOAD" system :silent t)
      (asdf:load-system system)))

(defvar *root*
  (let ((here (make-pathname :name nil :type nil
			     :defaults (or *load-truename* *load-pathname*))))
    (truename (merge-pathnames (make-pathname :directory '(:relative :up)) here))))
(pushnew *root* asdf:*central-registry* :test #'equal)

(let ((*standard-output* (make-broadcast-stream))
      (*error-output* (make-broadcast-stream)))
  (handler-bind ((warning #'muffle-warning))
    (load-system :pseudoscheme/r6rs)
    (load-system :bordeaux-threads)))

;; IEEE inexact arithmetic: (/ 1. 0.) => +inf.0, as R6RS/R7RS expect.
(ps:disable-float-traps)

(defparameter *verbose* (member "-v" (uiop:command-line-arguments) :test #'string=))
;; --continuations=escape: compile the programs and the libraries they load
;; with escape-only continuations (src/continuations.lisp).
(defparameter *full* (not (member "--continuations=escape" (uiop:command-line-arguments) :test #'string=)))
(defparameter *only* (remove-if (lambda (a) (char= (char a 0) #\-))
				(uiop:command-line-arguments)))

(defun programs ()
  ;; run/run.sps and run/test.sps aren't tests of a library: the first
  ;; runs everything (in one program, which we'd rather not), the second
  ;; tests the testing library, against a version of it we don't have.
  (remove-if (lambda (p) (member (short-name p) '("run" "test") :test #'string=))
	     (sort (mapcar #'namestring
			   (directory (merge-pathnames "tests/r6rs/run/**/*.sps" *root*)))
		   #'string<)))

(defvar *scratch*
  (uiop:ensure-directory-pathname
   (merge-pathnames (format nil "pseudoscheme-r6rs-~D/" (random 1000000 (make-random-state t)))
		    (uiop:temporary-directory))))
(ensure-directories-exist *scratch*)

(defun short-name (path)
  (let* ((run (search "/run/" path)))
    (subseq path (+ run 5) (- (length path) 4))))

(defun run-one (path)
  "Returns (values passed failed status) for one program."
  (let* ((out (make-string-output-stream))
	 (status
	   (handler-case
	       ;; WITH-TIMEOUT interrupts even a CPU-bound loop (a
	       ;; deadline only covers blocking operations).
	       (bt:with-timeout (60)
		 (let ((*standard-output* out)
		       (psx:*library-path* (list (namestring *root*)))
		       ;; The io tests create files: keep them out of the repo.
		       (*default-pathname-defaults* *scratch*))
		   (psx::boot)
		   (let ((psx::*full-continuations* (and *full* t)))
		     (psx:load-file path))
		   :ok))
	     (bt:timeout () :timeout)
	     (serious-condition (e)
	       (list :error (remove #\Newline (princ-to-string e))))))
	 (text (get-output-stream-string out)))
    (when *verbose* (write-string text))
    (multiple-value-bind (passed failed)
	(let ((p (search " tests passed" text))
	      (f (search " tests failed." text)))
	  (cond (p (values (parse-integer text :start (1+ (or (position #\Newline text :end p :from-end t) -1))
					       :end p :junk-allowed t)
			   0))
		(f (let* ((line-start (1+ (or (position #\Newline text :end f :from-end t) -1)))
			  (line (subseq text line-start f))
			  (nums (with-input-from-string (s (substitute #\Space #\f line))
				  (loop for x = (read s nil) while x when (integerp x) collect x))))
		     (values (- (second nums) (first nums)) (first nums))))
		(t (values 0 0))))
      (values passed failed status))))

(let ((tp 0) (tf 0))
  (dolist (path (programs))
    (let ((name (short-name path)))
      (when (or (null *only*) (member name *only* :test #'string=))
	(multiple-value-bind (passed failed status) (run-one path)
	  (incf tp passed) (incf tf failed)
	  (format t "~&~24A ~5D passed ~5D failed  ~A~%" name passed failed
		  (if (eq status :ok) "" (if (consp status)
					     (subseq (second status) 0 (min 110 (length (second status))))
					     status)))
	  (finish-output)))))
  (format t "~&~%R6RS: ~D tests passed, ~D failed (programs that crash count only what they reported).~%" tp tf))
