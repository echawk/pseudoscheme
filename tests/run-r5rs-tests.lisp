; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER; -*-

;;;; Runs chibi's R5RS test suite (tests/chibi/r5rs-tests.scm) against
;;;; Pseudoscheme and reports the pass count.
;;;;
;;;; Usage:
;;;;   sbcl --script tests/run-r5rs-tests.lisp
;;;; or, from an already-running image with :pseudoscheme loaded:
;;;;   (load "tests/run-r5rs-tests.lisp")

(require :asdf)

;; The dependencies (float-features, cl-unicode, ...) come from
;; Quicklisp when it's installed; QUICKLOAD fetches any that are missing.
(let ((setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file setup) (load setup)))

(defun load-system (system)
  (if (find-package "QL")
      (uiop:symbol-call "QL" "QUICKLOAD" system :silent t)
      (asdf:load-system system)))

(let* ((here (make-pathname :name nil :type nil
			    :defaults (or *load-truename* *load-pathname*)))
       (root (merge-pathnames (make-pathname :directory '(:relative :up))
			       here)))
  (pushnew (truename root) asdf:*central-registry* :test #'equal))

(load-system :pseudoscheme/r5rs)

;;; Loading :pseudoscheme/reader (a dependency of :pseudoscheme/r5rs)
;;; already switched ps:*scheme-read* to the dedicated Scheme48-derived
;;; reader (read.scm's SCHEME-READ) -- the CL-reader bridge this used
;;; to rely on can't parse the `...' ellipsis identifier syntax-rules
;;; uses throughout, or Scheme string escapes like \n, both of which
;;; this test file needs.

(defparameter *r5rs-tests-file*
  (merge-pathnames "chibi/r5rs-tests.scm"
		    (or *load-truename* *load-pathname*)))

;;; Load and evaluate the test file one top-level form at a time, so a
;;; single erroring test (an unimplemented corner case, say) doesn't
;;; abort the whole suite -- it's just counted as a failure and we
;;; move on, like any other test runner would. A READ error is treated
;;; the same way, but stops the run (its stream position can't be
;;; trusted enough to resynchronize and keep going).

(defvar *harness-failures* 0)
(defvar *reader-stopped-early* nil)

(with-open-file (in *r5rs-tests-file*)
  (loop
    (let ((form (handler-case (funcall ps:*scheme-read* in)
		  (error (e)
		    (format t "~&[READ ERROR] ~A~%" e)
		    (setq *reader-stopped-early* t)
		    ps:eof-object))))
      (when (eq form ps:eof-object)
	(return))
      (handler-case
	  (ps:scheme-eval form ps:scheme-user-environment)
	(error (e)
	  (incf *harness-failures*)
	  (format t "~&[HARNESS ERROR] ~A on form:~%  ~S~%" e form))))))

(let ((passed (symbol-value (intern "*TESTS-PASSED*" "SCHEME")))
      (run (symbol-value (intern "*TESTS-RUN*" "SCHEME"))))
  (format t "~&~%R5RS: ~A of ~A tests passed (~A harness-level errors~:[~; -- stopped early on a reader limitation~]).~%"
	  passed run *harness-failures* *reader-stopped-early*)
  (uiop:quit (if (and (= passed run) (zerop *harness-failures*)
		      (not *reader-stopped-early*))
		 0 1)))
