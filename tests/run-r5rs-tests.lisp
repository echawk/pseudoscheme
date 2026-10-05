; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER; -*-

;;;; Runs chibi's R5RS test suite (tests/chibi/r5rs-tests.scm) against
;;;; Pseudoscheme and reports the pass count.
;;;;
;;;; Usage:
;;;;   sbcl --script tests/run-r5rs-tests.lisp [--classic | --continuations=full]
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

(defparameter *arguments* (uiop:command-line-arguments))
;; R5RS is on psyntax ((pseudoscheme r5rs) at a top level of its own),
;; as the API's R5RS: functions are; --classic runs the suite on the
;; translator's own classic front end instead (what the translator is
;; bootstrapped with).
(defparameter *full* (member "--continuations=full" *arguments* :test #'string=))
(defparameter *psyntax* (not (member "--classic" *arguments* :test #'string=)))

(when *psyntax*
  (load-system :pseudoscheme/r7rs)
  (ps:disable-float-traps)
  (uiop:symbol-call "PSEUDOSCHEME-R7RS" "BOOT")
  (when *full* (setf (symbol-value (find-symbol "*FULL-CONTINUATIONS*" "PSEUDOSCHEME-PSYNTAX")) t)))

(defun evaluate (form)
  (if *psyntax*
      (uiop:symbol-call "PSEUDOSCHEME-R7RS" "EVAL-AT-R5RS-REPL" form)
      (ps:scheme-eval form ps:scheme-user-environment)))

(defun global-value (name)
  (if *psyntax*
      (evaluate (ps:intern-scheme-symbol name))
      (symbol-value (intern (string-upcase name) "SCHEME"))))

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
	  (evaluate form)
	(error (e)
	  (incf *harness-failures*)
	  (format t "~&[HARNESS ERROR] ~A on form:~%  ~S~%" e form))))))

(let ((passed (global-value "*tests-passed*"))
      (run (global-value "*tests-run*")))
  (format t "~&~%R5RS: ~A of ~A tests passed (~A harness-level errors~:[~; -- stopped early on a reader limitation~]).~%"
	  passed run *harness-failures* *reader-stopped-early*)
  (uiop:quit (if (and (= passed run) (zerop *harness-failures*)
		      (not *reader-stopped-early*))
		 0 1)))
