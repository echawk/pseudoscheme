; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER -*-

;;;; Runs the conformance tests of the older Revised Reports
;;;; (tests/reports/<report>.scm, docs/reports.md), each at its report's
;;;; top level, and reports the pass count.
;;;;
;;;; A test file is Scheme forms, read case-folded and evaluated one at a
;;;; time at the report's top level, except these two, which the runner
;;;; takes:
;;;;   (test EXPECTED EXPRESSION)   EXPRESSION's value must write as
;;;;                                EXPECTED (unevaluated) does
;;;;   (test-error EXPRESSION)      evaluating EXPRESSION must signal an error
;;;; (so an unbound name, for what a report doesn't have, is a test-error).
;;;;
;;;; Usage: sbcl --script tests/run-reports-tests.lisp [report ...]
;;;; (report: rrs r2rs r3rs ieee r4rs; default all)

(require :asdf)

(let ((setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file setup) (load setup)))

(defun load-system (system)
  (if (find-package "QL")
      (uiop:symbol-call "QL" "QUICKLOAD" system :silent t)
      (asdf:load-system system)))

(let* ((here (make-pathname :name nil :type nil
			    :defaults (or *load-truename* *load-pathname*)))
       (root (merge-pathnames (make-pathname :directory '(:relative :up)) here)))
  (pushnew (truename root) asdf:*central-registry* :test #'equal))

(load-system :pseudoscheme/api)
(ps:disable-float-traps)

(defparameter *tests-directory*
  (merge-pathnames "reports/" (make-pathname :name nil :type nil
					     :defaults (or *load-truename* *load-pathname*))))

(defun scheme-string (x)
  (with-output-to-string (s) (funcall ps:*scheme-write* x s)))

(defun head-p (form name)
  (and (consp form) (symbolp (car form)) (string= (ps:scheme-symbol-name (car form)) name)))

(defvar *passed* 0)
(defvar *run* 0)

(defun run-report (report)
  (let* ((file (merge-pathnames (format nil "~(~A~).scm" report) *tests-directory*))
	 (passed 0) (run 0))
    (flet ((evaluate (form) (ps-r7rs::eval-at-report-top-level report form)))
      (let ((ps:*fold-case* t))
	(with-open-file (in file)
	  (loop for form = (funcall ps:*scheme-read* in)
		until (eq form ps:eof-object)
		do (cond
		     ((head-p form "test")
		      (incf run)
		      (let ((expected (scheme-string (ps-r7rs::replace-null-markers (second form) t)))
			    (got (handler-case (scheme-string (evaluate (third form)))
				   (error (e) (format nil "ERROR ~A" e)))))
			(if (string= expected got)
			    (incf passed)
			    (format t "~&FAIL ~(~A~): ~A~%  expected ~A~%  got      ~A~%"
				    report (scheme-string (third form)) expected got))))
		     ((head-p form "test-error")
		      (incf run)
		      (if (handler-case (progn (evaluate (second form)) nil)
			    (error () t))
			  (incf passed)
			  (format t "~&FAIL ~(~A~): ~A should be an error~%" report
				  (scheme-string (second form)))))
		     (t (handler-case (evaluate form)
			  (error (e)
			    (format t "~&FAIL ~(~A~): ~A~%  ~A~%" report (scheme-string form) e))))))))
      (format t "~&~A: ~D of ~D tests passed~%" (string-upcase (symbol-name report)) passed run)
      (incf *passed* passed)
      (incf *run* run))))

(let ((reports (or (mapcar (lambda (a) (intern (string-upcase a) :keyword))
			   (uiop:command-line-arguments))
		   '(:r4rs :ieee :r3rs :r2rs :rrs))))
  (uiop:symbol-call "PSEUDOSCHEME-R7RS" "BOOT")
  (dolist (r reports) (run-report r))
  (format t "~&~%Reports: ~D of ~D tests passed.~%" *passed* *run*)
  (uiop:quit (if (= *passed* *run*) 0 1)))
