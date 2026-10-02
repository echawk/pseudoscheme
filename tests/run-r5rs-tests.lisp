; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER; -*-

;;;; Runs chibi's R5RS test suite (tests/chibi/r5rs-tests.scm) against
;;;; Pseudoscheme and reports the pass count.
;;;;
;;;; Usage:
;;;;   sbcl --script tests/run-r5rs-tests.lisp
;;;; or, from an already-running image with :pseudoscheme loaded:
;;;;   (load "tests/run-r5rs-tests.lisp")

(require :asdf)

(let* ((here (make-pathname :name nil :type nil
			    :defaults (or *load-truename* *load-pathname*)))
       (src (merge-pathnames (make-pathname :directory '(:relative :up "src"))
			      here)))
  (pushnew (truename src) asdf:*central-registry* :test #'equal))

(asdf:load-system :pseudoscheme/r5rs)

(setq ps:*scheme-read* #'ps:scheme-read-using-commonlisp-reader)

(defparameter *r5rs-tests-file*
  (merge-pathnames "chibi/r5rs-tests.scm"
		    (or *load-truename* *load-pathname*)))

;;; Load and evaluate the test file one top-level form at a time, so a
;;; single erroring test (an unimplemented corner case, say) doesn't
;;; abort the whole suite -- it's just counted as a failure and we
;;; move on, like any other test runner would.
;;;
;;; A READ error is different: it means the CL reader (used here via
;;; PS:SCHEME-READ-USING-COMMONLISP-READER -- see readwrite.lisp) hit
;;; Scheme syntax it fundamentally can't parse, e.g. the `...' ellipsis
;;; identifier used throughout syntax-rules (an unbroken run of dots
;;; isn't a valid CL token) or a `\n'/`\t' string escape (CL's reader
;;; treats a backslash as "read the next character literally", not as
;;; introducing a control character the way Scheme's reader does). That
;;; corrupts the stream position for resuming, so we stop the run
;;; there rather than guess at resynchronizing -- see the project's R5RS
;;; follow-up notes about enabling the dedicated Scheme reader
;;; (read.scm/write.scm, currently excluded from pseudoscheme.asd) for
;;; full conformance instead of borrowing the CL reader.

(defvar *harness-failures* 0)
(defvar *reader-stopped-early* nil)

(with-open-file (in *r5rs-tests-file*)
  (loop
    (let ((form (handler-case (funcall ps:*scheme-read* in)
		  (reader-error (e)
		    (format t "~&[READER LIMITATION] ~A~%" e)
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
