;;;; What the Lisp programs in tests/programs/ load first: Quicklisp,
;;;; Pseudoscheme, tests/programs/lib/ on the Scheme library path, and a
;;;; few checks that report the way SRFI 64 does in the Scheme programs
;;;; (tests/run-program-tests.sh reads the tally).

(require :asdf)

(let ((setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (if (probe-file setup)
      (load setup)
      (error "These programs use Common Lisp libraries from Quicklisp, ~
              and there is no ~A" setup)))

(defpackage "PROGRAM-TESTS"
  (:use "COMMON-LISP")
  (:export "*ROOT*" "QUICKLOAD" "TEST" "TEST-EQUAL" "TEST-ASSERT" "TEST-ERROR" "FINISH"))

(in-package "PROGRAM-TESTS")

(defvar *root*
  (truename (merge-pathnames "../../" (make-pathname :name nil :type nil
						     :defaults (or *load-truename* *load-pathname*)))))
(pushnew *root* asdf:*central-registry* :test #'equal)

(defun quickload (&rest systems)
  (let ((*standard-output* (make-broadcast-stream))
	(*error-output* (make-broadcast-stream)))
    (handler-bind ((warning #'muffle-warning))
      (uiop:symbol-call "QL" "QUICKLOAD" systems :silent t))))

(quickload :r7rs)
(ps:disable-float-traps)
(r7rs:add-library-directory (merge-pathnames "tests/programs/lib/" *root*))

(defvar *passes* 0)
(defvar *failures* 0)

(defun report (name ok expected actual)
  (cond (ok (incf *passes*) (format t "[PASS] ~A~%" name))
	(t (incf *failures*)
	   (format t "[FAIL] ~A~%  expected: ~S~%  got:      ~S~%" name expected actual)))
  (finish-output))

(defun run-check (name test expected thunk)
  (let ((actual (handler-case (funcall thunk)
		  (serious-condition (e)
		    (report name nil expected (list :error (princ-to-string e)))
		    (return-from run-check)))))
    (report name (funcall test expected actual) expected actual)))

(defmacro test-equal (name expected form &key (test '#'equal))
  `(run-check ,name ,test ,expected (lambda () ,form)))

(defmacro test-assert (name form)
  `(run-check ,name (lambda (e a) (declare (ignore e)) a) t (lambda () ,form)))

(defmacro test-error (name form)
  `(run-check ,name (lambda (e a) (declare (ignore e)) (eq a :signalled)) :signalled
	      (lambda () (handler-case (progn ,form :returned) (error () :signalled)))))

(defun finish ()
  (format t "~%Passes:            ~D~%Failures:          ~D~%" *passes* *failures*)
  (finish-output)
  (uiop:quit (if (zerop *failures*) 0 1)))
