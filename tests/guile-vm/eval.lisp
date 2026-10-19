;;; Evaluate a Scheme file in the Guile mode, from this source tree.
;;; Usage: sbcl --dynamic-space-size 4GB --control-stack-size 500MB \
;;;          --script tests/guile-vm/eval.lisp FILE.scm
(require :asdf)
(let ((setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file setup) (load setup)))
(push (merge-pathnames "../../" (make-pathname :name nil :type nil :defaults *load-truename*))
      asdf:*central-registry*)
(let ((*standard-output* (make-broadcast-stream)) (*error-output* (make-broadcast-stream)))
  (handler-bind ((warning #'muffle-warning)) (asdf:load-system :quasischeme/guile)))
(qsg:boot)
(qsg:eval-string (uiop:read-file-string (first (uiop:command-line-arguments))))
