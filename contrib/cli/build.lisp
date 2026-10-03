;;;; Build bin/pseudoscheme: (load "build.lisp") from any Common Lisp
;;;; with ASDF.  contrib/cli/Makefile does this.

(require :asdf)

(let* ((here (make-pathname :name nil :type nil
			    :defaults (or *load-truename* *load-pathname*)))
       (root (merge-pathnames (make-pathname :directory '(:relative :up :up)) here)))
  (pushnew (truename root) asdf:*central-registry* :test #'equal)
  (pushnew (truename here) asdf:*central-registry* :test #'equal))

(handler-bind ((warning #'muffle-warning))
  (asdf:make :pseudoscheme-cli))
