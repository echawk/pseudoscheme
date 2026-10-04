;;;; Build bin/pseudoscheme: (load "build.lisp") from any Common Lisp
;;;; with ASDF.  contrib/cli/Makefile does this.

(require :asdf)

(let* ((here (make-pathname :name nil :type nil
			    :defaults (or *load-truename* *load-pathname*)))
       (root (merge-pathnames (make-pathname :directory '(:relative :up :up)) here)))
  (pushnew (truename root) asdf:*central-registry* :test #'equal)
  (pushnew (truename here) asdf:*central-registry* :test #'equal))

;; The dependencies (float-features, cl-unicode, ...) come from
;; Quicklisp when it's installed; QUICKLOAD fetches any that are missing.
(let ((setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file setup)
    (load setup)
    (uiop:symbol-call "QL" "QUICKLOAD" :pseudoscheme-cli :silent t)))

(handler-bind ((warning #'muffle-warning))
  (asdf:make :pseudoscheme-cli))
