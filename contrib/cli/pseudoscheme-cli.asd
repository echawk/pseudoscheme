;;; -*- Mode: Lisp -*-

;;;; The `pseudoscheme' command-line program.  Build it with
;;;;   make -C contrib/cli            (or: make -C contrib/cli LISP=ccl)
;;;; which runs (asdf:make :pseudoscheme-cli) and leaves bin/pseudoscheme
;;;; at the top of the repository.

(defsystem :pseudoscheme-cli
  :description "A standalone Scheme (R7RS, R6RS, R5RS) on Pseudoscheme"
  :depends-on (:pseudoscheme/api)
  :components ((:file "cli"))
  :build-operation "program-op"
  :build-pathname "../../bin/pseudoscheme"
  :entry-point "pseudoscheme-cli:main")
