;;; -*- Mode: Lisp -*-

;;;; The `pseudoscheme' command-line program.  Build it with
;;;;   make -C contrib/cli            (or: make -C contrib/cli LISP=ccl)
;;;; which runs (asdf:make :pseudoscheme-cli), writing bin/pseudoscheme.new,
;;;; and renames that bin/pseudoscheme (so a program running the old one
;;;; never finds it missing or half written).

(defsystem :pseudoscheme-cli
  :version "3.0"
  :description "A standalone Scheme (R7RS, R6RS, R5RS) on Pseudoscheme"
  :depends-on (:pseudoscheme/api)
  :components ((:file "cli"))
  :build-operation "program-op"
  :build-pathname "../../bin/pseudoscheme.new"
  :entry-point "pseudoscheme-cli:main")
