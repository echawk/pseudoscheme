;;; -*- Mode: Lisp -*-

;;;; The `quasischeme' command-line program.  Build it with
;;;;   make -C contrib/cli            (or: make -C contrib/cli LISP=ccl)
;;;; which runs (asdf:make :quasischeme-cli), writing bin/quasischeme.new,
;;;; and renames that bin/quasischeme (so a program running the old one
;;;; never finds it missing or half written).

(defsystem :quasischeme-cli
  :maintainer "Ethan Hawk <ethhawk@iu.edu>"
  :version "1.0"
  :description "A standalone Scheme (R7RS, R6RS, R5RS) on Quasischeme"
  :depends-on (:quasischeme/api :quasischeme/guile :quasischeme/libguile :quasischeme/racket :quasischeme/repl
	       :quasischeme/fast)		; last: only code compiled at run time
  :components ((:file "cli"))
  :build-operation "program-op"
  :build-pathname "../../bin/quasischeme.new"
  :entry-point "quasischeme-cli:main")
