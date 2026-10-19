;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER -*-

;;;; How much of Racket CS's primitive tables a --racket strategy's
;;;; instances have (src/racket/instances.lisp, COVERAGE-REPORT).
;;;;
;;;; Usage:  sbcl --dynamic-space-size 4GB --script tests/racket/coverage.lisp \
;;;;              [STRATEGY] [--missing] [--expander] [--instantiate] [--fork]
;;;; Needs sh utils/racket/setup.sh first (the tables are in Racket's
;;;; source).  STRATEGY is linklet (the default); cs and hybrid have
;;;; rumble's instances, which need milestone 1 of docs/racket.md.
;;;; --expander: also lower the expander linklet (from BC's startup.inc)
;;;; with strategy B's compiler, and count the primitives it uses that
;;;; the instances lack.  --instantiate: compile it and instantiate it
;;;; (about 30 seconds), or say which top-level form fails.  --fork: milestone 0's inventory for strategies
;;;; A and C, the Chez internals rumble uses that --chez lacks
;;;; (src/racket/cs/fork.lisp).

(require :asdf)

(let ((setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file setup) (load setup)))

(let* ((here (make-pathname :name nil :type nil
			    :defaults (or *load-truename* *load-pathname*)))
       (root (merge-pathnames (make-pathname :directory '(:relative :up :up)) here)))
  (pushnew (truename root) asdf:*central-registry* :test #'equal))

(let ((*standard-output* (make-broadcast-stream))
      (*error-output* (make-broadcast-stream)))
  (handler-bind ((warning #'muffle-warning))
    (if (find-package "QL")
	(uiop:symbol-call "QL" "QUICKLOAD" :quasischeme/racket :silent t)
	(asdf:load-system :quasischeme/racket))))

(let* ((args (uiop:command-line-arguments))
       (strategy (or (find-if-not (lambda (a) (uiop:string-prefix-p "--" a)) args) "linklet")))
  (qsr:select-strategy strategy)
  (handler-bind ((warning #'muffle-warning))
    (uiop:symbol-call "QSR" "BOOT"))
  (uiop:symbol-call "QSR" "COVERAGE-REPORT" :missing (member "--missing" args :test #'string=))
  (when (member "--fork" args :test #'string=)
    (uiop:symbol-call "QSR" "FORK-INVENTORY"))
  (when (member "--instantiate" args :test #'string=)
    (uiop:symbol-call "QSR" "INSTANTIATE-EXPANDER"))
  (when (member "--expander" args :test #'string=)
    (uiop:symbol-call "QSR" "EXPANDER-REPORT" :missing (member "--missing" args :test #'string=))))
