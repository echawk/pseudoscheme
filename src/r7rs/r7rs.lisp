; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-R7RS -*-

;;;; The R7RS implementation environment
;;;;
;;;; The procedures behind R7RS-small, built in a native Pseudoscheme
;;;; environment that the psyntax host (src/psyntax.lisp) then copies:
;;;;
;;;; 1. Start from a *copy* of the R5RS bindings
;;;;    (REVISED^4-SCHEME-STRUCTURE already carries R5RS's additions).
;;;; 2. Install the Lisp primitives of rts.lisp.
;;;; 3. Evaluate base.scm in it (procedures, and the R7RS layer's own
;;;;    native macros, e.g. for promises).
;;;; 4. Any procedure named in Appendix A still missing is bound to a
;;;;    stub that signals "not implemented", so libraries that export it
;;;;    load and fail only when it's called.
;;;;
;;;; The (scheme ...) libraries themselves are psyntax libraries now; see
;;;; front.lisp.

(in-package "PSEUDOSCHEME-R7RS")

(defvar *implementation-env* nil
  "The environment the R7RS procedures are defined in.")

(defvar *stubs* '()
  "Names of Appendix A procedures bound only to stubs.")

(defun sym (string) (psl:scheme-symbol string))

(defun load-scheme-file-from (path env)
  "Evaluate every form of PATH (relative to src/) in ENV with the
dedicated reader, natively."
  (with-open-file (in (asdf:system-relative-pathname
		       :pseudoscheme (concatenate 'string "src/" path)))
    (loop for form = (funcall ps:*scheme-read* in)
	  until (eq form ps:eof-object)
	  do (ps:scheme-eval form env))))

(defprim "features" ()
  ;; and full-continuations when code is compiled with re-entrant
  ;; continuations (src/continuations.lisp, loaded later)
  (mapcar #'sym (append psl:*scheme-features*
			(let ((full (find-symbol "*FULL-CONTINUATIONS*" "PSEUDOSCHEME-PSYNTAX")))
			  (when (and full (symbol-value full)) '("full-continuations"))))))

(defun stub-function (export)
  (lambda (&rest args)
    (declare (ignore args))
    (ps:scheme-error "~A: not implemented yet" export)))

(defun build-implementation-env ()
  (let ((env (psl:new-library-env "r7rs implementation")))
    (psl:copy-all! env (psl:base-structure))
    (loop for (name . function) in *primitives*
	  do (psl:install-variable! env (sym name) function))
    (load-scheme-file-from "r7rs/base.scm" env)
    (setq *stubs* '())
    (dolist (export (remove-duplicates
		     (loop for (nil s) in *standard-libraries* append (split-names s))
		     :test #'string=))
      (unless (or (syntax-export-p export)
		  (psl:binding-defined-p env (sym export)))
	(psl:install-variable! env (sym export) (stub-function export))
	(push export *stubs*)))
    env))

(setq *implementation-env* (build-implementation-env))
