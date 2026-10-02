;;; hooks-pseudoscheme.ss
;;; Pseudoscheme port of hooks.ss (see ../ReadMe: "The file hooks.ss may
;;; be the only file you have to change in order to successfully run
;;; the macro system in your Scheme implementation").
;;;
;;; Load this instead of hooks.ss when loading syntax-case into
;;; Pseudoscheme: (load "compat") (load "hooks-pseudoscheme")
;;; (load "output") (load "init") (load "expand.pp") ... as loadpp.ss
;;; does for Chez.
;;;
;;; NB: like Pseudoscheme's own translator sources (builtin.scm,
;;; p-utils.scm, ...), this file uses bare PS-LISP:FOO package-qualified
;;; symbols, so it must be loaded with the CL-reader bridge active
;;; (ps:*scheme-read* = #'ps:scheme-read-using-commonlisp-reader, the
;;; default -- see README.md's "Which reader to use"), not
;;; pseudoscheme/reader's dedicated Scheme reader, which deliberately
;;; doesn't give ":" any special meaning (see read.scm). The rest of
;;; syntax-case (compat.ss, output.ss, init.ss, expand.pp, macro-defs.ss)
;;; has no such requirement and loads fine under either reader.

(define eval-hook
  (lambda (x)
    (eval x (interaction-environment))))

(define expand-install-hook
  (lambda (expand)
    expand  ;; Not hooked into Pseudoscheme's own LOAD/EVAL here --
    ;; that would mean teaching PS:SCHEME-EVAL about a pluggable
    ;; front-end expander, a bigger integration step. For now the
    ;; expander is just available to call directly, e.g.
    ;; (eval (expand-syntax '(...)) (interaction-environment)).
    (values)))

(define error-hook
  (lambda (who why what)
    (error "~a: ~a ~s" who why what)))

;; CL's MAKE-SYMBOL creates a genuinely uninterned symbol -- exactly
;; what's needed here for non-capturing hygienic renaming. (Not
;; PS-LISP:MAKE-SYMBOL: MAKE-SYMBOL isn't in PS's curated re-export of
;; CL -- see pack.lisp -- so it has to be named via the COMMON-LISP
;; package directly.)
(define new-symbol-hook
  (lambda (string)
    (common-lisp:make-symbol string)))

;; Scheme has no standard symbol-plist, but Pseudoscheme is hosted on
;; one: CL's GET/SETF GET, used elsewhere in this codebase the same way
;; (e.g. read.scm's RECORD-ORIGINAL-SPELLING!).
(define put-global-definition-hook
  (lambda (symbol binding)
    (ps-lisp:setf (ps-lisp:get symbol 'scheme::%macro-transformer) binding)))

;; CL's GET returns NIL, not Scheme #f, when the property is absent --
;; and NIL is a legitimate Scheme value here (the empty list), not the
;; same thing as #f (see core.lisp). expand.ss's GLOBAL-LOOKUP does
;; (or (get-global-definition-hook sym) '(global-unbound)), relying on
;; an #f result the way Chez's own GETPROP gives it; TRUE? maps CL's
;; "absent" NIL to real Scheme FALSE, which this hook must return
;; instead of bare NIL for that OR to work.
(define get-global-definition-hook
  (lambda (symbol)
    (ps-lisp:true? (ps-lisp:get symbol 'scheme::%macro-transformer))))
