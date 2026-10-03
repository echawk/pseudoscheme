;;; -*- Mode: Scheme -*-
;;;; (pseudoscheme lisp): Scheme's side of the Lisp bridge
;;;;
;;;; Lisp packages themselves are imported as (cl <package>) libraries
;;;; (src/interop.lisp); this library has what's left over: Lisp truth,
;;;; Lisp symbols and functions by name, dynamic binding of Lisp special
;;;; variables, loading Lisp systems, and evaluating Lisp source.
;;;;
;;;;   (lisp-true? x)          is X true in Lisp (neither NIL nor #f)?
;;;;   (lisp-false? x)
;;;;   (lisp-symbol name [package])     the symbol, by Scheme spelling:
;;;;                                    (lisp-symbol "equal" "cl") => EQUAL
;;;;   (lisp-keyword name)              what #:name reads as
;;;;   (lisp-function name [package])   the raw function (no conversion)
;;;;   (lisp-funcall f arg ...)         call F with #f passed as NIL,
;;;;   (lisp-apply f arg ... list)        results unconverted
;;;;   (lisp-value symbol)              SYMBOL-VALUE, and set-lisp-value!
;;;;   (lisp-let ((*print-base* 16)) body ...)
;;;;                                    bind Lisp specials (identifiers
;;;;                                    imported from a (cl ...) library)
;;;;   (lisp-symbol-of id)              the Lisp symbol behind such an id
;;;;   (lisp-set! (accessor arg ...) value)
;;;;                                    Lisp's SETF: (lisp-set! (cl:gethash
;;;;                                    k table) v), (lisp-set! (cl:aref a 0) 1)
;;;;   (call-with-lisp-bindings symbols values thunk)
;;;;   (lisp-require system)            load a Lisp system now
;;;;   (lisp-eval-string string)        read and evaluate Lisp source
;;;;   (verbatim proc)                  pass PROC to Lisp with no
;;;;                                    boolean conversion of its results

(library (pseudoscheme lisp)
  (export lisp-true? lisp-false? lisp-symbol lisp-keyword lisp-function
          lisp-funcall lisp-apply lisp-value set-lisp-value!
          call-with-lisp-bindings lisp-require lisp-eval-string verbatim
          lisp-let lisp-symbol-of lisp-set! %%lisp-symbol)
  (import (rnrs) (pseudoscheme lisp primitives))

  ;; A keyword the (cl ...) libraries' variable macros recognize:
  ;; (*print-base* %%lisp-symbol) is the symbol *PRINT-BASE*.
  (define-syntax %%lisp-symbol
    (lambda (x) (syntax-violation #f "misplaced %%lisp-symbol" x)))

  (define-syntax lisp-symbol-of
    (syntax-rules ()
      ((_ id) (id %%lisp-symbol))))

  (define-syntax lisp-set!
    (syntax-rules ()
      ((_ (accessor arg ...) value) (%lisp-setf! accessor value arg ...))))

  (define-syntax lisp-let
    (syntax-rules ()
      ((_ () body1 body2 ...) (let () body1 body2 ...))
      ((_ ((var value) ...) body1 body2 ...)
       (call-with-lisp-bindings (list (lisp-symbol-of var) ...)
                                (list value ...)
                                (lambda () body1 body2 ...))))))
