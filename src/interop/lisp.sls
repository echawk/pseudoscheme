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
;;;;   (lisp form ...)                  Lisp code, with Scheme's variables
;;;;                                    and procedures visible (a PROGN;
;;;;                                    see lisp-macro-transformer)
;;;;   (lisp-macro-transformer symbol)  the transformer the (cl ...)
;;;;                                    libraries give Lisp macros

;;; The transformers of the (cl ...) libraries' macros, variables and
;;; other symbols (src/interop.lisp), in a library of their own so that
;;; (pseudoscheme lisp) can use them in its own macros.
(library (pseudoscheme lisp transformers)
  (export lisp-macro-transformer lisp-variable-transformer
          lisp-symbol-transformer %%lisp-symbol)
  (import (rnrs) (pseudoscheme lisp primitives))

  ;; A keyword the (cl ...) libraries' variable macros recognize:
  ;; (*print-base* %%lisp-symbol) is the symbol *PRINT-BASE*.
  (define-syntax %%lisp-symbol
    (lambda (x) (syntax-violation #f "misplaced %%lisp-symbol" x)))

  ;; The transformer of a Lisp macro (or special operator) imported from
  ;; a (cl ...) library; see "Lisp macros used from Scheme" in
  ;; src/interop.lisp.  The form is walked: Scheme variables become
  ;; placeholders, other identifiers Lisp symbols; quoted data stays
  ;; Scheme data.  The result is compiled once, as a Lisp function of the
  ;; placeholders, and the expansion calls it with the variables' values.
  (define (lisp-macro-transformer macro)
    (%register-lisp-macro!
     (lambda (x)
       (syntax-case x ()
         ((k . args)
          (let ((holes '()))            ; ((identifier . placeholder) ...)
            (define (hole id)
              (let loop ((h holes))
                (cond ((null? h)
                       (let ((p (%lisp-placeholder (syntax->datum id))))
                         (set! holes (cons (cons id p) holes))
                         p))
                      ((bound-identifier=? (caar h) id) (cdar h))
                      (else (loop (cdr h))))))
            ;; Quoted data stays Scheme data, except that a (cl ...)
            ;; import is its Lisp symbol: '(cl:integer) is (INTEGER).
            (define (walk-quoted s)
              (syntax-case s ()
                (id (identifier? #'id)
                 (or (%lisp-import-symbol #'id) (syntax->datum #'id)))
                ((a . d) (cons (walk-quoted #'a) (walk-quoted #'d)))
                (#(e ...) (list->vector (map walk-quoted #'(e ...))))
                (other (syntax->datum #'other))))
            (define (walk s)
              (syntax-case s ()
                (id (identifier? #'id)
                 (let ((r (%lisp-identifier #'id macro)))
                   (if (%lisp-variable-marker? r) (hole #'id) r)))
                ((q datum) (and (identifier? #'q) (free-identifier=? #'q #'quote))
                 (list (lisp-symbol "quote" "cl") (walk-quoted #'datum)))
                ((a . d) (cons (walk #'a) (walk #'d)))
                (#(e ...) (list->vector (map walk #'(e ...))))
                (other (%lisp-literal (syntax->datum #'other)))))
            (let* ((form (cons macro (walk #'args)))
                   (holes (reverse holes))
                   (f (%compile-lisp-form form (map cdr holes))))
              (with-syntax ((f (datum->syntax #'k f))
                            ((h ...) (map car holes)))
                #'((quote f) h ...)))))))
     macro))

  ;; The transformer of a Lisp special variable (or constant) imported
  ;; from a (cl ...) library: reading it reads the symbol's value, set!
  ;; sets it, and (id %%lisp-symbol) is the symbol itself (for lisp-let).
  ;; Registered, so that inside a Lisp macro call it is the Lisp symbol.
  (define (lisp-variable-transformer symbol)
    (%register-lisp-macro!
     (make-variable-transformer
      (lambda (x)
        (syntax-case x (set! %%lisp-symbol)
          ((set! _ e) (with-syntax ((s (datum->syntax #'_ symbol)))
                        #'(set-lisp-value! 's e)))
          ((_ %%lisp-symbol) (with-syntax ((s (datum->syntax #'_ symbol))) #''s))
          ((_ arg ...) (with-syntax ((s (datum->syntax #'_ symbol)))
                         #'((lisp-value 's) arg ...)))
          (_ (identifier? x) (with-syntax ((s (datum->syntax x symbol)))
                               #'(lisp-value 's))))))
     symbol))

  ;; The transformer of any other external symbol of a (cl ...)
  ;; library's package (a type, a class, a lambda-list keyword): the
  ;; symbol itself, so (cl:typep x cl:integer) needs no quote.
  (define (lisp-symbol-transformer symbol)
    (%register-lisp-macro!
     (lambda (x)
       (syntax-case x ()
         (id (identifier? #'id) (with-syntax ((s (datum->syntax #'id symbol))) #''s))))
     symbol)))

(library (pseudoscheme lisp)
  (export lisp-true? lisp-false? lisp-symbol lisp-keyword lisp-function
          lisp-funcall lisp-apply lisp-value set-lisp-value!
          call-with-lisp-bindings lisp-require lisp-eval-string verbatim
          lisp-let lisp-symbol-of lisp-set! %%lisp-symbol
          lisp-macro-transformer lisp-variable-transformer
          lisp-symbol-transformer lisp)
  (import (rnrs) (pseudoscheme lisp primitives) (pseudoscheme lisp transformers))


  (define-syntax lisp-symbol-of
    (syntax-rules ()
      ((_ id) (id %%lisp-symbol))))

  ;; (lisp form ...): Lisp code in Scheme -- a PROGN, compiled as Lisp.
  (define-syntax lisp (lisp-macro-transformer (lisp-symbol "progn" "cl")))

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
