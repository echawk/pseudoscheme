;;; (srfi private srfi-211-transformers): explicit-renaming, implicitly
;;; renaming, Lisp and syntactic-closure transformers on psyntax's
;;; syntax-case, for SRFI 211's sublibraries (and SRFI 147's
;;; er-macro-transformer).  Written for Pseudoscheme, after SRFI 211's
;;; operational semantics:
;;;
;;;   - The input form is fully unwrapped: pairs, vectors and constants,
;;;     with identifiers (syntax objects) at the leaves.
;;;   - Injecting a symbol is datum->syntax with the keyword of the
;;;     macro use, which came from the input and so carries its context.
;;;   - Renaming a symbol is datum->syntax with the identifier that
;;;     names the transformer facility where the transformer was made
;;;     (er-macro-transformer and friends are macros for this reason;
;;;     used as a variable, each is a procedure).  That identifier is not
;;;     part of the macro use, so psyntax marks the renamed identifiers
;;;     as introduced by the macro: they are hygienic.
;;;   - compare is free-identifier=? of the injections (er) or the
;;;     renamings (ir).
;;;
;;; Syntactic closures (SRFI 211 gives them no semantics) follow MIT
;;; Scheme's: a syntactic environment is an identifier's context;
;;; make-syntactic-closure closes the symbols of a form in it, except the
;;; free names; identifiers already in a form (from the input) keep their
;;; own context.
(define-library (srfi private srfi-211-transformers)
  (export er-macro-transformer ir-macro-transformer lisp-transformer
          sc-macro-transformer rsc-macro-transformer
          make-syntactic-closure close-syntax capture-syntactic-environment
          sc-identifier=? make-synthetic-identifier
          preidentifier? presyntax->datum preidentifier->symbol
          unwrap-syntax unwrap-presyntax identifier->symbol
          generate-identifier construct-identifier
          unwrap-all close-presyntax make-er-transformer)
  (import (scheme base)
          (rnrs syntax-case))
  (begin
    ;; Presyntax

    (define (preidentifier? obj)
      (or (symbol? obj) (identifier? obj)))

    (define (identifier->symbol id) (syntax->datum id))

    (define (preidentifier->symbol id)
      (if (symbol? id) id (syntax->datum id)))

    (define (presyntax->datum obj)
      (cond ((symbol? obj) obj)
            ((pair? obj) (cons (presyntax->datum (car obj))
                               (presyntax->datum (cdr obj))))
            ((vector? obj) (vector-map presyntax->datum obj))
            (else (syntax->datum obj))))

    ;; One level: a wrapped pair or vector becomes a pair or vector of
    ;; syntax objects; identifiers and unwrapped objects are unchanged.
    (define (unwrap-syntax obj)
      (cond ((identifier? obj) obj)
            ((pair? obj) obj)
            ((vector? obj) obj)
            ((symbol? obj) obj)
            (else
             (syntax-case obj ()
               ((a . b) (cons #'a #'b))
               (#(a ...) (list->vector #'(a ...)))
               (_ (syntax->datum obj))))))

    (define (unwrap-presyntax obj) (unwrap-syntax obj))

    ;; Recursively, preserving identifiers.
    (define (unwrap-all obj)
      (cond ((identifier? obj) obj)
            ((symbol? obj) obj)
            ((pair? obj) (cons (unwrap-all (car obj)) (unwrap-all (cdr obj))))
            ((vector? obj) (vector-map unwrap-all obj))
            (else
             (syntax-case obj ()
               ((a . b) (cons (unwrap-all #'a) (unwrap-all #'b)))
               (#(a ...) (vector-map unwrap-all (list->vector #'(a ...))))
               (_ (syntax->datum obj))))))

    (define (generate-identifier . symbol)
      (car (generate-temporaries (if (pair? symbol) symbol '(g)))))

    (define (construct-identifier id symbol) (datum->syntax id symbol))

    ;; Replace the symbols of a presyntax object by identifiers in the
    ;; context of the identifier CTX (except those in FREE), keeping its
    ;; shared structure; captured environments are resolved in CTX.
    (define (close-presyntax ctx obj free)
      (let ((seen '()))                 ; ((pair-or-vector . copy) ...)
        (let walk ((obj obj))
          (cond ((symbol? obj)
                 (if (memq obj free) obj (datum->syntax ctx obj)))
                ((capture? obj) (walk ((capture-proc obj) (make-env ctx))))
                ((syntactic-closure? obj) (closure-form obj))
                ((pair? obj)
                 (cond ((assq obj seen) => cdr)
                       (else
                        (let ((copy (cons #f #f)))
                          (set! seen (cons (cons obj copy) seen))
                          (set-car! copy (walk (car obj)))
                          (set-cdr! copy (walk (cdr obj)))
                          copy))))
                ((vector? obj)
                 (cond ((assq obj seen) => cdr)
                       (else
                        (let ((copy (make-vector (vector-length obj))))
                          (set! seen (cons (cons obj copy) seen))
                          (do ((i 0 (+ i 1))) ((= i (vector-length obj)) copy)
                            (vector-set! copy i (walk (vector-ref obj i))))))))
                (else obj)))))

    (define (keyword-of form)
      (syntax-case form ()
        ((k . _) (identifier? #'k) #'k)
        (k (identifier? #'k) #'k)
        (_ (syntax-violation #f "bad macro use" form))))

    ;; compare: free-identifier=? of the closed preidentifiers.  Given
    ;; anything else (an error by SRFI 211; legacy code does it), it
    ;; compares the data with eqv?, as Chibi's compare does.
    (define (make-compare close)
      (lambda (a b)
        (let ((a (unwrap-all a)) (b (unwrap-all b)))
          (if (and (preidentifier? a) (preidentifier? b))
              (free-identifier=? (close a) (close b))
              (eqv? (presyntax->datum a) (presyntax->datum b))))))

    ;; Explicit renaming

    (define (make-er-transformer def proc)
      (lambda (stx)
        (let* ((use (keyword-of stx))
               (rename (lambda (x) (close-presyntax def x '())))
               (inject (lambda (x) (close-presyntax use x '())))
               (compare (make-compare inject)))
          (inject (proc (unwrap-all stx) rename compare)))))

    ;; Implicit renaming

    (define (make-ir-transformer def proc)
      (lambda (stx)
        (let* ((use (keyword-of stx))
               (rename (lambda (x) (close-presyntax def x '())))
               (inject (lambda (x) (close-presyntax use x '())))
               (compare (make-compare rename)))
          (rename (proc (unwrap-all stx) inject compare)))))

    ;; Lisp transformers: the datum in, the injected datum out

    (define (lisp-transformer proc)
      (lambda (stx)
        (datum->syntax (keyword-of stx) (proc (syntax->datum stx)))))

    ;; Syntactic closures

    (define-record-type syntactic-environment
      (make-env id)
      env?
      (id env-id))

    (define-record-type syntactic-closure
      (make-closure form)
      syntactic-closure?
      (form closure-form))

    (define-record-type captured-environment
      (make-capture proc)
      capture?
      (proc capture-proc))

    (define (make-syntactic-closure env free-names form)
      (make-closure (close-presyntax (env-id env) form free-names)))

    (define (close-syntax form env) (make-syntactic-closure env '() form))

    (define (capture-syntactic-environment proc) (make-capture proc))

    (define (closed-identifier env id)
      (if (symbol? id) (datum->syntax (env-id env) id) id))

    (define (sc-identifier=? env1 id1 env2 id2)
      (free-identifier=? (closed-identifier env1 id1) (closed-identifier env2 id2)))

    (define (make-synthetic-identifier id)
      (generate-identifier (preidentifier->symbol id)))

    (define (make-sc-transformer def proc)
      (lambda (stx)
        (close-presyntax def (proc (unwrap-all stx) (make-env (keyword-of stx))) '())))

    (define (make-rsc-transformer def proc)
      (lambda (stx)
        (let ((use (keyword-of stx)))
          (close-presyntax use (proc (unwrap-all stx) (make-env def)) '()))))

    ;; The transformer makers, as macros that capture where they're used

    (define-syntax define-transformer-maker
      (syntax-rules ()
        ((_ name maker)
         (define-syntax name
           (lambda (x)
             (syntax-case x ()
               ((k proc) #'(maker (syntax k) proc))
               (k (identifier? #'k)
                  #'(lambda (proc) (maker (syntax k) proc)))))))))

    (define-transformer-maker er-macro-transformer make-er-transformer)
    (define-transformer-maker ir-macro-transformer make-ir-transformer)
    (define-transformer-maker sc-macro-transformer make-sc-transformer)
    (define-transformer-maker rsc-macro-transformer make-rsc-transformer)))
