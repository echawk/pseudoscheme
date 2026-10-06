;;; (srfi private srfi-148-a): part of SRFI 148's sample implementation
;;; (see 148.sld): 148.scm and the sections General, Boolean logic, Folding, unfolding and
;;; mapping, and Vector processing (148.scm and these refer to each other).  It exports everything
;;; it defines, for the other parts and (srfi 148).
(define-library (srfi private srfi-148-a)
  (export :call :prepare ck em-syntax-rules em-syntax-rules-aux1
          em-syntax-rules-aux2 em em-suspend em-resume em-cut em-cute
          em-cut-eval em-cut-aux em-cute-eval em-quasiquote
          em-quasiquote-aux em-constant em-constant-aux em-append em-list
          em-cons em-cons* em-car em-cdr em-apply em-call em-eval em-error
          em-error-aux em-gensym em-generate-temporaries em-quote em-if
          em-not em-or em-and em-null? em-pair? em-list? em-boolean?
          em-vector? em-symbol? em-symbol?-aux em-bound-identifier=?
          em-bound-identifier=?-aux em-free-identifier=?
          em-free-identifier=?-aux em-constant=? em-constant=?-aux em-equal?
          em-fold em-fold-right em-unfold em-unfold-right em-map
          em-append-map em-vector em-list->vector em-vector->list
          em-vector-map em-vector-ref free-identifier=? bound-identifier=?)
  (import (except (scheme base) define-syntax let-syntax letrec-syntax syntax-rules)
          (srfi 147)
          (srfi 26)
          (srfi 147 er-macro-transformer)
          (scheme cxr)
          (prefix (only (rnrs syntax-case) bound-identifier=? identifier? syntax->datum) r6:))
  (begin
    (define-syntax free-identifier=?
      (er-macro-transformer
       (lambda (expr rename compare)
         (if (compare (car (cdr expr))
                      (cadr (cdr expr)))
             (caddr (cdr expr))
             (cadddr (cdr expr))))))

    (define-syntax bound-identifier=?
      (er-macro-transformer
       (lambda (expr rename compare)
         (if (let ((a (car (cdr expr))) (b (cadr (cdr expr))))
               (if (and (r6:identifier? a) (r6:identifier? b))
                   (r6:bound-identifier=? a b)
                   (eqv? (r6:syntax->datum a) (r6:syntax->datum b))))
             (caddr (cdr expr))
             (cadddr (cdr expr)))))))
  (include "../reference/srfi-148/148.scm"
           "../reference/srfi-148/148.macros.1.scm"
           "../reference/srfi-148/148.macros.3.scm"
           "../reference/srfi-148/148.macros.5.scm"))
