;;; SRFI 72: Hygienic macros.  Written for Pseudoscheme, approximately,
;;; on psyntax's syntax-case (the SRFI's reference implementation is an
;;; expander of its own).  What differs:
;;;
;;; - There is one environment for all phases (psyntax's, as R6RS
;;;   systems with implicit phasing have): begin-for-syntax is begin, so
;;;   its definitions are seen by transformers only where definitions
;;;   take effect before later forms are expanded (the REPL), and the
;;;   SRFI's examples of separate bindings per phase don't hold: in a
;;;   transformer, (let ((x 2)) (syntax x)) refers to that x, not to an x
;;;   where the output is used.
;;; - A transformer gets its form as lists and vectors of identifiers and
;;;   constants, as the SRFI says, so car and cdr work on it; syntax-case
;;;   pattern variables are psyntax's, used in syntax templates.
;;; - quasisyntax is the SRFI's, unquote (,) and unquote-splicing (,@)
;;;   marking what is evaluated; identifiers it introduces are fresh per
;;;   expansion, as psyntax marks them, not per evaluation.
;;; - make-capturing-identifier is datum->syntax: what it captures is
;;;   decided by bound-identifier=?, not free-identifier=?.
;;; - around-syntax evaluates its expressions at expansion time, before
;;;   and after the form is expanded, as psyntax expands left to right.
;;; - let-syntax and letrec-syntax are R6RS's, which splice.
(define-library (srfi 72)
  (export define-syntax let-syntax letrec-syntax
          identifier? bound-identifier=? free-identifier=? literal-identifier=?
          syntax quasisyntax datum->syntax-object syntax-object->datum
          make-capturing-identifier begin-for-syntax around-syntax syntax-error
          syntax-case with-syntax syntax-rules)
  (import (rename (except (rnrs) syntax-rules)
                  (define-syntax r6:define-syntax)
                  (let-syntax r6:let-syntax)
                  (letrec-syntax r6:letrec-syntax)
                  (quasisyntax r6:quasisyntax))
          (rename (only (scheme base) syntax-rules) (syntax-rules r7:syntax-rules)))
  (begin
    (define (literal-identifier=? a b) (free-identifier=? a b))
    (define (datum->syntax-object template datum) (datum->syntax template datum))
    (define (syntax-object->datum x) (syntax->datum x))
    (define (make-capturing-identifier template symbol) (datum->syntax template symbol))
    (define (syntax-error . objects)
      (apply assertion-violation 'syntax-error "syntax error" objects))

    ;; A form as the SRFI's transformers see it: pairs and vectors of
    ;; identifiers and constants.
    (define (unwrap x)
      (syntax-case x ()
        ((a . b) (cons (unwrap #'a) (unwrap #'b)))
        (#(e ...) (list->vector (map unwrap #'(e ...))))
        (_ (if (identifier? x) x (syntax->datum x)))))

    (define (transformer t)
      (cond ((procedure? t) (lambda (form) (t (unwrap form))))
            (else t)))

    ;; syntax-rules transformers want psyntax's forms, not unwrapped ones
    (r6:define-syntax syntax-rules
      (r7:syntax-rules ()
        ((_ . rest) (r7:syntax-rules . rest))))

    (r6:define-syntax define-syntax
      (lambda (x)
        (syntax-case x ()
          ((_ (k . formals) e1 e2 ...)
           #'(r6:define-syntax k
               (let ((t (lambda (dummy . formals) e1 e2 ...)))
                 (lambda (form) (apply t (unwrap form))))))
          ((_ k (syntax-rules . rest))
           (and (identifier? #'syntax-rules)
                (free-identifier=? #'syntax-rules #'r7:syntax-rules))
           #'(r6:define-syntax k (syntax-rules . rest)))
          ((_ k e) #'(r6:define-syntax k (transformer e))))))

    (r6:define-syntax let-syntax
      (lambda (x)
        (syntax-case x ()
          ((_ ((k e) ...) b ...) #'(r6:let-syntax ((k (transformer e)) ...) b ...)))))

    (r6:define-syntax letrec-syntax
      (lambda (x)
        (syntax-case x ()
          ((_ ((k e) ...) b ...) #'(r6:letrec-syntax ((k (transformer e)) ...) b ...)))))

    ;; (quasisyntax template), unquote and unquote-splicing evaluated:
    ;; R6RS's quasisyntax, with unsyntax and unsyntax-splicing
    (r6:define-syntax quasisyntax
      (lambda (x)
        (define (convert t level)
          (syntax-case t (unquote unquote-splicing quasisyntax)
            ((unquote e)
             (if (= level 0)
                 #'(unsyntax e)
                 (with-syntax ((e* (convert #'e (- level 1)))) #'(unquote e*))))
            ((unquote-splicing e)
             (if (= level 0)
                 #'(unsyntax-splicing e)
                 (with-syntax ((e* (convert #'e (- level 1)))) #'(unquote-splicing e*))))
            ((quasisyntax e)
             (with-syntax ((e* (convert #'e (+ level 1)))) #'(quasisyntax e*)))
            ((a . b)
             (with-syntax ((a* (convert #'a level)) (b* (convert #'b level))) #'(a* . b*)))
            (#(e ...)
             (with-syntax (((e* ...) (map (lambda (e) (convert e level)) #'(e ...))))
               #'#(e* ...)))
            (_ t)))
        (syntax-case x ()
          ((_ t) (with-syntax ((t* (convert #'t 0))) #'(r6:quasisyntax t*))))))

    (r6:define-syntax begin-for-syntax
      (lambda (x)
        (syntax-case x ()
          ((_ form ...) #'(begin form ...)))))

    (r6:define-syntax around-syntax
      (lambda (x)
        (syntax-case x ()
          ((_ before form after)
           #'(begin
               (r6:let-syntax ((m (lambda (x) before #'(if #f #f)))) (m))
               (call-with-values
                   (lambda () form)
                 (lambda results
                   (r6:let-syntax ((m (lambda (x) after #'(if #f #f)))) (m))
                   (apply values results))))))))))
