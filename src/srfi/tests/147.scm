;;; Tests for SRFI 147: the sample implementation's two test suites
;;; (reference/srfi-147/test.sld and er-macro-transformer-test.sld),
;;; with their library framing replaced by this program's, plus tests of
;;; the transformer specs that pass through (syntax-case, SRFI 211).
(import (except (scheme base) define-syntax let-syntax letrec-syntax syntax-rules)
        (scheme process-context)
        (srfi 64)
        (srfi 147)
        (srfi 147 er-macro-transformer)
        (only (rnrs syntax-case) syntax-case syntax)
        (only (rnrs base) identifier-syntax)
        (prefix (srfi 211 explicit-renaming) srfi-211:))

(test-begin "srfi-147")



(test-group "R7RS macros"
  (define-syntax foo
    (syntax-rules ()
      ((foo)
       42)))

  (test-equal 42 (foo))

  (test-equal 42 (let-syntax
                     ((foo
                       (syntax-rules ()
                         ((foo)
                          42))))
                   (foo)))

  (test-equal 42 (letrec-syntax
                     ((foo
                       (syntax-rules ()
                         ((foo)
                          42)))
                      (bar
                       (syntax-rules ()
                         ((bar)
                          (foo)))))
                   (bar))))

(test-group "Custom macro transformers"
  (define-syntax simple-syntax-rules
    (syntax-rules ()
      ((simple-syntax-rules . rules)
       (syntax-rules () . rules))))

  (define-syntax bar-rules
    (simple-syntax-rules
     ((bar-rules (pattern template) ...)
      (simple-syntax-rules (pattern '(bar template)) ...))))
  
  (define-syntax foo
    (simple-syntax-rules
     ((foo)
      42)))

  (define-syntax bar
    (bar-rules
     ((bar x) x)))
  
  (test-equal 42 (foo))

  (test-equal 42 (let-syntax
                     ((foo
                       (simple-syntax-rules
                         ((foo)
                          42))))
                   (foo)))

  (test-equal '(bar 42) (bar 42)))

(test-group "Auxiliary definitions in custom macro transformers"
  (define-syntax my-macro-transformer
    (syntax-rules ()
      ((my-macro-transformer)
       (begin (define foo 2)
              (syntax-rules ()
                ((_) foo))))))
  
  (test-equal 42 (* 21 (letrec-syntax ((foo (my-macro-transformer)))
                         (foo)))))

(test-group "Scoping of expansion"
  (define-syntax simple-syntax-rules
    (syntax-rules ()
      ((simple-syntax-rules . rules)
       (syntax-rules () . rules))))

  (test-equal 'foo (let-syntax
                       ((simple-syntax-rules
                         (simple-syntax-rules ((_) 'foo))))
                     (simple-syntax-rules))))

(test-group "Custom ellipsis"
  (define-syntax my-syntax-rules
    (syntax-rules !!! ()
      ((my-syntax-rules e l* rule !!!)
       (syntax-rules e l* rule !!!))))
  
  (define-syntax foo
    (my-syntax-rules ::: ()
      ((foo a) 'a)
      ((foo a b) '(a . b))                   
      ((foo a :::) (list 'a :::))))

  (test-equal '(a b c) (foo a b c)))

(test-group "Aliases for keywords"
  (define-syntax λ lambda)
  (define foo (λ () 'baz))
  (test-equal 'baz (foo)))

(test-group "Example from specification"
  (define-syntax syntax-rules*
    (syntax-rules ()
      ((syntax-rules* (literal ...) (pattern . templates) ...)
       (syntax-rules (literal ...) (pattern (begin . templates)) ...))
      ((syntax-rules* ellipsis (literal ...) (pattern . templates) ...)
       (syntax-rules ellipsis (literal ...) (pattern (begin . templates)) ...))))

  (test-equal '(1 2) (let-syntax
                         ((foo
                           (syntax-rules* ()
                                          ((foo a b)
                                           (define a 1)
                                           (define b 2)))))
                       (foo x y)
                       (list x y))))


(test-group "SRFI 147: er-macro-transformer"

(test-group "R7RS macros"

  (define-syntax foo
    (er-macro-transformer
     (lambda (expr rename compare)
       42)))

  (test-equal 42 (foo))

  (test-equal 42 (let-syntax
                     ((foo
                       (er-macro-transformer
                        (lambda (expr rename compare)
                          42))))
                   (foo)))

  (test-equal 42 (letrec-syntax
                     ((foo
                       (er-macro-transformer
                        (lambda (expr rename compare)
                          42)))
                      (bar
                       (er-macro-transformer
                        (lambda (expr rename compare)
                          `(,(rename 'foo))))))
                   (bar))))

(test-group "Custom macro transformers"

  (define-syntax unhygienic-transformer
    (syntax-rules ()
      ((unhygienic-transformer transformer)
       (er-macro-transformer
        (lambda (expr rename compare)
          (transformer expr))))))

  (define-syntax bar-transformer
    (unhygienic-transformer
     (lambda (expr)
       '(unhygienic-transformer
         (lambda (expr)
           ''bar)))))
  
  (define-syntax foo
    (unhygienic-transformer
     (lambda (expr)
       42)))

  (define-syntax bar
    (bar-transformer))
  
  (test-equal 42 (foo))
    
  (test-equal 42 (let-syntax
                     ((foo
                       (unhygienic-transformer
                         (lambda (expr)
                           42))))
                   (foo)))

  (test-equal 'bar (bar 42)))

)

(test-group "Transformer specs used as they are"
  (define-syntax sc-swap
    (lambda (x)
      (syntax-case x ()
        ((_ a b) (syntax (list b a))))))
  (define-syntax er-first
    (srfi-211:er-macro-transformer
     (lambda (x r c) (cadr x))))
  (define the-value 7)
  (define-syntax seven (identifier-syntax the-value))
  (test-equal '(2 1) (sc-swap 1 2))
  (test-equal 1 (er-first 1 2))
  (test-equal 7 seven)
  (test-equal 3 (let-syntax ((m (lambda (x) (syntax 3)))) (m))))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-147")
  (exit (if (zero? failures) 0 1)))
