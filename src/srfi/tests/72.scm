;;; Tests for SRFI 72: the SRFI's examples that hold with one environment
;;; for all phases (see 72.sld).
(import (except (scheme base) define-syntax let-syntax letrec-syntax syntax-rules syntax-error)
        (scheme cxr) (scheme process-context) (srfi 64) (srfi 72))
(test-begin "srfi-72")

;; a transformer sees lists: cadr of the form
(define-syntax swap!
  (lambda (form)
    (let ((a (cadr form)) (b (caddr form)))
      (quasisyntax
       (let ((tmp ,a))
         (set! ,a ,b)
         (set! ,b tmp))))))
(test-equal '(2 1) (let ((x 1) (y 2)) (swap! x y) (list x y)))

;; define-syntax with formals
(define-syntax (my-if c a b) (quasisyntax (cond (,c ,a) (else ,b))))
(test-equal 'yes (my-if #t 'yes 'no))

;; unquote-splicing
(define-syntax (my-list . xs) (quasisyntax (list ,@xs)))
(test-equal '(1 2 3) (my-list 1 2 3))

;; syntax-case
(define-syntax my-or
  (lambda (x)
    (syntax-case x ()
      ((_) (syntax #f))
      ((_ e) (syntax e))
      ((_ e r ...) (syntax (let ((t e)) (if t t (my-or r ...))))))))
(test-equal 3 (my-or #f 3))
(test-equal 5 (let ((t 5)) (my-or #f t)))

(test-equal 1
  (let-syntax ((m (lambda (_)
                    (let ((x (syntax x)))
                      (let ((x* (datum->syntax-object x 'x)))
                        (quasisyntax
                         (let ((,x 1)) ,x*)))))))
    (m)))

;; make-capturing-identifier, as datum->syntax
(define-syntax if-it
  (lambda (x)
    (syntax-case x ()
      ((k e1 e2 e3)
       (with-syntax ((it (make-capturing-identifier (syntax k) 'it)))
         (syntax (let ((it e1))
                   (if it e2 e3))))))))
(test-equal 2 (if-it 2 it 3))

(test-assert (literal-identifier=? (syntax else) (syntax else)))
(test-equal 'x (syntax-object->datum (syntax x)))
(test-equal 7 (around-syntax #t (+ 3 4) #t))
(test-equal '(1 2) (let-syntax ((m (syntax-rules () ((_ a b) (list a b))))) (m 1 2)))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-72")
  (exit (if (zero? failures) 0 1)))
