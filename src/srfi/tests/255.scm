;;; Tests for SRFI 255: the sample implementation's tests
;;; (reference/srfi-255/test-restarters.scm), as an R7RS program on
;;; (rnrs), as the sample's test-chez.scm runs them.  Changes: the
;;; sample's custom test runner is left out (the default runner writes
;;; the log), the suite is named srfi-255, the ending is the exit
;;; below, and restart/tag's error call names con instead of the unbound
;;; variable restarters, and "restarter-guard unrestartable"'s interactor
;;; tests (restarter-tag c) instead of the unbound (restarter-tag r)
;;; (Chez only warns about unbound variables; psyntax rejects them).

;;; SPDX-FileCopyrightText: 2024 Wolfgang Corcoran-Mathe, Marc Nieper-Wißkirchen
;;; SPDX-License-Identifier: MIT

(import (rnrs)
        (srfi 39)
        (srfi 64)
        (srfi 255))

;; Invoke the restarter with tag *tag* on the args, if
;; such a restarter is a component of *con*.
(define (restart/tag tag con . args)
  (let ((r (find (lambda (c)
                   (and (restarter? c)
                        (eqv? tag (restarter-tag c))))
                 (simple-conditions con))))
    (if r
        (apply restart r args)
        (error 'restart/tag
               "no restarter found with this tag"
               tag con))))

(test-begin "srfi-255")
(test-assert "restarter objects 1"
 (call-with-current-continuation
  (lambda (k)
    (with-exception-handler
     (lambda (con) (restart/tag 'use-value con #t))
     (lambda ()
       (raise-continuable
        (make-restarter 'use-value "Return x." 'foo '(x) k)))))))

(test-assert "restarter objects 2"
 (call-with-current-continuation
  (lambda (k)
    (with-exception-handler
     (lambda (con) (restart/tag 'use-not-value con #f))
     (lambda ()
       (raise-continuable
        (make-restarter 'use-not-value
                        "Return (not x)."
                        'foo
                        '(x)
                        (lambda (x) (k (not x))))))))))

(test-equal "restarter objects 3"
 '(dump-restarter-data "Return restarter info as a list." () foo)
 (call-with-current-continuation
  (lambda (k)
    (with-exception-handler
     (lambda (con)
       (restart/tag 'dump-restarter-data con))
     (lambda ()
       (letrec*
        ((dump
          (lambda ()
            (k (list (restarter-tag r)
                     (restarter-description r)
                     (restarter-formals r)
                     (restarter-who r)))))
         (r (make-restarter 'dump-restarter-data
                            "Return restarter info as a list."
                            'foo
                            '()
                            dump)))

         (raise-continuable r)))))))

(test-assert "restarter-guard 1"
 (with-exception-handler
  (lambda (con) (restart/tag 'return-true con))
  (lambda ()
    (restarter-guard
     (((return-true) "Return #t." condition? #t))
     (error 'no-one "something happen!")))))

(test-equal "restarter-guard 2"
 '(1 2 3)
 (with-exception-handler
  (lambda (con) (restart/tag 'return-values con 1 2 3))
  (lambda ()
    (restarter-guard
     (((return-values . vs) "Return vs." error? vs))
     (error 'no-one "something happen!")))))

(test-equal "restarter-guard filters restarters"
 '(foo)
 (guard (con ((restarter? con)
              (map restarter-tag
                   (filter restarter? (simple-conditions con)))))
   (restarter-guard somewhere
    (con ((foo) "" assertion-violation? 0)
         ((bar) "" error? 1))
     (assertion-violation 'somewhere "bad"))))

(test-eqv "restarter-guard unrestartable"
 0
 (guard (con
         ((assertion-violation? con) 0))
   (parameterize ((current-interactor
                   (lambda (con)
                     (let ((r (find (lambda (c)
                                      (and (restarter? c)
                                           (eqv? (restarter-tag c)
                                                 'return-1)))
                                    (simple-conditions con))))
                       (and r (restart r))))))
     (with-current-interactor
      (lambda ()
        (restarter-guard foo
         (con ((return-1) "" error? 1))
          (assertion-violation 'foo "bad")))))))

(test-assert "define-restartable 1"
 (with-exception-handler
  (lambda (con) (restart/tag 'use-arguments con #t))
  (lambda ()
    (define-restartable (f x)
      (if (not x)
          (assertion-violation 'f "false")
          x))
    (f #f))))

(test-equal "define-restartable 2"
 '(1 2)
 (with-exception-handler
  (lambda (con) (restart/tag 'use-arguments con 1 2))
  (lambda ()
    (define-restartable (f . xs)
      (if (null? xs)
          (assertion-violation 'f "empty")
          xs))
    (f))))

(test-assert "define-restartable 3"
 (with-exception-handler
  (lambda (con) (restart/tag 'use-arguments con #t))
  (lambda ()
    (define-restartable f
      (lambda (x)
        (if (not x)
            (assertion-violation 'f "false")
            x)))
    (f #f))))

(test-equal "define-restartable 4 (polyvariadic)"
 '(1 (2))
 (with-exception-handler
  (lambda (con) (restart/tag 'use-arguments con 1 2))
  (lambda ()
    (define-restartable (f x . rest)
      (if (null? rest)
          (assertion-violation 'f "empty rest parameter")
          (list x rest)))
    (f 1))))

(test-equal "restartable 1"
 '(2 3 4 5)
 (with-exception-handler
  (lambda (con) (restart/tag 'use-arguments con 3))
  (lambda ()
    (map (restartable
          "anonymous"
          (lambda (x)
	    (if x (+ x 1) (assertion-violation 'no-one "false"))))
	 '(1 2 #f 4)))))

(test-equal "restartable 2 (variadic)"
 '(1 2)
 (with-exception-handler
  (lambda (con) (restart/tag 'use-arguments con 1 2))
  (lambda ()
    ((restartable "test"
                  (lambda xs
                    (if (null? xs)
                        (assertion-violation 'test "empty")
                        xs)))))))

(test-equal "restartable 3 (polyvariadic)"
 '(1 (2))
 (with-exception-handler
  (lambda (con) (restart/tag 'use-arguments con 1 2))
  (lambda ()
    ((restartable "test"
                  (lambda (x . rest)
                    (if (null? rest)
                        (assertion-violation 'test "empty")
                        (list x rest))))
     0))))

(test-equal "with-current-interactor"
 0
 (with-current-interactor
  (lambda ()
    (parameterize ((current-interactor
                    (lambda (con)
                      (let ((r (find (lambda (c)
                                       (and (restarter? c)
                                            (eqv? (restarter-tag c)
                                                  'return-zero)))
                                     (simple-conditions con))))
                        (and r (restart r))))))
      (restarter-guard somewhere
       (con ((return-zero)
              "return zero"
              assertion-violation?
              0))
       (assertion-violation 'somewhere "bad"))))))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-255")
  (exit (if (zero? failures) 0 1)))
