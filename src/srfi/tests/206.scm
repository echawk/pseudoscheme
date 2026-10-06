;;; Tests for SRFI 206, after the sample's tests
;;; (reference/srfi-206/srfi-206-tests.sch).  The tests that need two
;;; separately defined auxiliary keywords to share a binding are marked
;;; as expected failures: psyntax can't do that (see 206.sld).  The
;;; sample's syntax-parameterize test needs SRFI 139, which Pseudoscheme
;;; lacks, and is left out.
(import (scheme base) (scheme process-context) (srfi 64)
        (srfi 206)
        (only (rnrs syntax-case) syntax free-identifier=?)
        (rename (only (srfi 206 all) $ yield) (yield baz))
        (prefix (only (srfi 206 all) else => _ ... <>) all:)
        (only (srfi 26) cut <>))

(test-begin "srfi-206")

(test-assert
    (let* ()
      (define-auxiliary-syntax foo foo)
      (define-syntax is-foo?
        (syntax-rules (foo)
          ((_ foo) #t)
          ((_ _) #f)))
      (let* ()
        (is-foo? foo))))

(test-expect-fail 1)
(test-assert
    (let* ()
      (define-auxiliary-syntax foo foo)
      (define-syntax is-foo?
        (syntax-rules (foo)
          ((_ foo) #t)
          ((_ _) #f)))
      (let* ()
        (define-auxiliary-syntax bar foo)
        (is-foo? bar))))

(test-assert
    (not
     (let* ()
       (define-auxiliary-syntax foo foo)
       (define-syntax is-foo?
         (syntax-rules (foo)
           ((_ foo) #t)
           ((_ _) #f)))
       (let ()
         (define-syntax foo (syntax-rules ()))
         (is-foo? foo)))))

(test-expect-fail 1)
(test-assert
    (let* ()
      (define-auxiliary-syntax $2 $)
      (free-identifier=? #'$2 #'$)))

(test-assert
    (let* ()
      (define-auxiliary-syntax bar2 bar)
      (not (free-identifier=? #'bar2 #'baz))))

(test-assert
    (let* ()
      (define-auxiliary-syntax baz2 baz)
      (not (free-identifier=? #'baz2 #'$))))

;; One-argument form
(test-assert
    (let* ()
      (define-auxiliary-syntax qux)
      (define-syntax is-qux?
        (syntax-rules (qux)
          ((_ qux) #t)
          ((_ _) #f)))
      (and (is-qux? qux) (not (is-qux? other)))))

;; (srfi 206 all): the same bindings as R7RS's and SRFI 26's
(test-assert (free-identifier=? #'all:else #'else))
(test-assert (free-identifier=? #'all:=> #'=>))
(test-assert (free-identifier=? #'all:_ #'_))
(test-assert (free-identifier=? #'(... all:...) #'(... ...)))
(test-assert (free-identifier=? #'all:<> #'<>))
(test-equal 'yes (cond (#f 'no) (all:else 'yes)))
(test-equal 3 ((cut + 1 all:<>) 2))
(test-assert (not (free-identifier=? #'baz #'$)))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-206")
  (exit (if (zero? failures) 0 1)))
