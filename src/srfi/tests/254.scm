;;; Tests for SRFI 254: the SRFI's tests.sps, and more from its
;;; specification.  Collection is provoked with SBCL's gc; SBCL scans the
;;; stack conservatively, so the tests ask only that most of a batch of
;;; unreachable keys be collected.
(import (scheme base) (scheme process-context) (srfi 64) (srfi 254)
        (only (prefix (cl sb-ext) sb-ext:) sb-ext:gc))
(define (collect) (sb-ext:gc #:full #t) (sb-ext:gc #:full #t))
(test-begin "srfi-254")

;; tests.sps
(define k1 (vector #f))
(define e1 (make-ephemeron k1 'foo))
(test-eqv 'foo (ephemeron-ref e1 k1))
(test-eqv 'bar (ephemeron-ref e1 (vector #f) 'bar))
(test-assert (not (ephemeron-ref e1 (vector #f))))

(test-assert (ephemeron? e1))
(test-assert (not (ephemeron? k1)))
(test-eq k1 (ephemeron-key e1))
(test-eqv 'foo (ephemeron-value e1))
(test-assert (not (ephemeron-broken? e1)))
(collect)
(test-assert "a reachable key survives" (not (ephemeron-broken? e1)))
(reference-barrier k1)

;; unreachable keys break their ephemerons, values that refer to the key
;; included
(define ephemerons
  (let loop ((i 0) (acc '()))
    (if (= i 200)
        acc
        (loop (+ i 1) (cons (let ((k (vector i))) (make-ephemeron k (list 'value k))) acc)))))
(collect)
(define broken (length (filter-broken ephemerons)))
(define (filter-broken es)
  (let loop ((es es) (acc '()))
    (cond ((null? es) acc)
          ((ephemeron-broken? (car es)) (loop (cdr es) (cons (car es) acc)))
          (else (loop (cdr es) acc)))))
(test-assert "unreachable keys break their ephemerons" (> broken 100))
(test-assert (let ((b (car (filter-broken ephemerons))))
               (and (not (ephemeron-key b)) (not (ephemeron-value b)))))

;; guardians
(define g (make-guardian))
(test-assert (guardian? g))
(test-assert (not (guardian? (lambda () #f))))
(test-assert (not (g)))
(let loop ((i 0)) (when (< i 200) (g (vector i) i) (loop (+ i 1))))
(collect)
(define resurrected
  (let loop ((acc '())) (let ((r (g))) (if r (loop (cons r acc)) acc))))
(test-assert "representatives of collected objects come back" (> (length resurrected) 100))
(test-assert (let loop ((rs resurrected)) (or (null? rs) (and (exact-integer? (car rs)) (loop (cdr rs))))))
(define kept (vector 'kept))
(g kept 'kept-rep)
(collect)
(test-assert "a reachable object's representative doesn't" (not (memq 'kept-rep (let loop ((acc '())) (let ((r (g))) (if r (loop (cons r acc)) acc))))))
(reference-barrier kept)

;; transport cells
(define tg (make-transport-cell-guardian))
(test-assert (transport-cell-guardian? tg))
(define key (vector 1))
(define cell (tg key 'v))
(test-assert (transport-cell? cell))
(test-eq key (transport-cell-key cell))
(test-eqv 'v (transport-cell-value cell))
(test-assert (not (transport-cell-broken? cell)))
(test-assert (exact-integer? (current-hash key)))
(test-eqv (current-hash key) (current-hash key))
(collect)
(test-eq "after a collection the cell is reported" cell (tg))
(test-assert (not (tg)))
(reference-barrier key)

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-254")
  (exit (if (zero? failures) 0 1)))
