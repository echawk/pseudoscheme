;;; Tests for SRFI 124: the SRFI's operations, and that the collector
;;; breaks an ephemeron whose key is unreachable, even when the datum
;;; refers to the key.
(import (scheme base) (scheme process-context) (srfi 64) (srfi 124)
        (pseudoscheme lisp))

(test-begin "srfi-124")

(define key (list 'k))
(define e (make-ephemeron key 'datum))
(test-assert (ephemeron? e))
(test-assert (not (ephemeron? key)))
(test-eq key (ephemeron-key e))
(test-eq 'datum (ephemeron-datum e))
(test-assert (not (ephemeron-broken? e)))
(reference-barrier key)

(define gc (lisp-function "gc" "sb-ext"))
(define (collect!) (lisp-funcall gc #:full #t))

;; the datum refers to the key: still collectable.  The collector scans
;; the stack conservatively, so a stray word may keep one key alive:
;; make many, and expect most to break.
(define (dropped)
  (let ((k (list 'gone)))
    (make-ephemeron k (vector k k))))
(define many (let loop ((i 0) (acc '()))
               (if (= i 100) acc (loop (+ i 1) (cons (dropped) acc)))))
(collect!)
(collect!)
(define broken (let loop ((l many) (acc '()))
                 (cond ((null? l) acc)
                       ((ephemeron-broken? (car l)) (loop (cdr l) (cons (car l) acc)))
                       (else (loop (cdr l) acc)))))
(test-assert "most break once their keys are gone" (> (length broken) 90))
(test-assert (pair? broken))
(test-eq #f (ephemeron-key (car broken)))
(test-eq #f (ephemeron-datum (car broken)))

;; a reachable key keeps it
(collect!)
(test-assert (not (ephemeron-broken? e)))
(test-eq key (ephemeron-key e))
(reference-barrier key)

(test-end "srfi-124")
(exit (= 0 (test-runner-fail-count (test-runner-current))))
