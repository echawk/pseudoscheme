;;; -*- Mode: Scheme -*-
;;;; (chezscheme): Chez Scheme's hashtables beyond R6RS: weak and
;;;; ephemeron tables, and the eq- and symbol- procedures, which are
;;;; R6RS's here.  Part of the library's body (src/chez/chez.lisp).

(define make-weak-eq-hashtable %chez:make-weak-eq-hashtable)
(define make-weak-eqv-hashtable %chez:make-weak-eqv-hashtable)
;; SBCL's tables weak on their keys are ephemeron tables
(define make-ephemeron-eq-hashtable %chez:make-weak-eq-hashtable)
(define make-ephemeron-eqv-hashtable %chez:make-weak-eqv-hashtable)
(define hashtable-weak? %chez:hashtable-weak?)
(define hashtable-ephemeron? %chez:hashtable-weak?)

(define (eq-hashtable? x)
  (and (hashtable? x) (eq? (hashtable-equivalence-function x) eq?)))
(define (eq-hashtable-weak? h) (hashtable-weak? h))
(define (eq-hashtable-ephemeron? h) (hashtable-weak? h))
(define (eq-hashtable-ref h k default) (hashtable-ref h k default))
(define (eq-hashtable-set! h k v) (hashtable-set! h k v))
(define (eq-hashtable-contains? h k) (hashtable-contains? h k))
(define (eq-hashtable-delete! h k) (hashtable-delete! h k))
(define (eq-hashtable-update! h k proc default) (hashtable-update! h k proc default))

(define (symbol-hashtable? x) (hashtable? x))
(define (symbol-hashtable-ref h k default) (hashtable-ref h k default))
(define (symbol-hashtable-set! h k v) (hashtable-set! h k v))
(define (symbol-hashtable-contains? h k) (hashtable-contains? h k))
(define (symbol-hashtable-delete! h k) (hashtable-delete! h k))
(define (symbol-hashtable-update! h k proc default) (hashtable-update! h k proc default))

(define (hashtable-values h)
  (let-values (((keys values) (hashtable-entries h))) values))

(define (hashtable-cells h . limit)
  (let-values (((keys values) (hashtable-entries h)))
    (vector-map cons keys values)))

;; Chez's cell is the pair the table holds, so that setting its cdr sets
;; the entry; here it is a fresh pair, and setting it does nothing.
(define (hashtable-cell h k default)
  (unless (hashtable-contains? h k) (hashtable-set! h k default))
  (cons k (hashtable-ref h k default)))
(define eq-hashtable-cell hashtable-cell)
(define symbol-hashtable-cell hashtable-cell)
