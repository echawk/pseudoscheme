;;; Tests for SRFI 240: the sample implementation's test suite
;;; (reference/srfi-240/tests.sps), as SRFI 64 tests, plus a few more.
;;; Most definitions are internal ones, in several procedures:
;;; Pseudoscheme compiles many definitions and calls in one body slowly
;;; (see ../57.sld).
(import (except (scheme base) define-record-type)
        (scheme process-context)
        (srfi 64)
        (except (srfi 237) define-record-type)
        (srfi 240)
        (prefix (srfi :240 define-record-type) r6:))

;; An R7RS-style definition at top level.
(define-record-type foo
  (make-foo x)
  foo?
  (x foo-x)
  (y foo-y foo-set-y!))

(define (foo-tests)
  (define-record-type bar
    (parent foo)
    (fields z)
    (protocol
     (lambda (n)
       (lambda (x z)
         ((n x) z)))))
  (test-equal #t (foo? (make-foo 1)))
  (test-equal 2 (foo-x (make-foo 2)))
  (test-equal '(3 4) (let ((foo (make-foo 3)))
                       (foo-set-y! foo 4)
                       (list (foo-x foo) (foo-y foo))))
  (let ((rtd (record-type-descriptor foo))
        (rcd (record-constructor-descriptor foo)))
    (test-equal 'foo (record-type-name rtd))
    (test-assert (not (record-type-parent rtd)))
    (test-assert (record-type-generative? rtd))
    (test-assert (not (record-type-sealed? rtd)))
    (test-assert (not (record-type-opaque? rtd)))
    (test-equal '#(x y) (record-type-field-names rtd))
    (test-assert (not (record-field-mutable? rtd 0)))
    (test-assert (record-field-mutable? rtd 1))
    (test-assert rcd))
  (test-assert (foo? (make-bar 5 6)))
  (test-equal 5 (foo-x (make-bar 5 6)))
  (test-equal 6 (bar-z (make-bar 5 6)))
  (test-equal 7 (let ((bar (make-bar 5 6)))
                  (foo-set-y! bar 7)
                  (foo-y bar)))
  (test-equal "uninitialized field" #f (foo-y (make-foo 1))))

(define (layer-tests)
  (define-record-type rec1
    (fields a)
    (protocol
     (lambda (p)
       (lambda (a/2)
         (p (* 2 a/2))))))
  (define rec2
    (make-record-descriptor
     'rec2 rec1 #f #f #f '#((immutable b))
     (lambda (n)
       (lambda (a/2 b)
         ((n a/2) b)))))
  (define make-rec2 (record-constructor rec2))
  (define rec2? (record-predicate rec2))
  (define rec2-b (record-accessor rec2 0))
  (define-record-type rec3
    (parent rec2)
    (fields c)
    (protocol
     (lambda (n)
       (lambda (c)
         ((n c c) c)))))
  (test-equal '(10 5 5) (let ((r (make-rec3 5)))
                          (list (rec1-a r) (rec2-b r) (rec3-c r))))
  (test-assert (rec2? (make-rec2 1 2))))

(define (misc-tests)
  (define-record-type gen
    (generative))
  (define-record-type (sname rname))
  (test-assert (gen? (make-gen)))
  (test-assert (sname? (make-sname)))
  (test-assert (record-descriptor? rname)))

(define (salmon-tests)
  (define-record-type fish
    (fields name))
  (define-record-name (salmon fish)
    (protocol
     (lambda (p)
       (lambda ()
         (p 'salmon)))))
  (define-record-type colored-salmon
    (parent salmon)
    (fields color)
    (protocol
     (lambda (n)
       (lambda (c)
         ((n) c)))))
  (define-record-name (green-salmon colored-salmon)
    (protocol
     (lambda (n)
       (lambda ()
         ((n) 'green)))))
  (define-record-name (blue-salmon colored-salmon)
    (parent fish)
    (protocol
     (lambda (n)
       (lambda ()
         ((n 'salmon) 'blue)))))
  (test-equal 'salmon (fish-name (make-salmon)))
  (test-equal 'green (colored-salmon-color (make-green-salmon)))
  (test-equal 'blue (colored-salmon-color (make-blue-salmon))))

(define (r7rs-tests)
  ;; Constructor fields in another order, an R6RS-named sublibrary.
  (r6:define-record-type pare
    (kons y x)
    pare?
    (x kar set-kar!)
    (y kdr))
  (let ((p (kons 1 2)))
    (set-kar! p 3)
    (test-equal '(3 1) (list (kar p) (kdr p)))
    (test-assert (pare? p))
    (test-assert (not (pare? (cons 1 2))))))

(test-begin "srfi-240")
(foo-tests)
(layer-tests)
(misc-tests)
(salmon-tests)
(r7rs-tests)
(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-240")
  (exit (if (zero? failures) 0 1)))
