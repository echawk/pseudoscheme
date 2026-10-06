;;; Tests for SRFI 252: the sample implementation's test suite
;;; (reference/srfi-252/property-test-tests.scm), with its import header
;;; changed to (srfi 252), the suite named srfi-252, the error type of its test-property-error-type
;;; test changed (see there), and its test-end replaced by the exit
;;; below; the rest is unchanged.

;; Tests for property-test library.
;; SPDX-License-Identifier: MIT
;; Copyright 2024 Antero Mejr <mail@antr.me>

(import (scheme base)
        (scheme complex)
        (scheme read)
        (scheme process-context)
        (srfi 1)
        (srfi 27)
        (srfi 64)
        (srfi 158)
        (srfi 194)
        (srfi 252))

(test-begin "srfi-252")

(define (three x) 3)
(define (wrong-three x) x)
(define (three-property x) (= (three x) 3))
(define (wrong-three-property x) (= (wrong-three x) 3))
(define (error-three-property x) (string-append 1 2))
(define (make-read-error x) (read (open-input-string (string-append ")" x))))
(define (make-read-error-property x) (symbol? (make-read-error x)))
(define (bad-generator) (gmap (lambda (x)
                                (string-append 1 2)
                                x)
                              (boolean-generator)))

(test-group "test-property"
  (test-property three-property (list (integer-generator)))
  (test-property three-property (list (real-generator)))
  (test-property three-property (list (integer-generator)) 10))

(test-group "test-property-expect-fail"
  (test-property-expect-fail wrong-three-property (list (integer-generator)))
  (test-property-expect-fail wrong-three-property (list (integer-generator)) 10))

(test-group "test-property-skip" ; shouldn't run
  (test-property-skip three-property (list (bad-generator)))
  (test-property-skip three-property (list (bad-generator)) 10))

(test-group "test-property-error"
  (test-property-error error-three-property (list (integer-generator)))
  (test-property-error error-three-property (list (integer-generator)) 10))

;; The sample tests this with SRFI 36's &read-error, which this SRFI 64
;; does not accept as an error type (it takes predicates) and which
;; Pseudoscheme's reader does not raise (it raises R6RS &lexical); so
;; the type here is R7RS's read-error? predicate instead.
(test-group "test-property-error-type"
            (test-property-error-type read-error? make-read-error-property
                                      (list (string-generator))))

(test-group "test-property/with-2-arguments"
  (test-property (lambda (x y)
                   (and (boolean? x) (integer? y)))
                 (list (boolean-generator) (integer-generator))))

;; Testing basic generators

(test-group "boolean-generator"
  (test-property boolean? (list (boolean-generator))))

(test-group "bytevector-generator"
  (test-property bytevector? (list (bytevector-generator))))

(test-group "char-generator"
  (test-property char? (list (char-generator))))

(test-group "string-generator"
  (test-property string? (list (string-generator))))

(test-group "symbol-generator"
  (test-property symbol? (list (symbol-generator))))

;; Testing exact generators

(cond-expand
 (exact-complex
  (test-group "exact-complex-generator"
    (test-property (lambda (x)
                     (and (complex? x)
                          (exact? (real-part x))
                          (exact? (imag-part x))))
                   (list (exact-complex-generator)))))
 (else))

(test-group "exact-integer-generator"
  (test-property (lambda (x)
                   (and (integer? x) (exact? x)))
                 (list (exact-integer-generator))))

(test-group "exact-number-generator"
  (test-property exact? (list (exact-number-generator))))

(test-group "exact-rational-generator"
  (test-property (lambda (x)
                   (and (exact? x) (rational? x)))
                 (list (exact-rational-generator))))

(test-group "exact-real-generator"
  (test-property (lambda (x)
                   (and (exact? x) (real? x)))
                 (list (exact-real-generator))))

(test-group "exact-integer-complex-generator"
  (cond-expand
   (exact-complex
    (test-property (lambda (x)
                     (and (complex? x)
                          (exact? (real-part x))
                          (exact? (imag-part x))
                          (integer? (real-part x))
                          (integer? (imag-part x))))
                   (list (exact-integer-complex-generator))))
   (else)))

;; Testing inexact generators

(test-group "inexact-complex-generator"
  (test-property (lambda (x)
                   (and (complex? x)
                        (inexact? (real-part x))
                        (inexact? (imag-part x))))
                 (list (inexact-complex-generator))))

(test-group "inexact-integer-generator"
  (test-property (lambda (x)
                   (and (inexact? x) (integer? x)))
                 (list (inexact-integer-generator))))

(test-group "inexact-number-generator"
  (test-property inexact? (list (inexact-number-generator))))

(test-group "inexact-rational-generator"
  (test-property (lambda (x)
                   (and (inexact? x) (rational? x)))
                 (list (inexact-rational-generator))))

(test-group "inexact-integer-generator"
  (test-property (lambda (x)
                   (and (inexact? x) (real? x)))
                 (list (inexact-real-generator))))

;; Testing union generators

(test-group "complex-generator"
  (test-property complex? (list (complex-generator))))

(test-group "integer-generator"
  (test-property integer? (list (integer-generator))))

(test-group "number-generator"
  (test-property number? (list (number-generator))))

(test-group "rational-generator"
  (test-property rational? (list (rational-generator))))

(test-group "real-generator"
  (test-property real? (list (real-generator))))

;; Testing special generators

(test-group "list-generator-of"
  (test-property (lambda (x)
                   (and (list? x) (<= (length x) 1001)
                        (every integer? x)))
                 (list (list-generator-of (integer-generator)))))

(test-group "pair-generator-of"
  (test-property (lambda (x)
                   (pair? x) (integer? (car x)) (boolean? (cdr x)))
                 (list (pair-generator-of (integer-generator)
                                          (boolean-generator)))))

(test-group "procedure-generator-of"
  (test-property (lambda (x)
                   (procedure? x) (integer? (x)))
                 (list (procedure-generator-of (integer-generator)))))

(test-group "vector-generator-of"
  (test-property (lambda (x)
                   (vector? x)
                   (<= (vector-length x) 1001)
                   (every integer? (vector->list x)))
                 (list (vector-generator-of (integer-generator)))))

(test-group "non-determinism"
  (let ((gen1 (gdrop (exact-number-generator) 30)) ;skip the initial sequence
        (gen2 (gdrop (exact-number-generator) 30)))
    (test-property (lambda (x y)
                     (not (= x y)))
                   (list gen1 gen2))))

(test-group "determinism"
  (parameterize ((current-random-source (make-random-source)))
    (let ((gen1 (gdrop (exact-number-generator) 30)))
      (parameterize ((current-random-source (make-random-source)))
        (let ((gen2 (gdrop (exact-number-generator) 30)))
          (test-property = (list gen1 gen2)))))))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-252")
  (exit (if (zero? failures) 0 1)))
