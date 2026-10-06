;;; Tests for SRFI 123: the reference test suite
;;; (reference/srfi-123/tests-srfi-123.sld), as a program.  Records are
;;; R7RS define-record-type's.  "bad record assignment" is an expected
;;; failure: fields of R7RS records are mutable through the R6RS record
;;; API whether or not they have a modifier.
(import (except (scheme base) set!)
        (scheme process-context)
        (rnrs hashtables)
        (srfi 4) (srfi 111)
        (srfi 64)
        (srfi 123))

(define-record-type <foo> (make-foo a b) foo?
  (a foo-a set-foo-a!)
  (b foo-b))

(test-begin "srfi-123")

(test-begin "ref")
(test-assert "bytevector" (= 1 (ref (bytevector 0 1 2) 1)))
(test-assert "hashtable" (let ((table (make-eqv-hashtable)))
                           (hashtable-set! table 'foo 0)
                           (= 0 (ref table 'foo))))
(test-assert "hashtable default" (let ((table (make-eqv-hashtable)))
                                   (= 1 (ref table 0 1))))
(test-error "hashtable missing" #t (ref (make-eqv-hashtable) 0))
(test-assert "pair" (= 1 (ref (cons 0 1) 'cdr)))
(test-assert "list" (= 1 (ref (list 0 1 2) 1)))
(test-assert "string" (char=? #\b (ref "abc" 1)))
(test-assert "vector" (= 1 (ref (vector 0 1 2) 1)))
(test-assert "record" (= 1 (ref (make-foo 0 1) 'b)))
(test-assert "srfi-4" (= 1 (ref (s16vector 0 1 2) 1)))
(test-assert "srfi-111" (= 1 (ref (box 1) '*)))
(test-end "ref")

(test-assert "ref*" (= 1 (ref* '(_ #(_ (0 . 1) _) _) 1 1 'cdr)))
(test-assert "~" (= 1 (~ '(_ #(_ (0 . 1) _) _) 1 1 'cdr)))

(test-begin "ref setter")
(test-assert "bytevector" (let ((bv (bytevector 0 1 2)))
                            (set! (ref bv 1) 3)
                            (= 3 (ref bv 1))))
(test-assert "hashtable" (let ((ht (make-eqv-hashtable)))
                           (set! (ref ht 'foo) 0)
                           (= 0 (ref ht 'foo))))
(test-assert "pair" (let ((p (cons 0 1)))
                      (set! (ref p 'cdr) 2)
                      (= 2 (ref p 'cdr))))
(test-assert "list" (let ((l (list 0 1 2)))
                      (set! (ref l 1) 3)
                      (= 3 (ref l 1))))
(test-assert "string" (let ((s (string #\a #\b #\c)))
                        (set! (ref s 1) #\d)
                        (char=? #\d (ref s 1))))
(test-assert "vector" (let ((v (vector 0 1 2)))
                        (set! (ref v 1) 3)
                        (= 3 (ref v 1))))
(test-assert "record" (let ((r (make-foo 0 1)))
                        (set! (ref r 'a) 2)
                        (= 2 (ref r 'a))))
(test-expect-fail 1)
(test-assert "bad record assignment"
  (not (guard (err (else #f)) (set! (ref (make-foo 0 1) 'b) 2) #t)))
(test-assert "srfi-4" (let ((s16v (s16vector 0 1 2)))
                        (set! (ref s16v 1) 3)
                        (= 3 (ref s16v 1))))
(test-assert "srfi-111" (let ((b (box 0)))
                          (set! (ref b '*) 1)
                          (= 1 (ref b '*))))
(test-end "ref setter")

(test-assert "ref* setter"
  (let ((obj (list '_ (vector '_ (cons 0 1) '_) '_)))
    (set! (ref* obj 1 1 'cdr) 2)
    (= 2 (ref* obj 1 1 'cdr))))

;; register-getter-with-setter!, from the SRFI document
(define-record-type <point> (make-point x y) point? (x point-x) (y point-y))
(register-getter-with-setter!
 point?
 (getter-with-setter
  (lambda (p field) (case field ((x) (point-x p)) ((y) (point-y p))))
  (lambda (p field v) (error "points are immutable" p)))
 #f)
(test-equal "registered getter" 4 (ref (make-point 3 4) 'y))
(test-error "registered setter" #t (set! (ref (make-point 3 4) 'y) 5))
(test-equal "plain set!" 5 (let ((v 1)) (set! v 5) v))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-123")
  (exit (if (zero? failures) 0 1)))
