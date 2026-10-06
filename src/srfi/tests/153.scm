;;; SRFI 153 tests: the SRFI's oset-tests.scm (Marc Nieper-Wißkirchen,
;;; John Cowan; MIT licence), with (chibi test) replaced by SRFI 64, and
;;; the second definition of oset6l made a set! (a program cannot define
;;; a name twice; chibi let the second win), and oset4's unfold made to
;;; stop at 6, so that it holds 500 as its comment and the tests say.
;;; Five mapping tests compared osets with different comparators, which
;;; oset=? (mapping=?) treats as unequal, or built an oset on the
;;; unordered eq-comparator; they now use an ordered symbol comparator
;;; and compare the element lists.
(import (scheme base) (scheme char) (scheme write) (scheme process-context)
        (srfi 64) (srfi 128) (srfi 153))

;; (chibi test) on SRFI 64: test, test-not, and a settable
;; current-test-comparator.  (test-assert, test-error and test-group
;; are SRFI 64's own.)
(define %test-comparator equal?)
(define (current-test-comparator . new)
  (if (null? new) %test-comparator (set! %test-comparator (car new))))
(define-syntax test
  (syntax-rules ()
    ((_ name expected expr)
     (test-assert name (%test-comparator expected expr)))
    ((_ expected expr)
     (test-assert (%test-comparator expected expr)))))
(define-syntax test-not
  (syntax-rules ()
    ((_ name expr) (test-assert name (not expr)))
    ((_ expr) (test-assert (not expr)))))
;; chibi's (test-equal equal expected expr), renamed: SRFI 64's
;; test-equal takes a name, not an equality.
(define-syntax chibi-test-equal
  (syntax-rules ()
    ((_ equal expected expr) (test-assert (equal expected expr)))))

(test-begin "srfi-153")
(define (oset-print o)
  (write (oset->list o))
  (newline))

(define default-comparator (make-default-comparator))

(define number-comparator
  (make-comparator number? = < #f))

(define string-ci-comparator
  (make-comparator string? string-ci=? string-ci<? #f))

(define string-comparator
  (make-comparator string? string=? string<? #f))

(define eq-comparator (make-eq-comparator))

;; An ordered comparator for symbols (eq-comparator has no ordering,
;; which a mapping needs).
(define symbol-comparator
  (make-comparator symbol? eq?
                   (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
                   #f))

(define vlist '())

(define (failure) 'fail)

(define default-test-equal? (current-test-comparator))

(define (oset-equal? x y)
  (cond
    ((and (oset? x) (oset? y)) (oset=? x y))
    ((oset? x) #f)
    ((oset? y) #f)
    (else (default-test-equal? x y))))

(current-test-comparator oset-equal?)

; oset0 = {}
(define oset0 (oset number-comparator))

; oset1 = oset2 = {1, 2, 3, 4, 5} both settable
(define oset1 (oset number-comparator 5 4 3 2 1))
(define oset2 (oset/ordered number-comparator 1 2 3 4 5))

; oset3 = oset4 = {100, 200, 300, 400, 500}
; oset3 settable, oset4 not settable
(define oset3 (oset-unfold
                (lambda (x) (< x 1))
                (lambda (x) (* x 100))
                (lambda (x) (- x 1))
                5 number-comparator))


(define oset4 (oset-unfold/ordered
                (lambda (x) (= x 6))  ; was 5, which leaves out 500
                (lambda (x) (* x 100))
                (lambda (x) (+ x 1))
                1 number-comparator))

; oset5 = {"a", "b", "c", "d", "e"} case insensitive, settable
; oset5a = {"A", "b", "c", "d", "e"} case insensitive, settable
; oset5z = {"A", "b", "c", "d", "e", "Z"} case insensitive, settable
(define oset5 (oset string-ci-comparator "a" "b" "c" "d" "e"))
(define oset5a (oset string-ci-comparator "A" "b" "c" "d" "e"))
(define oset5z (oset string-ci-comparator "A" "b" "c" "d" "e" "Z"))

; oset6 = {1, 2, 3, 4, 5} not settable
; oset6s = {1, 2, 3, 4} not settable
; oset6l = {1, 2, 3, 4, 5, 6} not settable
; oset6e = {2, 4} not settable
; oset6o = {1, 3, 5} not settable
; oset6l = {1, 2} not settable
; oset6m = {3} not settable
; oset6h = {4, 5} not settable
; oset6lm = {1, 2, 3} not settable
; oset6mh = {3, 4, 5} not settable
(define oset6 (oset number-comparator 1 2 3 4 5))
(define oset6s (oset number-comparator 1 2 3 4))
(define oset6l (oset number-comparator 1 2 3 4 5 6))
(define oset6e (oset number-comparator 2 4))
(define oset6o (oset number-comparator 1 3 5))
(set! oset6l (oset number-comparator 1 2))  ; was a second define
(define oset6m (oset number-comparator 3))
(define oset6h (oset number-comparator 4 5))
(define oset6lm (oset number-comparator 1 2 3))
(define oset6mh (oset number-comparator 3 4 5))

; oset7 = {"a", "b", "c", "d", "e"} not settable
(define oset7 (oset string-comparator "a" "b" "c" "d" "e"))

; oset8 = {"a", "b", "c", "d", "e", 1, 2, 3, 4, 5}
(define oset8 (oset default-comparator "a" "b" "c" "d" "e" 1 2 3 4 5))

; oset9 = {1, 2}
(define oset9 (oset number-comparator 1 2))

;; Constructors

(test-group "osets"
(test-group "oset/constructors"
(test-assert (oset-contains? oset1 1))
(test-assert (oset-contains? oset1 2))
(test-assert (oset-contains? oset1 3))
(test-assert (oset-contains? oset1 4))
(test-assert (oset-contains? oset1 5))
(test 5 (oset-size oset1))

(test-assert (oset-contains? oset2 1))
(test-assert (oset-contains? oset2 2))
(test-assert (oset-contains? oset2 3))
(test-assert (oset-contains? oset2 4))
(test-assert (oset-contains? oset2 5))
(test 5 (oset-size oset2))

(test oset3 (oset number-comparator 100 200 300 400 500))
(test oset4 (oset number-comparator 100 200 300 400 500))
(test-assert (oset-contains? oset5 "a"))
(test-assert (oset-contains? oset5 "A"))
(test 10 (oset-size oset8))

(test oset3 (oset-accumulate
             (lambda (terminate i)
               (if (> i 500)
                   (terminate)
                   (values i (+ i 100))))
             number-comparator
             100))

(test-assert
  (let-values (((set last)
                (oset-accumulate
                 (lambda (terminate i)
                   (if (< i 1)
                       (terminate i)
                       (values (* i 100) (- i 1))))
                 number-comparator
                 5)))
    (and (oset=? set oset4) (zero? last))))
)

;; Predicates

(test-group "osets/predicates"

(test-assert (oset? oset1))
(test-assert (oset? oset2))
(test-assert (oset? oset3))
(test-assert (oset? oset4))
(test-assert (oset? oset5))

(test-assert (oset-contains? oset1 3))
(test-not (oset-contains? oset1 10))

(test-assert (oset-empty? oset0))
(test-not (oset-empty? oset1))

(test-not (oset-disjoint? oset1 oset2))
(test-not (oset-disjoint? oset1 oset2))
)

;; Accessors

(test-group "osets/accessors"

(test 1 (oset-member oset1 1 (failure)))
(test 'fail (oset-member oset1 100 (failure)))
(chibi-test-equal string-ci=? "a" (oset-member oset5 "A" (failure)))
(test 'fail (oset-member oset5 "z" (failure)))

(chibi-test-equal eq? number-comparator (oset-element-comparator oset4))
)

;; Updaters

(test-group "osets/updaters"

(test (oset number-comparator 1 2 3 4 5 6 7) (oset-adjoin oset1 6 7))
; oset2 = {1, 2, 3, 4, 5, 6, 7}
(set! oset2 (oset-adjoin oset2 6 7))
(test oset2 (oset number-comparator 1 2 3 4 5 6 7))
(test oset5 (oset-adjoin oset5 "A"))
(test oset5a (oset-adjoin/replace oset5 "A"))
(test oset5z (oset-adjoin/replace oset5 "A" "Z"))

; oset5 = {"A", "b", "c", "d", "e"}
(test oset5 (oset string-ci-comparator "A" "b" "c" "d" "e"))

(test (oset number-comparator 1 2 3) (oset-delete oset1 4 5))
(test (oset number-comparator 1 2 3) (oset-delete-all oset1 (list 4 5)))
; oset1 = {1, 2, 3, 4}
(set! oset1 (oset-delete oset1 5))
(test oset1 (oset number-comparator 1 2 3 4))
; oset1 = {1, 2, 3}
(set! oset1 (oset-delete-all oset1 (list 4)))
(test oset1 (oset number-comparator 1 2 3))

(test 'fail (oset-pop oset0 failure))
(test 'fail (oset-pop/reverse oset0 failure))

(test-assert
  (let-values (((o x) (oset-pop oset2 failure)))
    (and (oset=? o (oset number-comparator 2 3 4 5 6 7))
         (= x 1))))

(test-assert
  (let-values (((o x) (oset-pop/reverse oset2 failure)))
    (and (oset=? o (oset number-comparator 1 2 3 4 5 6))
         (= x 7))))

(set! vlist (call-with-values (lambda () (oset-pop oset2 failure)) list))
; oset2 = {2, 3, 4, 5, 6, 7}
(set! oset2 (car vlist))
(test oset2 (oset number-comparator 2 3 4 5 6 7))
(test 1 (cadr vlist))

)

;; The whole oset
(test-group "oset/whole"

(test 5 (oset-size oset6))
(test 1 (oset-find odd? oset6 failure))
(test 'fail (oset-find zero? oset6 failure))

(test 2 (oset-count even? oset6))
(test 0 (oset-count zero? oset6))

(test-assert (oset-any? odd? (oset number-comparator 1 2 4 6 8)))
(test-assert (oset-every? even? (oset number-comparator 2 4 6 8)))
)


;; Mapping and folding

(test-group "osets/mapping"
(test oset7 (oset-map symbol->string string-comparator (oset symbol-comparator 'a 'b 'c 'd 'e)))
(test oset7 (oset-map/monotone symbol->string string-comparator (oset symbol-comparator 'a 'b 'c 'd 'e)))

(test '(5 4 3 2 1)
      (let ((r '()))
        (oset-for-each
          (lambda (i) (set! r (cons i r)))
          oset6)
	r))

(test 15 (oset-fold + 0 oset6))
(test 1 (oset-fold - 0 oset9))
(test -1 (oset-fold/reverse - 0 oset9))
(test "edcba" (oset-fold string-append "" oset7))
(test '(1 2 3 4 5) (oset->list (oset-filter number? oset8)))
(test '("a" "b" "c" "d" "e") (oset->list (oset-remove number? oset8)))
(set! vlist
  (call-with-values
    (lambda () (oset-partition number? oset8))
    list))

(test-assert
  (and (equal? (oset->list (car vlist)) '(1 2 3 4 5))
       (equal? (oset->list (cadr vlist)) '("a" "b" "c" "d" "e")))))

(test-group "osets/conversions"
(test '(100 200 300 400 500) (oset->list oset3))
(test oset3 (list->oset number-comparator '(100 200 300 400 500)))
(test oset3 (list->oset/ordered number-comparator '(100 200 300 400 500))))

(test-group "osets/subsets"
(test-assert (oset=? oset6 oset6))
(test-assert (oset<? oset6s oset6))
(test-assert (oset>? oset6 oset6l))
(test-assert (oset<=? oset6s oset6))
(test-assert (oset<=? oset6s oset6s))
(test-assert (oset>=? oset6 oset6l))
(test-assert (oset>=? oset6l oset6l)))

(test-group "osets/setops"
(test oset6 (oset-union oset6e oset6o))
(test oset0 (oset-intersection oset6e oset6o))
(test oset6e (oset-difference oset6 oset6o))
(test (oset number-comparator 3 4 5 6)
  (oset-xor (oset number-comparator 1 2 3 4)
              (oset number-comparator 1 2 5 6))))

(test-group "osets/single"
(test 1 (oset-min-element oset6))
(test 5 (oset-max-element oset6))
(test 1 (oset-element-predecessor oset6 2 failure))
(test 'fail (oset-element-predecessor oset6 1 failure))
(test 5 (oset-element-successor oset6 4 failure))
(test 'fail (oset-element-successor oset6 5 failure)))

(test-group "osets/divide"
(test oset6l (oset-range< oset6 3))
(test oset6m (oset-range= oset6 3))
(test oset6h (oset-range> oset6 3))
(test oset6lm (oset-range<= oset6 3))
(test oset6mh (oset-range>= oset6 3)))
)

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-153")
  (exit (if (zero? failures) 0 1)))
