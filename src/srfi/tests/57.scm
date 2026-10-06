;;; Tests for SRFI 57, from the examples and tests in the SRFI document
;;; (reference/srfi-57/examples.scm).
;;;
;;; The record definitions come first, at top level as the SRFI
;;; requires, and the tests are in procedures: with full continuations,
;;; Pseudoscheme compiles many top-level calls to non-primitive
;;; procedures slowly (see 57.sld), and calls inside procedure bodies
;;; don't have that cost.
(import (except (scheme base) define-record-type) (scheme process-context)
        (srfi 64) (srfi 57))

;;; Definitions.

;; A simple record declaration.
(define-record-type point0 (make-point0 x y) point0?
  (x point0.x point0.x-set!)
  (y point0.y point0.y-set!))

;; Record schemes, and concrete types conforming to them.
(define-record-scheme <point #f <point?
  (x <point.x)
  (y <point.y))
(define-record-scheme <color #f <color?
  (hue <color.hue))
(define-record-type (point <point) make-point point?
  (x point.x)
  (y point.y))
(define-record-type (color <color) make-color)
(define-record-type (color-point <color <point) (make-color-point x y hue)
  color-point?
  (extra color-point.extra))

;; Small module-functor example.
(define-record-type monoid #f #f
  (mult monoid.mult)
  (one  monoid.one))
(define-record-type abelian-group #f #f
  (add  group.add)
  (zero group.zero)
  (sub  group.sub))
(define-record-type ring #f #f
  (mult ring.mult)
  (one  ring.one)
  (add  ring.add)
  (zero ring.zero)
  (sub  ring.sub))
(define (make-ring g m)
  (record-compose (monoid m) (abelian-group g) (ring)))

;; Tree data type.
(define-record-scheme <tree #f <tree?)
(define-record-type (node <tree) make-node node?
  (lhs node.lhs)
  (rhs node.rhs))
(define-record-type (leaf <tree) make-leaf leaf?
  (val leaf.val))
(define (tree->list t)
  (cond
    ((leaf? t) (leaf.val t))
    ((node? t) (cons (tree->list (node.lhs t))
                     (tree->list (node.rhs t))))))

;; Optional elements, and punning.
(define-record-type monday)
(define-record-type tuesday #f tuesday?)
(define-record-type node2 make-node2 #f
  (left left)
  (right right))

;; Two schemes with different accessors for the same field.
(define-record-scheme foo #f #f (x foo-x))
(define-record-scheme bar #f #f (x bar-x))
(define-record-type (foo-bar foo bar) make-foo-bar)

;;; Tests.

(define (simple-tests)
  (let ((p0 (make-point0 1 2)))
    (test-assert "predicate" (point0? p0))
    (test-assert "predicate, other value" (not (point0? 5)))
    (test-equal "accessor" 2 (point0.y p0))
    (point0.y-set! p0 7)
    (test-equal "modifier" 7 (point0.y p0))))

(define (scheme-tests)
  (let ((cp (make-color-point 1 2 'blue)))
    (test-assert "scheme predicate <point?" (<point? cp))
    (test-assert "scheme predicate <color?" (<color? cp))
    (test-assert "type predicate" (color-point? cp))
    (test-assert "point? on a color-point" (not (point? cp)))
    (test-equal "polymorphic accessor <point.y" 2 (<point.y cp))
    (test-equal "polymorphic accessor <color.hue" 'blue (<color.hue cp))
    (test-equal "undefined field" '<undefined> (color-point.extra cp))
    (test-equal "default constructor order" '(3 4)
      (let ((p (make-point 3 4))) (list (point.x p) (<point.y p))))
    (test-error "monomorphic accessor on another type" #t (point.x cp))
    (test-error "scheme accessor on a non-conforming value" #t
      (<point.x (make-color 'red)))))

(define (label-tests)
  (let ((p (point (x 1) (y 2)))
        (cp (color-point (hue 'blue) (x 1) (y 2))))
    (test-equal "labeled construction" '(1 2) (list (point.x p) (point.y p)))
    (test-equal "labeled construction, scheme fields" '(blue 1 2 <undefined>)
      (list (<color.hue cp) (<point.x cp) (<point.y cp)
            (color-point.extra cp)))

    ;; Monomorphic functional update.
    (let ((p2 (record-update p point (x 7))))
      (test-equal "record-update result" '(7 2) (list (point.x p2) (point.y p2)))
      (test-equal "record-update original" '(1 2) (list (point.x p) (point.y p))))

    ;; Polymorphic functional update.
    (let ((cp3 (record-update cp <point (x 7))))
      (test-assert "polymorphic update keeps type" (color-point? cp3))
      (test-equal "polymorphic update result" '(blue 7 2)
        (list (<color.hue cp3) (<point.x cp3) (<point.y cp3)))
      (test-equal "polymorphic update original" 1 (<point.x cp)))

    ;; In-place update.
    (let ((r (record-update! cp <point (x 7))))
      (test-eq "record-update! returns the record" cp r)
      (test-equal "record-update! mutates" 7 (<point.x cp)))
    (record-update! p point (y 9))
    (test-equal "monomorphic record-update!" 9 (point.y p))

    ;; record-compose: polymorphic in argument, monomorphic in result.
    (let ((r (record-compose (<point cp) (point (x 8)))))
      (test-assert "compose result type" (point? r))
      (test-equal "compose fields" '(8 2) (list (point.x r) (point.y r))))))

(define (compose-tests)
  (let* ((cp (make-color-point 1 2 'green))
         (c (make-color 'blue))
         (r (record-compose (<point cp) (color c)
                            (color-point (x 8) (extra 'hi)))))
    (test-equal "general compose" '(hi blue 8 2)
      (list (color-point.extra r) (<color.hue r) (<point.x r) (<point.y r))))
  (let* ((integer-monoid (monoid (mult *) (one 1)))
         (integer-group (abelian-group (add +) (zero 0) (sub -)))
         (integer-ring (make-ring integer-group integer-monoid)))
    (test-equal "functor example" 3 ((ring.add integer-ring) 1 2))
    (test-equal "functor example, one" 1 (ring.one integer-ring))
    (test-equal "functor example, sub" 5 ((ring.sub integer-ring) 7 2))))

(define (misc-tests)
  (let ((t (make-node (make-node (make-leaf 1) (make-leaf 2))
                      (make-leaf 3))))
    (test-assert "tree scheme predicate" (<tree? t))
    (test-assert "tree scheme predicate, other" (not (<tree? 'x)))
    (test-equal "tree->list" '((1 . 2) . 3) (tree->list t)))
  (test-assert "predicate without constructor" (not (tuesday? 1)))
  (test-equal "punned accessors, default constructor" '(a b)
    (let ((n (make-node2 'a 'b))) (list (left n) (right n))))
  (test-equal "shared field through two schemes" '(5 5)
    (let ((fb (make-foo-bar 5))) (list (foo-x fb) (bar-x fb)))))

(test-begin "srfi-57")
(simple-tests)
(scheme-tests)
(label-tests)
(compose-tests)
(misc-tests)
(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-57")
  (exit (if (zero? failures) 0 1)))
