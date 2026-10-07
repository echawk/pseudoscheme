;;;; Common Lisp programming with Scheme's hygienic macros: a miniKanren
;;;; written in Scheme (tests/programs/lib/kanren/micro.sld), whose
;;;; syntax-rules macros -- fresh, conde, run* -- are Lisp macros here.
;;;; Relations are Lisp functions (DEFUN) whose bodies are those macros
;;;; over Lisp variables, Lisp functions and Lisp data.  The answers are
;;;; checked against cl-kanren, a miniKanren written in Common Lisp.
;;;;
;;;;   sbcl --dynamic-space-size 4GB --control-stack-size 500MB \
;;;;        --script tests/programs/kanren.lisp

(load (merge-pathnames "harness.lisp" (or *load-truename* *load-pathname*)))

(program-tests:quickload :cl-kanren)

(defpackage "KANREN-DEMO" (:use "COMMON-LISP" "PROGRAM-TESTS"))
(in-package "KANREN-DEMO")

;;; Only the surface: the macros, ==, and the two trivial goals.  The
;;; library's call/fresh, conj, disj and zzz, which the macros expand
;;; into, stay out of this package -- and the decoys below, Lisp
;;; functions of the same names, show that the expansions don't care.
(r7rs:import (only (kanren micro) == fresh conde run run* succeed fail))

(defun call/fresh (&rest args) (error "Lisp's CALL/FRESH, called with ~S" args))
(defun conj (&rest args) (error "Lisp's CONJ, called with ~S" args))
(defun disj (&rest args) (error "Lisp's DISJ, called with ~S" args))

;;; Relations, in Lisp.  Inside the macros, L, S and OUT are Lisp
;;; variables, CONS a Lisp function, APPENDO this very function, and
;;; '() Lisp's NIL.

(defun appendo (l s out)
  (conde ((== l '()) (== s out))
	 ((fresh (a d res)
	    (== (cons a d) l)
	    (== (cons a res) out)
	    (appendo d s res)))))

(defun membero (x l)
  (fresh (head tail)
    (== (cons head tail) l)
    (conde ((== x head))
	   ((membero x tail)))))

(defun reverso (l out)
  (conde ((== l '()) (== out '()))
	 ((fresh (a d rev-d)
	    (== (cons a d) l)
	    (reverso d rev-d)
	    (appendo rev-d (list a) out)))))

;; A relation over a table of Lisp data: the parents, as a list of
;; (parent child) lists.  The goal is built from the data by Lisp
;; (REDUCE over a list of goals), with a Scheme macro per fact.
(defparameter *parents*
  '((abe homer) (mona homer) (homer bart) (homer lisa) (homer maggie)
    (marge bart) (marge lisa) (marge maggie) (clancy marge) (jackie marge)
    (bart bart-jr)))

(defun parento (parent child)
  ;; any of the facts: G1 and G2 are Lisp variables holding Scheme goals
  (reduce (lambda (g1 g2) (conde (g1) (g2)))
	  (mapcar (lambda (fact) (== (list parent child) fact)) *parents*)))

(defun grandparento (g c)
  (fresh (p) (parento g p) (parento p c)))

(defun ancestoro (a d)
  (conde ((parento a d))
	 ((fresh (p) (parento a p) (ancestoro p d)))))

;;; Peano numerals: z, (s z), (s (s z)) ...
(defun peano (n) (if (zerop n) 'z (list 's (peano (1- n)))))
(defun unpeano (p) (if (eq p 'z) 0 (1+ (unpeano (second p)))))

(defun pluso (x y sum)
  (conde ((== x 'z) (== y sum))
	 ((fresh (x-1 sum-1)
	    (== x (list 's x-1))
	    (== sum (list 's sum-1))
	    (pluso x-1 y sum-1)))))

(defun timeso (x y product)
  (conde ((== x 'z) (== product 'z))
	 ((fresh (x-1 partial)
	    (== x (list 's x-1))
	    (timeso x-1 y partial)
	    (pluso y partial product)))))

;;; ------------------------------------------------------------------

(defun set-equal (a b) (and (subsetp a b :test #'equal) (subsetp b a :test #'equal)))

(defun fresh-name-p (x n)
  "Is X the reified fresh variable _.N?"
  (and (symbolp x) (string= (symbol-name x) (format nil "_.~D" n))))

(test-equal "the four ways to split (1 2 3)"
	    '((() (1 2 3)) ((1) (2 3)) ((1 2) (3)) ((1 2 3) ()))
	    (run* (q) (fresh (x y) (appendo x y '(1 2 3)) (== q (list x y))))
	    :test #'set-equal)

(test-equal "appendo forwards" '((a b c d))
	    (run* (q) (appendo '(a b) '(c d) q)))

(test-equal "membero enumerates" '(x y z) (run* (q) (membero q '(x y z))))

(test-equal "reverso, run backwards from its output" '((3 2 1))
	    (run 1 (q) (reverso q '(1 2 3))))

(test-assert "an infinite relation, taken five at a time"
	     (let ((answers (run 5 (q) (fresh (front) (appendo front '(end) q)))))
	       (and (= (length answers) 5)
		    (every (lambda (a) (eq (car (last a)) 'end)) answers)
		    (equal (mapcar #'length answers) '(1 2 3 4 5))
		    (fresh-name-p (first (second answers)) 0))))

;; Lisp variables inside the macros
(let ((target '(1 2 3 4))
      (prefix '(1 2)))
  (test-equal "Lisp lexical variables in a query" '((3 4))
	      (run* (q) (appendo prefix q target))))

;; s/c is the variable zzz's expansion binds; the user's s/c is another
(let ((s/c '(a lisp value)))
  (test-equal "hygiene: zzz's s/c doesn't capture the user's" '((a lisp value))
	      (run* (q) (== q s/c))))

;; fresh's own x and the user's x: the user's fresh variable named
;; like a Lisp variable outside shadows it, as lexical scope says
(let ((x 'outer))
  (test-equal "fresh binds a Scheme variable that shadows Lisp's" '((outer inner))
	      (run* (q) (fresh (y) (== y x) (fresh (x) (== x 'inner) (== q (list y x)))))))

(test-equal "the Lisp decoys CALL/FRESH, CONJ, DISJ are never called" '(ok)
	    (run* (q) (fresh (a b) (conde ((== a 1)) ((== a 2))) (== b a) (== q 'ok) (== b 2))))

(test-equal "family: Bart's grandparents" '(abe mona clancy jackie)
	    (run* (q) (grandparento q 'bart))
	    :test #'set-equal)

(test-equal "family: Abe's descendants" '(homer bart lisa maggie bart-jr)
	    (run* (q) (ancestoro 'abe q))
	    :test #'set-equal)

(test-equal "family: who is both a parent and a child?" '(homer marge bart)
	    (remove-duplicates (run* (q) (fresh (x y) (parento x q) (parento q y))))
	    :test #'set-equal)

(test-equal "arithmetic: 3 + 4" '(7)
	    (mapcar #'unpeano (run* (q) (pluso (peano 3) (peano 4) q))))

(test-equal "arithmetic, backwards: every x + y = 4"
	    '((0 4) (1 3) (2 2) (3 1) (4 0))
	    (mapcar (lambda (pair) (mapcar #'unpeano pair))
		    (run* (q) (fresh (x y) (pluso x y (peano 4)) (== q (list x y)))))
	    :test #'set-equal)

(test-equal "arithmetic, division: x * 3 = 12" '(4)
	    (mapcar #'unpeano (run 1 (q) (timeso q (peano 3) (peano 12)))))

;;; The same queries in cl-kanren, a miniKanren in Common Lisp

(test-equal "cl-kanren agrees: splitting a list"
	    (run* (q) (fresh (x y) (appendo x y '(a b c d)) (== q (list x y))))
	    (cl-kanren:run* (q)
	      (cl-kanren:fresh (x y)
		(cl-kanren:appendo x y '(a b c d))
		(cl-kanren:== q (list x y))))
	    :test #'set-equal)

(test-equal "cl-kanren agrees: membership"
	    (run* (q) (membero q '(1 2 3 4 5)))
	    (cl-kanren:run* (q) (cl-kanren:membero q '(1 2 3 4 5)))
	    :test #'set-equal)

;; Mixing the two isn't possible -- their goals and variables differ --
;; but their answers are plain Lisp data, so one can feed the other.
(test-equal "cl-kanren's answers as Scheme miniKanren's data" '((c d e) (d e) (e))
	    (let ((tails (cl-kanren:run* (q) (cl-kanren:fresh (x) (cl-kanren:appendo x q '(c d e))))))
	      (run* (q) (membero q tails) (fresh (a d) (== (cons a d) q))))
	    :test #'set-equal)

(finish)
