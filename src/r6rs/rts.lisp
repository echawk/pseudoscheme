; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-R6RS -*-

;;;; R6RS primitives written in Common Lisp.
;;;;
;;;; psyntax (src/psyntax.lisp) supplies every R6RS library's *namespace*
;;;; and all of its syntax; each non-syntax export is a reference to a
;;;; host global of the same name.  The host starts with the R7RS layer's
;;;; bindings; the DEFPRIMs here and in the other files of this directory
;;;; add or override what R6RS needs, following the R6RS libraries report
;;;; (rnrs-pdfs/errata-corrected-r6rs-lib.pdf).

(defpackage "PSEUDOSCHEME-R6RS"
  (:nicknames "PS-R6RS")
  (:use "COMMON-LISP")
  (:export "*PRIMITIVES*" "DEFPRIM"))

(in-package "PSEUDOSCHEME-R6RS")

(defvar *primitives* '())

(defmacro defprim (name lambda-list &body body)
  `(let ((cell (assoc ,name *primitives* :test #'string=))
	 (fn (lambda ,lambda-list ,@body)))
     (if cell (setf (cdr cell) fn)
	 (setq *primitives* (nconc *primitives* (list (cons ,name fn)))))))

(defun bool (x) (ps:true? x))

;;; ------------------------------------------------------------------
;;; Numbers (11.7.4.1): div/mod and div0/mod0.  div and mod are floor
;;; division for a positive divisor and ceiling division for a negative
;;; one, so that 0 <= mod < |x2|; div0 and mod0 center the remainder:
;;; -|x2|/2 <= mod0 < |x2|/2.  The report's table is
;;;   123 div0 10 = 12   123 div0 -10 = -12   -123 div0 10 = -12   -123 div0 -10 = 12

(defun check-real (who x)
  (unless (realp x) (ps:scheme-error "~A: not a real number: ~S" who x)))

(defun float-quotient (x y)
  "x/y for a flonum argument, IEEE-style: a NaN or infinity when the
quotient isn't finite, else NIL to say \"go on\"."
  (let ((q (ps:call-with-ieee-arithmetic (lambda () (/ (float x 1d0) (float y 1d0))))))
    (values (and (or (ps:nan-p q) (ps:infinite-p q)) q) q)))

(defun div-of (x y)
  (if (or (floatp x) (floatp y))
      (multiple-value-bind (special q) (float-quotient x y)
	(or special (if (> y 0) (ffloor q) (fceiling q))))
      (values (if (> y 0) (floor x y) (ceiling x y)))))

(defun div0-of (x y)
  (if (or (floatp x) (floatp y))
      (multiple-value-bind (special q) (float-quotient x y)
	(or special (if (> y 0) (ffloor (+ q 0.5d0)) (fceiling (- q 0.5d0)))))
      (let ((q (/ x y)))
	(values (if (> y 0) (floor (+ q 1/2)) (ceiling (- q 1/2)))))))

(defmacro def-division (name-div name-mod name-both fn)
  `(progn
     (defprim ,name-div (x y)
       (check-real ,name-div x) (check-real ,name-div y)
       (when (zerop y) (ps:scheme-error "~A: division by zero" ,name-div))
       (values (,fn x y)))
     (defprim ,name-mod (x y)
       (check-real ,name-mod x) (check-real ,name-mod y)
       (when (zerop y) (ps:scheme-error "~A: division by zero" ,name-mod))
       (ps:call-with-ieee-arithmetic (lambda () (- x (* y (,fn x y))))))
     (defprim ,name-both (x y)
       (check-real ,name-both x) (check-real ,name-both y)
       (when (zerop y) (ps:scheme-error "~A: division by zero" ,name-both))
       (let ((q (,fn x y)))
	 (values q (ps:call-with-ieee-arithmetic (lambda () (- x (* y q)))))))))

(def-division "div" "mod" "div-and-mod" div-of)
(def-division "div0" "mod0" "div0-and-mod0" div0-of)

(defprim "real-valued?" (x) (bool (realp x)))
(defprim "rational-valued?" (x) (bool (rationalp x)))
(defprim "integer-valued?" (x) (bool (and (realp x) (= x (round x)))))
