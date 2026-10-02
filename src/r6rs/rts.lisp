; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-R6RS -*-

;;;; R6RS primitives written in Common Lisp: the pieces of (rnrs base)
;;;; that differ from, or are missing in, the R7RS layer it is built on.

(in-package "PSEUDOSCHEME-R6RS")

(defvar *primitives* '())

(defmacro defprim (name lambda-list &body body)
  `(let ((cell (assoc ,name *primitives* :test #'string=))
	 (fn (lambda ,lambda-list ,@body)))
     (if cell (setf (cdr cell) fn)
	 (setq *primitives* (nconc *primitives* (list (cons ,name fn)))))))

(defun bool (x) (ps:true? x))

;;; ------------------------------------------------------------------
;;; Conditions (a minimal slice of the standard-libraries report's
;;; (rnrs conditions), enough for ERROR / ASSERT / ASSERTION-VIOLATION).
;;;
;;; A condition is the R7RS layer's ERROR-OBJECT, which already carries
;;; message and irritants, plus WHO and KIND (:ERROR or :ASSERTION).
;;; Real R6RS conditions are *compound*: a bag of simple condition
;;; types (&who, &message, &irritants, &violation, ...) discoverable by
;;; predicate and accessor.  That is TODO; see ROADMAP.md.

(defun new-condition (kind who message irritants)
  (r7rs::make-error-object message irritants who kind))

(defprim "error" (who message &rest irritants)
  ;; R6RS's ERROR takes WHO first (a string or symbol naming the
  ;; procedure, or #f), unlike R7RS's (message irritant ...).
  (r7rs::raise-object (new-condition :error who message irritants) nil))

(defprim "assertion-violation" (who message &rest irritants)
  (r7rs::raise-object (new-condition :assertion who message irritants) nil))

(defprim "syntax-violation" (who message form &optional subform)
  (r7rs::raise-object (new-condition :error who message
				      (if (eq subform nil) (list form) (list form subform)))
		      nil))

(defprim "condition?" (x) (bool (r7rs::error-object-p x)))
(defprim "error?" (x) (bool (r7rs::error-object-p x)))
(defprim "assertion-violation?" (x)
  (bool (and (r7rs::error-object-p x) (eq (r7rs::error-object-kind x) :assertion))))
(defprim "message-condition?" (x) (bool (r7rs::error-object-p x)))
(defprim "irritants-condition?" (x) (bool (r7rs::error-object-p x)))
(defprim "who-condition?" (x) (bool (and (r7rs::error-object-p x) (r7rs::error-object-who x))))
(defprim "condition-message" (c) (r7rs::error-object-message c))
(defprim "condition-irritants" (c) (r7rs::error-object-irritants c))
(defprim "condition-who" (c) (or (r7rs::error-object-who c) ps:false))

;;; ------------------------------------------------------------------
;;; Numbers (11.7.4.1): div/mod and div0/mod0.  div and mod are floor
;;; division for a positive divisor and ceiling division for a negative
;;; one, so that 0 <= mod < |x2|; div0 and mod0 center the remainder:
;;; -|x2|/2 <= mod0 < |x2|/2.  The report's table is
;;;   123 div0 10 = 12   123 div0 -10 = -12   -123 div0 10 = -12   -123 div0 -10 = 12

(defun check-real (who x)
  (unless (realp x) (ps:scheme-error "~A: not a real number: ~S" who x)))

(defun div-of (x y)
  (if (> y 0) (floor x y) (ceiling x y)))

(defun div0-of (x y)
  (let ((q (/ x y)))
    (if (> y 0) (floor (+ q 1/2)) (ceiling (- q 1/2)))))

(defmacro def-division (name-div name-mod name-both fn)
  `(progn
     (defprim ,name-div (x y)
       (check-real ,name-div x) (check-real ,name-div y)
       (when (zerop y) (ps:scheme-error "~A: division by zero" ,name-div))
       (values (,fn x y)))
     (defprim ,name-mod (x y)
       (check-real ,name-mod x) (check-real ,name-mod y)
       (when (zerop y) (ps:scheme-error "~A: division by zero" ,name-mod))
       (- x (* y (,fn x y))))
     (defprim ,name-both (x y)
       (check-real ,name-both x) (check-real ,name-both y)
       (when (zerop y) (ps:scheme-error "~A: division by zero" ,name-both))
       (let ((q (,fn x y))) (values q (- x (* y q)))))))

(def-division "div" "mod" "div-and-mod" div-of)
(def-division "div0" "mod0" "div0-and-mod0" div0-of)

(defprim "real-valued?" (x) (bool (realp x)))
(defprim "rational-valued?" (x) (bool (rationalp x)))
(defprim "integer-valued?" (x) (bool (and (realp x) (= x (round x)))))
