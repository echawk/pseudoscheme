; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-RACKET -*-

;;;; Racket's equal? and equal-hash-code, natively, and the hash tables'
;;;; equal?-based test (hash.lisp's RACKET-EQUAL).
;;;;
;;;; equal? is eqv?, or recursive on pairs, mutable pairs, vectors,
;;;; strings, byte strings, boxes, hash tables and paths; a struct is
;;;; compared by its type's prop:equal+hash, else field by field when its
;;;; type is transparent or prefab, else by identity.  The hash agrees:
;;;; prop:equal+hash's hash procedure, or the fields of a transparent
;;;; struct.  Not yet: cycles (an equal? of cyclic data doesn't end, as
;;;; Racket's does); a chaperoned box or hash table is compared through
;;;; its chaperone, a chaperoned struct by its fields underneath.

(in-package "QUASISCHEME-RACKET")

(defvar *prop-equal+hash* (%make-struct-type-property (rsym "prop:equal+hash") nil '()))
(define-primitive "#%kernel" "prop:equal+hash" *prop-equal+hash*)

(defun equal+hash-procedures (x)
  "X's prop:equal+hash: its equal procedure and hash procedure, or NIL."
  (multiple-value-bind (v found) (property-value x *prop-equal+hash*)
    (when found
      (cond ((consp v) (values (first v) (second v)))
	    ((simple-vector-p v) (values (svref v 0) (svref v 1)))
	    (t nil)))))

(defun equal* (a b)
  (cond ((eq a b) t)
	;; symbols, fixnums and characters are equal? only when eq?/eqv?:
	;; the common keys, answered before the other types are tried
	((or (symbolp a) (symbolp b)) nil)
	((typep a 'fixnum) (eql a b))
	((consp a) (and (consp b) (equal* (car a) (car b)) (equal* (cdr a) (cdr b))))
	((numberp a) (racket-eqv a b))
	((characterp a) (eql a b))
	((stringp a) (and (stringp b) (string= a b)))
	((typep a '(simple-array (unsigned-byte 8) (*)))
	 (and (typep b '(simple-array (unsigned-byte 8) (*))) (equalp a b)))
	((simple-vector-p a)
	 (and (simple-vector-p b) (= (length a) (length b)) (every #'equal* a b)))
	((rbox-p a) (and (rbox-p b) (equal* (box-ref* a) (box-ref* b))))
	((mpair-p a) (and (mpair-p b) (equal* (mpair-car a) (mpair-car b)) (equal* (mpair-cdr a) (mpair-cdr b))))
	((rpath-p a) (and (rpath-p b) (string= (rpath-string a) (rpath-string b))))
	((hash-table-p* a) (hash-equal a b))
	;; fxvectors and flvectors (more.lisp): flonums as eqv?
	((typep a '(simple-array double-float (*)))
	 (and (typep b '(simple-array double-float (*))) (= (length a) (length b))
	      (every #'racket-eqv a b)))
	((typep a 'fxvector)
	 (and (typep b 'fxvector) (equalp (fxvector-items a) (fxvector-items b))))
	;; a stencil vector (data.lisp)
	((typep a 'stencil)
	 (and (typep b 'stencil) (= (stencil-mask a) (stencil-mask b))
	      (every #'equal* (stencil-items a) (stencil-items b))))
	((struct-instance-p a)
	 (and (struct-instance-p b)
	      (eq (struct-of-type a) (struct-of-type b))
	      (let ((eqp (equal+hash-procedures a)))
		(cond (eqp (truthy (funcall eqp a b (lambda (x y) (bool (equal* x y))))))
		      ((transparent-p (struct-of-type a))
		       (let ((n (struct-field-count a))) (and (= n (struct-field-count b)) (loop for i below n always (equal* (struct-field a i) (struct-field b i))))))
		      (t nil)))))
	(t nil)))

(defun hash-equal (a b)
  (and (hash-table-p* b)
       (eq (hash-kind a) (hash-kind b))
       (eq (hash-immutable-p a) (hash-immutable-p b))
       (= (length (entries a)) (length (entries b)))
       (loop for (k . v) across (entries a)
	     always (multiple-value-bind (w found) (lookup* b k)
		      (and found (equal* v w))))))

(defun equal-hash* (x &optional (depth 0))
  (cond ((> depth 16) 0)
	((consp x) (mix (equal-hash* (car x) (1+ depth)) (equal-hash* (cdr x) (1+ depth))))
	((stringp x) (logand (sxhash (coerce x 'simple-string)) most-positive-fixnum))
	((simple-vector-p x)
	 (let ((h (length x)))
	   (loop for e across x for i below 8 do (setq h (mix h (equal-hash* e (1+ depth)))))
	   h))
	((typep x '(simple-array (unsigned-byte 8) (*)))
	 (let ((h (length x)))
	   (loop for b across x for i below 64 do (setq h (mix h b)))
	   h))
	((rbox-p x) (mix 7 (equal-hash* (box-ref* x) (1+ depth))))
	((typep x 'stencil)
	 (let ((h (mix 17 (stencil-mask x))))
	   (loop for e across (stencil-items x) for i below 8 do (setq h (mix h (equal-hash* e (1+ depth)))))
	   h))
	((typep x 'fxvector)
	 (let ((h (mix 19 (length (fxvector-items x)))))
	   (loop for e across (fxvector-items x) for i below 8 do (setq h (mix h (eqv-hash e))))
	   h))
	((typep x '(simple-array double-float (*)))
	 (let ((h (mix 23 (length x))))
	   (loop for e across x for i below 8 do (setq h (mix h (eqv-hash e))))
	   h))
	((mpair-p x) (mix (equal-hash* (mpair-car x) (1+ depth)) (equal-hash* (mpair-cdr x) (1+ depth))))
	((rpath-p x) (mix 11 (logand (sxhash (rpath-string x)) most-positive-fixnum)))
	((hash-table-p* x) (mix 13 (length (entries x))))
	((struct-instance-p x)
	 (multiple-value-bind (eqp hashp) (equal+hash-procedures x)
	   (cond (eqp (logand (funcall hashp x (lambda (y) (equal-hash* y (1+ depth)))) most-positive-fixnum))
		 ((transparent-p (struct-of-type x))
		  (let ((h (eq-hash (struct-of-type x))))
		    (loop for i below (min 8 (struct-field-count x))
			  do (setq h (mix h (equal-hash* (struct-field x i) (1+ depth)))))
		    h))
		 (t (eq-hash x)))))
	(t (eqv-hash x))))

(defnative "equal?" (a b) () (bool (equal* a b)))
(defnative "equal-always?" (a b) () (bool (equal* a b)))
(defnative "equal?/recur" (a b recur) ()
  (declare (ignore recur))
  (bool (equal* a b)))
