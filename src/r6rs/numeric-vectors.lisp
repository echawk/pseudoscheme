; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-R6RS -*-

;;;; Homogeneous numeric vectors: SRFI 4, and the base of SRFI 160.  Each
;;;; is a CL specialized vector (see PS:NUMERIC-VECTOR-TYPES): a u8vector
;;;; is a bytevector, an s16vector a (simple-array (signed-byte 16) (*)),
;;;; an f32vector a (simple-array single-float (*)) whose elements read
;;;; as double floats, Scheme's inexact reals.  For each TAG (u8, s8,
;;;; u16, ... f64, c64, c128) the host has TAGvector?, make-TAGvector,
;;;; TAGvector, TAGvector-length, TAGvector-ref, TAGvector-set!,
;;;; TAGvector->list and list->TAGvector; src/srfi/4.sld exports them.

(in-package "PSEUDOSCHEME-R6RS")

(defun numeric-vector-scheme-value (x)
  (typecase x
    (single-float (coerce x 'double-float))
    ((complex single-float) (coerce x '(complex double-float)))
    (t x)))

(defun numeric-vector-index (who v i)
  (unless (and (integerp i) (< -1 i (length v)))
    (r6rs-assertion-violation who "index out of range" i))
  i)

(defun numeric-vector-range (who v start end)
  (let ((n (length v)))
    (unless (and (integerp start) (<= 0 start n))
      (r6rs-assertion-violation who "bad start index" start))
    (let ((end (if (or (null end) (eq end ps:false)) n end)))
      (unless (and (integerp end) (<= start end n))
	(r6rs-assertion-violation who "bad end index" end))
      end)))

(defun numeric-vector-names ()
  "The host primitives this file defines, for (pseudoscheme host)."
  (loop for (tag) in ps:numeric-vector-types
	append (mapcar (lambda (f) (format nil f tag))
		       '("~Avector?" "make-~Avector" "~Avector" "~Avector-length"
			 "~Avector-ref" "~Avector-set!" "~Avector->list" "list->~Avector"))))

(defun define-numeric-vector-primitives (tag type)
  (let ((name (lambda (f) (format nil f tag))))
    (flet ((check (who v)
	     (unless (equal (ps:numeric-vector-tag v) tag)
	       (r6rs-assertion-violation who (format nil "not a ~Avector" tag) v))
	     v)
	   (element (who x)
	     (or (ps:numeric-vector-element type x)
		 (r6rs-assertion-violation who (format nil "not a valid ~Avector element" tag) x))))
      (let ((pred (funcall name "~Avector?"))
	    (make (funcall name "make-~Avector"))
	    (cons (funcall name "~Avector"))
	    (len (funcall name "~Avector-length"))
	    (ref (funcall name "~Avector-ref"))
	    (set (funcall name "~Avector-set!"))
	    (to-list (funcall name "~Avector->list"))
	    (from-list (funcall name "list->~Avector")))
	(defprim pred (x) (bool (equal (ps:numeric-vector-tag x) tag)))
	(defprim make (k &optional (fill 0))
	  (unless (and (integerp k) (>= k 0))
	    (r6rs-assertion-violation make "bad length" k))
	  (make-array k :element-type type :initial-element (element make fill)))
	(defprim cons (&rest xs)
	  (make-array (length xs) :element-type type
				  :initial-contents (mapcar (lambda (x) (element cons x)) xs)))
	(defprim len (v) (length (check len v)))
	(defprim ref (v i)
	  (check ref v)
	  (numeric-vector-scheme-value (aref v (numeric-vector-index ref v i))))
	(defprim set (v i x)
	  (check set v)
	  (setf (aref v (numeric-vector-index set v i)) (element set x))
	  ps:unspecific)
	(defprim to-list (v &optional (start 0) end)
	  (check to-list v)
	  (let ((end (numeric-vector-range to-list v start end)))
	    (loop for i from start below end
		  collect (numeric-vector-scheme-value (aref v i)))))
	(defprim from-list (list)
	  (make-array (length list) :element-type type
				    :initial-contents (mapcar (lambda (x) (element from-list x)) list)))))))

(loop for (tag . type) in ps:numeric-vector-types
      do (define-numeric-vector-primitives tag type))
