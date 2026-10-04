; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-R6RS -*-

;;;; (rnrs bytevectors (6)), library report chapter 2.  Bytevectors are
;;;; (simple-array (unsigned-byte 8) (*)), as in the R7RS layer, which
;;;; already provides make-bytevector, bytevector?, bytevector-length,
;;;; bytevector-u8-ref/set!, utf8->string and string->utf8.

(in-package "PSEUDOSCHEME-R6RS")

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(defun check-bv (who x)
  (unless (typep x 'octets) (r6rs-assertion-violation who "not a bytevector" x))
  x)

(defun endianness-of (who e)
  (let ((name (and (symbolp e) (ps:scheme-symbol-name e))))
    (cond ((equal name "big") :big)
	  ((equal name "little") :little)
	  (t (r6rs-assertion-violation who "bad endianness" e)))))

(defprim "make-bytevector" (k &optional (fill 0))
  ;; R6RS allows a fill of -128..255 (R7RS: 0..255).
  (unless (and (integerp fill) (<= -128 fill 255))
    (r6rs-assertion-violation "make-bytevector" "bad fill" fill))
  (make-array k :element-type '(unsigned-byte 8) :initial-element (ldb (byte 8 0) fill)))

(defprim "native-endianness" ()
  (ps:intern-scheme-symbol #+little-endian "little" #-little-endian "big"))

(defprim "bytevector=?" (a b)
  (check-bv "bytevector=?" a) (check-bv "bytevector=?" b)
  (ps:true? (equalp a b)))

(defprim "bytevector-fill!" (bv fill)
  (check-bv "bytevector-fill!" bv)
  (fill bv (ldb (byte 8 0) fill))
  ps:unspecific)

(defprim "bytevector-copy!" (source source-start target target-start k)
  ;; R6RS argument order; see R7RS's in ../r7rs/rts.lisp.
  (check-bv "bytevector-copy!" source) (check-bv "bytevector-copy!" target)
  (unless (and (<= 0 source-start) (<= (+ source-start k) (length source))
	       (<= 0 target-start) (<= (+ target-start k) (length target)))
    (r6rs-assertion-violation "bytevector-copy!" "range out of bounds" source-start target-start k))
  (replace target source :start1 target-start :start2 source-start :end2 (+ source-start k))
  ps:unspecific)

(defprim "bytevector->u8-list" (bv) (check-bv "bytevector->u8-list" bv) (coerce bv 'list))
(defprim "u8-list->bytevector" (list)
  (make-array (length list) :element-type '(unsigned-byte 8) :initial-contents list))

(defprim "bytevector-s8-ref" (bv k)
  (check-bv "bytevector-s8-ref" bv)
  (let ((b (aref bv k))) (if (>= b 128) (- b 256) b)))
(defprim "bytevector-s8-set!" (bv k v)
  (check-bv "bytevector-s8-set!" bv)
  (unless (and (integerp v) (<= -128 v 127)) (r6rs-assertion-violation "bytevector-s8-set!" "out of range" v))
  (setf (aref bv k) (ldb (byte 8 0) v))
  ps:unspecific)

;;; Multi-byte integers

(defun bv-uint-ref (who bv k endian size)
  (check-bv who bv)
  (unless (and (integerp k) (<= 0 k) (<= (+ k size) (length bv)))
    (r6rs-assertion-violation who "index out of range" k))
  (let ((n 0))
    (dotimes (i size n)
      (let ((byte (aref bv (+ k (if (eq endian :big) i (- size 1 i))))))
	(setf n (logior (ash n 8) byte))))))

(defun bv-sint-ref (who bv k endian size)
  (let ((u (bv-uint-ref who bv k endian size)))
    (if (logbitp (1- (* 8 size)) u) (- u (ash 1 (* 8 size))) u)))

(defun bv-int-set (who bv k n endian size signed)
  (check-bv who bv)
  (unless (and (integerp k) (<= 0 k) (<= (+ k size) (length bv)))
    (r6rs-assertion-violation who "index out of range" k))
  (unless (and (integerp n)
	       (if signed
		   (<= (- (ash 1 (1- (* 8 size)))) n (1- (ash 1 (1- (* 8 size)))))
		   (<= 0 n (1- (ash 1 (* 8 size))))))
    (r6rs-assertion-violation who "value out of range" n))
  (let ((u (ldb (byte (* 8 size) 0) n)))
    (dotimes (i size)
      (setf (aref bv (+ k (if (eq endian :big) (- size 1 i) i)))
	    (ldb (byte 8 (* 8 i)) u))))
  ps:unspecific)

(defmacro def-sized-int (bits)
  (let* ((size (/ bits 8))
	 (u (format nil "bytevector-u~D" bits))
	 (s (format nil "bytevector-s~D" bits)))
    `(progn
       (defprim ,(format nil "~A-ref" u) (bv k e)
	 (bv-uint-ref ,u bv k (endianness-of ,u e) ,size))
       (defprim ,(format nil "~A-ref" s) (bv k e)
	 (bv-sint-ref ,s bv k (endianness-of ,s e) ,size))
       (defprim ,(format nil "~A-set!" u) (bv k v e)
	 (bv-int-set ,u bv k v (endianness-of ,u e) ,size nil))
       (defprim ,(format nil "~A-set!" s) (bv k v e)
	 (bv-int-set ,s bv k v (endianness-of ,s e) ,size t))
       (defprim ,(format nil "~A-native-ref" u) (bv k)
	 (unless (zerop (mod k ,size)) (r6rs-assertion-violation ,u "unaligned index" k))
	 (bv-uint-ref ,u bv k #+little-endian :little #-little-endian :big ,size))
       (defprim ,(format nil "~A-native-ref" s) (bv k)
	 (unless (zerop (mod k ,size)) (r6rs-assertion-violation ,s "unaligned index" k))
	 (bv-sint-ref ,s bv k #+little-endian :little #-little-endian :big ,size))
       (defprim ,(format nil "~A-native-set!" u) (bv k v)
	 (unless (zerop (mod k ,size)) (r6rs-assertion-violation ,u "unaligned index" k))
	 (bv-int-set ,u bv k v #+little-endian :little #-little-endian :big ,size nil))
       (defprim ,(format nil "~A-native-set!" s) (bv k v)
	 (unless (zerop (mod k ,size)) (r6rs-assertion-violation ,s "unaligned index" k))
	 (bv-int-set ,s bv k v #+little-endian :little #-little-endian :big ,size t)))))

(def-sized-int 16)
(def-sized-int 32)
(def-sized-int 64)

(defprim "bytevector-uint-ref" (bv k e size) (bv-uint-ref "bytevector-uint-ref" bv k (endianness-of "bytevector-uint-ref" e) size))
(defprim "bytevector-sint-ref" (bv k e size) (bv-sint-ref "bytevector-sint-ref" bv k (endianness-of "bytevector-sint-ref" e) size))
(defprim "bytevector-uint-set!" (bv k n e size) (bv-int-set "bytevector-uint-set!" bv k n (endianness-of "bytevector-uint-set!" e) size nil))
(defprim "bytevector-sint-set!" (bv k n e size) (bv-int-set "bytevector-sint-set!" bv k n (endianness-of "bytevector-sint-set!" e) size t))

(defun bv->int-list (who bv e size signed)
  (check-bv who bv)
  (unless (zerop (mod (length bv) size)) (r6rs-assertion-violation who "length not a multiple of size" size))
  (loop for k from 0 below (length bv) by size
	collect (if signed
		    (bv-sint-ref who bv k (endianness-of who e) size)
		    (bv-uint-ref who bv k (endianness-of who e) size))))

(defun int-list->bv (who list e size signed)
  (let ((bv (make-array (* size (length list)) :element-type '(unsigned-byte 8))))
    (loop for n in list for k from 0 by size
	  do (bv-int-set who bv k n (endianness-of who e) size signed))
    bv))

(defprim "bytevector->uint-list" (bv e size) (bv->int-list "bytevector->uint-list" bv e size nil))
(defprim "bytevector->sint-list" (bv e size) (bv->int-list "bytevector->sint-list" bv e size t))
(defprim "uint-list->bytevector" (list e size) (int-list->bv "uint-list->bytevector" list e size nil))
(defprim "sint-list->bytevector" (list e size) (int-list->bv "sint-list->bytevector" list e size t))

;;; IEEE floats

(defun float->bits (x size)
  (if (= size 4)
      (ldb (byte 32 0) (float-features:single-float-bits (coerce x 'single-float)))
      (ldb (byte 64 0) (float-features:double-float-bits (coerce x 'double-float)))))

(defun bits->float (bits size)
  (if (= size 4)
      (coerce (float-features:bits-single-float bits) 'double-float)
      (float-features:bits-double-float bits)))

(macrolet ((def-ieee (kind size)
	     (let ((name (format nil "bytevector-ieee-~A" kind)))
	       `(progn
		  (defprim ,(format nil "~A-ref" name) (bv k e)
		    (bits->float (bv-uint-ref ,name bv k (endianness-of ,name e) ,size) ,size))
		  (defprim ,(format nil "~A-native-ref" name) (bv k)
		    (bits->float (bv-uint-ref ,name bv k #+little-endian :little #-little-endian :big ,size) ,size))
		  (defprim ,(format nil "~A-set!" name) (bv k x e)
		    (bv-int-set ,name bv k (float->bits x ,size) (endianness-of ,name e) ,size nil))
		  (defprim ,(format nil "~A-native-set!" name) (bv k x)
		    (bv-int-set ,name bv k (float->bits x ,size) #+little-endian :little #-little-endian :big ,size nil))))))
  (def-ieee "single" 4)
  (def-ieee "double" 8))

;;; Strings (2.9): UTF-16 and UTF-32

(defun utf16-encode (string endian bom)
  (let ((out '()))
    (flet ((emit (u) (push (if (eq endian :big) (ldb (byte 8 8) u) (ldb (byte 8 0) u)) out)
		   (push (if (eq endian :big) (ldb (byte 8 0) u) (ldb (byte 8 8) u)) out)))
      (when bom (emit #xFEFF))
      (loop for c across string
	    for code = (char-code c)
	    do (if (< code #x10000)
		   (emit code)
		   (let ((v (- code #x10000)))
		     (emit (+ #xD800 (ash v -10)))
		     (emit (+ #xDC00 (logand v #x3FF)))))))
    (coerce (nreverse out) 'octets)))

(defprim "string->utf16" (string &optional (e nil))
  (utf16-encode string (if e (endianness-of "string->utf16" e) :big) nil))

(defun detect-bom (bv size e endianness-mandatory)
  "Returns (values endian start)."
  (let ((default (if e (endianness-of "utf->string" e) :big)))
    (if (or endianness-mandatory (< (length bv) size))
	(values default 0)
	(let ((first (bv-uint-ref "utf->string" bv 0 :big size)))
	  (cond ((= first (if (= size 2) #xFEFF #x0000FEFF)) (values :big size))
		((= first (if (= size 2) #xFFFE #xFFFE0000)) (values :little size))
		(t (values default 0)))))))

(defprim "utf16->string" (bv e &optional mandatory)
  (check-bv "utf16->string" bv)
  (multiple-value-bind (endian start) (detect-bom bv 2 e (and mandatory (truthy mandatory)))
    (let ((out (make-string-output-stream)) (k start))
      (loop while (< (1+ k) (length bv))
	    do (let ((u (bv-uint-ref "utf16->string" bv k endian 2)))
		 (incf k 2)
		 (if (and (<= #xD800 u #xDBFF) (< (1+ k) (length bv)))
		     (let ((lo (bv-uint-ref "utf16->string" bv k endian 2)))
		       (incf k 2)
		       (write-char (code-char (+ #x10000 (ash (- u #xD800) 10) (- lo #xDC00))) out))
		     (write-char (code-char (if (<= #xD800 u #xDFFF) #xFFFD u)) out))))
      (coerce (get-output-stream-string out) 'simple-string))))

(defprim "string->utf32" (string &optional (e nil))
  (let ((endian (if e (endianness-of "string->utf32" e) :big)))
    (int-list->bv "string->utf32" (map 'list #'char-code string)
		  (ps:intern-scheme-symbol (if (eq endian :big) "big" "little")) 4 nil)))

(defprim "utf32->string" (bv e &optional mandatory)
  (check-bv "utf32->string" bv)
  (multiple-value-bind (endian start) (detect-bom bv 4 e (and mandatory (truthy mandatory)))
    (coerce (loop for k from start below (- (length bv) 3) by 4
		  collect (let ((code (bv-uint-ref "utf32->string" bv k endian 4)))
			    (if (or (> code #x10FFFF) (<= #xD800 code #xDFFF)) (code-char #xFFFD) (code-char code))))
	    'simple-string)))
