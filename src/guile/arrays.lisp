; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-GUILE -*-

;;;; Guile's arrays, written for Pseudoscheme.  An array is a view of a
;;;; root vector: an offset, and for each dimension its lower and upper
;;;; bounds and its stride in the root.  A one-dimensional, zero-based
;;;; view of the whole of a root is the root itself, as in Guile, so
;;;; vectors, strings, bitvectors and bytevectors are arrays; any other
;;;; view is a GARRAY.  Shared arrays (make-shared-array, transpose-array,
;;;; array-slice, array-cell-ref) are views of the same root.
;;;;
;;;; Array types are Guile's: #t (vectors), a (strings), b (bitvectors),
;;;; vu8 (bytevectors), and the SRFI 4 types u8 ... f64, c32 and c64,
;;;; which, as in Guile 3, are bytevectors tagged with their element type.

(in-package "PSEUDOSCHEME-GUILE")

(defstruct (garray (:constructor %make-garray (root type offset dims)) (:copier nil))
  root
  type					; a string: "#t", "a", "b", "vu8", "u16", ...
  (offset 0 :type fixnum)
  (dims '() :type list))			; ((lower upper stride) ...)

(defmethod print-object ((a garray) stream) (guile-display a stream))

;;; Element types

(defvar *bytevector-types* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "A SRFI 4 vector (a bytevector) -> its element type, a string.")

(defparameter *type-sizes*
  '(("u8" . 1) ("s8" . 1) ("u16" . 2) ("s16" . 2) ("u32" . 4) ("s32" . 4) ("u64" . 8) ("s64" . 8)
    ("f32" . 4) ("f64" . 8) ("c32" . 8) ("c64" . 16) ("vu8" . 1)))

(setq ps::*bytevector-type* (lambda (v) (gethash v *bytevector-types* "vu8")))

(defun type-size (type) (cdr (assoc type *type-sizes* :test #'string=)))

(defun type-name (type)
  "TYPE, a symbol or #t, as a string."
  (cond ((eq type ps:true) "#t")
	((symbolp type) (ps:scheme-symbol-name type))
	((stringp type) type)
	(t (wrong-type "make-typed-array" 1 type))))

(defun type-symbol (type)
  (if (string= type "#t") ps:true (ssym type)))

(defun root-type (root)
  "The array type of ROOT, a vector, or NIL if it isn't an array."
  (cond ((stringp root) "a")
	((bit-vector-p root) "b")
	((typep root 'ps-r6rs::octets) (gethash root *bytevector-types* "vu8"))
	((simple-vector-p root) "#t")
	((and (vectorp root) (ps::numeric-vector-tag root))
	 (let ((tag (ps::numeric-vector-tag root)))
	   (cond ((string= tag "c64") "c32") ((string= tag "c128") "c64") (t tag))))
	(t nil)))

(defun root-length (root type)
  (if (and (typep root 'ps-r6rs::octets) (string/= type "vu8"))
      (floor (length root) (type-size type))
      (length root)))

(defun bytes-ref (v start size)
  (loop for k below size sum (ash (aref v (+ start k)) (* 8 k))))

(defun bytes-set (v start size n)
  (dotimes (k size) (setf (aref v (+ start k)) (ldb (byte 8 (* 8 k)) n))))

(defun signed (n bits) (if (logbitp (1- bits) n) (- n (ash 1 bits)) n))

(defun root-ref (root type i)
  (cond ((simple-vector-p root) (svref root i))
	((stringp root) (char root i))
	((bit-vector-p root) (bool (= 1 (sbit root i))))
	((and (typep root 'ps-r6rs::octets) (string/= type "vu8") (string/= type "u8"))
	 (let* ((size (type-size type)) (at (* i size)))
	   (flet ((float-at (at bytes)
		    (if (= bytes 4)
			(coerce (sb-kernel:make-single-float (signed (bytes-ref root at 4) 32)) 'double-float)
			(sb-kernel:make-double-float (signed (bytes-ref root (+ at 4) 4) 32) (bytes-ref root at 4)))))
	     (cond ((string= type "s8") (signed (aref root i) 8))
		   ((char= (char type 0) #\u) (bytes-ref root at size))
		   ((char= (char type 0) #\s) (signed (bytes-ref root at size) (* 8 size)))
		   ((string= type "f32") (float-at at 4))
		   ((string= type "f64") (float-at at 8))
		   ((string= type "c32") (complex (float-at at 4) (float-at (+ at 4) 4)))
		   (t (complex (float-at at 8) (float-at (+ at 8) 8)))))))
	(t (aref root i))))

(defun check-element (type x)
  "X fits an element of integer TYPE, or it is out of range."
  (let ((size (type-size type)))
    (when (and size (or (char= (char type 0) #\u) (char= (char type 0) #\s) (string= type "vu8")))
      (let ((bits (* 8 size)))
	(unless (and (integerp x)
		     (if (char= (char type 0) #\s)
			 (<= (- (ash 1 (1- bits))) x (1- (ash 1 (1- bits))))
			 (<= 0 x (1- (ash 1 bits)))))
	  (if (integerp x)
	      (out-of-range "array-set!" x)
	      (wrong-type "array-set!" 2 x)))))))

(defun root-set (root type i x)
  (check-element type x)
  (cond ((simple-vector-p root) (setf (svref root i) x))
	((stringp root) (setf (char root i) x))
	((bit-vector-p root) (setf (sbit root i) (if (truthy x) 1 0)))
	((and (typep root 'ps-r6rs::octets) (string/= type "vu8") (string/= type "u8"))
	 (let* ((size (type-size type)) (at (* i size)))
	   (flet ((float-to (at bytes x)
		    (if (= bytes 4)
			(bytes-set root at 4 (ldb (byte 32 0) (sb-kernel:single-float-bits (coerce x 'single-float))))
			(let ((d (coerce x 'double-float)))
			  (bytes-set root at 4 (sb-kernel:double-float-low-bits d))
			  (bytes-set root (+ at 4) 4 (ldb (byte 32 0) (sb-kernel:double-float-high-bits d)))))))
	     (cond ((member type '("f32" "f64") :test #'string=) (float-to at size x))
		   ((string= type "c32") (float-to at 4 (realpart x)) (float-to (+ at 4) 4 (imagpart x)))
		   ((string= type "c64") (float-to at 8 (realpart x)) (float-to (+ at 8) 8 (imagpart x)))
		   (t (bytes-set root at size (ldb (byte (* 8 size) 0) x)))))))
	(t (setf (aref root i) x))))

(defun make-root (type n fill)
  (let ((unspecified (or (eq fill *unspecified*) (null fill))))
    (cond ((string= type "#t") (make-array n :initial-element (if (null fill) *unspecified* fill)))
	  ((string= type "a") (make-string n :initial-element (if (characterp fill) fill #\Space)))
	  ((string= type "b") (make-array n :element-type 'bit :initial-element (if (and (not unspecified) (truthy fill)) 1 0)))
	  ((string= type "vu8")
	   (unless (or unspecified (typep fill '(unsigned-byte 8)))
	     (guile-error (ssym "out-of-range") "make-typed-array" "Value out of range 0 to 255: ~S"
			  (list fill) (list fill)))
	   (make-array n :element-type '(unsigned-byte 8) :initial-element (if unspecified 0 fill)))
	  ((type-size type)
	   (let ((v (make-array (* n (type-size type)) :element-type '(unsigned-byte 8) :initial-element 0)))
	     (setf (gethash v *bytevector-types*) type)
	     (unless (or unspecified (eql fill 0))
	       (dotimes (i n) (root-set v type i fill)))
	     v))
	  (t (guile-error (ssym "wrong-type-arg") "make-typed-array" "Wrong type argument: ~S" (list (type-symbol type)) (list (type-symbol type)))))))

;;; Views

(defun array-view (x who)
  "X as root, type, offset and dimensions; an error if it isn't an array."
  (cond ((garray-p x) (values (garray-root x) (garray-type x) (garray-offset x) (garray-dims x)))
	((root-type x)
	 (let ((type (root-type x)))
	   (values x type 0 (list (list 0 (1- (root-length x type)) 1)))))
	(t (wrong-type who 1 x))))

(defun make-view (root type offset dims)
  "The array viewing ROOT so: ROOT itself if that is the view."
  (if (and (= offset 0) (= (length dims) 1)
	   (destructuring-bind (lo hi stride) (first dims)
	     (and (= lo 0) (= stride 1) (= (1+ hi) (root-length root type)))))
      root
      (%make-garray root type offset dims)))

(defun guile-array-p (x) (or (garray-p x) (and (root-type x) t)))

(defun out-of-range (who index)
  (guile-error (ssym "out-of-range") who "Value out of range: ~S" (list index) (list index)))

(defun view-index (who offset dims indices)
  (unless (= (length indices) (length dims))
    (guile-error (ssym "misc-error") who "wrong number of indices, expecting ~A" (list (length dims))))
  (let ((i offset))
    (loop for (lo hi stride) in dims for index in indices
	  do (unless (and (integerp index) (<= lo index hi)) (out-of-range who index))
	     (incf i (* (- index lo) stride)))
    i))

(defun bound-pair (b who)
  "A bound, n or (lo hi), as (lo hi)."
  (cond ((integerp b) (list 0 (1- b)))
	((and (consp b) (integerp (first b)) (consp (cdr b)) (integerp (second b))) (list (first b) (second b)))
	(t (wrong-type who 2 b))))

(defun row-major-dims (bounds)
  "Dimensions with row-major strides for BOUNDS ((lo hi) ...), and the size."
  (let ((stride 1) (dims '()))
    (dolist (b (reverse bounds))
      (destructuring-bind (lo hi) b
	(push (list lo hi stride) dims)
	(setq stride (* stride (max 0 (1+ (- hi lo)))))))
    (values dims stride)))

(defun make-typed-array* (type fill bounds)
  (let ((type (type-name type))
	(bounds (mapcar (lambda (b) (bound-pair b "make-typed-array")) bounds)))
    (multiple-value-bind (dims size) (row-major-dims bounds)
      (make-view (make-root type size fill) type 0 dims))))

(defun dims-size (dims) (reduce #'* dims :key (lambda (d) (max 0 (1+ (- (second d) (first d))))) :initial-value 1))

(defun walk-indices (dims function)
  "Call FUNCTION with each index list of DIMS, in row-major order."
  (labels ((walk (dims indices)
	     (if (null dims)
		 (funcall function (reverse indices))
		 (destructuring-bind (lo hi &rest ignore) (first dims)
		   (declare (ignore ignore))
		   (loop for i from lo to hi do (walk (rest dims) (cons i indices)))))))
    (walk dims '())))

(defun array-element (a indices who)
  (multiple-value-bind (root type offset dims) (array-view a who)
    (root-ref root type (view-index who offset dims indices))))

(defun set-array-element (a value indices who)
  (multiple-value-bind (root type offset dims) (array-view a who)
    (root-set root type (view-index who offset dims indices) value)))

(defun cell-view (a indices who)
  "The cell of A at INDICES (fewer than its rank): a view of what remains."
  (multiple-value-bind (root type offset dims) (array-view a who)
    (when (> (length indices) (length dims)) (out-of-range who (length indices)))
    (let ((i offset))
      (loop for (lo hi stride) in dims for index in indices
	    do (unless (and (integerp index) (<= lo index hi)) (out-of-range who index))
	       (incf i (* (- index lo) stride)))
      (values root type i (nthcdr (length indices) dims)))))

(defun array->list* (a)
  (multiple-value-bind (root type offset dims) (array-view a "array->list")
    (labels ((walk (dims at)
	       (if (null dims)
		   (root-ref root type at)
		   (destructuring-bind (lo hi stride) (first dims)
		     (loop for i from lo to hi collect (walk (rest dims) (+ at (* (- i lo) stride))))))))
      (walk dims offset))))

(defun list->typed-array* (type shape list)
  (let* ((rank (if (integerp shape) shape (length shape)))
	 ;; a shape's element is (lo hi), or a lower bound, the length the list's
	 (lengths (labels ((lens (x r) (if (zerop r) '() (cons (if (listp x) (length x) 0)
							       (lens (if (consp x) (car x) nil) (1- r))))))
		    (lens list rank)))
	 (bounds (if (integerp shape)
		     (mapcar (lambda (n) (list 0 (1- n))) lengths)
		     (loop for b in shape for n in lengths
			   collect (if (integerp b) (list b (+ b n -1)) (bound-pair b "list->typed-array")))))
	 (a (make-typed-array* type *unspecified* bounds)))
    (multiple-value-bind (root type offset dims) (array-view a "list->typed-array")
      (labels ((fill-cells (dims at x)
		 (if (null dims)
		     (root-set root type at x)
		     (destructuring-bind (lo hi stride) (first dims)
		       (unless (and (listp x) (= (length x) (max 0 (1+ (- hi lo)))))
			 (guile-error (ssym "misc-error") "list->array" "bad list ~S for shape" (list x)))
		       (loop for i from lo to hi for y in x
			     do (fill-cells (rest dims) (+ at (* (- i lo) stride)) y))))))
	(fill-cells dims offset list)))
    a))

(defun array-shape* (a)
  (multiple-value-bind (root type offset dims) (array-view a "array-shape")
    (declare (ignore root type offset))
    (mapcar (lambda (d) (list (first d) (second d))) dims)))

(defun shared-array (a mapping bounds)
  ;; As Guile's: what matters is that the new array's corners map into
  ;; A's elements, by their position in the root; an index may run past
  ;; one of A's dimensions into the next.
  (multiple-value-bind (root type offset dims) (array-view a "make-shared-array")
    (let ((bounds (mapcar (lambda (b) (bound-pair b "make-shared-array")) bounds))
	  (lowest (+ offset (loop for (lo hi stride) in dims sum (min 0 (* (- hi lo) stride)))))
	  (highest (+ offset (loop for (lo hi stride) in dims sum (max 0 (* (- hi lo) stride))))))
      (flet ((position-of (indices)
	       (let ((old (apply mapping indices)))	; a list of indices
		 (unless (and (listp old) (= (length old) (length dims)) (every #'integerp old))
		   (guile-error (ssym "misc-error") "make-shared-array" "bad mapping: ~S" (list old)))
		 (+ offset (loop for i in old for (lo nil stride) in dims sum (* (- i lo) stride))))))
	(if (some (lambda (b) (> (first b) (second b))) bounds)
	    (%make-garray root type 0 (mapcar (lambda (b) (list (first b) (second b) 0)) bounds))
	    (let* ((los (mapcar #'first bounds))
		   (base (position-of los))
		   (far (position-of (mapcar #'second bounds))))
	      (unless (<= lowest base highest) (out-of-range "make-shared-array" base))
	      (unless (<= lowest far highest)
		(guile-error (ssym "misc-error") "make-shared-array" "mapping out of range" '()))
	      (make-view root type base
			 (loop for (lo hi) in bounds for k from 0
			       collect (list lo hi
					     (if (= lo hi)
						 0
						 (- (position-of (loop for l in los for j from 0
								       collect (if (= j k) (1+ l) l)))
						    base)))))))))))

(defun transpose (a axes)
  (multiple-value-bind (root type offset dims) (array-view a "transpose-array")
    (unless (= (length axes) (length dims)) (wrong-type "transpose-array" 2 axes))
    (let* ((rank (if axes (1+ (reduce #'max axes)) 0))
	   (new (loop for j below rank
		      collect (let ((olds (loop for d in dims for k in axes when (= k j) collect d)))
				(unless olds (wrong-type "transpose-array" 2 axes))
				(list (reduce #'max olds :key #'first)
				      (reduce #'min olds :key #'second)
				      (reduce #'+ olds :key #'third)
				      olds)))))
      ;; a diagonal starts at the largest lower bound
      (let ((offset (+ offset (loop for (lo nil nil olds) in new
				    sum (loop for (olo nil ostride) in olds sum (* (- lo olo) ostride))))))
	(make-view root type offset (mapcar (lambda (d) (subseq d 0 3)) new))))))

(defun array-contents* (a strict)
  (multiple-value-bind (root type offset dims) (array-view a "array-contents")
    (let ((stride 1) (contiguous t))
      (dolist (d (reverse dims))
	(destructuring-bind (lo hi s) d
	  (unless (or (= s stride) (= lo hi)) (setq contiguous nil))
	  (setq stride (* stride (max 0 (1+ (- hi lo)))))))
      (cond ((and (= (length dims) 1) (not (truthy strict))) a)
	    ((not contiguous) ps:false)
	    ((and (truthy strict) (/= offset 0)) ps:false)
	    (t (make-view root type offset (list (list 0 (1- (dims-size dims)) 1))))))))

(defun arrays-equal (a b)
  (and (guile-array-p a) (guile-array-p b)
       (multiple-value-bind (ra ta oa da) (array-view a "array-equal?")
	 (multiple-value-bind (rb tb ob db) (array-view b "array-equal?")
	   (declare (ignore ra rb oa ob))
	   (and (string= ta tb)
		(equal (mapcar (lambda (d) (subseq d 0 2)) da) (mapcar (lambda (d) (subseq d 0 2)) db))
		(ps:scheme-equal-p (array->list* a) (array->list* b)))))))

(defun same-shape-p (arrays)
  "Whether ARRAYS have the same lengths (their bounds may differ)."
  (flet ((lengths (a) (mapcar (lambda (b) (- (second b) (first b))) (array-shape* a))))
    (let ((shape (lengths (first arrays))))
      (every (lambda (a) (equal (lengths a) shape)) (rest arrays)))))

;;; The primitives

(defguile "array?" (x) (bool (guile-array-p x)))
(defguile "typed-array?" (x type)
  (bool (and (guile-array-p x) (string= (nth-value 1 (array-view x "typed-array?")) (type-name type)))))
(defguile "array-type" (a) (type-symbol (nth-value 1 (array-view a "array-type"))))
(defguile "array-rank" (a) (if (guile-array-p a) (length (nth-value 3 (array-view a "array-rank"))) 0))
(defguile "array-dimensions" (a)
  (mapcar (lambda (d) (if (= (first d) 0) (1+ (second d)) (list (first d) (second d))))
	  (nth-value 3 (array-view a "array-dimensions"))))
(defguile "array-shape" (a) (array-shape* a))
(defguile "array-length" (a)
  (let ((dims (nth-value 3 (array-view a "array-length"))))
    (when (null dims) (wrong-type "array-length" 1 a))
    (max 0 (1+ (- (second (first dims)) (first (first dims)))))))
(defguile "array-in-bounds?" (a &rest indices)
  (let ((dims (nth-value 3 (array-view a "array-in-bounds?"))))
    (bool (and (= (length indices) (length dims))
	       (every (lambda (i d) (and (integerp i) (<= (first d) i (second d)))) indices dims)))))
(defguile "array-ref" (a &rest indices) (array-element a indices "array-ref"))
(defguile "array-set!" (a value &rest indices) (set-array-element a value indices "array-set!") *unspecified*)
(defguile "array->list" (a) (array->list* a))
(defguile "shared-array-root" (a) (values (array-view a "shared-array-root")))
(defguile "shared-array-offset" (a)	; the root index of the first element
  (nth-value 2 (array-view a "shared-array-offset")))
(defguile "shared-array-increments" (a) (mapcar #'third (nth-value 3 (array-view a "shared-array-increments"))))
(defguile "make-typed-array" (type fill &rest bounds) (make-typed-array* type fill bounds))
(defguile "make-array" (fill &rest bounds) (make-typed-array* ps:true fill bounds))
(defguile "make-shared-array" (a mapping &rest bounds) (shared-array a mapping bounds))
(defguile "array-slice" (a &rest indices)
  (multiple-value-bind (root type offset dims) (cell-view a indices "array-slice")
    (%make-garray root type offset dims)))
(defguile "array-cell-ref" (a &rest indices)
  (multiple-value-bind (root type offset dims) (cell-view a indices "array-cell-ref")
    (if (null dims) (root-ref root type offset) (make-view root type offset dims))))
(defguile "array-cell-set!" (a value &rest indices)
  (multiple-value-bind (root type offset dims) (cell-view a indices "array-cell-set!")
    (if (null dims)
	(root-set root type offset value)
	(let ((cell (%make-garray root type offset dims)))
	  (walk-indices dims (lambda (ix) (set-array-element cell (array-element value ix "array-cell-set!") ix "array-cell-set!")))))
    a))
(defguile "transpose-array" (a &rest axes) (transpose a axes))
(defguile "array-contents" (a &optional (strict ps:false)) (array-contents* a strict))
(defguile "list->typed-array" (type shape list) (list->typed-array* type shape list))
(defguile "list->array" (shape list) (list->typed-array* ps:true shape list))
(defguile "array-fill!" (a fill)
  (let ((dims (nth-value 3 (array-view a "array-fill!"))))
    (walk-indices dims (lambda (ix) (set-array-element a fill ix "array-fill!"))))
  *unspecified*)
(defun covers-p (big small)
  "Whether array BIG has every index position SMALL has (by offset from
the lower bounds), with the same rank."
  (let ((b (array-shape* big)) (s (array-shape* small)))
    (and (= (length b) (length s))
	 (every (lambda (x y) (>= (- (second x) (first x)) (- (second y) (first y)))) b s))))

(defun shape-mismatch (who)
  (guile-error (ssym "misc-error") who "array shape mismatch" '()))

(defun corresponding (a ix b)
  "The index in B of index IX of A: the same offsets from the lower bounds."
  (mapcar (lambda (i da db) (+ (- i (first da)) (first db))) ix (array-shape* a) (array-shape* b)))

(defguile "array-copy!" (src dst)
  ;; SRC's elements, each to the same place in DST, which may be bigger
  (unless (covers-p dst src) (shape-mismatch "array-copy!"))
  (let ((cells '()))
    (walk-indices (nth-value 3 (array-view src "array-copy!"))
		  (lambda (ix) (push (cons (corresponding src ix dst) (array-element src ix "array-copy!")) cells)))
    (dolist (c (nreverse cells))
      (set-array-element dst (cdr c) (car c) "array-copy!")))
  *unspecified*)
(setf (gethash "array-copy-in-order!" *guile-primitives*) (gethash "array-copy!" *guile-primitives*))

(defun map-arrays (who dest proc sources)
  "Set each element of DEST to PROC of the elements in the same places
of SOURCES, which must have them."
  (dolist (s sources) (unless (covers-p s dest) (shape-mismatch who)))
  (let ((results '()))
    (walk-indices (nth-value 3 (array-view dest who))
		  (lambda (ix)
		    (push (cons ix (apply proc (mapcar (lambda (s) (array-element s (corresponding dest ix s) who))
						       sources)))
			  results)))
    (dolist (r (nreverse results))
      (set-array-element dest (cdr r) (car r) who))))

(defguile "array-map!" (dest proc &rest sources) (map-arrays "array-map!" dest proc sources) *unspecified*)
(setf (gethash "array-map-in-order!" *guile-primitives*) (gethash "array-map!" *guile-primitives*))
(defguile "array-for-each" (proc a &rest more)
  (dolist (m more) (unless (covers-p m a) (shape-mismatch "array-for-each")))
  (walk-indices (nth-value 3 (array-view a "array-for-each"))
		(lambda (ix)
		  (apply proc (array-element a ix "array-for-each")
			 (mapcar (lambda (m) (array-element m (corresponding a ix m) "array-for-each")) more))))
  *unspecified*)
(defguile "array-index-map!" (a proc)
  (walk-indices (nth-value 3 (array-view a "array-index-map!"))
		(lambda (ix) (set-array-element a (apply proc ix) ix "array-index-map!")))
  *unspecified*)
(defguile "array-equal?" (&rest arrays)
  (bool (loop for (a b) on arrays while b always (arrays-equal a b))))

(defun slice-for-each (frame-rank proc arrays)
  (let* ((first-dims (nth-value 3 (array-view (first arrays) "array-slice-for-each")))
	 (frame (subseq first-dims 0 frame-rank)))
    (walk-indices frame
		  (lambda (ix)
		    (apply proc (mapcar (lambda (a)
					  (multiple-value-bind (root type offset dims) (cell-view a ix "array-slice-for-each")
					    (%make-garray root type offset dims)))
					arrays))))
    *unspecified*))

(defguile "array-slice-for-each" (frame-rank proc &rest arrays) (slice-for-each frame-rank proc arrays))
(defguile "array-slice-for-each-in-order" (frame-rank proc &rest arrays) (slice-for-each frame-rank proc arrays))

(defguile "make-generalized-vector" (type length &optional (fill *unspecified*))
  (make-root (type-name type) length fill))
(defguile "make-srfi-4-vector" (type length &optional (fill 0))
  (make-root (type-name type) length fill))
(defguile "srfi-4-vector-type-size" (v)
  ;; of a SRFI 4 vector: its elements' size in bytes
  (or (and (guile-array-p v) (type-size (nth-value 1 (array-view v "srfi-4-vector-type-size"))))
      (wrong-type "srfi-4-vector-type-size" 1 v)))

;;; Writing an array as Guile does: #2((1 2) (3 4)), #1@1(a b), #0f64(9.0)

(defun write-array (a stream display)
  (multiple-value-bind (root type offset dims) (array-view a "write")
    (let ((rank (length dims)))
      (write-char #\# stream)
      (format stream "~D" rank)
      (unless (string= type "#t") (write-string type stream))
      (let* ((lengths (mapcar (lambda (d) (max 0 (1+ (- (second d) (first d))))) dims))
	     ;; lengths only where the parentheses can't show them: a
	     ;; dimension after an empty one isn't empty
	     (empty (let ((at (position 0 lengths))) (and at (some #'plusp (nthcdr at lengths)))))
	     (bounds (some (lambda (d) (/= (first d) 0)) dims)))
	(dolist (d dims)
	  (when bounds (format stream "@~D" (first d)))
	  (when empty (format stream ":~D" (max 0 (1+ (- (second d) (first d))))))))
      (if (zerop rank)
	  (progn (write-char #\( stream) (guile-print (root-ref root type offset) stream display) (write-char #\) stream))
	  (labels ((walk (dims at)
		     (write-char #\( stream)
		     (destructuring-bind (lo hi stride) (first dims)
		       (loop for i from lo to hi
			     do (unless (= i lo) (write-char #\Space stream))
				(if (rest dims)
				    (walk (rest dims) (+ at (* (- i lo) stride)))
				    (guile-print (root-ref root type (+ at (* (- i lo) stride))) stream display))))
		     (write-char #\) stream)))
	    (walk dims offset))))))

(defun write-tagged-bytevector (v type stream display)
  (write-string "#" stream)
  (write-string type stream)
  (write-char #\( stream)
  (dotimes (i (root-length v type))
    (unless (zerop i) (write-char #\Space stream))
    (guile-print (root-ref v type i) stream display))
  (write-char #\) stream))

(defguile "uniform-array->bytevector" (a)
  ;; the elements' bytes, in row-major order
  (uniform-array->bytevector a))

(defun uniform-array->bytevector (a)
  (multiple-value-bind (root type offset dims) (array-view a "uniform-array->bytevector")
    (declare (ignore root offset))
    (when (string= type "b")
      ;; a bitvector's bits in 32-bit words, least significant first
      (let* ((bits (array->list* a))
	     (out (make-array (* 4 (ceiling (length bits) 32)) :element-type '(unsigned-byte 8) :initial-element 0)))
	(loop for bit in bits for i from 0
	      when (truthy bit)
		do (setf (ldb (byte 1 (mod i 8)) (aref out (floor i 8))) 1))
	(return-from uniform-array->bytevector out)))
    (let* ((size (or (type-size type) (wrong-type "uniform-array->bytevector" 1 a)))
	   (n (dims-size dims))
	   (out (make-root (if (string= type "vu8") "vu8" type) n 0))
	   (i 0))
      (walk-indices dims (lambda (ix) (root-set out type i (array-element a ix "uniform-array->bytevector")) (incf i)))
      (remhash out *bytevector-types*)
      (if (= (length out) (* n size)) out (subseq out 0 (* n size))))))

(defguile "vector->list" (v &optional (start 0) end)
  ;; any rank-1 array, as Guile's
  (unless (and (guile-array-p v) (= 1 (length (nth-value 3 (array-view v "vector->list")))))
    (wrong-type "vector->list" 1 v))
  (let ((list (array->list* v)))
    (subseq list start end)))

;; (rnrs bytevectors) exports uniform-array->bytevector, which isn't in
;; the root module: libguile defines it in that module's extension.
(setf (gethash "uniform-array->bytevector" *extension-primitives*)
      (gethash "uniform-array->bytevector" *guile-primitives*))
(remhash "uniform-array->bytevector" *guile-primitives*)
