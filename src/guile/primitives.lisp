; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-GUILE -*-

;;;; More of libguile's primitives: lists and association lists, sorting,
;;;; numbers, symbols, strings, vectors, bit vectors and arrays, the
;;;; operating system, and the C halves of (ice-9 ports) and friends,
;;;; which Guile's modules install with load-extension.

(in-package "QUASISCHEME-GUILE")

(defun scheme-equal (a b) (qs:scheme-equal-p a b))
(defun scheme-eqv (a b) (or (eql a b) (and (numberp a) (numberp b) (= a b) (eq (type-of a) (type-of b)))))

;;; ------------------------------------------------------------------
;;; Lists

;; assq-ref and the other alist procedures: runtime.lisp
;; sloppy: an improper list, or anything but a list, just ends the search
(defun sloppy-ass (test key alist)
  (loop while (consp alist)
	do (let ((e (car alist)))
	     (when (and (consp e) (funcall test (car e) key)) (return e))
	     (setq alist (cdr alist)))
	finally (return qs:false)))
(defguile "sloppy-assq" (key alist) (sloppy-ass #'eq key alist))
(defguile "sloppy-assv" (key alist) (sloppy-ass #'scheme-eqv key alist))
(defguile "sloppy-assoc" (key alist) (sloppy-ass #'scheme-equal key alist))

(defguile "delq" (x list) (remove x list :test #'eq))
(defguile "delv" (x list) (remove x list :test #'scheme-eqv))
(defguile "delete" (x list &optional (eq qs:false))
  (if (truthy eq)
      (remove-if (lambda (y) (truthy (funcall eq x y))) list)
      (remove x list :test #'scheme-equal)))
(defguile "delq!" (x list) (delete x list :test #'eq))
(defguile "delv!" (x list) (delete x list :test #'scheme-eqv))
(defguile "delete!" (x list &optional (eq qs:false))
  (if (truthy eq)
      (delete-if (lambda (y) (truthy (funcall eq x y))) list)
      (delete x list :test #'scheme-equal)))
(defguile "delq1!" (x list) (delete x list :test #'eq :count 1))
(defguile "delv1!" (x list) (delete x list :test #'scheme-eqv :count 1))
(defguile "delete1!" (x list) (delete x list :test #'scheme-equal :count 1))
(defguile "list-head" (list k) (subseq list 0 k))
(defguile "list-copy" (list) (copy-list list))
(defguile "list-cdr-ref" (list k) (nthcdr k list))
(defguile "list-cdr-set!" (list k v) (setf (cdr (nthcdr k list)) v) *unspecified*)
(defguile "list-set!" (list k v) (setf (nth k list) v) *unspecified*)
(defguile "reverse!" (list &optional (tail '())) (nreconc list tail))
(defguile "filter" (pred list) (remove-if-not (lambda (x) (truthy (funcall pred x))) list))
(defguile "filter!" (pred list) (delete-if-not (lambda (x) (truthy (funcall pred x))) list))
(defguile "make-list" (n &optional (fill (quote ()))) (make-list n :initial-element fill))  ; Guile fills with ()

;;; ------------------------------------------------------------------
;;; Sorting: (sort sequence less), lists and vectors

(defun guile-less (less) (lambda (a b) (truthy (funcall less a b))))

(defun sort-elements (seq less who)
  "SEQ's elements, a list or a rank-1 array's, sorted (stably) by LESS."
  (stable-sort (if (listp seq) (copy-list seq) (array->list* (check-sortable seq who))) (guile-less less)))

(defun check-sortable (seq who)
  (unless (and (guile-array-p seq) (= 1 (length (nth-value 3 (array-view seq who)))))
    (wrong-type who 1 seq))
  seq)

(defun store-elements (array elements)
  "Put ELEMENTS into ARRAY, a rank-1 array, in index order."
  (destructuring-bind (lo hi stride) (first (nth-value 3 (array-view array "sort!")))
    (declare (ignore hi stride))
    (loop for x in elements for i from lo do (set-array-element array x (list i) "sort!")))
  array)

(defguile "sort" (seq less)
  (let ((sorted (sort-elements seq less "sort")))
    (if (listp seq)
	sorted
	(multiple-value-bind (root type offset dims) (array-view seq "sort")
	  (declare (ignore root offset))
	  (store-elements (make-typed-array* (type-symbol type) *unspecified*
					      (list (list (first (first dims)) (second (first dims)))))
			  sorted)))))

(defguile "sort!" (seq less)
  ;; a vector literal of compiled code is constant
  (when (read-only-vector-p seq)
    (guile-error (ssym "wrong-type-arg") "sort!" "Wrong type argument in position ~A: ~S"
		 (list 1 seq) (list seq)))
  (if (listp seq)
      (stable-sort seq (guile-less less))
      (store-elements seq (sort-elements seq less "sort!"))))

(setf (gethash "stable-sort" *guile-primitives*) (gethash "sort" *guile-primitives*)
      (gethash "stable-sort!" *guile-primitives*) (gethash "sort!" *guile-primitives*)
      (gethash "sort-list" *guile-primitives*) (gethash "sort" *guile-primitives*)
      (gethash "sort-list!" *guile-primitives*) (gethash "sort!" *guile-primitives*))
(defguile "merge" (a b less)
  (merge (if (listp a) 'list 'vector) (copy-seq a) (copy-seq b) (guile-less less)))
(defguile "merge!" (a b less)
  (merge (if (listp a) 'list 'vector) a b (guile-less less)))
(defguile "sorted?" (seq less)
  (let ((list (if (listp seq) seq (array->list* (check-sortable seq "sorted?")))))
    (bool (loop for tail on list
		while (cdr tail)
		never (truthy (funcall less (cadr tail) (car tail)))))))
(defguile "restricted-vector-sort!" (v less start end)
  (setf (subseq v start end) (stable-sort (subseq v start end) (guile-less less)))
  *unspecified*)

;;; ------------------------------------------------------------------
;;; Numbers

(defun euclid (n d)
  (multiple-value-bind (q r) (floor n d)
    (if (minusp r) (values (1+ q) (- r d)) (values q r))))
;;; Guile's six integer divisions, each as KIND/, KIND-quotient and
;;; KIND-remainder.  Exact operands give exact results; an inexact one
;;; gives inexact results, infinities and NaNs going through as IEEE
;;; arithmetic takes them (SBCL's FFLOOR and friends trap on those).

(defun float-round-to (kind x)
  "X, a double, rounded to an integer as KIND rounds; an infinity or a
NaN is itself."
  (if (or (sb-ext:float-infinity-p x) (sb-ext:float-nan-p x))
      x
      (ecase kind
	(:floor (ffloor x))
	(:ceiling (fceiling x))
	(:truncate (ftruncate x))
	(:round (fround x)))))

(defun integer-round-to (kind n d)
  (ecase kind
    (:floor (floor n d))
    (:ceiling (ceiling n d))
    (:truncate (truncate n d))
    (:round (round n d))))

(defun guile-division (kind n d)
  "The quotient and remainder of N by D, the quotient rounded as KIND:
:floor, :ceiling, :truncate, :round, :euclidean (remainder >= 0) or
:centered (-|d|/2 <= remainder < |d|/2)."
  (when (zerop d)
    ;; exact or inexact, as Guile's
    (guile-error (ssym "numerical-overflow") (format nil "~(~A~)/" kind) "Numerical overflow" '()))
  (if (and (rationalp n) (rationalp d))
      (let ((q (ecase kind
		 ((:floor :ceiling :truncate :round) (values (integer-round-to kind n d)))
		 (:euclidean (values (if (plusp d) (floor n d) (ceiling n d))))
		 (:centered (if (plusp d)
				(values (floor (+ (/ n d) 1/2)))
				(values (ceiling (- (/ n d) 1/2))))))))
	(values q (- n (* q d))))
      (let* ((x (coerce n 'double-float))
	     (y (coerce d 'double-float))
	     (ratio (/ x y))
	     (q (ecase kind
		  ((:floor :ceiling :truncate :round) (float-round-to kind ratio))
		  (:euclidean (float-round-to (if (plusp y) :floor :ceiling) ratio))
		  (:centered (if (plusp y)
				 (float-round-to :floor (+ ratio 0.5d0))
				 (float-round-to :ceiling (- ratio 0.5d0)))))))
	(values q (- x (* q y))))))

(macrolet ((divisions (&rest kinds)
	     `(progn
		,@(loop for kind in kinds
			for name = (string-downcase (symbol-name kind))
			append `((defguile ,(format nil "~A/" name) (n d) (guile-division ,kind n d))
				 (defguile ,(format nil "~A-quotient" name) (n d)
				   (values (guile-division ,kind n d)))
				 (defguile ,(format nil "~A-remainder" name) (n d)
				   (nth-value 1 (guile-division ,kind n d))))))))
  (divisions :floor :ceiling :truncate :round :euclidean :centered))

;;; Transcendental and other numeric procedures where Guile's results
;;; at the edges (zero, infinities, NaNs, huge exact numbers) differ from
;;; the host's.

(defvar +nan+ (sb-kernel:make-double-float #x7FF80000 0) "+nan.0")

(defun numerical-overflow (who)
  (guile-error (ssym "numerical-overflow") who "Numerical overflow" '()))

(defun host-call (name &rest args) (apply (qsx:host-ref name) args))

(defun host-function-cached (name cell)
  "The host's NAME, kept in CELL (a call site's) once looked up: looking
it up interns the name."
  (or (car cell) (setf (car cell) (qsx:host-ref name))))

(define-compiler-macro host-call (&whole whole name &rest args)
  (if (stringp name)
      `(funcall (the function (host-function-cached ,name (load-time-value (list nil)))) ,@args)
      whole))

(defun exact-zero-p (x) (eql x 0))

(defun complex-integer-expt (z k)
  "Complex Z to the integer K, by multiplying, as Guile's integer-expt:
no rounding error a logarithm would bring (0+bi squared is real)."
  (if (minusp k)
      (guile-divide 1 (complex-integer-expt z (- k)))
      (let ((result 1) (base z))
	(loop while (plusp k)
	      do (when (oddp k) (setq result (* result base)))
		 (setq k (ash k -1))
		 (when (plusp k) (setq base (* base base))))
	result)))

(defguile "integer-expt" (n k)
  (unless (integerp k) (wrong-type "integer-expt" 2 k))
  (cond ((zerop k) 1)
	((and (zerop n) (minusp k)) +nan+)
	((complexp n) (complex-integer-expt n k))
	(t (expt n k))))

(defguile "expt" (z w)
  (cond ((exact-zero-p w) 1)
	((and (zerop z) (realp w) (minusp w)) +nan+)
	;; a complex to an integer, even an inexact one: by multiplying
	((and (complexp z) (integerp w)) (complex-integer-expt z w))
	((and (complexp z) (floatp w) (= w (ffloor w)) (< (abs w) (expt 2 53)))
	 (complex-integer-expt z (round w)))
	(t (host-call "expt" z w))))

(defguile "modulo-expt" (n k m)
  (let ((base (mod n m)))
    (when (minusp k)
      ;; the inverse of N modulo M, raised to -K
      (multiple-value-bind (g inverse) (extended-gcd base m)
	(unless (= g 1) (numerical-overflow "modulo-expt"))
	(setq base (mod inverse m) k (- k))))
    (let ((result (mod 1 m)))
      (loop while (plusp k)
	    do (when (oddp k) (setq result (mod (* result base) m)))
	       (setq base (mod (* base base) m) k (ash k -1)))
      result)))

(defun extended-gcd (a b)
  "(gcd a b) and x such that a x = gcd (mod b)."
  (let ((r0 a) (r1 b) (s0 1) (s1 0))
    (loop until (zerop r1)
	  do (let ((q (floor r0 r1)))
	       (psetq r0 r1 r1 (- r0 (* q r1)))
	       (psetq s0 s1 s1 (- s0 (* q s1)))))
    (values (abs r0) (if (minusp r0) (- s0) s0))))

(defun log-of-positive-rational (x)
  "The natural log of X, a positive rational, as a double, even when X is
past the range of doubles."
  (flet ((log-integer (n)
	   (let ((shift (max 0 (- (integer-length n) 64))))
	     (+ (log (coerce (ash n (- shift)) 'double-float))
		(* shift (log 2d0))))))
    (- (log-integer (numerator x)) (log-integer (denominator x)))))

(defun guile-log (who z)
  (cond ((exact-zero-p z) (numerical-overflow who))
	((nan-p z) +nan+)
	((and (floatp z) (zerop z))
	 (if (minusp (float-sign z))
	     (complex sb-ext:double-float-negative-infinity pi)
	     sb-ext:double-float-negative-infinity))
	((rationalp z)
	 (let ((magnitude (log-of-positive-rational (abs z))))
	   (if (minusp z) (complex magnitude pi) magnitude)))
	(t (host-call "log" z))))

(defguile "log" (z) (guile-log "log" z))
(defguile "log10" (z)
  (let ((l (guile-log "log10" z)))
    (if (or (nan-p l) (and (floatp l) (sb-ext:float-infinity-p l)))
	l
	(/ l (log 10d0)))))

(defun sqrt-of-nonnegative (x)
  "The square root of X, a nonnegative real: exact if X is an exact
square, else the nearest double, however big or small X is."
  (cond ((floatp x)
	 (if (and (plusp x) (< x least-positive-normalized-double-float))
	     (/ (sqrt (* x (expt 2d0 108))) (expt 2d0 54))
	     (sqrt x)))
	(t (let* ((n (numerator x)) (d (denominator x))
		  (rn (isqrt n)) (rd (isqrt d)))
	     (if (and (= (* rn rn) n) (= (* rd rd) d))
		 (/ rn rd)
		 ;; sqrt (n/d) = isqrt (n 4^s / d) / 2^s, with 64 bits or so
		 (let* ((s (max 0 (ceiling (- 128 (- (integer-length n) (integer-length d))) 2)))
			(q (floor (ash n (* 2 s)) d)))
		   (scale-float (coerce (isqrt q) 'double-float) (- s))))))))

(defguile "sqrt" (z)
  (cond ((nan-p z) z)
	((and (realp z) (or (plusp z) (zerop z))) (sqrt-of-nonnegative z))
	((realp z)
	 (let ((root (sqrt-of-nonnegative (- z))))
	   (complex 0d0 (coerce root 'double-float))))
	(t (host-call "sqrt" z))))

(defun guile-divide (x y)
  "X over Y, a complex by Smith's algorithm, as Guile divides: (/ 0-1i) is
0.0+1.0i, where the textbook formula gives -0.0+1.0i."
  (if (not (complexp y))
      (host-call "/" x y)
      (let* ((a (realpart y)) (b (imagpart y))
	     (xr (realpart x)) (xi (imagpart x)))
	(when (realp x)
	  ;; a real has no imaginary part to add (an exact 0 would turn a
	  ;; -0.0 into 0.0)
	  (return-from guile-divide
	    (if (>= (abs a) (abs b))
		(let* ((r (/ b a)) (d (+ a (* b r))))
		  (complex (/ x d) (/ (- (* x r)) d)))
		(let* ((r (/ a b)) (d (+ (* a r) b)))
		  (complex (/ (* x r) d) (/ (- x) d))))))
	(if (>= (abs a) (abs b))
	    (let* ((r (/ b a)) (d (+ a (* b r))))
	      (complex (/ (+ xr (* xi r)) d) (/ (- xi (* xr r)) d)))
	    (let* ((r (/ a b)) (d (+ (* a r) b)))
	      (complex (/ (+ (* xr r) xi) d) (/ (- (* xi r) xr) d)))))))

(defguile "/" (x &rest ys)
  (when (if ys (some #'exact-zero-p ys) (exact-zero-p x))
    (numerical-overflow "divide"))
  (cond ((not (or (complexp x) (some #'complexp ys))) (apply #'host-call "/" x ys))
	((null ys) (guile-divide 1 x))
	(t (let ((q x)) (dolist (y ys q) (setq q (guile-divide q y)))))))

(defguile "inexact->exact" (z)
  (when (or (nan-p z) (and (floatp z) (sb-ext:float-infinity-p z)))
    (out-of-range "inexact->exact" z))
  (host-call "exact" z))

(defguile "string->number" (string &optional (radix 10))
  (guile-string->number string radix))

(defun guile-string->number (string radix)
  (unless (stringp string) (wrong-type "string->number" 1 string))
  (let ((at (position #\@ string)))
    (if (and at (not (find #\@ string :start (1+ at))))
	(polar-string->number string at radix)
	(let ((n (guile-string->number-1 string radix)))
	  ;; Guile has no exact complex numbers: +i is 0.0+1.0i
	  (if (numberp n) (guile-number n) n)))))

(defun polar-string->number (string at radix)
  "STRING, magnitude@angle, as Guile reads it: an exact zero magnitude is
0, an exact zero angle the magnitude; -0 is -0.0 under #i."
  (let* ((prefix-end (loop with i = 0
			   while (and (< (1+ i) at) (char= (char string i) #\#)) do (incf i 2)
			   finally (return i)))
	 (prefix (subseq string 0 prefix-end))
	 (inexact (search "#i" prefix :test #'char-equal))
	 (mag-text (subseq string prefix-end at))
	 (ang-text (subseq string (1+ at)))
	 (mag (guile-string->number-1 (concatenate 'string prefix mag-text) radix))
	 (ang (guile-string->number-1 (concatenate 'string prefix ang-text) radix)))
    (unless (and (realp mag) (realp ang) (plusp (length mag-text)) (plusp (length ang-text))
		 ;; reals only: 12@12+0i isn't a number
		 (not (char-equal (char mag-text (1- (length mag-text))) #\i))
		 (not (char-equal (char ang-text (1- (length ang-text))) #\i)))
      (return-from polar-string->number qs:false))
    (when (and inexact (zerop ang) (char= (char ang-text 0) #\-))
      (setq ang -0d0))
    (cond ((eql mag 0) 0)
	  ((eql ang 0) mag)
	  ((and (zerop mag) (or (nan-p ang) (and (floatp ang) (sb-ext:float-infinity-p ang))))
	   (complex (float mag 1d0) 0d0))
	  (t (let ((m (float mag 1d0)) (a (float ang 1d0)))
	       (complex (* m (cos a)) (* m (sin a))))))))

(defun guile-string->number-1 (string radix)
  ;; R5RS's # for a digit not known ("2#" is 20.0), which Guile still
  ;; reads: each # after a digit is a 0, no digit follows one in the
  ;; same digits ("5#.0" isn't a number), and the number is inexact
  (let ((digits (let ((s (copy-seq string)) (after-digit nil) (hashed nil))
		  (dotimes (i (length s) s)
		    (let ((c (char s i)))
		      (cond ((digit-char-p c radix)
			     (when hashed (return-from guile-string->number-1 qs:false))
			     (setq after-digit t))
			    ((and (char= c #\#) after-digit) (setf (char s i) #\0 hashed t))
			    ((char= c #\.))
			    (t (setq after-digit nil hashed nil))))))))
    (if (string= digits string)
	(let ((n (host-call "string->number" string radix)))
	  ;; an exponent past the range of doubles
	  (when (and (floatp n) (sb-ext:float-infinity-p n) (not (search "inf" string :test #'char-equal)))
	    (out-of-range "string->number" string))
	  n)
	(let ((n (host-call "string->number" digits radix)))
	  (cond ((not (numberp n)) qs:false)
		((search "#e" string :test #'char-equal) n) ; exact, as asked
		((rationalp n) (coerce n 'double-float))
		(t n))))))

(defguile "make-rectangular" (re im)
  (unless (realp re) (wrong-type "make-rectangular" 1 re))
  (unless (realp im) (wrong-type "make-rectangular" 2 im))
  (if (exact-zero-p im)
      re
      (complex (coerce re 'double-float) (coerce im 'double-float))))
(defun simple-upcase (c) (or (cl-unicode:uppercase-mapping c) c))
(defun simple-downcase (c) (or (cl-unicode:lowercase-mapping c) c))

;; case mapping a character at a time (Unicode's simple mappings), as
;; Guile's: no final sigma, and ß stays ß; START and END name the part
;; mapped, the rest kept
(macrolet ((case-mapping (name mapper in-place)
	     `(defguile ,name (s &optional (start 0) (end (length s)))
		(unless (stringp s) (wrong-type ,name 1 s))
		(unless (and (integerp start) (<= 0 start (length s))) (out-of-range ,name start))
		(unless (and (integerp end) (<= start end (length s))) (out-of-range ,name end))
		(let ((result ,(if in-place 's '(copy-seq s))))
		  (loop for i from start below end
			do (setf (char result i) (,mapper (char result i))))
		  ,(if in-place '*unspecified* 'result)))))
  (case-mapping "string-upcase" simple-upcase nil)
  (case-mapping "string-downcase" simple-downcase nil)
  (case-mapping "string-upcase!" simple-upcase t)
  (case-mapping "string-downcase!" simple-downcase t))

;; case folding a character at a time (Unicode's simple folding), as
;; Guile's: a final sigma is σ, and ß stays ß
(defguile "string-foldcase" (s)
  (unless (stringp s) (wrong-type "string-foldcase" 1 s))
  (map 'string (lambda (c)
		 (let ((folded (sb-unicode:casefold (string c))))
		   (if (= (length folded) 1) (char folded 0) c)))
       s))

;; a real's angle is inexact, as Guile's: 0.0, or pi if its sign is
;; negative (-0.0's too)
(defguile "angle" (z)
  (cond ((rationalp z) (if (minusp z) pi 0d0))
	((floatp z) (if (and (not (nan-p z)) (minusp (float-sign z))) pi 0d0))
	(t (host-call "angle" z))))

(defguile "make-polar" (magnitude angle)
  (cond ((exact-zero-p magnitude) 0)
	((exact-zero-p angle) magnitude)
	(t (host-call "make-polar" magnitude angle))))

(defun real-argument (who x)
  (unless (realp x) (wrong-type who 1 x))
  x)
(defguile "finite?" (x)
  (real-argument "finite?" x)
  (bool (not (and (floatp x) (or (nan-p x) (sb-ext:float-infinity-p x))))))
(defguile "nan?" (x) (real-argument "nan?" x) (bool (nan-p x)))
(defguile "sinh" (x) (sinh x))
(defguile "cosh" (x) (cosh x))
(defguile "tanh" (x) (tanh x))
(defguile "asinh" (x) (asinh x))
(defguile "acosh" (x) (acosh x))
(defguile "atanh" (x) (atanh x))
(defguile "inf?" (x)
  (real-argument "inf?" x)
  (bool (and (floatp x) (sb-ext:float-infinity-p x))))

(defvar *guile-random-state* (make-random-state t))
(defguile "random" (n &optional (state *guile-random-state*))
  (random n (if (random-state-p state) state *guile-random-state*)))
(defguile "random:uniform" (&optional (state *guile-random-state*))
  (random 1d0 (if (random-state-p state) state *guile-random-state*)))
(defguile "random:normal" (&optional (state *guile-random-state*))
  (let ((s (if (random-state-p state) state *guile-random-state*)))
    (* (sqrt (* -2 (log (- 1d0 (random 1d0 s))))) (cos (* 2 pi (random 1d0 s))))))
(defguile "random:exp" (&optional (state *guile-random-state*))
  (- (log (- 1d0 (random 1d0 (if (random-state-p state) state *guile-random-state*))))))
(defguile "copy-random-state" (&optional (state *guile-random-state*)) (make-random-state state))
(defguile "seed->random-state" (seed)
  (sb-ext:seed-random-state (cond ((stringp seed) (sxhash seed))
				  ((integerp seed) (abs seed))
				  ((realp seed) (abs (truncate seed)))
				  (t (wrong-type "seed->random-state" 1 seed)))))
(defguile "random-state-from-platform" () (make-random-state t))
;; a state as a datum: the generator's words, as a vector
(defguile "random-state->datum" (state)
  (unless (random-state-p state) (wrong-type "random-state->datum" 1 state))
  (coerce (sb-kernel::random-state-state state) 'simple-vector))
(defguile "datum->random-state" (datum)
  (let ((state (make-random-state t))
	(words (sb-kernel::random-state-state (make-random-state t))))
    (unless (and (simple-vector-p datum) (= (length datum) (length words)))
      (wrong-type "datum->random-state" 1 datum))
    (replace (sb-kernel::random-state-state state) datum)
    state))

;;; ------------------------------------------------------------------
;;; Symbols

(defguile "make-symbol" (name) (make-symbol (qs::invert-case name)))
(defguile "symbol-interned?" (s) (bool (symbol-package s)))
(defguile "string-ci->symbol" (s) (ssym (string-downcase s)))
(defguile "symbol-pref" (s) (get s 'guile-pref qs:false))
(defguile "symbol-pset!" (s v) (setf (get s 'guile-pref) v) *unspecified*)
(defguile "symbol-fref" (s) (get s 'guile-fref qs:false))
(defguile "symbol-fset!" (s v) (setf (get s 'guile-fref) v) *unspecified*)

;;; ------------------------------------------------------------------
;;; Strings (most are SRFI 13's: see GUILE-LIBRARY-BINDINGS)

(defun char-matcher (x)
  (cond ((characterp x) (lambda (c) (char= c x)))
	((functionp x) (lambda (c) (truthy (funcall x c))))
	(t (let ((contains (library-value "(srfi 14)" "char-set-contains?")))
	     (lambda (c) (truthy (funcall contains x c)))))))

(defguile "string-split" (s pred)
  (let ((match (char-matcher pred)) (parts '()) (start 0))
    (loop for i from 0 below (length s)
	  when (funcall match (char s i))
	    do (push (subseq s start i) parts) (setq start (1+ i)))
    (push (subseq s start) parts)
    (nreverse parts)))
(defguile "string-rindex" (s pred &optional (start 0) (end (length s)))
  (let ((match (char-matcher pred)))
    (or (loop for i from (1- end) downto start when (funcall match (char s i)) return i)
	qs:false)))
(defguile "substring" (s start &optional end)
  ;; checked in Guile's order: the string, then the indices
  (unless (stringp s) (wrong-type "substring" 1 s))
  (unless (and (integerp start) (<= 0 start (length s))) (out-of-range "substring" start))
  (let ((end (if (or (null end) (eq end +unbound+)) (length s) end)))
    (unless (and (integerp end) (<= start end (length s))) (out-of-range "substring" end))
    (subseq s start end)))
(defguile "substring/copy" (s start &optional (end (length s))) (subseq s start end))
;;; libguile's debugging dumps of a string's and a symbol's buffer, as
;;; they are here: a string is its own buffer, "shared" if it's a shared
;;; substring (see SUBSTRING/SHARED), mutable once it has been changed;
;;; "wide" if a character is past Latin-1.

(defun wide-string-p (s) (some (lambda (c) (> (char-code c) 255)) s))

(defguile "%string-dump" (s)
  (unless (stringp s) (wrong-type "%string-dump" 1 s))
  (flet ((entry (key value) (cons (ssym key) value)))
    (progn
      (list (entry "string" s) (entry "start" 0) (entry "length" (length s))
	  (entry "shared" (let ((base (gethash s *shared-substrings*))) (if base (car base) qs:false)))
	  (entry "read-only" (bool (read-only-string-p s)))
	  (entry "stringbuf" s) (entry "stringbuf-chars" (copy-seq s))
	  (entry "stringbuf-length" (length s)) (entry "stringbuf-shared" qs:false)
	  (entry "stringbuf-mutable" (bool (gethash s *mutated-strings*)))
	  (entry "stringbuf-wide" (bool (wide-string-p s)))))))

(defguile "%symbol-dump" (sym)
  (unless (qs:scheme-symbol-p sym) (wrong-type "%symbol-dump" 1 sym))
  (let ((name (qs:scheme-symbol-name sym)))
    (flet ((entry (key value) (cons (ssym key) value)))
      (list (entry "symbol" sym) (entry "hash" (sxhash name)) (entry "interned" qs:true)
	    (entry "stringbuf" name) (entry "stringbuf-chars" (copy-seq name))
	    (entry "stringbuf-length" (length name)) (entry "stringbuf-shared" qs:false)
	    (entry "stringbuf-wide" (bool (wide-string-p name)))))))

;;; String comparisons: any number of strings, as Guile's (the host's
;;; take string designators, such as symbols).
(macrolet ((comparisons (&rest pairs)
	     `(progn
		,@(loop for (name test) in pairs
			collect `(defguile ,name (&rest strings)
				   (loop for x in strings for i from 1
					 unless (stringp x) do (wrong-type ,name i x))
				   (bool (loop for (a b) on strings while b
					       always (,test a b))))))))
  (comparisons ("string=?" string=) ("string<?" string<) ("string>?" string>)
	       ("string<=?" string<=) ("string>=?" string>=)
	       ("string-ci=?" string-equal) ("string-ci<?" string-lessp)
	       ("string-ci>?" string-greaterp) ("string-ci<=?" string-not-greaterp)
	       ("string-ci>=?" string-not-lessp)))

;;; Read-only strings: literals in compiled code (strings of a VM image),
;;; substring/read-only's and symbol->string's.  The root's string
;;; mutators check (WRAP-STRING-MUTATORS, as the root is installed).

(defvar *read-only-strings* (trivial-garbage:make-weak-hash-table :weakness :key :test 'eq :synchronized t))

(defvar *last-mutated-string* nil
  "The string a mutator last changed, if it is mutable, marked mutated,
and shares characters with no other string: its next change needs none
of the weak tables' lookups (each takes a lock).  Forgotten whenever a
string is made read-only or shared (READ-ONLY-STRING, SUBSTRING/SHARED).")

(defun read-only-string (s)
  (setq *last-mutated-string* nil)
  (setf (gethash s *read-only-strings*) t)
  s)

(defun read-only-string-p (s) (and (stringp s) (gethash s *read-only-strings*)))

;;; Vector literals of compiled code are constant too: (compile '(sort!
;;; #(0) <)) is an error.  They're in the same table; only sort! and
;;; stable-sort! check (vector-set! is open-coded).

(defun read-only-vector (v)
  (setf (gethash v *read-only-strings*) t)
  v)

(defun read-only-vector-p (v) (and (simple-vector-p v) (gethash v *read-only-strings*)))

(defun check-mutable-string (s)
  (when (read-only-string-p s)
    (guile-error (ssym "misc-error") qs:false "string is read-only: ~s" (list s))))

(defparameter +string-mutators+
  '(("string-set!" . 0) ("string-fill!" . 0) ("substring-fill!" . 0) ("string-copy!" . 0)
    ("substring-move!" . 3) ("string-upcase!" . 0) ("string-downcase!" . 0)
    ("string-capitalize!" . 0) ("string-titlecase!" . 0) ("string-reverse!" . 0)
    ("string-map!" . 1) ("string-xcopy!" . 0))
  "The root's procedures that change a string, and which argument it is.")

(defun read-only-bytevector (bv)
  (setf (gethash bv *read-only-strings*) t)
  bv)

(defun read-only-bytevector-p (bv)
  (and (typep bv '(array (unsigned-byte 8) (*))) (gethash bv *read-only-strings*)))

(defparameter +bytevector-mutators+
  '("bytevector-u8-set!" "bytevector-s8-set!" "bytevector-fill!" "bytevector-copy!")
  "Bytevector mutators that refuse a read-only bytevector (a literal of
compiled code), when called rather than open-coded.")

(defun wrap-bytevector-mutator (variable name)
  "Make the procedure in VARIABLE, bytevector mutator NAME, refuse a
read-only bytevector."
  (let ((f (and variable (gvariable-value variable))))
    (when (and (functionp f) (not (gethash f *checking-mutators*)))
      (let ((checking (lambda (bv &rest args)
			(let ((target (if (string= name "bytevector-copy!") (second args) bv)))
			  (when (read-only-bytevector-p target)
			    (guile-error (ssym "wrong-type-arg") name "Wrong type argument in position ~A: ~S"
					 (list 1 target) (list target))))
			(apply f bv args))))
	(setf (gethash checking *checking-mutators*) t
	      (gvariable-value variable) checking)))))

(defvar *checking-mutators* (trivial-garbage:make-weak-hash-table :weakness :key :test 'eq))

(defun wrap-string-mutators ()
  "Make the root's string mutators refuse read-only strings, and its
bytevector mutators read-only bytevectors."
  (dolist (name +bytevector-mutators+)
    (wrap-bytevector-mutator (obarray-variable (ssym name)) name))
  (loop for (name . position) in +string-mutators+
	do (let* ((v (obarray-variable (ssym name)))
		  (f (and v (gvariable-value v)))
		  (position position))
	     (when (functionp f)
	       (setf (gvariable-value v)
		     (flet ((after (s)
			      (when (stringp s)
				(string-changed s)
				(unless (or (gethash s *shared-substrings*) (gethash s *string-sharers*))
				  (setq *last-mutated-string* s)))))
		       (if (string= name "string-set!")
			   ;; per character: no argument list
			   (lambda (s k c)
			     (if (eq s *last-mutated-string*)
				 (funcall f s k c)
				 (progn (check-mutable-string s)
					(multiple-value-prog1 (funcall f s k c) (after s)))))
			   (lambda (&rest args)
			     (let ((s (nth position args)))
			       (if (eq s *last-mutated-string*)
				   (apply f args)
				   (progn (check-mutable-string s)
					  (multiple-value-prog1 (apply f args) (after s))))))))))))
  (let ((v (obarray-variable (ssym "substring/shared"))))
    (when v (setf (gvariable-value v) #'substring/shared))))

;;; Shared substrings.  A Scheme string is a simple Lisp string, which
;;; can't be a window on another, so substring/shared copies, and
;;; remembers where from: after a root mutator changes a string, the
;;; change is copied to the strings it shares characters with, the one
;;; it is a substring of and those that are substrings of it, and so on.
;;; (A string changed by vector-style open-coded code isn't seen.)  And a
;;; string's buffer is "mutable", as %string-dump reports it, once it
;;; has been changed.

(defvar *shared-substrings* (trivial-garbage:make-weak-hash-table :weakness :key :test 'eq :synchronized t)
  "A shared substring -> (string . start).")
(defvar *string-sharers* (trivial-garbage:make-weak-hash-table :weakness :key :test 'eq :synchronized t)
  "A string -> weak pointers to its shared substrings.")
(defvar *mutated-strings* (trivial-garbage:make-weak-hash-table :weakness :key :test 'eq :synchronized t))

(defun substring/shared (s start &optional end)
  (unless (stringp s) (wrong-type "substring/shared" 1 s))
  (unless (and (integerp start) (<= 0 start (length s))) (out-of-range "substring/shared" start))
  (let ((end (if (or (null end) (eq end +unbound+)) (length s) end)))
    (unless (and (integerp end) (<= start end (length s))) (out-of-range "substring/shared" end))
    (if (and (= start 0) (= end (length s)))
	s
	(let ((sub (subseq s start end)))
	  (setq *last-mutated-string* nil)
	  (setf (gethash sub *shared-substrings*) (cons s start))
	  (push (trivial-garbage:make-weak-pointer sub) (gethash s *string-sharers*))
	  sub))))

(defun string-changed (s)
  "S was changed: so were the strings it shares characters with."
  (let ((done (list s)))
    (labels ((changed (s)
	       (setf (gethash s *mutated-strings*) t)
	       (let ((base (gethash s *shared-substrings*)))
		 (when (and base (not (member (car base) done)))
		   (push (car base) done)
		   (replace (car base) s :start1 (cdr base))
		   (changed (car base))))
	       (let ((live '()))
		 (dolist (p (gethash s *string-sharers*))
		   (let ((sub (trivial-garbage:weak-pointer-value p)))
		     (when sub
		       (push p live)
		       (unless (member sub done)
			 (push sub done)
			 (replace sub s :start2 (cdr (gethash sub *shared-substrings*)))
			 (changed sub)))))
		 (if live
		     (setf (gethash s *string-sharers*) live)
		     (remhash s *string-sharers*)))))
      (changed s))))

(defguile "substring/read-only" (s start &optional (end (length s)))
  (read-only-string (subseq s start end)))

(defvar *symbol-strings* (trivial-garbage:make-weak-hash-table :weakness :key :test 'eq :synchronized t)
  "Symbol -> its name, read-only, as symbol->string gives it each time.")

(defguile "symbol->string" (sym)
  (unless (qs:scheme-symbol-p sym) (wrong-type "symbol->string" 1 sym))
  (or (gethash sym *symbol-strings*)
      (setf (gethash sym *symbol-strings*) (read-only-string (copy-seq (qs:scheme-symbol-name sym))))))

(defguile "string-append/shared" (&rest strings) (apply #'concatenate 'string strings))
(defguile "string-bytes-per-char" (s) (if (every (lambda (c) (< (char-code c) 256)) s) 1 4))
(defguile "string-utf8-length" (s) (length (sb-ext:string-to-octets s :external-format :utf-8)))
(defguile "string-capitalize" (s) (string-capitalize s))
(defguile "string-capitalize!" (s) (nstring-capitalize s))
(defguile "string-copy!" (to at from &optional (start 0) (end (length from)))
  (replace to from :start1 at :start2 start :end2 end) *unspecified*)
(defguile "string-fill!" (s c &optional (start 0) (end (length s)))
  (fill s c :start start :end end) *unspecified*)
(defguile "substring-move!" (from start end to at)
  (unless (stringp from) (wrong-type "substring-move!" 1 from))
  (unless (stringp to) (wrong-type "substring-move!" 4 to))
  (unless (and (integerp start) (<= 0 start (length from))) (out-of-range "substring-move!" start))
  (unless (and (integerp end) (<= start end (length from))) (out-of-range "substring-move!" end))
  (unless (and (integerp at) (<= 0 at) (<= (+ at (- end start)) (length to)))
    (out-of-range "substring-move!" at))
  (replace to from :start1 at :start2 start :end2 end) *unspecified*)
(defguile "char-is-both?" (c) (bool (both-case-p c)))	; has case: upper or lower

;;; ------------------------------------------------------------------
;;; Vectors

(defun check-move (who from start end to at)
  (unless (and (integerp start) (integerp end) (<= 0 start end (length from)))
    (guile-error (ssym "out-of-range") who "Value out of range: ~S" (list start) (list start)))
  (unless (and (integerp at) (<= 0 at) (<= (+ at (- end start)) (length to)))
    (guile-error (ssym "out-of-range") who "Value out of range: ~S" (list at) (list at))))

;; element by element, from the left or from the right, as Guile's
(defguile "vector-move-left!" (from start end to at)
  (check-move "vector-move-left!" from start end to at)
  (loop for i from start below end for j from at do (setf (aref to j) (aref from i)))
  *unspecified*)
(defguile "vector-move-right!" (from start end to at)
  (check-move "vector-move-right!" from start end to at)
  (loop for i from (1- end) downto start for j downfrom (+ at (- end start 1)) do (setf (aref to j) (aref from i)))
  *unspecified*)
(defguile "vector-copy!" (to at from &optional (start 0) (end (length from)))
  (unless (simple-vector-p to) (wrong-type "vector-copy!" 1 to))
  (unless (simple-vector-p from) (wrong-type "vector-copy!" 3 from))
  (unless (and (integerp start) (<= 0 start (length from))) (out-of-range "vector-copy!" start))
  (unless (and (integerp end) (<= start end (length from))) (out-of-range "vector-copy!" end))
  (unless (and (integerp at) (<= 0 at) (<= (+ at (- end start)) (length to)))
    (out-of-range "vector-copy!" at))
  (replace to from :start1 at :start2 start :end2 end) *unspecified*)
(defguile "vector-fill!" (v x &optional (start 0) (end (length v)))
  (fill v x :start start :end end) *unspecified*)

;;; ------------------------------------------------------------------
;;; Bit vectors

(defguile "bitvector?" (x) (bool (typep x 'simple-bit-vector)))
(defguile "make-bitvector" (n &optional (fill qs:false))
  (make-array n :element-type 'bit :initial-element (if (truthy fill) 1 0)))
(defguile "bitvector" (&rest bits)
  (make-array (length bits) :element-type 'bit :initial-contents (mapcar (lambda (b) (if (truthy b) 1 0)) bits)))
(defguile "list->bitvector" (bits)
  (make-array (length bits) :element-type 'bit :initial-contents (mapcar (lambda (b) (if (truthy b) 1 0)) bits)))
(defguile "bitvector->list" (v) (map 'list (lambda (b) (bool (= b 1))) v))
(defguile "bitvector-length" (v) (length v))
(defguile "bitvector-ref" (v i) (bool (= 1 (sbit v i))))
(defguile "bitvector-bit-set?" (v i) (bool (= 1 (sbit v i))))
(defguile "bitvector-bit-clear?" (v i) (bool (= 0 (sbit v i))))
(defguile "bitvector-set!" (v i b) (setf (sbit v i) (if (truthy b) 1 0)) *unspecified*)
(defguile "bitvector-set-bit!" (v i) (setf (sbit v i) 1) *unspecified*)
(defguile "bitvector-clear-bit!" (v i) (setf (sbit v i) 0) *unspecified*)
(defguile "bitvector-fill!" (v b) (fill v (if (truthy b) 1 0)) *unspecified*)
(defguile "bitvector-set-all-bits!" (v) (fill v 1) *unspecified*)
(defguile "bitvector-clear-all-bits!" (v) (fill v 0) *unspecified*)
(defguile "bitvector-flip-all-bits!" (v) (bit-not v v) *unspecified*)
(defguile "bitvector-count" (v) (count 1 v))
(defguile "bitvector-copy" (v &optional (start 0) (end (length v))) (subseq v start end))
(defguile "bitvector-position" (v b &optional (start 0))
  (or (position (if (truthy b) 1 0) v :start start) qs:false))
(defguile "bit-count" (b v) (count (if (truthy b) 1 0) v))
(defguile "bit-position" (b v start) (or (position (if (truthy b) 1 0) v :start start) qs:false))
(defun bit-selection (who kv v)
  (declare (ignore who v))
  "The indices KV (a bitvector's set bits, or a u32vector's elements) select."
  (cond ((typep kv 'simple-bit-vector) (loop for i below (length kv) when (= 1 (sbit kv i)) collect i))
	((typep kv '(simple-array (unsigned-byte 32) (*))) (coerce kv 'list))
	(t (guile-error (ssym "wrong-type-arg") nil "Wrong type (expecting ~A): ~S" (list "bitvector or u32vector" kv)
			(list kv)))))
(defguile "bit-count*" (v kv b)
  (let ((bit (if (truthy b) 1 0)))
    (count-if (lambda (i) (= bit (sbit v i))) (bit-selection "bit-count*" kv v))))
(defguile "bit-set*!" (v kv b)
  (let ((bit (if (truthy b) 1 0)))
    (dolist (i (bit-selection "bit-set*!" kv v)) (setf (sbit v i) bit)))
  *unspecified*)
(defguile "bit-invert!" (v) (bit-not v v) *unspecified*)

;;; ------------------------------------------------------------------
;;; The operating system

(defun system-error (subr &optional (errno (sb-alien:extern-alien "errno" sb-alien:int)))
  (guile-error (ssym "system-error") subr "~A" (list (sb-int:strerror errno)) (list errno)))

(defguile "getenv" (name) (or (sb-posix:getenv name) qs:false))
(defguile "setenv" (name value)
  (if (truthy value) (sb-posix:setenv name value 1) (sb-posix:unsetenv name))
  *unspecified*)
(defguile "putenv" (string)
  (let ((i (position #\= string)))
    (if i
	(sb-posix:setenv (subseq string 0 i) (subseq string (1+ i)) 1)
	(sb-posix:unsetenv string)))
  *unspecified*)
(defguile "environ" (&optional env)
  (declare (ignore env))
  (sb-ext:posix-environ))
(defguile "getcwd" () (sb-posix:getcwd))
(defguile "chdir" (dir)
  ;; a directory port too (fchdir)
  (handler-case (if (gport-p dir)
		    (progn (unless (and (port-open-p dir) (port-fd dir)) (wrong-type "chdir" 1 dir))
			   (sb-posix:fchdir (port-fd dir)))
		    (sb-posix:chdir dir))
    (sb-posix:syscall-error (e) (system-error "chdir" (sb-posix:syscall-errno e))))
  (setf *default-pathname-defaults* (uiop:ensure-directory-pathname (sb-posix:getcwd)))
  *unspecified*)
(defguile "getpid" () (sb-posix:getpid))
(defguile "getppid" () (sb-posix:getppid))
(defguile "getuid" () (sb-posix:getuid))
(defguile "geteuid" () (sb-posix:geteuid))
(defguile "getgid" () (sb-posix:getgid))
(defguile "getegid" () (sb-posix:getegid))
(defguile "getlogin" () (or (sb-posix:getenv "USER") qs:false))
(defguile "gethostname" () (machine-instance))
(defguile "umask" (&optional mode) (if mode (sb-posix:umask mode) (let ((m (sb-posix:umask 0))) (sb-posix:umask m) m)))
(defguile "mkdir" (path &optional (mode #o777)) (sb-posix:mkdir path mode) *unspecified*)
(defguile "rmdir" (path) (sb-posix:rmdir path) *unspecified*)
(defguile "rename-file" (old new) (sb-posix:rename old new) *unspecified*)
(defguile "delete-file" (path) (sb-posix:unlink path) *unspecified*)
(defguile "link" (old new) (sb-posix:link old new) *unspecified*)
(defguile "symlink" (old new) (sb-posix:symlink old new) *unspecified*)
(defguile "readlink" (path) (sb-posix:readlink path))
(defguile "chmod" (path mode) (sb-posix:chmod path mode) *unspecified*)
(defguile "copy-file" (old new &key copy-on-write)
  (declare (ignore copy-on-write))
  ;; by descriptors, so a failure is its errno
  (let ((in (handler-case (sb-posix:open old sb-posix:o-rdonly)
	      (sb-posix:syscall-error (e) (system-error "copy-file" (sb-posix:syscall-errno e))))))
    (unwind-protect
	 (let ((out (handler-case (sb-posix:open new (logior sb-posix:o-wronly sb-posix:o-creat sb-posix:o-trunc)
						 (logand #o777 (sb-posix:stat-mode (sb-posix:fstat in))))
		      (sb-posix:syscall-error (e) (system-error "copy-file" (sb-posix:syscall-errno e))))))
	   (unwind-protect
		(let ((buf (make-array 65536 :element-type '(unsigned-byte 8))))
		  (sb-sys:with-pinned-objects (buf)
		    (loop for n = (sb-unix:unix-read in (sb-sys:vector-sap buf) 65536)
			  while (and n (plusp n))
			  do (sb-unix:unix-write out buf 0 n))))
	     (sb-posix:close out)))
      (sb-posix:close in)))
  *unspecified*)
(defguile "access?" (path how)
  (bool (handler-case (progn (sb-posix:access path how) t) (sb-posix:syscall-error () nil))))
(defguile "basename" (path &optional suffix)
  (let* ((p (string-right-trim "/" path))
	 (base (if (zerop (length p)) "/" (subseq p (1+ (or (position #\/ p :from-end t) -1))))))
    (if (and (stringp suffix) (> (length base) (length suffix))
	     (string= suffix base :start2 (- (length base) (length suffix))))
	(subseq base 0 (- (length base) (length suffix)))
	base)))
(defguile "dirname" (path)
  (let* ((p (string-right-trim "/" path))
	 (i (position #\/ p :from-end t)))
    (cond ((zerop (length p)) "/")
	  ((null i) ".")
	  ((zerop i) "/")
	  (t (string-right-trim "/" (subseq p 0 i))))))
(defguile "canonicalize-path" (path)
  (let ((p (probe-file path)))
    (unless p (system-error "canonicalize-path" sb-posix:enoent))
    (let ((s (namestring p))) (if (and (> (length s) 1) (char= (char s (1- (length s))) #\/)) (subseq s 0 (1- (length s))) s))))
(defguile "system-file-name-convention" () (ssym "posix"))
(defguile "file-encoding" (port) (declare (ignore port)) qs:false)

(defun stat-vector (st)
  (vector (sb-posix:stat-dev st) (sb-posix:stat-ino st) (sb-posix:stat-mode st)
	  (sb-posix:stat-nlink st) (sb-posix:stat-uid st) (sb-posix:stat-gid st)
	  (sb-posix:stat-rdev st) (sb-posix:stat-size st) (sb-posix:stat-atime st)
	  (sb-posix:stat-mtime st) (sb-posix:stat-ctime st) 4096
	  (ceiling (sb-posix:stat-size st) 512)
	  (ssym (let ((m (logand (sb-posix:stat-mode st) sb-posix:s-ifmt)))
		  (cond ((= m sb-posix:s-ifdir) "directory") ((= m sb-posix:s-iflnk) "symlink")
			((= m sb-posix:s-ifchr) "char-special") ((= m sb-posix:s-ifblk) "block-special")
			((= m sb-posix:s-ififo) "fifo") ((= m sb-posix:s-ifsock) "socket")
			(t "regular"))))
	  (logand (sb-posix:stat-mode st) #o7777)
	  0 0 0))
(defguile "stat" (path &optional (exception-on-error qs:true))
  (handler-case (stat-vector (cond ((integerp path) (sb-posix:fstat path))
				   ((streamp path) (sb-posix:fstat (port-fdes path "stat")))
				   (t (sb-posix:stat path))))
    (sb-posix:syscall-error (e)
      (if (truthy exception-on-error)
	  (system-error "stat" (sb-posix:syscall-errno e))
	  qs:false))))
(defguile "lstat" (path)
  (handler-case (stat-vector (sb-posix:lstat path))
    (sb-posix:syscall-error (e) (system-error "lstat" (sb-posix:syscall-errno e)))))

(defstruct (directory-stream (:constructor make-directory-stream (entries))) entries (open t))
(defguile "opendir" (path)
  (let ((d (sb-posix:opendir path)) (entries '()))
    (unwind-protect
	 (loop for ent = (sb-posix:readdir d)
	       until (sb-alien:null-alien ent)
	       do (push (sb-posix:dirent-name ent) entries))
      (sb-posix:closedir d))
    (make-directory-stream (nreverse entries))))
(defguile "readdir" (d)
  (if (directory-stream-entries d) (pop (directory-stream-entries d)) qs:eof-object))
(defguile "closedir" (d) (setf (directory-stream-open d) nil) *unspecified*)
(defguile "directory-stream?" (x) (bool (directory-stream-p x)))
(defguile "rewinddir" (d) (declare (ignore d)) *unspecified*)

(defun run-and-wait (prog args)
  "Run PROG with ARGS on the current ports' descriptors; its wait status
(that of an exit with 127 if it couldn't be run, as the shell's)."
  (flush-all-ports)
  (let ((pid (spawn-process prog args
			    (port-fd-or *standard-input* sb-posix:o-rdonly)
			    (port-fd-or *standard-output* sb-posix:o-wronly)
			    (port-fd-or *error-output* sb-posix:o-wronly))))
    (if (minusp pid)
	(ash 127 8)
	(nth-value 1 (sb-posix:waitpid pid 0)))))

(defguile "system" (&optional (command qs:false))
  (if (truthy command)
      (run-and-wait "/bin/sh" (list "-c" command))
      qs:true))
(defguile "system*" (prog &rest args) (run-and-wait prog args))
(defguile "status:exit-val" (status) (if (zerop (logand status #x7f)) (ash status -8) qs:false))
(defguile "status:term-sig" (status) (let ((s (logand status #x7f))) (if (zerop s) qs:false s)))
(defguile "status:stop-sig" (status) (declare (ignore status)) qs:false)
(defguile "sleep" (n) (sleep n) 0)
(defguile "usleep" (n) (sleep (/ n 1000000)) 0)
(defguile "gettimeofday" ()
  (multiple-value-bind (sec usec) (sb-ext:get-time-of-day) (cons sec usec)))
(defguile "times" ()
  (vector (get-internal-real-time) (get-internal-run-time) 0 0 0))
(defguile "uname" ()
  ;; libc's: sysname nodename release version machine
  (let ((width #+darwin 256 #-darwin 65))
    (cffi:with-foreign-object (u :char (* 6 width))
      (cffi:foreign-funcall "uname" :pointer u :int)
      (coerce (loop for i below 5
		    collect (cffi:foreign-string-to-lisp (cffi:inc-pointer u (* i width))))
	      'simple-vector))))
(defguile "isatty?" (port) (declare (ignore port)) qs:false)
(defguile "kill" (pid sig) (sb-posix:kill pid sig) *unspecified*)
(defguile "strerror" (n) (sb-int:strerror n))

;;; Broken-down time is libc's (struct tm: nine ints, then tm_gmtoff and
;;; tm_zone), with a zone given as TZ is for the call, as Guile's.

(defconstant +tm-size+ 64)

(defun call-with-tz (zone thunk)
  (if (stringp zone)
      (let ((old (sb-posix:getenv "TZ")))
	(sb-posix:setenv "TZ" zone 1)
	(cffi:foreign-funcall "tzset" :void)
	(unwind-protect (funcall thunk)
	  (if old (sb-posix:setenv "TZ" old 1) (sb-posix:unsetenv "TZ"))
	  (cffi:foreign-funcall "tzset" :void)))
      (funcall thunk)))

(defun tm->vector (tm)
  (let ((zone (cffi:mem-ref tm :pointer 48)))
    (vector (cffi:mem-aref tm :int 0) (cffi:mem-aref tm :int 1) (cffi:mem-aref tm :int 2)
	    (cffi:mem-aref tm :int 3) (cffi:mem-aref tm :int 4) (cffi:mem-aref tm :int 5)
	    (cffi:mem-aref tm :int 6) (cffi:mem-aref tm :int 7) (cffi:mem-aref tm :int 8)
	    ;; Guile's gmtoff is seconds west of UTC
	    (- (cffi:mem-ref tm :long 40))
	    (if (cffi:null-pointer-p zone) qs:false (cffi:foreign-string-to-lisp zone)))))

(defun vector->tm (v tm)
  (dotimes (i 9) (setf (cffi:mem-aref tm :int i) (svref v i)))
  (setf (cffi:mem-ref tm :long 40) (- (if (integerp (svref v 9)) (svref v 9) 0))))

(defun broken-down-time (function time zone)
  (cffi:with-foreign-objects ((tm :uint8 +tm-size+) (clock :long))
    (setf (cffi:mem-ref clock :long) time)
    (call-with-tz zone (lambda ()
			 (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer function) ()
						       :pointer clock :pointer tm :pointer)
			 (tm->vector tm)))))

(defguile "gmtime" (time) (broken-down-time "gmtime_r" time nil))
(defguile "localtime" (time &optional zone) (broken-down-time "localtime_r" time zone))

;;; ------------------------------------------------------------------
;;; Odds and ends

(defguile "defined?" (sym &optional (module qs:false))
  (let ((v (module-variable* (if (truthy module) module *current-module*) sym)))
    (bool (and v (not (eq (gvariable-value v) +unbound+))))))
(macrolet ((comparisons (&rest pairs)
	     `(progn
		,@(loop for (name test real) in pairs
			collect `(defguile ,name (&rest args)
				   ;; any number of arguments, as Guile's: (=) and (= x) are #t
				   (dolist (x args)
				     (unless (if ,real (realp x) (numberp x)) (wrong-type ,name 1 x)))
				   (bool (or (null (cdr args)) (apply #',test args))))))))
  (comparisons ("=" = nil) ("<" < t) (">" > t) ("<=" <= t) (">=" >= t)))

(defun all-adjacent (test args)
  (loop for tail on args while (cdr tail) always (funcall test (car tail) (cadr tail))))
(defguile "eq?" (&rest args) (bool (all-adjacent #'eq args)))
(defguile "eqv?" (&rest args) (bool (all-adjacent #'eql args)))
(defguile "equal?" (&rest args) (bool (all-adjacent #'qs:scheme-equal-p args)))

(defun nan-p (x) (and (floatp x) (sb-ext:float-nan-p x)))

(defun guile-extremum (name test args)
  "max or min as Guile's: a NaN wins, inexactness spreads, and -0.0 is
less than 0.0."
  (unless args (guile-error (ssym "wrong-number-of-args") name "Wrong number of arguments" '()))
  (dolist (x args) (unless (realp x) (wrong-type name 1 x)))
  (let ((nan (find-if #'nan-p args)))
    (if nan
	nan
	(let ((best (reduce (lambda (a b)
			      (cond ((funcall test b a) b)
				    ((and (= a b) (or (floatp a) (floatp b))
					  (funcall test (float-sign (float b 1d0)) (float-sign (float a 1d0))))
				     b)
				    (t a)))
			    args)))
	  (if (some #'floatp args) (coerce best 'double-float) best)))))

(defguile "max" (&rest args) (guile-extremum "max" #'> args))
(defguile "min" (&rest args) (guile-extremum "min" #'< args))

(defun float-integer-op (name op n d)
  "OP of N and D, integers; inexact if either is."
  (flet ((integral-p (x)
	   (or (integerp x)
	       (and (floatp x) (not (sb-ext:float-infinity-p x)) (not (nan-p x))
		    (= x (ftruncate x))))))
    (unless (integral-p n) (wrong-type name 1 n))
    (unless (integral-p d) (wrong-type name 2 d)))
  (when (zerop d) (guile-error (ssym "numerical-overflow") name "Numerical overflow" '()))
  (if (and (integerp n) (integerp d))
      (funcall op n d)
      (coerce (funcall op (round n) (round d)) 'double-float)))

(defguile "quotient" (n d) (float-integer-op "quotient" (lambda (a b) (values (truncate a b))) n d))
(defguile "remainder" (n d) (float-integer-op "remainder" #'rem n d))
(defguile "modulo" (n d) (float-integer-op "modulo" #'mod n d))

(defguile "numerator" (x)
  (cond ((rationalp x) (numerator x))
	((or (sb-ext:float-infinity-p x) (nan-p x) (zerop x)) x)
	(t (float (numerator (rational x)) x))))
(defguile "denominator" (x)
  (cond ((rationalp x) (denominator x))
	((sb-ext:float-infinity-p x) 1.0d0)
	((nan-p x) x)
	(t (float (denominator (rational x)) x))))

(defun proper-list-p (x)
  "Whether X is a finite list ending in () or Emacs Lisp's nil."
  (loop for slow = x then (cdr slow)
	for fast = x then (cddr fast)
	for first = t then nil
	do (cond ((or (null fast) (eq fast *elisp-nil*)) (return t))
		 ((not (consp fast)) (return nil))
		 ((or (null (cdr fast)) (eq (cdr fast) *elisp-nil*)) (return t))
		 ((not (consp (cdr fast))) (return nil))
		 ((and (not first) (eq slow fast)) (return nil)))))

(defun check-proper-list (who pos x)
  (unless (proper-list-p x) (wrong-type who pos x)))

(defguile "append!" (&rest lists)
  (let ((lists (remove '() lists :end (max 0 (1- (length lists))))))
    (loop for (l . more) on lists for pos from 1
	  when more do (check-proper-list "append!" pos l))
    (apply #'nconc lists)))

(defguile "last-pair" (l)
  (check-proper-list "last-pair" 1 (if (consp l) (loop for x on l while (consp (cdr x)) finally (return (list (car x)))) l))
  (last l))

(defun list-index-tail (who list k)
  "The tail of LIST after K pairs, which must exist."
  (flet ((out () (guile-error (ssym "out-of-range") who "Value out of range: ~S" (list k) (list k))))
    (unless (and (integerp k) (>= k 0)) (out))
    (dotimes (i k list)
      (unless (consp list) (out))
      (setq list (cdr list)))))

(defun list-index-pair (who list k)
  (let ((tail (list-index-tail who list k)))
    (unless (consp tail)
      (guile-error (ssym "out-of-range") who "Value out of range: ~S" (list k) (list k)))
    tail))

(defguile "list-ref" (list k) (car (list-index-pair "list-ref" list k)))
(defguile "list-set!" (list k v) (setf (car (list-index-pair "list-set!" list k)) v) *unspecified*)
(defguile "list-cdr-ref" (list k) (list-index-tail "list-cdr-ref" list k))
(defguile "list-cdr-set!" (list k v) (setf (cdr (list-index-pair "list-cdr-set!" list k)) v) *unspecified*)
(defguile "list-head" (list k)
  (list-index-tail "list-head" list k)
  (subseq list 0 k))

(defguile "log2-binary-factors" (n)
  ;; the number of trailing zero bits; -1 for 0
  (if (zerop n) -1 (1- (integer-length (logand n (- n))))))

(defguile "list-tail" (list k)
  (unless (and (integerp k) (>= k 0))
    (guile-error (ssym "out-of-range") "list-tail" "Value out of range: ~S" (list k) (list k)))
  (dotimes (i k list)
    (unless (consp list)
      (guile-error (ssym "wrong-type-arg") "list-tail"
		   "Wrong type argument in position 1 (expecting pair): ~S" (list list) (list list)))
    (setq list (cdr list))))

(defguile "procedure-minimum-arity" (p)
  ;; (required optional rest?)
  (destructuring-bind (required optional rest) (or (function-arity p) '(0 0 t))
    (list required optional (bool rest))))
(defguile "object->string" (x &optional printer)
  (if (functionp printer)
      (with-output-to-string (s) (funcall printer x s))
      (with-output-to-string (s) (guile-write x s))))
(defguile "syntax-source" (s)
  (let ((v (and (syntax-object-p s) (syntax-object-sourcev s))))
    (if (simple-vector-p v)
	(list (cons (ssym "filename") (svref v 0)) (cons (ssym "line") (svref v 1))
	      (cons (ssym "column") (svref v 2)))
	qs:false)))
(defvar *guardian-procedures* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "The procedures that are guardians, for scm->object.")
(defguile "make-guardian" ()
  ;; src/r6rs/guardians.lisp; checked after a (gc)
  (let* ((g (qs-r6rs::new-guardian))
	 (f (lambda (&optional (x nil x-p))
	      (if x-p
		  (progn (qs-r6rs::guardian-register g x) *unspecified*)
		  (qs-r6rs::guardian-next g)))))
    (setf (gethash f *guardian-procedures*) t)
    f))
(defguile "display-error" (frame port subr message args rest)
  (declare (ignore frame rest))
  (when (truthy subr) (format port "In procedure ~A: " subr))
  (write-string (apply #'simple-format qs:false message (if (listp args) args '())) port)
  (terpri port)
  *unspecified*)
(defguile "eval-string" (string &optional (module qs:false))
  (with-input-from-string (in string)
    (let ((result *unspecified*))
      (loop for form = (guile-read in)
	    until (eq form qs:eof-object)
	    do (setq result (guile-eval form module)))
      result)))
(defguile "gettext" (msg &rest args) (declare (ignore args)) msg)
(defguile "ngettext" (msg plural n &rest args) (declare (ignore args)) (if (= n 1) msg plural))
(defguile "textdomain" (&rest args) (declare (ignore args)) qs:false)
(defguile "bindtextdomain" (&rest args) (declare (ignore args)) qs:false)
(defguile "bind-textdomain-codeset" (&rest args) (declare (ignore args)) qs:false)
(defguile "setlocale" (&rest args) (declare (ignore args)) "C")
(defguile "%package-data-dir" () (namestring (uiop:pathname-parent-directory-pathname *guile-source-directory*)))
(defguile "%library-dir" () (namestring *guile-source-directory*))	; guile-procedures.txt is here
(defguile "%site-dir" () "")
(defguile "%global-site-dir" () "")
(defguile "%site-ccache-dir" () "")
(defguile "%resolve-variable" (spec module)
  (or (module-variable* module (if (consp spec) (cdr spec) spec)) qs:false))
(defguile "module-import-interface" (module sym)
  ;; the interface MODULE's binding of SYM comes from
  (let ((v (imported-variable module sym)))
    (or (and v (find-if (lambda (iface) (eq (module-variable* iface sym) v))
			(module-slot module +module-uses+)))
	qs:false)))
(defguile "memoized-typecode" (x) (declare (ignore x)) 0)
(defguile "unmemoize-expression" (x) x)
;;; Signals, timers and asyncs.  A Guile signal handler is an SBCL
;;; interrupt handler: it runs in the thread the signal interrupts, and
;;; may leave by a non-local exit (abort-to-prompt), as Guile's asyncs
;;; may.  An async is run by interrupting its thread.

(defvar *signal-handlers* (make-hash-table) "signal -> (Guile handler . flags)")
(defvar *original-interrupt-handlers* (make-hash-table) "signal -> SBCL's own handler")

(defguile "sigaction" (signum &optional (handler nil handler-p) (flags 0) thread)
  (declare (ignore thread))
  (unless (integerp signum) (wrong-type "sigaction" 1 signum))
  (let ((previous (gethash signum *signal-handlers* (cons 0 0))))
    (when handler-p
      (flet ((install (lisp-handler)
	       (let ((old (sb-sys:enable-interrupt signum lisp-handler)))
		 (unless (nth-value 1 (gethash signum *original-interrupt-handlers*))
		   (setf (gethash signum *original-interrupt-handlers*) old)))))
	(cond ((eq handler qs:false)
	       ;; the handler there was before Guile's
	       (multiple-value-bind (original found) (gethash signum *original-interrupt-handlers*)
		 (when found (sb-sys:enable-interrupt signum original)))
	       (remhash signum *signal-handlers*))
	      ((integerp handler)
	       ;; SIG_DFL or SIG_IGN
	       (unless (member handler '(0 1))
		 (guile-error (ssym "out-of-range") "sigaction" "Value out of range: ~S" (list handler) (list handler)))
	       (install (if (= handler 1) :ignore :default))
	       (setf (gethash signum *signal-handlers*) (cons handler flags)))
	      ((functionp handler)
	       (install (lambda (signal info context)
			  (declare (ignore info context))
			  (funcall handler signal)))
	       (setf (gethash signum *signal-handlers*) (cons handler flags)))
	      (t (wrong-type "sigaction" 2 handler)))))
    previous))

(defun itimer-which (which)
  (case which (0 :real) (1 :virtual) (2 :profile) (t (wrong-type "setitimer" 1 which))))

(defun itimer-values (results)
  "(interval . value) of UNIX-SETITIMER's or UNIX-GETITIMER's values, as
Guile's ((seconds . microseconds) (seconds . microseconds))."
  (destructuring-bind (ok interval-s interval-us value-s value-us) results
    (declare (ignore ok))
    (list (cons interval-s interval-us) (cons value-s value-us))))

(defguile "setitimer" (which interval-seconds interval-microseconds value-seconds value-microseconds)
  ;; microseconds past a second carry into the seconds, as Guile's do
  (multiple-value-bind (is ius) (floor (+ (* interval-seconds 1000000) interval-microseconds) 1000000)
    (multiple-value-bind (vs vus) (floor (+ (* value-seconds 1000000) value-microseconds) 1000000)
      (itimer-values (multiple-value-list (sb-unix:unix-setitimer (itimer-which which) is ius vs vus))))))
(defguile "getitimer" (which)
  (itimer-values (multiple-value-list (sb-unix:unix-getitimer (itimer-which which)))))

(defguile "system-async-mark" (proc &optional thread)
  (let ((thread (if (typep thread 'sb-thread:thread) thread sb-thread:*current-thread*)))
    (sb-thread:interrupt-thread thread (lambda () (funcall proc)))
    *unspecified*))

;;; after-gc-hook, run after each garbage collection (by SBCL's
;;; *after-gc-hooks*, in whichever thread SBCL runs them)

(defun run-after-gc-hook ()
  (qsx::with-thread-state (run-after-gc-hook-1)))

(defvar *hooked-gc-epoch* nil
  "The collection the after-gc-hook last ran after: (gc) runs it at once,
and SBCL's own run, later, in another thread, then skips it.")

(defun run-after-gc-hook-1 ()
  (let ((hook *after-gc-hook*)
	(epoch sb-kernel::*gc-epoch*))
    (when (and hook (hook-procedures hook)
	       (not (eq (shiftf *hooked-gc-epoch* epoch) epoch)))
      (dolist (p (hook-procedures hook))
	(ignore-errors (funcall p))))))

(pushnew 'run-after-gc-hook sb-ext:*after-gc-hooks*)
(defguile "restore-signals" () *unspecified*)
(defguile "alarm" (n) (declare (ignore n)) 0)
;; a compiled file, Guile's .go, run on the VM (vm.lisp): its value
(defguile "load-compiled" (file) (funcall (the function (load-go-file file))))

;;; ------------------------------------------------------------------
;;; load-extension: libguile's init functions for modules partly in C.
;;; Each defines its bindings in the current module.

(defvar *extensions* (make-hash-table :test 'equal)
  "C init function name -> function returning ((name . value) ...).")

(defmacro defextension (name &body body)
  `(setf (gethash ,name *extensions*) (lambda () ,@body)))

(defvar *c-extension-loader* nil
  "With our libguile (src/libguile/, docs/libguile.md) loaded: a function
of a library and an init function's names that runs a C extension, or
returns NIL if there is no such library.")

(defguile "load-extension" (library init)
  (block load-extension
    (let ((ext (gethash init *extensions*)))
      (cond (ext
	     (loop for (name . value) in (funcall ext)
		   do (define-in-module *current-module* (ssym name) value)))
	    ((and *c-extension-loader* (funcall *c-extension-loader* library init))
	     (return-from load-extension *unspecified*)))
      (fill-unbound-exports init)
      (when (string= init "scm_init_ice_9_ports") (share-root-variables *current-module*))
      ;; (rnrs bytevectors)'s mutators refuse read-only bytevectors
      (dolist (name +bytevector-mutators+)
	(let ((v (module-local-variable* *current-module* (ssym name))))
	  (when v (wrap-bytevector-mutator v name))))
      *unspecified*)))

(defun share-root-variables (module)
  "Make MODULE's variables of the names the root module binds, the root's
variables, as in Guile, whose C code defines them once.  (ice-9 ports)
that defined its own made a module that imported it, (scheme base),
warn that it overrode core bindings, which only differing variables do."
  (let ((shared '()))
    (funcall (root-value "module-for-each")
	     (lambda (name variable)
	       (let ((root (obarray-variable name)))
		 (when (and root (not (eq root variable))
			    (not (eq (gvariable-value root) +unbound+)))
		   (push (cons name root) shared))))
	     module)
    (let ((interface (module-public-interface* module)))
      (loop for (name . root) in shared
	    do (funcall (root-value "module-add!") module name root)
	       ;; the exports were made already, holding the old variables
	       (when (and interface (funcall (root-value "module-local-variable") interface name))
		 (funcall (root-value "module-add!") interface name root))))))

(defvar *extension-primitives* (make-hash-table :test 'equal)
  "Procedures of Guile's C extensions that aren't in the root module:
name -> function, for FILL-UNBOUND-EXPORTS.")

(defvar *missing-extension-names* '()
  "(init . name): what load-extension couldn't define.")

(defun fill-unbound-exports (init)
  "Give each variable of the current module that is still unbound (the
module exports it, and its C half was to define it) the host's value of
that name, if the host has one."
  (let ((obarray (and (module-p* *current-module*) (module-slot *current-module* +module-obarray+))))
    (when (ghash-p obarray)
      (dolist (handle (ghash-handles obarray))
	(let ((name (car handle)) (v (cdr handle)))
	  (when (and (gvariable-p v) (eq (gvariable-value v) +unbound+))
	    (let ((s (qs:scheme-symbol-name name)))
	      (cond ((gethash s *extension-primitives*)
		     (setf (gvariable-value v) (gethash s *extension-primitives*)))
		    ((gethash s *guile-primitives*)
		     (setf (gvariable-value v) (gethash s *guile-primitives*)))
		    ((host-bound-p s) (setf (gvariable-value v) (qsx:host-ref s)))
		    (t (push (cons init s) *missing-extension-names*))))))))))

(defextension "scm_init_ice_9_control"
  ;; a prompt for TAG with no continuation barrier inside it, so that
  ;; what is captured up to it can be resumed
  (list (cons "suspendable-continuation?"
	      (lambda (tag)
		(bool (dolist (frame qsx::*fstack*)
			(when (vectorp frame)
			  (case (svref frame 0)
			    (:barrier (return nil))
			    (:prompt (when (eq (cdr (svref frame 2)) tag) (return t)))))))))))

;;; (rnrs bytevectors gnu)'s bytevector-slice: a copy here, where Guile's
;;; shares the bytes
(defextension "scm_init_bytevectors"
  (list (cons "u8-list->bytevector"
	      (lambda (list)
		(check-proper-list "u8-list->bytevector" 1 list)
		(dolist (b list) (unless (typep b '(unsigned-byte 8)) (wrong-type "u8-list->bytevector" 1 b)))
		(make-array (length list) :element-type '(unsigned-byte 8) :initial-contents list)))
	;; the host's, after Guile's check of the list (a circular one would loop)
	(cons "sint-list->bytevector"
	      (lambda (list &rest args)
		(check-proper-list "sint-list->bytevector" 1 list)
		(apply (qsx:host-ref "sint-list->bytevector") list args)))
	(cons "uint-list->bytevector"
	      (lambda (list &rest args)
		(check-proper-list "uint-list->bytevector" 1 list)
		(apply (qsx:host-ref "uint-list->bytevector") list args)))
	(cons "bytevector-fill!"
	      ;; with Guile's optional range
	      (lambda (bv fill &optional (start 0) (end (length bv)))
		(unless (typep bv 'qs-r6rs::octets) (wrong-type "bytevector-fill!" 1 bv))
		(unless (and (integerp fill) (<= -128 fill 255)) (wrong-type "bytevector-fill!" 2 fill))
		(fill bv (ldb (byte 8 0) fill) :start start :end end)
		*unspecified*))
	(cons "bytevector-slice"
	      (lambda (bv offset &optional (size nil size-p))
		(unless (typep bv 'qs-r6rs::octets) (wrong-type "bytevector-slice" 1 bv))
		(unless (and (integerp offset) (<= 0 offset (length bv)))
		  (out-of-range "bytevector-slice" offset))
		(let ((size (if size-p size (- (length bv) offset))))
		  (unless (and (integerp size) (<= 0 size (- (length bv) offset)))
		    (out-of-range "bytevector-slice" size))
		  ;; sharing BV's bytes, as Guile's (its linker writes each
		  ;; section of an image through a slice)
		  ;; read-only if BV is (a literal of compiled code), as Guile's
		  (let ((slice (make-array size :element-type '(unsigned-byte 8)
						:displaced-to bv :displaced-index-offset offset)))
		    (if (read-only-bytevector-p bv) (read-only-bytevector slice) slice)))))
	;; Guile's checks of the size, before the host's
	(cons "bytevector->sint-list" (lambda (bv endianness size) (integer-list-size-check "bytevector->sint-list" bv size)
					(funcall (qsx:host-ref "bytevector->sint-list") bv endianness size)))
	(cons "bytevector->uint-list" (lambda (bv endianness size) (integer-list-size-check "bytevector->uint-list" bv size)
					(funcall (qsx:host-ref "bytevector->uint-list") bv endianness size)))
	(cons "utf8->string"
	      (lambda (bv &optional (start 0) (end nil))
		(unless (typep bv 'qs-r6rs::octets) (wrong-type "utf8->string" 1 bv))
		(handler-case (sb-ext:octets-to-string bv :external-format '(:utf-8 :replacement nil)
							  :start start :end end)
		  (sb-int:character-decoding-error ()
		    (call-throw (ssym "decoding-error") (list "utf8->string" "input decoding error" +eilseq+ bv))))))
	;; the endianness is optional, big by default
	(cons "utf16->string" (lambda (bv &optional (endianness (ssym "big")) endianness-mandatory)
				(funcall (qsx:host-ref "utf16->string") bv endianness
					 (if (truthy endianness-mandatory) qs:true qs:false))))
	(cons "utf32->string" (lambda (bv &optional (endianness (ssym "big")) endianness-mandatory)
				(funcall (qsx:host-ref "utf32->string") bv endianness
					 (if (truthy endianness-mandatory) qs:true qs:false))))))

(defun integer-list-size-check (who bv size)
  (unless (typep bv 'qs-r6rs::octets) (wrong-type who 1 bv))
  (unless (and (integerp size) (plusp size)) (out-of-range who size))
  (unless (zerop (mod (length bv) size)) (wrong-type who 3 size)))

(defguile "raise" (signal) (sb-posix:kill (sb-posix:getpid) signal) qs:true)

;;; File descriptors and processes

(defguile "pipe" (&optional flags)
  (declare (ignore flags))		; close-on-exec always; O_NONBLOCK not
  (multiple-value-bind (in out) (sb-posix:pipe)
    ;; close-on-exec (FD_CLOEXEC, 1), as Guile's: a child gets the ends it is given
    ;; (piped-process), not the others, which would keep a pipe open
    (dolist (fd (list in out)) (sb-posix:fcntl fd sb-posix:f-setfd 1))
    (cons (fd-port in "r") (fd-port out "w"))))
(defguile "open-fdes" (path flags &optional (mode #o666)) (sb-posix:open path flags mode))
(defguile "open" (path flags &optional (mode #o666))
  (let ((fd (sb-posix:open path flags mode)))
    (fd-port fd (cond ((logtest flags sb-posix:o-rdwr) "rw")
		      ((logtest flags sb-posix:o-wronly) "w")
		      (t "r")))))
(defguile "close-fdes" (fd) (sb-posix:close fd) *unspecified*)
(defguile "close" (fd/port)
  (if (integerp fd/port) (sb-posix:close fd/port) (close fd/port))
  *unspecified*)
(defguile "waitpid" (pid &optional (options 0))
  (multiple-value-bind (p status) (sb-posix:waitpid pid options)
    (cons p status)))
(defguile "sync" () *unspecified*)
(defguile "fsync" (x) (declare (ignore x)) *unspecified*)
(defguile "flock" (&rest args) (declare (ignore args)) *unspecified*)
(defguile "mkstemp" (template &optional (mode "w+"))
  (multiple-value-bind (fd name) (sb-posix:mkstemp template)
    ;; Guile fills the template in place
    (replace template name)
    (let ((p (fd-port fd (if (stringp mode) mode "w+"))))
      (setf (port-filename* p) name)
      p)))
(setf (gethash "mkstemp!" *guile-primitives*) (gethash "mkstemp" *guile-primitives*))
(defguile "mkdtemp" (template) (sb-posix:mkdtemp template))
(defguile "tmpnam" () (format nil "/tmp/guile-~36R" (random (expt 36 8))))
(defguile "tmpfile" () (funcall (gethash "mkstemp" *guile-primitives*) (copy-seq "/tmp/guile-XXXXXX")))
(defguile "setuid" (id) (sb-posix:setuid id) *unspecified*)
(defguile "setgid" (id) (sb-posix:setgid id) *unspecified*)
(defguile "getpgrp" () (sb-posix:getpgrp))
(defguile "parse-path" (path &optional (tail '()))
  (if (and (stringp path) (plusp (length path)))
      (append (funcall (gethash "string-split" *guile-primitives*) path #\:) tail)
      tail))
(defguile "search-path" (path filename &optional (extensions '("")) (require-exts qs:false))
  (let ((extensions (let ((l (loop for tail = extensions then (cdr tail)
				   while (consp tail) collect (car tail))))	; #nil may end it
		      (or l '("")))))
    (flet ((try (base)
	     (loop for ext in (if (and (not (truthy require-exts))
				       (some (lambda (e) (and (plusp (length e)) (uiop:string-suffix-p filename e)))
					     extensions))
				  '("")
				  extensions)
		   for f = (concatenate 'string base ext)
		   when (and (probe-file f) (not (uiop:directory-exists-p f))) return f)))
      (or (if (uiop:absolute-pathname-p filename)
	      (try filename)
	      (loop for tail = path then (cdr tail)
		    while (consp tail)
		    thereis (try (format nil "~A/~A" (string-right-trim "/" (car tail)) filename))))
	  qs:false))))
(defguile "procedure" (x)
  (if (typep x 'applicable-struct) (svref (astruct-slots x) 0) (wrong-type "procedure" 1 x)))

(defguile "mktime" (v &optional zone)
  ;; the time, and the broken-down time normalized
  (cffi:with-foreign-object (tm :uint8 +tm-size+)
    (vector->tm v tm)
    (call-with-tz zone (lambda ()
			 (let ((time (cffi:foreign-funcall "mktime" :pointer tm :long)))
			   (cons time (tm->vector tm)))))))
(defguile "tzset" () (cffi:foreign-funcall "tzset" :void) *unspecified*)
(defguile "primitive-fork" () (sb-posix:fork))
(defguile "execlp" (program &rest args)
  (sb-ext:run-program program (cdr args) :search t :output t :error t :input t)
  (sb-ext:exit :code 0))
(setf (gethash "execl" *guile-primitives*) (gethash "execlp" *guile-primitives*))

;;; Locales: the categories, and libc's setlocale.  Text here is Lisp's,
;;; which no locale changes.

(defparameter *locale-categories*
  '(("LC_ALL" . 0) ("LC_COLLATE" . 1) ("LC_CTYPE" . 2) ("LC_MONETARY" . 3)
    ("LC_NUMERIC" . 4) ("LC_TIME" . 5) ("LC_MESSAGES" . 6)))

(defguile "setlocale" (category &optional locale)
  (let ((result (if (or (null locale) (eq locale qs:false))
		    (cffi:foreign-funcall "setlocale" :int category :pointer (cffi:null-pointer) :pointer)
		    (cffi:foreign-funcall "setlocale" :int category :string locale :pointer))))
    (if (cffi:null-pointer-p result)
	(guile-error (ssym "system-error") "setlocale" "~A" (list "Invalid argument") (list 22))
	(cffi:foreign-string-to-lisp result))))

;;; (language bytecode)'s instruction-list: there is no VM here, but the
;;; disassembler and assembler build their tables from it when they are
;;; expanded, and the test suite's driver loads them.  It is the
;;; installed Guile's own table, asked of its guile program once.

;;; So is intrinsic-list, which the assembler's tables also come from.

(defvar *bytecode-tables* (make-hash-table :test 'equal))

(defun bytecode-table (name)
  "(language bytecode)'s NAME (a procedure of no arguments), as the
installed Guile returns it; '() if there's no guile program."
  (multiple-value-bind (table found) (gethash name *bytecode-tables*)
    (if found
	table
	(setf (gethash name *bytecode-tables*)
	      (or (ignore-errors
		   (let ((text (uiop:run-program
				(list "guile" "-c" (format nil "(write ((@@ (language bytecode) ~A)))" name))
				:output :string :error-output nil)))
		     (with-input-from-string (in text) (guile-read in))))
		  '())))))

(defun instruction-list () (bytecode-table "instruction-list"))

(defextension "scm_init_instructions"
  (list (cons "instruction-list" #'instruction-list)))

(defextension "scm_init_intrinsics"
  (list (cons "intrinsic-list" (lambda () (bytecode-table "intrinsic-list")))))

;;; (system vm program): there is no VM, so no procedure is a program
;;; (Guile's callers, such as the arity analysis, then fall back to
;;; procedure-minimum-arity), and none is primitive code.
(defextension "scm_init_programs"
  (flet ((no-program (p) (guile-error (ssym "wrong-type-arg") "program-code"
				      "Wrong type argument in position ~A: ~S" (list 1 p) (list p))))
    (list (cons "program?" (lambda (x) (declare (ignore x)) qs:false))
	  (cons "program-code" #'no-program)
	  (cons "primitive-code?" (lambda (x) (declare (ignore x)) qs:false))
	  (cons "primitive-code-name" (lambda (x) (declare (ignore x)) qs:false))
	  (cons "program-num-free-variables" #'no-program)
	  (cons "program-free-variable-ref" (lambda (p i) (declare (ignore i)) (no-program p)))
	  (cons "program-free-variable-set!" (lambda (p i x) (declare (ignore i x)) (no-program p))))))

;;; number->string as Guile writes numbers.  A float is written with the
;;; fewest digits, in any radix, that read back as the same float: the
;;; algorithm of Burger and Dybvig, "Printing Floating-Point Numbers
;;; Quickly and Accurately" (PLDI 1996), with Guile's choices of when to
;;; use an exponent (itself written in the radix).

(defun shortest-float-digits (x radix out)
  "Write X, a positive double, to OUT."
  (multiple-value-bind (f e) (integer-decode-float x)
    (let* ((odd (if (oddp f) 1 0))
	   (even (- 1 odd))
	   (min-normal (and (= f (ash 1 52)) (/= e -1074)))
	   (mminus (if (minusp e) 1 (ash 1 e)))
	   (mplus (if min-normal (* 2 mminus) mminus))
	   (r (if (minusp e) (ash f (if min-normal 2 1)) (ash f (+ e (if min-normal 2 1)))))
	   (s (if (minusp e) (ash 1 (- (if min-normal 2 1) e)) (if min-normal 4 2)))
	   (k 0)
	   (a (make-array 32 :element-type 'character :adjustable t :fill-pointer 0))
	   (show-exp nil))
      (flet ((cmp (a b) (signum (- a b)))
	     (emit (c) (vector-push-extend c a)))
	;; the smallest k with (r + m+)/s < radix^k (<= when f is odd)
	(let ((hi (+ r mplus)))
	  (loop while (>= (cmp hi s) odd) do (setq s (* s radix)) (incf k))
	  (when (zerop k)
	    (setq hi (* hi radix))
	    (loop while (< (cmp hi s) odd)
		  do (setq r (* r radix) mplus (* mplus radix) mminus (* mminus radix)
			   hi (* hi radix))
		     (decf k))))
	(let ((expon (1- k)))
	  (when (<= k 0)
	    (if (<= k -3)
		(setq show-exp t k 1)
		(progn (emit #\0) (emit #\.) (loop repeat (- k) do (emit #\0)))))
	  (loop
	    (setq mplus (* mplus radix) mminus (* mminus radix))
	    (multiple-value-bind (d rem) (floor (* r radix) s)
	      (setq r rem)
	      (let* ((hi (+ r mplus))
		     (end-1 (< (cmp r mminus) even))
		     (end-2 (< (cmp s hi) even)))
		(cond ((or end-1 end-2)
		       (setq r (* r 2))
		       (cond ((not end-2))
			     ((not end-1) (incf d))
			     ((>= (cmp r s) (if (oddp d) 0 1)) (incf d)))
		       (emit (char-downcase (digit-char d radix)))
		       (when (zerop (decf k)) (emit #\.))
		       (return))
		      (t (emit (char-downcase (digit-char d radix)))
			 (when (zerop (decf k)) (emit #\.)))))))
	  (when (plusp k)
	    (if (and (>= expon 7) (>= k 4) (>= expon k))
		;; more than three zeroes before the point: an exponent
		(let* ((k2 (- k expon))
		       (at (+ (fill-pointer a) k2)))
		  (emit #\Space)
		  (replace a a :start1 (1+ at) :start2 at :end2 (1- (fill-pointer a)))
		  (setf (char a at) #\.)
		  (setq k k2 show-exp t))
		(progn (loop repeat k do (emit #\0)) (emit #\.) (setq k 0))))
	  (when (zerop k) (emit #\0))
	  (write-string a out)
	  (when show-exp
	    (write-char #\e out)
	    (write-string (string-downcase (write-to-string expon :base radix :radix nil)) out)))))))

(defun guile-float-string (x radix)
  (let ((x (coerce x 'double-float)))
    (cond ((sb-ext:float-infinity-p x) (if (plusp x) "+inf.0" "-inf.0"))
	  ((sb-ext:float-nan-p x) "+nan.0")
	  ((zerop x) (if (minusp (float-sign x)) "-0.0" "0.0"))
	  (t (with-output-to-string (out)
	       (when (minusp x) (write-char #\- out) (setq x (- x)))
	       (shortest-float-digits x radix out))))))

(defguile "number->string" (n &optional (radix 10))
  (unless (and (integerp radix) (<= 2 radix 36))
    (guile-error (ssym "out-of-range") "number->string" "Value out of range: ~S" (list radix) (list radix)))
  (cond ((floatp n) (guile-float-string n radix))
	((complexp n)
	 (let ((re (realpart n)) (im (imagpart n)))
	   (concatenate 'string (guile-float-string re radix)
			(if (and (not (minusp (float-sign (coerce im 'double-float))))
				 (not (sb-ext:float-infinity-p (coerce im 'double-float)))
				 (not (sb-ext:float-nan-p (coerce im 'double-float))))
			    "+" "")
			(guile-float-string im radix) "i")))
	((integerp n) (string-downcase (write-to-string n :base radix :radix nil)))
	((rationalp n) (string-downcase (write-to-string n :base radix :radix nil)))
	(t (wrong-type "number->string" 1 n))))

;;; SRFI 60's C half (scm_init_srfi_60), as the SRFI defines them

(defun bit-field-mask (start end) (ash (1- (ash 1 (- end start))) start))

(defextension "scm_init_srfi_60"
  (list
   (cons "copy-bit" (lambda (index n bit)
		      (if (truthy bit) (logior n (ash 1 index)) (logandc2 n (ash 1 index)))))
   (cons "rotate-bit-field"
	 (lambda (n count start end)
	   (let* ((width (- end start))
		  (field (ldb (byte width start) n)))
	     (if (zerop width)
		 n
		 (let* ((count (mod count width))
			(rotated (logand (logior (ash field count) (ash field (- count width)))
					 (1- (ash 1 width)))))
		   (dpb rotated (byte width start) n))))))
   (cons "reverse-bit-field"
	 (lambda (n start end)
	   (let* ((width (- end start))
		  (field (ldb (byte width start) n))
		  (reversed 0))
	     (dotimes (i width) (setq reversed (logior (ash reversed 1) (ldb (byte 1 i) field))))
	     (dpb reversed (byte width start) n))))
   (cons "integer->list"
	 (lambda (n &optional (length (integer-length n)))
	   (loop for i from (1- length) downto 0 collect (bool (logbitp i n)))))
   (cons "list->integer"
	 (lambda (list) (reduce (lambda (acc b) (logior (ash acc 1) (if (truthy b) 1 0))) list :initial-value 0)))
   (cons "booleans->integer"
	 (lambda (&rest list) (reduce (lambda (acc b) (logior (ash acc 1) (if (truthy b) 1 0))) list :initial-value 0)))))

;;; SRFI 13 where Guile's differs from the reference implementation:
;;; string-any and string-every take a character or char-set, as the
;;; others do; string-replace's indices are optional; string-concatenate
;;; checks its strings.

(defun char-or-set-matcher (who pos x)
  "A function of a character for X, a character, char-set or predicate:
true (the predicate's value) if it matches."
  (cond ((characterp x) (lambda (c) (char= c x)))
	((functionp x) x)
	((truthy (funcall (library-value "(srfi 14)" "char-set?") x))
	 (let ((contains (library-value "(srfi 14)" "char-set-contains?")))
	   (lambda (c) (funcall contains x c))))
	(t (wrong-type who pos x))))

(defun string-bounds (who s start end)
  (unless (stringp s) (wrong-type who 2 s))
  (let ((end (if (or (null end) (eq end qs:false)) (length s) end)))
    (unless (and (integerp start) (<= 0 start (length s))) (guile-error (ssym "out-of-range") who "Value out of range: ~S" (list start) (list start)))
    (unless (and (integerp end) (<= start end (length s))) (guile-error (ssym "out-of-range") who "Value out of range: ~S" (list end) (list end)))
    end))

(defguile "string-any-c-code" (pred s &optional (start 0) end)	; boot-9's string-any
  (let ((match (char-or-set-matcher "string-any" 1 pred))
	(end (string-bounds "string-any" s start end)))
    (loop for i from start below end
	  for v = (funcall match (char s i))
	  when (truthy* v) return (if (functionp pred) v qs:true)
	  finally (return qs:false))))

(defguile "string-every-c-code" (pred s &optional (start 0) end)	; and string-every
  (let ((match (char-or-set-matcher "string-every" 1 pred))
	(end (string-bounds "string-every" s start end))
	(last qs:true))
    (loop for i from start below end
	  for v = (funcall match (char s i))
	  unless (truthy* v) return qs:false
	  do (setq last (if (functionp pred) v qs:true))
	  finally (return last))))

(defun truthy* (v) (and v (not (eq v qs:false))))

(defguile "string-replace" (s1 s2 &optional (start1 0) end1 (start2 0) end2)
  (let ((end1 (string-bounds "string-replace" s1 start1 end1))
	(end2 (string-bounds "string-replace" s2 start2 end2)))
    (concatenate 'string (subseq s1 0 start1) (subseq s2 start2 end2) (subseq s1 end1))))

(defun check-string-list (who list)
  (check-proper-list who 1 list)
  (dolist (s list) (unless (stringp s) (wrong-type who 1 s)))
  list)

(defguile "string-concatenate" (list)
  (apply #'concatenate 'string (check-string-list "string-concatenate" list)))
(defguile "string-concatenate/shared" (list)
  (apply #'concatenate 'string (check-string-list "string-concatenate/shared" list)))

;;; Promises, Guile's: make-promise takes a thunk, forced once
(defstruct (gpromise (:constructor make-gpromise (thunk)) (:copier nil))
  thunk (value nil) (done nil))

(defmethod print-object ((p gpromise) stream)
  (format stream "#<promise ~A>" (if (gpromise-done p) "forced" "delayed")))

(defguile "make-promise" (thunk)
  (unless (functionp thunk) (wrong-type "make-promise" 1 thunk))
  (make-gpromise thunk))
(defguile "promise?" (x) (bool (gpromise-p x)))
(defguile "force" (p)
  (unless (gpromise-p p) (wrong-type "force" 1 p))
  (unless (gpromise-done p)
    (let ((v (funcall (gpromise-thunk p))))
      ;; forcing it may have forced it already: the first value wins
      (unless (gpromise-done p)
	(setf (gpromise-value p) v (gpromise-done p) t (gpromise-thunk p) nil))))
  (gpromise-value p))

;;; (ice-9 popen)'s C half: piped-process starts PROG with ARGS, its
;;; standard input and output the given pipe ends (FROM, the child's
;;; output pipe (read . write); TO, its input pipe) or else the current
;;; ports' descriptors, as libguile's does, by posix_spawnp.

(defun port-fd-or (port null-mode)
  (or (and (gport-p port) (port-open-p port) (port-fd port))
      (sb-posix:open "/dev/null" null-mode)))

(defun spawn-process (prog args in out err &key (environment (sb-ext:posix-environ)) (search t) argv0)
  "The pid of PROG run with ARGS (after ARGV0, or PROG), its descriptors
0-2 IN, OUT and ERR; looked for on PATH if SEARCH."
  (let ((argv (cons (or argv0 prog) args))
	(env environment))
    (flet ((string-array (strings)
	     (let ((a (cffi:foreign-alloc :pointer :count (1+ (length strings)))))
	       (loop for s in strings for i from 0
		     do (setf (cffi:mem-aref a :pointer i) (cffi:foreign-string-alloc s :encoding :utf-8)))
	       (setf (cffi:mem-aref a :pointer (length strings)) (cffi:null-pointer))
	       a)))
      (let ((c-argv (string-array argv)) (c-env (string-array env)))
	(cffi:with-foreign-objects ((actions :uint8 256) (pid :int))
	  (unwind-protect
	       (progn
		 (cffi:foreign-funcall "posix_spawn_file_actions_init" :pointer actions :int)
		 (loop for (fd target) in (list (list in 0) (list out 1) (list err 2))
		       do (cffi:foreign-funcall "posix_spawn_file_actions_adddup2"
						:pointer actions :int fd :int target :int))
		 (let ((code (if search
				 (cffi:foreign-funcall "posix_spawnp" :pointer pid :string prog
								      :pointer actions :pointer (cffi:null-pointer)
								      :pointer c-argv :pointer c-env :int)
				 (cffi:foreign-funcall "posix_spawn" :pointer pid :string prog
								     :pointer actions :pointer (cffi:null-pointer)
								     :pointer c-argv :pointer c-env :int))))
		   (if (zerop code)
		       (cffi:mem-ref pid :int)
		       (progn
			 (format *error-output* "In execvp of ~A: ~A~%" prog (sb-int:strerror code))
			 -1))))
	    (cffi:foreign-funcall "posix_spawn_file_actions_destroy" :pointer actions :int)
	    (dolist (a (list c-argv c-env))
	      (loop for i from 0 for p = (cffi:mem-aref a :pointer i)
		    until (cffi:null-pointer-p p) do (cffi:foreign-free p))
	      (cffi:foreign-free a))))))))

(defextension "scm_init_popen"
  (list
   (cons "piped-process"
	 (lambda (prog args &optional (from qs:false) (to qs:false))
	   (flush-all-ports)
	   (let* ((out (if (consp from) (cdr from) (port-fd-or *standard-output* sb-posix:o-wronly)))
		  (in (if (consp to) (car to) (port-fd-or *standard-input* sb-posix:o-rdonly)))
		  (err (port-fd-or *error-output* sb-posix:o-wronly))
		  (pid (spawn-process prog args in out err)))
	     ;; the child's ends, which the parent closes
	     (when (consp from) (sb-posix:close (cdr from)))
	     (when (consp to) (sb-posix:close (car to)))
	     pid)))))

;;; call-with-stack-overflow-handler: Guile limits the stack THUNK may use
;;; to LIMIT words, and calls HANDLER when it is reached.  Here the limit
;;; is the control stack's own: on its exhaustion HANDLER is called (its
;;; result checked as Guile checks it), and a non-local exit from it
;;; leaves THUNK.  A handler that returns, asking for more stack, can't be
;;; given it.  (SBCL recovers from an exhausted stack unless it runs with
;;; --lose-on-corruption, which --script implies: turned off meanwhile.)

(defun check-stack-limit (who x)
  (unless (integerp x) (wrong-type who 1 x))
  (unless (< 0 x (ash 1 62))
    (guile-error (ssym "out-of-range") who "Value out of range: ~S" (list x) (list x)))
  x)

(defguile "call-with-stack-overflow-handler" (limit thunk handler)
  (check-stack-limit "call-with-stack-overflow-handler" limit)
  ;; deep recursion (past the binding stack, src/continuations.lisp) stops
  ;; at 64 MB more of control stack, short of the stack's own end
  (handler-case (let ((qsx::*stack-limit* (cons (sb-sys:sap-int (sb-vm::current-sp)) (* 64 1024 1024)))
		      ;; frameless recursion (a procedure that calls only itself
		      ;; and safe ones) checks no limit: it exhausts the control
		      ;; stack itself, which SBCL recovers from only if not
		      ;; told to lose on corruption (--script tells it)
		      (lose (sb-alien:extern-alien "lose_on_corruption_p" sb-alien:int)))
		  (unwind-protect
		       (progn (setf (sb-alien:extern-alien "lose_on_corruption_p" sb-alien:int) 0)
			      (funcall thunk))
		    (setf (sb-alien:extern-alien "lose_on_corruption_p" sb-alien:int) lose)))
    (storage-condition ()
      (check-stack-limit "call-with-stack-overflow-handler" (funcall handler))
      (guile-error (ssym "stack-overflow") "call-with-stack-overflow-handler" "Stack overflow" '()))))

;; (system vm vm) exports it; it isn't in the root module
(setf (gethash "call-with-stack-overflow-handler" *extension-primitives*)
      (gethash "call-with-stack-overflow-handler" *guile-primitives*))
(remhash "call-with-stack-overflow-handler" *guile-primitives*)

;;; The rest of (system vm vm): calling on the VM, its engine and trace
;;; level, and its hooks.  Compiled code runs on this VM however it is
;;; called; the engine is a setting that nothing here reads.  The hooks
;;; are what (system vm coverage) and statprof use.
(defvar *vm-engine* (ssym "regular"))
(defvar *vm-default-engine* (ssym "regular"))
(defvar *vm-trace-level* 0)
(defvar *vm-next-hooks* '() "Called with each instruction's frame, before it runs.")
(defvar *vm-apply-hooks* '() "Called with a compiled procedure's frame as it's entered.")
(defvar *vm-return-hooks* '())
(defvar *vm-abort-hooks* '())

(macrolet ((vm-extensions (&rest specs)
	     `(progn
		,@(loop for (name lambda-list . body) in specs
			collect `(setf (gethash ,name *extension-primitives*) (lambda ,lambda-list ,@body))))))
  (vm-extensions
   ("call-with-vm" (proc &rest args) (apply (the function proc) args))
   ("vm-engine" () *vm-engine*)
   ("set-vm-engine!" (engine) (setq *vm-engine* engine) *unspecified*)
   ("set-default-vm-engine!" (engine) (setq *vm-default-engine* engine) *unspecified*)
   ("vm-trace-level" () *vm-trace-level*)
   ("set-vm-trace-level!" (level) (setq *vm-trace-level* level) *unspecified*)))

(macrolet ((hooks (&rest specs)
	     `(progn
		,@(loop for (kind var) in specs
			collect `(setf (gethash ,(format nil "vm-add-~A-hook!" kind) *extension-primitives*)
				       (lambda (f) (push f ,var) *unspecified*)
				       (gethash ,(format nil "vm-remove-~A-hook!" kind) *extension-primitives*)
				       (lambda (f) (setq ,var (remove f ,var :count 1)) *unspecified*))))))
  ;; the VM calls the next and apply hooks while the trace level is above
  ;; zero (vm.lisp's RUN-VM-1); the return and abort hooks are kept only
  (hooks ("next" *vm-next-hooks*) ("apply" *vm-apply-hooks*)
	 ("return" *vm-return-hooks*) ("abort" *vm-abort-hooks*)))

;;; ------------------------------------------------------------------
;;; More of libguile

(defguile "integer->char" (n)
  (unless (and (integerp n) (<= 0 n #x10ffff) (not (<= #xd800 n #xdfff)))
    (guile-error (ssym "out-of-range") "integer->char" "Value out of range: ~S" (list n) (list n)))
  (code-char n))

(defguile "bitvector-set-bits!" (v bits)
  (dotimes (i (min (length v) (length bits)) *unspecified*)
    (when (= 1 (sbit bits i)) (setf (sbit v i) 1))))
(defguile "bitvector-clear-bits!" (v bits)
  (dotimes (i (min (length v) (length bits)) *unspecified*)
    (when (= 1 (sbit bits i)) (setf (sbit v i) 0))))
(defguile "bitvector-count-bits" (v bits)
  (loop for i below (min (length v) (length bits)) count (and (= 1 (sbit v i)) (= 1 (sbit bits i)))))

;;; random's vector procedures
(defun random-normal (state)
  ;; Box and Muller
  (let ((u (- 1d0 (random 1d0 state))) (v (random 1d0 state)))
    (* (sqrt (* -2 (log u))) (cos (* 2 pi v)))))

(defun random-state-arg (state)
  (if (random-state-p state) state *random-state*))

(defun fill-vector (v function)
  (multiple-value-bind (root type offset dims) (array-view v "random")
    (declare (ignore root type offset))
    (walk-indices dims (lambda (ix) (set-array-element v (funcall function) ix "random")))))

(defguile "random:normal-vector!" (v &optional state)
  (let ((state (random-state-arg state)))
    (fill-vector v (lambda () (random-normal state))))
  *unspecified*)
(defun sphere-fill (v state scale)
  (fill-vector v (lambda () (random-normal state)))
  (let* ((xs (array->list* v))
	 (norm (sqrt (reduce #'+ xs :key (lambda (x) (* x x)) :initial-value 0d0)))
	 (k (if (zerop norm) 0d0 (/ (funcall scale (length xs)) norm)))
	 (i 0))
    (multiple-value-bind (root type offset dims) (array-view v "random")
      (declare (ignore root type offset))
      (walk-indices dims (lambda (ix) (set-array-element v (* k (nth i xs)) ix "random") (incf i))))))
(defguile "random:hollow-sphere!" (v &optional state)
  (sphere-fill v (random-state-arg state) (constantly 1d0))
  *unspecified*)
(defguile "random:solid-sphere!" (v &optional state)
  (let ((state (random-state-arg state)))
    (sphere-fill v state (lambda (n) (expt (random 1d0 state) (/ 1d0 (max n 1))))))
  *unspecified*)

;;; Weak vectors: each element held weakly (immediates as they are)
(defstruct (weak-vector (:constructor %make-weak-vector (cells)) (:copier nil))
  (cells #() :type simple-vector))
(defmethod print-object ((w weak-vector) stream)
  (format stream "#w~A" (with-output-to-string (s) (guile-write (coerce (weak-vector-elements w) 'simple-vector) s))))
(defun weak-cell (x) (if (or (numberp x) (characterp x) (symbolp x)) x (sb-ext:make-weak-pointer x)))
(defun weak-cell-value (c)
  (if (sb-ext:weak-pointer-p c)
      (multiple-value-bind (v alive) (sb-ext:weak-pointer-value c) (if alive v qs:false))
      c))
(defun weak-vector-elements (w) (map 'list #'weak-cell-value (weak-vector-cells w)))
(defguile "make-weak-vector" (n &optional (fill qs:false))
  (unless (and (integerp n) (>= n 0)) (wrong-type "make-weak-vector" 1 n))
  (%make-weak-vector (make-array n :initial-element (weak-cell fill))))
(defguile "list->weak-vector" (list)
  (check-proper-list "list->weak-vector" 1 list)
  (%make-weak-vector (map 'simple-vector #'weak-cell list)))
(defguile "weak-vector" (&rest elements) (%make-weak-vector (map 'simple-vector #'weak-cell elements)))
(defguile "weak-vector?" (x) (bool (weak-vector-p x)))
(defguile "weak-vector-length" (w) (length (weak-vector-cells w)))
(defguile "weak-vector-ref" (w i) (weak-cell-value (svref (weak-vector-cells w) i)))
(defguile "weak-vector-set!" (w i x) (setf (svref (weak-vector-cells w) i) (weak-cell x)) *unspecified*)

;;; module-reverse-lookup: the name a module binds to a variable
(defguile "module-reverse-lookup" (module variable)
  ;; the name VARIABLE has in MODULE: its own, or (through the modules it
  ;; uses, interfaces renaming) an imported one
  (unless (or (module-p* module) (eq module qs:false)) (wrong-type "module-reverse-lookup" 1 module))
  (unless (gvariable-p variable) (wrong-type "module-reverse-lookup" 2 variable))
  (let ((seen '()))
    (labels ((look-in (m)
	       (unless (member m seen :test #'eq)
		 (push m seen)
		 (let ((obarray (if (module-p* m) (module-slot m +module-obarray+) *obarray*)))
		   (or (loop for (name . v) in (ghash-handles obarray) when (eq v variable) return name)
		       (and (module-p* m)
			    (loop for iface in (module-slot m +module-uses+) thereis (look-in iface))))))))
      (or (look-in module) qs:false))))

;;; POSIX odds and ends
(defguile "ttyname" (port)
  (let ((p (cffi:foreign-funcall "ttyname" :int (port-fdes port "ttyname") :pointer)))
    (if (cffi:null-pointer-p p)
	(system-error "ttyname" (sb-alien:extern-alien "errno" sb-alien:int))
	(cffi:foreign-string-to-lisp p))))
(defguile "utime" (obj &optional actime modtime actimens modtimens flags)
  ;; utimensat(2): nanoseconds, and AT_SYMLINK_NOFOLLOW for a link's own times
  (let* ((now (- (get-universal-time) #.(encode-universal-time 0 0 0 1 1 1970 0)))
	 (a (if (integerp actime) actime now)) (m (if (integerp modtime) modtime now))
	 (an (if (integerp actimens) actimens 0)) (mn (if (integerp modtimens) modtimens 0))
	 (flags (if (integerp flags) flags 0))
	 (path (if (stringp obj) obj (port-filename* (->port obj "utime")))))
    (sb-alien:with-alien ((times (sb-alien:array (sb-alien:signed 64) 4)))
      (setf (sb-alien:deref times 0) a (sb-alien:deref times 1) an
	    (sb-alien:deref times 2) m (sb-alien:deref times 3) mn)
      (let ((result (sb-alien:alien-funcall
		     (sb-alien:extern-alien "utimensat"
					    (function sb-alien:int sb-alien:int sb-alien:c-string
						      (* (sb-alien:array (sb-alien:signed 64) 4))
						      sb-alien:int))
		     #+darwin -2 #-darwin -100	; AT_FDCWD
		     path (sb-alien:addr times) flags)))
	(when (minusp result)
	  (let ((e (sb-alien:get-errno)))
	    (guile-error (ssym "system-error") "utime" "~A" (list (sb-int:strerror e)) (list e)))))))
  *unspecified*)

;;; spawn: Guile 3.0.9's posix_spawn interface.  ARGS includes argv[0].
(defguile "spawn" (program args &key (environment nil) (input nil) (output nil) (error nil)
			   (search-path? qs:true))
  (flet ((fd-of (port default who)
	   (cond ((null port) (port-fd-or default (if (eq default *standard-input*) sb-posix:o-rdonly sb-posix:o-wronly)))
		 ((and (gport-p port) (port-fd port)) (flush-output port) (port-fd port))
		 (t (wrong-type "spawn" who port)))))
    (flush-all-ports)
    (let ((pid (spawn-process program (rest args)
			      (fd-of input *standard-input* 3)
			      (fd-of output *standard-output* 4)
			      (fd-of error *error-output* 5)
			      :argv0 (first args)
			      :environment (if (listp environment) environment (sb-ext:posix-environ))
			      :search (truthy search-path?))))
      (when (minusp pid)
	(guile-error (ssym "system-error") "spawn" "~A" (list "No such file or directory") (list 2)))
      pid)))

(defguile "fcntl" (object cmd &optional (value 0))
  (let ((fd (if (integerp object) object (port-fdes object "fcntl"))))
    (sb-posix:fcntl fd cmd value)))

(defguile "sendfile" (out in count &optional offset)
  ;; COUNT bytes of IN (from OFFSET, IN's position untouched, if given) to OUT
  (let* ((in-port (if (integerp in) (fd-port in "r") (->port in "sendfile")))
	 (out-port (if (integerp out) (fd-port out "w") (->port out "sendfile")))
	 (saved (and offset (port-position in-port))))
    (when offset (port-seek in-port offset 0))
    (let ((bytes (get-bytes in-port count)))
      (write-octets out-port bytes)
      (flush-output out-port)
      (when saved (port-seek in-port saved 0))
      (length bytes))))

(defguile "chmodat" (dir path mode &optional (flags 0))
  (unless (and (gport-p dir) (port-open-p dir) (port-fd dir)) (wrong-type "chmodat" 1 dir))
  (let ((r (cffi:foreign-funcall "fchmodat" :int (port-fd dir) :string path :unsigned-short mode :int flags :int)))
    (unless (zerop r) (system-error "chmodat" (sb-alien:extern-alien "errno" sb-alien:int)))
    *unspecified*))

;; SRFI 14's char-set-ref, with libguile's error for a bad cursor
(defguile "char-set-ref" (cs cursor)
  (handler-case (funcall (gethash (cons "(srfi 14)" "char-set-ref") *library-values*) cs cursor)
    (error () (wrong-type "char-set-ref" 2 cursor))))

;;; (system vm loader): an ELF image is checked as libguile's loader
;;; checks it, then loaded on the VM.

(defun load-thunk-from-memory (bv)
  (flet ((fail (message) (guile-error (ssym "misc-error") "load-thunk-from-memory" message '())))
    (unless (typep bv 'qs-r6rs::octets) (wrong-type "load-thunk-from-memory" 1 bv))
    (unless (and (>= (length bv) 64) (= (aref bv 0) #x7f) (= (aref bv 1) (char-code #\E))
		 (= (aref bv 2) (char-code #\L)) (= (aref bv 3) (char-code #\F)))
      (fail "not an ELF file"))
    (unless (= (aref bv 4) 2) (fail "ELF file does not have native word size"))
    (unless (= (aref bv 5) #+little-endian 1 #+big-endian 2)
      (fail "ELF file does not have native byte order"))
    (unless (member (aref bv 7) '(0 255)) (fail "ELF file does not have the expected OS ABI"))
    ;; the image's code runs on the VM (src/guile/vm.lisp)
    (load-image (if (typep bv '(simple-array (unsigned-byte 8) (*))) bv (coerce bv '(simple-array (unsigned-byte 8) (*)))))))

(defextension "scm_init_loader"
  (list (cons "load-thunk-from-memory" #'load-thunk-from-memory)
	(cons "load-thunk-from-file"
	      (lambda (file)
		(unless (stringp file) (wrong-type "load-thunk-from-file" 1 file))
		(load-thunk-from-memory (read-file-bytes file))))))

;; SRFI 13's string-join, with libguile's error for joining nothing with
;; the strict-infix grammar
(defguile "string-join" (strings &optional (delimiter " ") (grammar (ssym "infix")))
  (when (and (null strings) (symbolp grammar) grammar
	     (string= (qs:scheme-symbol-name grammar) "strict-infix"))
    (guile-error (ssym "misc-error") "string-join"
		 "strict-infix grammar requires non-empty list" '()))
  (funcall (gethash (cons "(srfi 13)" "string-join") *library-values*) strings delimiter grammar))

;;; The *at procedures: a file named relative to a directory port, by
;;; libc's *at calls.  statat stats the directory's path joined to the
;;; name (F_GETPATH on Darwin, /proc/self/fd elsewhere).

(defun at-directory-fd (who dir)
  "DIR's descriptor; #f is the current directory (AT_FDCWD)."
  (cond ((eq dir qs:false) #+darwin -2 #-darwin -100)
	((and (gport-p dir) (port-open-p dir) (port-fd dir)) (port-fd dir))
	(t (wrong-type who 1 dir))))

(defun at-result (who r)
  (when (minusp r) (system-error who (sb-alien:extern-alien "errno" sb-alien:int)))
  r)

(defun fd-directory-path (fd)
  #+darwin
  (cffi:with-foreign-object (buf :char 1024)
    (if (minusp (cffi:foreign-funcall-varargs "fcntl" (:int fd :int 50) :pointer buf :int)) ; F_GETPATH
	(system-error "statat")
	(cffi:foreign-string-to-lisp buf)))
  #-darwin
  (sb-posix:readlink (format nil "/proc/self/fd/~D" fd)))

(defguile "statat" (dir path &optional (flags 0))
  (unless (stringp path) (wrong-type "statat" 2 path))
  (let* ((fd (at-directory-fd "statat" dir))
	 (full (if (or (minusp fd) (and (plusp (length path)) (char= (char path 0) #\/)))
		   path
		   (concatenate 'string (fd-directory-path fd) "/" path))))
    (handler-case (stat-vector (if (logtest flags #+darwin #x20 #-darwin #x100) ; AT_SYMLINK_NOFOLLOW
				   (sb-posix:lstat full)
				   (sb-posix:stat full)))
      (sb-posix:syscall-error (e) (system-error "statat" (sb-posix:syscall-errno e))))))

(defguile "symlinkat" (dir old new)
  (unless (stringp old) (wrong-type "symlinkat" 2 old))
  (at-result "symlinkat" (cffi:foreign-funcall "symlinkat" :string old :int (at-directory-fd "symlinkat" dir)
							   :string new :int))
  *unspecified*)

(defguile "mkdirat" (dir path &optional (mode #o777))
  (unless (stringp path) (wrong-type "mkdirat" 2 path))
  (at-result "mkdirat" (cffi:foreign-funcall "mkdirat" :int (at-directory-fd "mkdirat" dir) :string path
						       :unsigned-short (if (integerp mode) mode #o777) :int))
  *unspecified*)

(defguile "delete-file-at" (dir path &optional (flags 0))
  (unless (stringp path) (wrong-type "delete-file-at" 2 path))
  (at-result "delete-file-at" (cffi:foreign-funcall "unlinkat" :int (at-directory-fd "delete-file-at" dir)
								:string path :int flags :int))
  *unspecified*)

(defguile "rename-file-at" (old-dir old new-dir new)
  (unless (stringp old) (wrong-type "rename-file-at" 2 old))
  (unless (stringp new) (wrong-type "rename-file-at" 4 new))
  (at-result "rename-file-at"
	     (cffi:foreign-funcall "renameat" :int (at-directory-fd "rename-file-at" old-dir) :string old
					      :int (at-directory-fd "rename-file-at" new-dir) :string new :int))
  *unspecified*)

(defguile "openat" (dir path flags &optional (mode #o666))
  (unless (stringp path) (wrong-type "openat" 2 path))
  (let ((fd (at-result "openat" (cffi:foreign-funcall-varargs "openat" (:int (at-directory-fd "openat" dir)
									  :string path :int flags)
							 :unsigned-int mode :int))))
    (fd-port fd (cond ((logtest flags sb-posix:o-rdwr) "rw")
		      ((logtest flags sb-posix:o-wronly) "w")
		      (t "r")))))

;;; ------------------------------------------------------------------
;;; More of Guile's POSIX: constants, ownership and process attributes,
;;; resource limits, the passwd/group/network databases, select, and
;;; strptime.  libc's, through CFFI; a failing call is a system-error.

(defparameter *posix-constants*
  ;; (name darwin linux): NIL where the platform has none
  '(("EXIT_SUCCESS" 0 0) ("EXIT_FAILURE" 1 1)
    ("F_OK" 0 0) ("R_OK" 4 4) ("W_OK" 2 2) ("X_OK" 1 1)
    ("LOCK_SH" 1 1) ("LOCK_EX" 2 2) ("LOCK_NB" 4 4) ("LOCK_UN" 8 8)
    ("PRIO_PROCESS" 0 0) ("PRIO_PGRP" 1 1) ("PRIO_USER" 2 2)
    ("WAIT_ANY" -1 -1) ("WAIT_MYPGRP" 0 0)
    ("PIPE_BUF" 512 4096) ("NSIG" 32 65) ("SIGSTKSZ" 131072 8192)
    ("F_GETOWN" 5 9) ("F_SETOWN" 6 8)
    ("O_ASYNC" #x40 #o20000) ("O_NDELAY" 4 #o4000) ("O_DIRECT" 0 #o40000) ("O_NOATIME" 0 #o1000000)
    ("O_SHLOCK" #x10 0) ("O_EXLOCK" #x20 0)
    ("O_IGNORE_CTTY" 0 0) ("O_NOLINK" 0 0) ("O_NOTRANS" 0 0)
    ("AT_NO_AUTOMOUNT" 0 #x800)
    ("SIGEMT" 7 nil) ("SIGINFO" 29 nil) ("SIGIOT" 6 6)
    ("EDEADLK" 11 35) ("ENOLCK" 77 37) ("EILSEQ" 92 84) ("EOVERFLOW" 84 75)
    ("ENOTSUP" 45 95) ("EOPNOTSUPP" 102 95) ("EADDRNOTAVAIL" 49 99) ("EAFNOSUPPORT" 47 97)
    ("ECANCELED" 89 125) ("ECONNABORTED" 53 103) ("EDESTADDRREQ" 39 89) ("EDQUOT" 69 122)
    ("EHOSTDOWN" 64 112) ("EIDRM" 90 43) ("EISCONN" 56 106) ("EMSGSIZE" 40 90)
    ("EMULTIHOP" 95 72) ("ENETDOWN" 50 100) ("ENETRESET" 52 102) ("ENETUNREACH" 51 101)
    ("ENOBUFS" 55 105) ("ENODATA" 96 61) ("ENOLINK" 97 67) ("ENOMSG" 91 42)
    ("ENOPROTOOPT" 42 92) ("ENOSR" 98 63) ("ENOSTR" 99 60) ("ENOTBLK" 15 15)
    ("ENOTRECOVERABLE" 104 131) ("EOWNERDEAD" 105 130) ("EPFNOSUPPORT" 46 96)
    ("EPROTO" 100 71) ("EPROTONOSUPPORT" 43 93) ("EPROTOTYPE" 41 91) ("EREMOTE" 71 66)
    ("ESHUTDOWN" 58 108) ("ESOCKTNOSUPPORT" 44 94) ("ESTALE" 70 116) ("ETIME" 101 62)
    ("ETOOMANYREFS" 59 109) ("ETXTBSY" 26 26) ("EUSERS" 68 87) ("EBADMSG" 94 74)
    ("EAUTH" 80 nil) ("EBADRPC" 72 nil) ("EFTYPE" 79 nil) ("ENEEDAUTH" 81 nil)
    ("EPROCLIM" 67 nil) ("EPROCUNAVAIL" 76 nil) ("EPROGMISMATCH" 75 nil)
    ("EPROGUNAVAIL" 74 nil) ("ERPCMISMATCH" 73 nil))
  "Constants the platform's headers define that boot.lisp's list doesn't.")

(defun install-posix-constants ()
  (loop for (name darwin linux) in *posix-constants*
	for value = #+darwin darwin #-darwin linux
	when value do (obarray-define (ssym name) value)))

(defmacro with-c-strings-list ((var strings) &body body)
  "BODY with VAR a NULL-terminated char* array of STRINGS."
  `(let* ((strings ,strings) (n (length strings)))
     (cffi:with-foreign-object (,var :pointer (1+ n))
       (let ((allocated (loop for s in strings for i from 0
			      collect (let ((p (cffi:foreign-string-alloc s)))
					(setf (cffi:mem-aref ,var :pointer i) p)
					p))))
	 (setf (cffi:mem-aref ,var :pointer n) (cffi:null-pointer))
	 (unwind-protect (progn ,@body)
	   (mapc #'cffi:foreign-string-free allocated))))))

(defun c-errno () (sb-alien:extern-alien "errno" sb-alien:int))

(defun check-c (who r)
  (when (minusp r) (system-error who (c-errno)))
  r)

(defun path-or-port-fd (who object)
  "OBJECT's descriptor if it is a port, else NIL (a path string)."
  (cond ((stringp object) nil)
	((gport-p object) (port-fdes object who))
	(t (wrong-type who 1 object))))

(defguile "chown" (object owner group)
  (let ((fd (path-or-port-fd "chown" object)))
    (check-c "chown" (if fd
			 (cffi:foreign-funcall "fchown" :int fd :int owner :int group :int)
			 (cffi:foreign-funcall "chown" :string object :int owner :int group :int))))
  *unspecified*)

(defguile "chown-at" (dir path owner group &optional (flags 0))
  (unless (stringp path) (wrong-type "chown-at" 2 path))
  (check-c "chown-at" (cffi:foreign-funcall "fchownat" :int (at-directory-fd "chown-at" dir) :string path
						       :int owner :int group :int flags :int))
  *unspecified*)

(defguile "open-fdes-at" (dir path flags &optional (mode #o666))
  (unless (stringp path) (wrong-type "open-fdes-at" 2 path))
  (check-c "open-fdes-at"
	   (cffi:foreign-funcall-varargs "openat" (:int (at-directory-fd "open-fdes-at" dir) :string path :int flags)
					 :unsigned-int mode :int)))

(defguile "chroot" (path)
  (unless (stringp path) (wrong-type "chroot" 1 path))
  (check-c "chroot" (cffi:foreign-funcall "chroot" :string path :int))
  *unspecified*)

(defguile "mknod" (path type perms dev)
  (unless (stringp path) (wrong-type "mknod" 1 path))
  (let ((kind (cond ((not (symbolp type)) (wrong-type "mknod" 2 type))
		    (t (let ((name (qs:scheme-symbol-name type)))
			 (cond ((string= name "regular") #o100000)
			       ((string= name "directory") #o040000)
			       ((string= name "symlink") #o120000)
			       ((string= name "block-special") #o060000)
			       ((string= name "char-special") #o020000)
			       ((string= name "fifo") #o010000)
			       ((string= name "socket") #o140000)
			       (t (out-of-range "mknod" type))))))))
    (check-c "mknod" (cffi:foreign-funcall "mknod" :string path :unsigned-int (logior kind perms) :int dev :int)))
  *unspecified*)

(defguile "crypt" (key salt)
  (unless (stringp key) (wrong-type "crypt" 1 key))
  (unless (stringp salt) (wrong-type "crypt" 2 salt))
  (let ((r (cffi:foreign-funcall "crypt" :string key :string salt :pointer)))
    (if (cffi:null-pointer-p r)
	(system-error "crypt" (c-errno))
	(cffi:foreign-string-to-lisp r))))

(defguile "ctermid" () "/dev/tty")

(defguile "getpass" (prompt)
  (unless (stringp prompt) (wrong-type "getpass" 1 prompt))
  (let ((r (cffi:foreign-funcall "getpass" :string prompt :pointer)))
    (if (cffi:null-pointer-p r) qs:false (cffi:foreign-string-to-lisp r))))

(defguile "nice" (increment)
  (setf (sb-alien:extern-alien "errno" sb-alien:int) 0)
  (let ((r (cffi:foreign-funcall "nice" :int increment :int)))
    (when (and (= r -1) (/= 0 (c-errno))) (system-error "nice" (c-errno))))
  *unspecified*)

(defguile "getpriority" (which who)
  (setf (sb-alien:extern-alien "errno" sb-alien:int) 0)
  (let ((r (cffi:foreign-funcall "getpriority" :int which :int who :int)))
    (when (and (= r -1) (/= 0 (c-errno))) (system-error "getpriority" (c-errno)))
    r))

(defguile "setpriority" (which who prio)
  (check-c "setpriority" (cffi:foreign-funcall "setpriority" :int which :int who :int prio :int))
  *unspecified*)

(defun rlim-in (n)
  (if (eq n qs:false) (1- (ash 1 63)) n))

(defun rlim-out (n)
  (if (or (= n (1- (ash 1 63))) (= n (1- (ash 1 64)))) qs:false n))

(defguile "getrlimit" (resource)
  (cffi:with-foreign-object (r :uint64 2)
    (check-c "getrlimit" (cffi:foreign-funcall "getrlimit" :int resource :pointer r :int))
    (values (rlim-out (cffi:mem-aref r :uint64 0)) (rlim-out (cffi:mem-aref r :uint64 1)))))

(defguile "setrlimit" (resource soft hard)
  (cffi:with-foreign-object (r :uint64 2)
    (setf (cffi:mem-aref r :uint64 0) (rlim-in soft) (cffi:mem-aref r :uint64 1) (rlim-in hard))
    (check-c "setrlimit" (cffi:foreign-funcall "setrlimit" :int resource :pointer r :int)))
  *unspecified*)

(defguile "getsid" (pid) (check-c "getsid" (cffi:foreign-funcall "getsid" :int pid :int)))
(defguile "setsid" () (check-c "setsid" (cffi:foreign-funcall "setsid" :int :int)))
(defguile "setpgid" (pid pgid)
  (check-c "setpgid" (cffi:foreign-funcall "setpgid" :int pid :int pgid :int))
  *unspecified*)
(defguile "seteuid" (id)
  (check-c "seteuid" (cffi:foreign-funcall "seteuid" :unsigned-int id :int))
  *unspecified*)
(defguile "setegid" (id)
  (check-c "setegid" (cffi:foreign-funcall "setegid" :unsigned-int id :int))
  *unspecified*)

(defguile "getgroups" ()
  (let ((n (check-c "getgroups" (cffi:foreign-funcall "getgroups" :int 0 :pointer (cffi:null-pointer) :int))))
    (cffi:with-foreign-object (g :unsigned-int (max n 1))
      (let ((n (check-c "getgroups" (cffi:foreign-funcall "getgroups" :int n :pointer g :int))))
	(coerce (loop for i below n collect (cffi:mem-aref g :unsigned-int i)) 'simple-vector)))))

(defguile "setgroups" (groups)
  (unless (simple-vector-p groups) (wrong-type "setgroups" 1 groups))
  (let ((n (length groups)))
    (cffi:with-foreign-object (g :unsigned-int (max n 1))
      (dotimes (i n) (setf (cffi:mem-aref g :unsigned-int i) (svref groups i)))
      (check-c "setgroups" (cffi:foreign-funcall "setgroups" :int n :pointer g :int))))
  *unspecified*)

(defguile "sethostname" (name)
  (unless (stringp name) (wrong-type "sethostname" 1 name))
  (check-c "sethostname" (cffi:foreign-funcall "sethostname" :string name :int (length name) :int))
  *unspecified*)

(defguile "tcgetpgrp" (port)
  (check-c "tcgetpgrp" (cffi:foreign-funcall "tcgetpgrp" :int (port-fdes port "tcgetpgrp") :int)))
(defguile "tcsetpgrp" (port pgid)
  (check-c "tcsetpgrp" (cffi:foreign-funcall "tcsetpgrp" :int (port-fdes port "tcsetpgrp") :int pgid :int))
  *unspecified*)

(defguile "pause" ()
  (cffi:foreign-funcall "pause" :int))

(defguile "execle" (program env &rest args)
  (unless (stringp program) (wrong-type "execle" 1 program))
  (flush-all-ports)
  (with-c-strings-list (argv args)
    (with-c-strings-list (envp (loop for tail = env then (cdr tail) while (consp tail) collect (car tail)))
      (cffi:foreign-funcall "execve" :string program :pointer argv :pointer envp :int)
      (system-error "execle" (c-errno)))))

;;; The user, group and network databases: the entry procedures that walk
;;; the file (getpw, getgr, gethost, ... with no argument; setpw, ... to
;;; rewind or close), on libc's get*ent.

(defun c-string-list (p)
  "The strings of the NULL-terminated char* array at P."
  (if (cffi:null-pointer-p p)
      '()
      (loop for i from 0
	    for s = (cffi:mem-aref p :pointer i)
	    until (cffi:null-pointer-p s)
	    collect (cffi:foreign-string-to-lisp s))))

(defun c-string-at (p offset)
  (let ((s (cffi:mem-ref p :pointer offset)))
    (if (cffi:null-pointer-p s) "" (cffi:foreign-string-to-lisp s))))

(defun passwd-vector (pw)
  (vector (c-string-at pw 0) (c-string-at pw 8) (cffi:mem-ref pw :unsigned-int 16) (cffi:mem-ref pw :unsigned-int 20)
	  #+darwin (c-string-at pw 40) #-darwin (c-string-at pw 24)
	  #+darwin (c-string-at pw 48) #-darwin (c-string-at pw 32)
	  #+darwin (c-string-at pw 56) #-darwin (c-string-at pw 40)))

(defun group-vector (gr)
  (vector (c-string-at gr 0) (c-string-at gr 8) (cffi:mem-ref gr :unsigned-int 16)
	  (c-string-list (cffi:mem-ref gr :pointer 24))))

(defun database-entry (who pointer vector-function)
  "The vector of the entry at POINTER; a missing entry is a system-error, as Guile's."
  (if (cffi:null-pointer-p pointer)
      (system-error who (c-errno))
      (funcall vector-function pointer)))

(defguile "getpw" (&optional user)
  (let ((pw (cond ((or (null user) (eq user qs:false)) (cffi:foreign-funcall "getpwent" :pointer))
		  ((integerp user) (cffi:foreign-funcall "getpwuid" :unsigned-int user :pointer))
		  ((stringp user) (cffi:foreign-funcall "getpwnam" :string user :pointer))
		  (t (wrong-type "getpw" 1 user)))))
    (cond ((not (cffi:null-pointer-p pw)) (passwd-vector pw))
	  ((or (null user) (eq user qs:false)) qs:false)
	  (t (system-error "getpw" (c-errno))))))

(defguile "setpw" (&optional arg)
  (if (and arg (truthy arg))
      (cffi:foreign-funcall "setpwent" :void)
      (cffi:foreign-funcall "endpwent" :void))
  *unspecified*)

(defguile "getgr" (&optional group)
  (let ((gr (cond ((or (null group) (eq group qs:false)) (cffi:foreign-funcall "getgrent" :pointer))
		  ((integerp group) (cffi:foreign-funcall "getgrgid" :unsigned-int group :pointer))
		  ((stringp group) (cffi:foreign-funcall "getgrnam" :string group :pointer))
		  (t (wrong-type "getgr" 1 group)))))
    (cond ((not (cffi:null-pointer-p gr)) (group-vector gr))
	  ((or (null group) (eq group qs:false)) qs:false)
	  (t (system-error "getgr" 0)))))

(defguile "setgr" (&optional arg)
  (if (and arg (truthy arg))
      (cffi:foreign-funcall "setgrent" :void)
      (cffi:foreign-funcall "endgrent" :void))
  *unspecified*)

(defun host-vector (h)
  (let ((length (cffi:mem-ref h :int 20)) (addresses (cffi:mem-ref h :pointer 24)))
    (vector (c-string-at h 0) (c-string-list (cffi:mem-ref h :pointer 8)) (cffi:mem-ref h :int 16) length
	    (loop for i from 0
		  for a = (cffi:mem-aref addresses :pointer i)
		  until (cffi:null-pointer-p a)
		  collect (loop with n = 0 for j below length
				do (setf n (+ (* n 256) (cffi:mem-aref a :uint8 j)))
				finally (return n))))))

(defun net-vector (n)
  (vector (c-string-at n 0) (c-string-list (cffi:mem-ref n :pointer 8)) (cffi:mem-ref n :int 16)
	  (cffi:mem-ref n :unsigned-int 20)))

(defun proto-vector (p)
  (vector (c-string-at p 0) (c-string-list (cffi:mem-ref p :pointer 8)) (cffi:mem-ref p :int 16)))

(defun serv-vector (s)
  (let ((port (cffi:mem-ref s :int 16)))
    (vector (c-string-at s 0) (c-string-list (cffi:mem-ref s :pointer 8))
	    (+ (ash (logand port #xff) 8) (logand (ash port -8) #xff)) (c-string-at s 24))))

(defmacro def-netdb (getter setter vector-function (&rest lookups) next end)
  "GETTER with no argument is the next entry (#f at the end), else looks
one up by the first matching LOOKUP, (type-test c-function arg-types...)."
  `(progn
     (defguile ,getter (&optional (key nil) (key2 nil))
       (declare (ignorable key2))
       (let ((entry (cond ((null key) (cffi:foreign-funcall ,next :pointer))
			  ,@(loop for (test form) in lookups collect `(,test ,form))
			  (t (wrong-type ,getter 1 key)))))
	 (cond ((not (cffi:null-pointer-p entry)) (,vector-function entry))
	       ((null key) qs:false)
	       (t (guile-error (ssym "misc-error") ,getter "no such ~A" (list ,getter) qs:false)))))
     (defguile ,setter (&optional arg)
       (if (and arg (truthy arg))
	   (cffi:foreign-funcall ,(format nil "set~A" (subseq next 3)) :int 1 :void)
	   (cffi:foreign-funcall ,end :void))
       *unspecified*)))

(def-netdb "gethost" "sethost" host-vector
  (((stringp key) (cffi:foreign-funcall "gethostbyname" :string key :pointer)))
  "gethostent" "endhostent")
(def-netdb "getnet" "setnet" net-vector
  (((stringp key) (cffi:foreign-funcall "getnetbyname" :string key :pointer))
   ((integerp key) (cffi:foreign-funcall "getnetbyaddr" :unsigned-int key :int 2 :pointer)))
  "getnetent" "endnetent")
(def-netdb "getproto" "setproto" proto-vector
  (((stringp key) (cffi:foreign-funcall "getprotobyname" :string key :pointer))
   ((integerp key) (cffi:foreign-funcall "getprotobynumber" :int key :pointer)))
  "getprotoent" "endprotoent")

(defguile "getserv" (&optional name proto)
  (let ((entry (cond ((null name) (cffi:foreign-funcall "getservent" :pointer))
		     ((not (stringp proto)) (wrong-type "getserv" 2 proto))
		     ((stringp name) (cffi:foreign-funcall "getservbyname" :string name :string proto :pointer))
		     ((integerp name)
		      (cffi:foreign-funcall "getservbyport" :int (+ (ash (logand name #xff) 8) (logand (ash name -8) #xff))
							    :string proto :pointer))
		     (t (wrong-type "getserv" 1 name)))))
    (cond ((not (cffi:null-pointer-p entry)) (serv-vector entry))
	  ((null name) qs:false)
	  (t (guile-error (ssym "misc-error") "getserv" "no such service ~A" (list name) qs:false)))))
(defguile "setserv" (&optional arg)
  (if (and arg (truthy arg))
      (cffi:foreign-funcall "setservent" :int 1 :void)
      (cffi:foreign-funcall "endservent" :void))
  *unspecified*)

(defguile "inet-netof" (address)
  (cond ((< address #x80000000) (ash address -24))
	((< address #xc0000000) (ash address -16))
	(t (ash address -8))))
(defguile "inet-lnaof" (address)
  (cond ((< address #x80000000) (logand address #xffffff))
	((< address #xc0000000) (logand address #xffff))
	(t (logand address #xff))))
(defguile "inet-makeaddr" (net host)
  (cond ((< net 256) (logior (ash net 24) (logand host #xffffff)))
	((< net 65536) (logior (ash net 16) (logand host #xffff)))
	(t (logior (ash net 8) (logand host #xff)))))

;;; strftime and strptime are libc's, on the broken-down time as a tm.

(defun call-with-tm (v who function)
  "FUNCTION on a struct tm holding the broken-down time V (zone included)."
  (unless (and (simple-vector-p v) (= (length v) 11) (every #'integerp (subseq v 0 10)))
    (wrong-type who 1 v))
  (cffi:with-foreign-object (tm :uint8 +tm-size+)
    (vector->tm v tm)
    (let ((zone (and (stringp (svref v 10)) (cffi:foreign-string-alloc (svref v 10)))))
      (setf (cffi:mem-ref tm :pointer 48) (or zone (cffi:null-pointer)))
      (unwind-protect (funcall function tm)
	(when zone (cffi:foreign-string-free zone))))))

(defun libc-strftime (format tm)
  (let ((octets (sb-ext:string-to-octets format :external-format :utf-8 :null-terminate t)))
    (call-with-tm tm "strftime"
      (lambda (ctm)
	(cffi:with-foreign-object (fmt :uint8 (length octets))
	  (dotimes (i (length octets)) (setf (cffi:mem-aref fmt :uint8 i) (aref octets i)))
	  (loop for size = 256 then (* size 4)
		do (cffi:with-foreign-object (buf :uint8 size)
		     (let ((n (cffi:foreign-funcall "strftime" :pointer buf :unsigned-long size :pointer fmt
							       :pointer ctm :unsigned-long)))
		       ;; 0 is "too small", or an empty result
		       (when (or (plusp n) (>= size 1048576) (= (length octets) 1))
			 (let ((out (make-array n :element-type '(unsigned-byte 8))))
			   (dotimes (i n) (setf (aref out i) (cffi:mem-aref buf :uint8 i)))
			   (return (sb-ext:octets-to-string out :external-format :utf-8))))))))))))

(defguile "strftime" (format tm)
  (unless (stringp format) (wrong-type "strftime" 1 format))
  (libc-strftime format tm))

(defguile "strptime" (format string)
  (unless (stringp format) (wrong-type "strptime" 1 format))
  (unless (stringp string) (wrong-type "strptime" 2 string))
  (let ((input (sb-ext:string-to-octets string :external-format :utf-8 :null-terminate t))
	(fmt (sb-ext:string-to-octets format :external-format :utf-8 :null-terminate t)))
    (cffi:with-foreign-objects ((tm :uint8 +tm-size+) (in :uint8 (length input)) (f :uint8 (length fmt)))
      (dotimes (i +tm-size+) (setf (cffi:mem-aref tm :uint8 i) 0))
      (setf (cffi:mem-aref tm :int 8) -1)
      (dotimes (i (length input)) (setf (cffi:mem-aref in :uint8 i) (aref input i)))
      (dotimes (i (length fmt)) (setf (cffi:mem-aref f :uint8 i) (aref fmt i)))
      (let ((end (cffi:foreign-funcall "strptime" :pointer in :pointer f :pointer tm :pointer)))
	(when (cffi:null-pointer-p end) (system-error "strptime" 22))
	(let ((v (tm->vector tm)))
	  ;; the zone is never reported
	  (setf (svref v 10) qs:false)
	  (cons v (length (sb-ext:octets-to-string input :external-format :utf-8
							 :end (- (cffi:pointer-address end) (cffi:pointer-address in))))))))))

;;; select, on poll(2): a port with input already buffered is ready.

(defun select-items (who position list)
  (cond ((listp list) list)
	((simple-vector-p list) (coerce list 'list))
	(t (wrong-type who position list))))

(defguile "select" (reads writes excepts &optional secs usecs)
  (let* ((reads (select-items "select" 1 reads)) (writes (select-items "select" 2 writes))
	 (excepts (select-items "select" 3 excepts))
	 (all (append (mapcar (lambda (x) (list x 1)) reads) (mapcar (lambda (x) (list x 4)) writes)
		      (mapcar (lambda (x) (list x 2)) excepts)))
	 (n (length all))
	 (timeout (cond ((not (integerp secs)) -1)
			(t (+ (* 1000 secs) (if (integerp usecs) (floor usecs 1000) 0)))))
	 (ready '()))
    (flet ((fd-of (x)
	     (cond ((integerp x) x)
		   ((gport-p x) (or (port-fd x) (wrong-type "select" 1 x)))
		   ((streamp x) (port-fdes x "select"))
		   (t (wrong-type "select" 1 x)))))
      (dolist (item all) (fd-of (first item)))
      ;; buffered input needs no wait
      (loop for (x events) in all
	    when (and (= events 1) (gport-p x) (trivial-gray-streams:stream-listen x)) do (push x ready))
      (cffi:with-foreign-object (fds :uint8 (* 8 (max n 1)))
	(loop for (x events) in all for i from 0
	      do (setf (cffi:mem-ref fds :int (* 8 i)) (fd-of x)
		       (cffi:mem-ref fds :short (+ (* 8 i) 4)) events
		       (cffi:mem-ref fds :short (+ (* 8 i) 6)) 0))
	(check-c "select" (cffi:foreign-funcall "poll" :pointer fds :unsigned-int n :int (if ready 0 timeout) :int))
	(flet ((result (kind)
		 (loop for (x events) in all for i from 0
		       when (and (= events kind)
				 (or (member x ready :test #'eq)
				     (logtest (cffi:mem-ref fds :short (+ (* 8 i) 6))
					      (logior kind (if (= kind 1) #x10 0) #x8)))) ; POLLHUP, POLLERR
			 collect x)))
	  (list (result 1) (result 4) (result 2)))))))

;;; GC switches, print states: nothing to turn off here

(defguile "gc-disable" () *unspecified*)
(defguile "gc-enable" () *unspecified*)
(defguile "gc-dump" () *unspecified*)
(defguile "dynamic-unlink" (lib) (declare (ignore lib)) *unspecified*)
(defguile "get-print-state" (port) (declare (ignore port)) qs:false)
(defguile "port-with-print-state" (port &optional state) (declare (ignore state)) port)
(defguile "has-shown-backtrace-hint?" () qs:false)
(defguile "set-procedure-minimum-arity!" (proc required optional rest)
  (declare (ignore required optional rest proc))
  *unspecified*)
