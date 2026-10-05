; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PS; -*-

;;;; Scheme numbers on Common Lisp numbers
;;;;
;;;; CL already has Scheme's numeric tower (integers, ratios, floats,
;;;; complexes); what differs is the edges, which this file fixes:
;;;;
;;;;  * Inexact means DOUBLE-FLOAT.  CL's transcendental functions return
;;;;    SINGLE-FLOATs for rational arguments ((sqrt 2) => 1.4142135), and
;;;;    FLOAT of a rational is single by default.
;;;;  * Exactness and integer-ness are separate: (integer? 3.0) is #t,
;;;;    (floor 3.5) is 3.0, (max 1 2.0) is 2.0 (R7RS 6.2.6).
;;;;  * Syntax: #x #b #o #d #e #i prefixes, rationals, decimals with any
;;;;    exponent marker, +inf.0 -inf.0 +nan.0, rectangular and polar
;;;;    complexes (R7RS 7.1.1, R6RS 4.2.8) -- parsed here rather than by
;;;;    the CL reader, and printed the way Scheme prints them.
;;;;
;;;; builtin.scm's integrations and rts.lisp's STRING->NUMBER /
;;;; NUMBER->STRING call these.

(in-package "PS")

(defconstant +inf+ float-features:double-float-positive-infinity)
(defconstant -inf+ float-features:double-float-negative-infinity)
(defparameter +nan+ float-features:double-float-nan)

(defun nan-p (x) (and (floatp x) (float-features:float-nan-p x)))
(defun infinite-p (x) (and (floatp x) (float-features:float-infinity-p x)))

(defun call-with-ieee-arithmetic (thunk)
  "Run THUNK with float traps off, so inexact arithmetic produces
infinities and NaNs (R6RS 3.5, R7RS 6.2.4) instead of signalling."
  (float-features:with-float-traps-masked t
    (funcall thunk)))

(defun disable-float-traps ()
  "Turn float traps off for the rest of this thread (the command-line
program does this once at startup).  float-features only masks traps
around a body, so this is per implementation."
  #+sbcl (sb-int:set-floating-point-modes :traps '())
  #+ecl (ext:trap-fpe t nil)
  #+abcl (ext:set-floating-point-modes :traps '())
  #+ccl (ccl:set-fpu-mode :overflow nil :underflow nil :division-by-zero nil
			  :invalid nil :inexact nil)
  t)

;;; ------------------------------------------------------------------
;;; Exactness and the numeric predicates

(defun inexact (x)
  "EXACT->INEXACT: double floats throughout."
  (typecase x
    (double-float x)
    (real (coerce x 'double-float))
    (complex (complex (coerce (realpart x) 'double-float)
		      (coerce (imagpart x) 'double-float)))
    (t (scheme-error "inexact: not a number: ~S" x))))

(defun exact (x)
  "INEXACT->EXACT: the exact value of a float (not a nearby simpler one)."
  (typecase x
    (rational x)
    (float (when (or (nan-p x) (infinite-p x))
	     (scheme-error "exact: no exact representation for ~S" x))
	   (rational x))
    (complex (let ((r (exact (realpart x))) (i (exact (imagpart x))))
	       (if (zerop i) r (complex r i))))
    (t (scheme-error "exact: not a number: ~S" x))))

(defun exact-p (x)
  (typecase x
    (rational t)
    (float nil)
    (complex (rationalp (realpart x)))
    (t (scheme-error "exact?: not a number: ~S" x))))

(defun inexact-p (x) (not (exact-p x)))

(defun real-number-p (x) (realp x))

(defun rational-number-p (x)
  (or (rationalp x) (and (floatp x) (not (nan-p x)) (not (infinite-p x)))))

(defun integer-number-p (x)
  (or (integerp x)
      (and (floatp x) (rational-number-p x) (= x (ffloor x)))))

(defun dbl (x)
  "Make a CL result Scheme-inexact if it's a single float."
  (typecase x
    (single-float (coerce x 'double-float))
    (complex (if (typep (realpart x) 'single-float) (inexact x) x))
    (t x)))

(defun arg (x)
  "Prepare an argument for a CL transcendental function, which would
compute an exact one in single floats."
  (cond ((rationalp x) (coerce x 'double-float))
	((and (complexp x) (rationalp (realpart x)))
	 (complex (coerce (realpart x) 'double-float)
		  (coerce (imagpart x) 'double-float)))
	(t x)))

;;; ------------------------------------------------------------------
;;; Operations whose exactness CL gets differently

(defmacro def-rounding (name exact-op float-op)
  `(defun ,name (x)
     (cond ((rationalp x) (values (,exact-op x)))
	   ((floatp x) (if (or (nan-p x) (infinite-p x)) x (values (,float-op x))))
	   (t (scheme-error "~(~A~): not a real number: ~S" ',name x)))))

(def-rounding scheme-floor floor ffloor)
(def-rounding scheme-ceiling ceiling fceiling)
(def-rounding scheme-truncate truncate ftruncate)
(def-rounding scheme-round round fround)	; both round to even, as Scheme does

(defun scheme-max (x &rest more)
  (let ((m (reduce #'max more :initial-value x)))
    (if (some #'floatp (cons x more)) (inexact m) m)))

(defun scheme-min (x &rest more)
  (let ((m (reduce #'min more :initial-value x)))
    (if (some #'floatp (cons x more)) (inexact m) m)))

(defun scheme-sqrt (x)
  ;; Exact for exact perfect squares (R7RS: (sqrt 9) => 3).
  (cond ((and (integerp x) (>= x 0))
	 (let ((r (isqrt x))) (if (= (* r r) x) r (sqrt (arg x)))))
	((and (rationalp x) (> x 0))
	 (let ((n (scheme-sqrt (numerator x))) (d (scheme-sqrt (denominator x))))
	   (if (and (integerp n) (integerp d)) (/ n d) (sqrt (arg x)))))
	((and (integerp x) (< x 0))
	 (let ((r (scheme-sqrt (- x))))
	   (if (integerp r) (complex 0 r) (dbl (sqrt (arg x))))))
	(t (dbl (sqrt (arg x))))))

(defun scheme-exp (x) (if (eql x 0) 1 (dbl (exp (arg x)))))

(defun scheme-log (x &optional (base nil base-p))
  (cond (base-p (/ (scheme-log x) (scheme-log base)))
	((eql x 1) 0)
	(t (dbl (log (arg x))))))

(defun scheme-sin (x) (if (eql x 0) 0 (dbl (sin (arg x)))))
(defun scheme-cos (x) (if (eql x 0) 1 (dbl (cos (arg x)))))
(defun scheme-tan (x) (if (eql x 0) 0 (dbl (tan (arg x)))))
(defun scheme-asin (x) (if (eql x 0) 0 (dbl (asin (arg x)))))
(defun scheme-acos (x) (if (eql x 1) 0 (dbl (acos (arg x)))))
(defun scheme-atan (y &optional (x nil x-p))
  (if x-p
      (if (and (eql y 0) (rationalp x) (> x 0)) 0 (dbl (atan (arg y) (arg x))))
      (if (eql y 0) 0 (dbl (atan (arg y))))))

(defun scheme-magnitude (z)
  (cond ((rationalp z) (abs z))
	;; Exact when it can be: (magnitude 3+4i) => 5.
	((rationalp (realpart z))
	 (scheme-sqrt (+ (* (realpart z) (realpart z)) (* (imagpart z) (imagpart z)))))
	(t (abs z))))

(defun scheme-angle (z)
  (if (and (rationalp z) (>= z 0)) 0 (dbl (phase (arg z)))))

(defun scheme-make-polar (magnitude angle)
  (if (eql angle 0) magnitude (* magnitude (cis (arg angle)))))

(defun scheme-expt (base power)
  (dbl (if (and (rationalp base) (floatp power)) (expt (arg base) power) (expt base power))))

;;; Comparisons.  CL compares a rational with a float by converting the
;;; float to a rational, which fails for infinities and NaNs; Scheme
;;; says NaN compares false with everything and infinities are beyond
;;; every finite number.

(defun finite-stand-in (x other)
  "X if finite; for an infinity compared with OTHER, a finite number on
the same side of every finite OTHER."
  (cond ((not (infinite-p x)) x)
	((rationalp other) (if (> x 0) (1+ (abs other)) (- (1+ (abs other)))))
	(t x)))

(defun compare2 (op a b)
  (cond ((or (nan-p a) (nan-p b)) nil)
	((and (realp a) (realp b)
	      (or (and (infinite-p a) (rationalp b)) (and (infinite-p b) (rationalp a))))
	 (if (eq op '=)
	     nil
	     (funcall op (finite-stand-in a b) (finite-stand-in b a))))
	(t (funcall op a b))))

(defmacro def-compare (name op)
  `(progn
     (defun ,name (a b &rest more)
       (and (compare2 ',op a b)
	    (loop for (x y) on (cons b more)
		  while y
		  always (compare2 ',op x y))))
     ;; Two arguments, both fixnums or both double-floats: CL's own
     ;; comparison, inline (IEEE comparisons are already false for NaN
     ;; and order infinities).  The translator integrates (< a b) as a
     ;; call to this, so this is what Scheme arithmetic tests compile to.
     (define-compiler-macro ,name (&whole form a b &rest more)
       (if more
	   form
	   (let ((x (gensym "A")) (y (gensym "B")))
	     (list 'let (list (list x a) (list y b))
		   (list 'cond
			 (list (list 'and (list 'typep x ''fixnum) (list 'typep y ''fixnum))
			       (list ',op x y))
			 (list (list 'and (list 'typep x ''double-float) (list 'typep y ''double-float))
			       (list ',op x y))
			 (list t (list 'compare2 '',op x y)))))))))

(def-compare scheme= =)
(def-compare scheme< <)
(def-compare scheme> >)
(def-compare scheme<= <=)
(def-compare scheme>= >=)

;;; ------------------------------------------------------------------
;;; Parsing (STRING->NUMBER and the reader)

(defun digit-value* (c radix)
  (digit-char-p c radix))

(defun parse-uinteger (s start end radix)
  (when (and (< start end)
	     (loop for i from start below end always (digit-value* (char s i) radix)))
    (parse-integer s :start start :end end :radix radix)))

(defun exponent-marker-p (c)
  (member (char-downcase c) '(#\e #\s #\f #\d #\l)))

(defun parse-decimal (s start end)
  "Digits with optional point and exponent, as an exact rational."
  (let ((i start) (mantissa 0) (scale 0) (digits 0))
    (loop while (and (< i end) (digit-char-p (char s i)))
	  do (setq mantissa (+ (* mantissa 10) (digit-char-p (char s i))))
	     (incf digits) (incf i))
    (when (and (< i end) (char= (char s i) #\.))
      (incf i)
      (loop while (and (< i end) (digit-char-p (char s i)))
	    do (setq mantissa (+ (* mantissa 10) (digit-char-p (char s i))))
	       (incf digits) (decf scale) (incf i)))
    (when (zerop digits) (return-from parse-decimal nil))
    (when (and (< i end) (exponent-marker-p (char s i)))
      (incf i)
      (let ((sign 1))
	(when (and (< i end) (member (char s i) '(#\+ #\-)))
	  (when (char= (char s i) #\-) (setq sign -1))
	  (incf i))
	(let ((e (parse-uinteger s i end 10)))
	  (unless e (return-from parse-decimal nil))
	  (incf scale (* sign e))
	  (setq i end))))
    ;; R6RS mantissa width suffix, |53: accepted and ignored.
    (when (and (< i end) (char= (char s i) #\|))
      (unless (parse-uinteger s (1+ i) end 10) (return-from parse-decimal nil))
      (setq i end))
    (when (= i end)
      (* mantissa (expt 10 scale)))))

(defun parse-ureal (s start end radix)
  "Returns the value and whether it is inexact by default."
  (let ((slash (position #\/ s :start start :end end)))
    (cond (slash
	   (let ((n (parse-uinteger s start slash radix))
		 (d (parse-uinteger s (1+ slash) end radix)))
	     (and n d (not (zerop d)) (values (/ n d) nil))))
	  ((parse-uinteger s start end radix)
	   (values (parse-uinteger s start end radix) nil))
	  ((and (= radix 10)
		(find-if (lambda (c) (or (char= c #\.) (exponent-marker-p c))) s :start start :end end))
	   (let ((v (parse-decimal s start end)))
	     (and v (values v t)))))))

(defun parse-real (s start end radix)
  "Returns value and inexact-by-default-p, or NIL."
  (when (>= start end) (return-from parse-real nil))
  (let ((text (string-downcase (subseq s start end))))
    (cond ((string= text "+inf.0") (values +inf+ t :special))
	  ((string= text "-inf.0") (values -inf+ t :special))
	  ((or (string= text "+nan.0") (string= text "-nan.0")) (values +nan+ t :special))
	  (t
	   (let ((sign 1) (i start))
	     (when (member (char s i) '(#\+ #\-))
	       (when (char= (char s i) #\-) (setq sign -1))
	       (incf i))
	     (multiple-value-bind (v inexact) (parse-ureal s i end radix)
	       (cond ((null v) nil)
		     ;; -0.0 is its own number (R6RS 3.6)
		     ((and inexact (zerop v) (= sign -1)) (values -0d0 t))
		     (t (values (* sign v) inexact)))))))))

(defun apply-exactness (value inexact-default exactness)
  (when (eql value -0d0)
    (return-from apply-exactness (if (eq exactness :exact) 0 -0d0)))
  (case exactness
    (:exact (if (and (floatp value) (or (nan-p value) (infinite-p value)))
		(return-from apply-exactness nil)
		(exact value)))
    (:inexact (inexact value))
    (t (if inexact-default (inexact value) value))))

(defun imaginary-split (s start end)
  "For text ending in i: the index of the sign starting the imaginary
part, or NIL.  The sign can't be the first character of a real part or
follow an exponent marker."
  (loop for i from (1- end) downto start
	for c = (char s i)
	when (and (member c '(#\+ #\-))
		  (or (= i start)
		      (not (and (exponent-marker-p (char s (1- i)))
				(> (1- i) start)
				(digit-char-p (char s (- i 2)))))))
	  return i))

(defun parse-complex (s start end radix exactness)
  (flet ((real-part* (a b)
	   (multiple-value-bind (v inexact) (parse-real s a b radix)
	     (and v (apply-exactness v inexact exactness))))
	 (imag (a b)
	   ;; text from a sign up to (not including) the i
	   (if (= (- b a) 1)
	       (apply-exactness (if (char= (char s a) #\-) -1 1) nil exactness)
	       (multiple-value-bind (v inexact) (parse-real s a b radix)
		 (and v (apply-exactness v inexact exactness))))))
    (cond
      ((and (> end start) (char-equal (char s (1- end)) #\i))
       (let ((split (imaginary-split s start (1- end))))
	 (when split
	   (let ((im (imag split (1- end)))
		 (re (if (= split start) 0 (real-part* start split))))
	     (and re im (if (and (rationalp im) (zerop im)) re (complex re im)))))))
      ((position #\@ s :start start :end end)
       (let* ((at (position #\@ s :start start :end end))
	      (mag (real-part* start at))
	      (ang (real-part* (1+ at) end)))
	 (and mag ang
	      (if (eql ang 0) mag
		  (let ((z (* mag (cis (arg ang)))))
		    (dbl z))))))
      (t (real-part* start end)))))

(defun parse-scheme-number (string &optional (radix 10))
  "The number STRING denotes, or NIL (STRING->NUMBER)."
  (let ((start 0) (end (length string)) (exactness nil) (radix radix))
    (loop while (and (< (1+ start) end) (char= (char string start) #\#))
	  do (case (char-downcase (char string (1+ start)))
	       (#\x (setq radix 16)) (#\b (setq radix 2)) (#\o (setq radix 8)) (#\d (setq radix 10))
	       (#\e (if exactness (return-from parse-scheme-number nil) (setq exactness :exact)))
	       (#\i (if exactness (return-from parse-scheme-number nil) (setq exactness :inexact)))
	       (t (return-from parse-scheme-number nil)))
	     (incf start 2))
    (ignore-errors (parse-complex string start end radix exactness))))

;;; ------------------------------------------------------------------
;;; Printing (NUMBER->STRING and the writer)

(defun format-real (x radix)
  (cond ((rationalp x)
	 (let ((s (let ((*print-base* radix) (*print-radix* nil)) (princ-to-string x))))
	   (string-downcase s)))
	((nan-p x) "+nan.0")
	((infinite-p x) (if (> x 0) "+inf.0" "-inf.0"))
	(t (let ((*read-default-float-format* 'double-float))
	     (let ((s (prin1-to-string (coerce x 'double-float))))
	       ;; CL writes 1.0e10 as "1.0e10", 1.0e-5 as "1.0e-5" and
	       ;; 123.0 as "123.0", all valid Scheme.
	       s)))))

(defun format-scheme-number (x &optional (radix 10))
  (if (complexp x)
      (let ((re (realpart x)) (im (imagpart x)))
	(format nil "~A~A~Ai"
		(if (and (rationalp re) (zerop re)) "" (format-real re radix))
		(if (or (nan-p im) (infinite-p im) (minusp im)) "" "+")
		(format-real im radix)))
      (format-real x radix)))
