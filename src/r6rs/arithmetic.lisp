; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-R6RS -*-

;;;; (rnrs arithmetic fixnums (6)), (rnrs arithmetic flonums (6)) and
;;;; (rnrs arithmetic bitwise (6)): library report chapter 11.
;;;;
;;;; Fixnums are CL fixnums (on 64-bit SBCL, 63 bits: (fixnum-width) is
;;;; 63, as R6RS defines it, one more than the bits of
;;;; MOST-POSITIVE-FIXNUM).  Flonums are double floats.  Bitwise
;;;; operations are CL's LOGxxx on integers, which already use the
;;;; two's-complement view R6RS specifies.

(in-package "PSEUDOSCHEME-R6RS")

(defun implementation-restriction (who message &rest irritants)
  (ps-r7rs:raise-object
   (funcall (prim "condition")
	    (funcall (prim "make-implementation-restriction-violation"))
	    (funcall (prim "make-who-condition") who)
	    (funcall (prim "make-message-condition") message)
	    (funcall (prim "make-irritants-condition") irritants))
   nil))

;;; ------------------------------------------------------------------
;;; Bitwise (11.4) -- written first; the fixnum versions reuse them.

(defun check-int (who x)
  (unless (integerp x) (r6rs-assertion-violation who "not an exact integer" x)))

(defun bit-count* (n)
  (if (>= n 0) (logcount n) (lognot (logcount (lognot n)))))

(defun first-bit-set* (n)
  (if (zerop n) -1 (1- (integer-length (logand n (- n))))))

(defun bit-field* (n start end)
  (ldb (byte (- end start) start) n))

(defun copy-bit-field* (to start end from)
  (let ((mask (ash (1- (ash 1 (- end start))) start)))
    (logior (logand (ash from start) mask) (logandc2 to mask))))

(defun rotate-bit-field* (n start end count)
  (let ((width (- end start)))
    (if (zerop width)
	n
	(let* ((count (mod count width))
	       (field (bit-field* n start end))
	       (rotated (logior (ldb (byte width 0) (ash field count))
				(ash field (- count width)))))
	  (copy-bit-field* n start end rotated)))))

(defun reverse-bit-field* (n start end)
  (let* ((width (- end start))
	 (field (bit-field* n start end))
	 (reversed 0))
    (dotimes (i width)
      (when (logbitp i field) (setf reversed (logior reversed (ash 1 (- width 1 i))))))
    (copy-bit-field* n start end reversed)))

(defun check-range (who start end)
  (unless (and (integerp start) (integerp end) (<= 0 start end))
    (r6rs-assertion-violation who "bad bit range" start end)))

(defprim "bitwise-not" (n) (check-int "bitwise-not" n) (lognot n))
(defprim "bitwise-and" (&rest ns) (dolist (n ns) (check-int "bitwise-and" n)) (apply #'logand ns))
(defprim "bitwise-ior" (&rest ns) (dolist (n ns) (check-int "bitwise-ior" n)) (apply #'logior ns))
(defprim "bitwise-xor" (&rest ns) (dolist (n ns) (check-int "bitwise-xor" n)) (apply #'logxor ns))
(defprim "bitwise-if" (a b c)
  (logior (logand a b) (logandc1 a c)))
(defprim "bitwise-bit-count" (n) (check-int "bitwise-bit-count" n) (bit-count* n))
(defprim "bitwise-length" (n) (check-int "bitwise-length" n) (integer-length n))
(defprim "bitwise-first-bit-set" (n) (check-int "bitwise-first-bit-set" n) (first-bit-set* n))
(defprim "bitwise-bit-set?" (n k) (check-int "bitwise-bit-set?" n) (bool (logbitp k n)))
(defprim "bitwise-copy-bit" (n k bit)
  (if (eql bit 1) (logior n (ash 1 k)) (logandc2 n (ash 1 k))))
(defprim "bitwise-bit-field" (n start end)
  (check-range "bitwise-bit-field" start end) (bit-field* n start end))
(defprim "bitwise-copy-bit-field" (to start end from)
  (check-range "bitwise-copy-bit-field" start end) (copy-bit-field* to start end from))
(defprim "bitwise-rotate-bit-field" (n start end count)
  (check-range "bitwise-rotate-bit-field" start end) (rotate-bit-field* n start end count))
(defprim "bitwise-reverse-bit-field" (n start end)
  (check-range "bitwise-reverse-bit-field" start end) (reverse-bit-field* n start end))
(defprim "bitwise-arithmetic-shift" (n count) (ash n count))
(defprim "bitwise-arithmetic-shift-left" (n count) (ash n count))
(defprim "bitwise-arithmetic-shift-right" (n count) (ash n (- count)))

;;; ------------------------------------------------------------------
;;; Fixnums (11.2)

(defconstant +fixnum-width+ (1+ (integer-length most-positive-fixnum)))

(defun check-fx (who x)
  (unless (typep x 'fixnum) (r6rs-assertion-violation who "not a fixnum" x))
  x)

(defun fx-result (who x)
  (if (typep x 'fixnum)
      x
      (implementation-restriction who "result is not a fixnum" x)))

(defmacro def-fx (name lambda-list &body body)
  "A fixnum primitive: checks every required argument is a fixnum."
  (let ((required (loop for a in lambda-list until (member a '(&rest &optional)) collect a)))
    `(defprim ,name ,lambda-list
       ,@(mapcar (lambda (a) `(check-fx ,name ,a)) required)
       ,@body)))

(defprim "fixnum?" (x) (bool (typep x 'fixnum)))
(defprim "fixnum-width" () +fixnum-width+)
(defprim "least-fixnum" () most-negative-fixnum)
(defprim "greatest-fixnum" () most-positive-fixnum)

(defmacro def-fx-compare (name op)
  `(defprim ,name (a b &rest more)
     (let ((all (list* a b more)))
       (dolist (x all) (check-fx ,name x))
       (bool (apply #',op all)))))
(def-fx-compare "fx=?" =)
(def-fx-compare "fx>?" >)
(def-fx-compare "fx<?" <)
(def-fx-compare "fx>=?" >=)
(def-fx-compare "fx<=?" <=)

(def-fx "fxzero?" (x) (bool (zerop x)))
(def-fx "fxpositive?" (x) (bool (plusp x)))
(def-fx "fxnegative?" (x) (bool (minusp x)))
(def-fx "fxodd?" (x) (bool (oddp x)))
(def-fx "fxeven?" (x) (bool (evenp x)))
(def-fx "fxmax" (x &rest more) (dolist (m more) (check-fx "fxmax" m)) (reduce #'max more :initial-value x))
(def-fx "fxmin" (x &rest more) (dolist (m more) (check-fx "fxmin" m)) (reduce #'min more :initial-value x))
(def-fx "fx+" (a b) (fx-result "fx+" (+ a b)))
(def-fx "fx*" (a b) (fx-result "fx*" (* a b)))
(defprim "fx-" (a &optional (b nil b-p))
  (check-fx "fx-" a)
  (if b-p
      (progn (check-fx "fx-" b) (fx-result "fx-" (- a b)))
      (fx-result "fx-" (- a))))

(defmacro def-fx-division (name fn)
  `(def-fx ,name (a b)
     (when (zerop b) (r6rs-assertion-violation ,name "division by zero" a b))
     (fx-result ,name (,fn a b))))
(def-fx-division "fxdiv" div-of)
(def-fx-division "fxdiv0" div0-of)
(def-fx "fxmod" (a b)
  (when (zerop b) (r6rs-assertion-violation "fxmod" "division by zero" a b))
  (- a (* b (div-of a b))))
(def-fx "fxmod0" (a b)
  (when (zerop b) (r6rs-assertion-violation "fxmod0" "division by zero" a b))
  (- a (* b (div0-of a b))))
(def-fx "fxdiv-and-mod" (a b)
  (when (zerop b) (r6rs-assertion-violation "fxdiv-and-mod" "division by zero" a b))
  (let ((q (fx-result "fxdiv-and-mod" (div-of a b)))) (values q (- a (* b q)))))
(def-fx "fxdiv0-and-mod0" (a b)
  (when (zerop b) (r6rs-assertion-violation "fxdiv0-and-mod0" "division by zero" a b))
  (let ((q (fx-result "fxdiv0-and-mod0" (div0-of a b)))) (values q (- a (* b q)))))

;; (fx+/carry a b c): s = a + b + c, s0 = s mod0 2^w, s1 = s div0 2^w
(defun carry-split (s)
  (let ((m (expt 2 +fixnum-width+)))
    (let ((q (div0-of s m))) (values (- s (* q m)) q))))
(def-fx "fx+/carry" (a b c) (carry-split (+ a b c)))
(def-fx "fx-/carry" (a b c) (carry-split (- a b c)))
(def-fx "fx*/carry" (a b c) (carry-split (+ (* a b) c)))

(def-fx "fxnot" (x) (lognot x))
(defprim "fxand" (&rest xs) (dolist (x xs) (check-fx "fxand" x)) (apply #'logand xs))
(defprim "fxior" (&rest xs) (dolist (x xs) (check-fx "fxior" x)) (apply #'logior xs))
(defprim "fxxor" (&rest xs) (dolist (x xs) (check-fx "fxxor" x)) (apply #'logxor xs))
(def-fx "fxif" (a b c) (logior (logand a b) (logandc1 a c)))
(def-fx "fxbit-count" (x) (bit-count* x))
(def-fx "fxlength" (x) (integer-length x))
(def-fx "fxfirst-bit-set" (x) (first-bit-set* x))

(defun check-fx-index (who k)
  (unless (and (typep k 'fixnum) (<= 0 k) (< k +fixnum-width+))
    (r6rs-assertion-violation who "bad bit index" k)))

(defun check-fx-range (who start end)
  (check-fx-index who start)
  (unless (and (typep end 'fixnum) (<= start end +fixnum-width+))
    (r6rs-assertion-violation who "bad bit range" start end)))

(defun fx-wrap (x)
  "Interpret the low +FIXNUM-WIDTH+ bits of X as a fixnum."
  (let ((low (ldb (byte +fixnum-width+ 0) x)))
    (if (logbitp (1- +fixnum-width+) low) (- low (ash 1 +fixnum-width+)) low)))

(def-fx "fxbit-set?" (x k) (check-fx-index "fxbit-set?" k) (bool (logbitp k x)))
(def-fx "fxcopy-bit" (x k bit)
  (check-fx-index "fxcopy-bit" k)
  (unless (member bit '(0 1)) (r6rs-assertion-violation "fxcopy-bit" "bit must be 0 or 1" bit))
  (fx-wrap (if (eql bit 1) (logior x (ash 1 k)) (logandc2 x (ash 1 k)))))
(def-fx "fxbit-field" (x start end)
  (check-fx-range "fxbit-field" start end) (bit-field* x start end))
(def-fx "fxcopy-bit-field" (to start end from)
  (check-fx-range "fxcopy-bit-field" start end) (fx-wrap (copy-bit-field* to start end from)))
(def-fx "fxrotate-bit-field" (x start end count)
  (check-fx-range "fxrotate-bit-field" start end)
  (unless (and (<= 0 count) (< count (max 1 (- end start))))
    (unless (and (zerop count) (= start end))
      (r6rs-assertion-violation "fxrotate-bit-field" "bad count" count)))
  (fx-wrap (rotate-bit-field* x start end count)))
(def-fx "fxreverse-bit-field" (x start end)
  (check-fx-range "fxreverse-bit-field" start end) (fx-wrap (reverse-bit-field* x start end)))

(defun check-shift (who count)
  (unless (and (typep count 'fixnum) (< (abs count) +fixnum-width+))
    (r6rs-assertion-violation who "bad shift count" count)))
(def-fx "fxarithmetic-shift" (x count)
  (check-shift "fxarithmetic-shift" count) (fx-result "fxarithmetic-shift" (ash x count)))
(def-fx "fxarithmetic-shift-left" (x count)
  (unless (<= 0 count) (r6rs-assertion-violation "fxarithmetic-shift-left" "bad shift count" count))
  (check-shift "fxarithmetic-shift-left" count)
  (fx-result "fxarithmetic-shift-left" (ash x count)))
(def-fx "fxarithmetic-shift-right" (x count)
  (unless (<= 0 count) (r6rs-assertion-violation "fxarithmetic-shift-right" "bad shift count" count))
  (check-shift "fxarithmetic-shift-right" count)
  (ash x (- count)))

;;; ------------------------------------------------------------------
;;; Flonums (11.3)

(defun check-fl (who x)
  (unless (typep x 'double-float) (r6rs-assertion-violation who "not a flonum" x))
  x)

(defmacro def-fl (name lambda-list &body body)
  (let ((required (loop for a in lambda-list until (member a '(&rest &optional)) collect a)))
    `(defprim ,name ,lambda-list
       ,@(mapcar (lambda (a) `(check-fl ,name ,a)) required)
       ,@body)))

(defmacro ieee (&body body)
  `(ps:call-with-ieee-arithmetic (lambda () ,@body)))

(defprim "flonum?" (x) (bool (typep x 'double-float)))
(defprim "real->flonum" (x)
  (unless (realp x) (r6rs-assertion-violation "real->flonum" "not a real number" x))
  (ps:inexact x))
(defprim "fixnum->flonum" (x) (check-fx "fixnum->flonum" x) (coerce x 'double-float))

(defmacro def-fl-compare (name op)
  `(defprim ,name (a b &rest more)
     (let ((all (list* a b more)))
       (dolist (x all) (check-fl ,name x))
       (bool (apply #',op all)))))
(def-fl-compare "fl=?" =)
(def-fl-compare "fl<?" <)
(def-fl-compare "fl>?" >)
(def-fl-compare "fl<=?" <=)
(def-fl-compare "fl>=?" >=)

(defun fl-finite-p (x) (not (or (ps:nan-p x) (ps:infinite-p x))))
(def-fl "flinteger?" (x) (bool (and (fl-finite-p x) (= x (ffloor x)))))
(def-fl "flzero?" (x) (bool (zerop x)))
(def-fl "flpositive?" (x) (bool (plusp x)))
(def-fl "flnegative?" (x) (bool (minusp x)))
(def-fl "flodd?" (x)
  (unless (and (fl-finite-p x) (= x (ffloor x))) (r6rs-assertion-violation "flodd?" "not an integer" x))
  (bool (oddp (truncate x))))
(def-fl "fleven?" (x)
  (unless (and (fl-finite-p x) (= x (ffloor x))) (r6rs-assertion-violation "fleven?" "not an integer" x))
  (bool (evenp (truncate x))))
(def-fl "flfinite?" (x) (bool (fl-finite-p x)))
(def-fl "flinfinite?" (x) (bool (ps:infinite-p x)))
(def-fl "flnan?" (x) (bool (ps:nan-p x)))

(defun fl-extreme (pred x more)
  (if (or (ps:nan-p x) (some #'ps:nan-p more))
      ps::+nan+
      (reduce (lambda (a b) (if (funcall pred b a) b a)) more :initial-value x)))
(def-fl "flmax" (x &rest more) (dolist (m more) (check-fl "flmax" m)) (fl-extreme #'> x more))
(def-fl "flmin" (x &rest more) (dolist (m more) (check-fl "flmin" m)) (fl-extreme #'< x more))

(defprim "fl+" (&rest xs) (dolist (x xs) (check-fl "fl+" x)) (ieee (reduce #'+ xs :initial-value 0d0)))
(defprim "fl*" (&rest xs) (dolist (x xs) (check-fl "fl*" x)) (ieee (reduce #'* xs :initial-value 1d0)))
(defprim "fl-" (x &rest xs)
  (check-fl "fl-" x) (dolist (y xs) (check-fl "fl-" y))
  (ieee (if xs (reduce #'- xs :initial-value x) (- x))))
(defprim "fl/" (x &rest xs)
  (check-fl "fl/" x) (dolist (y xs) (check-fl "fl/" y))
  (ieee (if xs (reduce #'/ xs :initial-value x) (/ 1d0 x))))
(def-fl "flabs" (x) (abs x))

(defun fl-div (x y) (ieee (if (> y 0) (ffloor (/ x y)) (fceiling (/ x y)))))
(defun fl-div0 (x y) (ieee (let ((q (/ x y))) (if (> y 0) (ffloor (+ q 0.5d0)) (fceiling (- q 0.5d0))))))
(def-fl "fldiv" (x y) (fl-div x y))
(def-fl "flmod" (x y) (ieee (- x (* y (fl-div x y)))))
(def-fl "fldiv-and-mod" (x y) (let ((q (fl-div x y))) (values q (ieee (- x (* y q))))))
(def-fl "fldiv0" (x y) (fl-div0 x y))
(def-fl "flmod0" (x y) (ieee (- x (* y (fl-div0 x y)))))
(def-fl "fldiv0-and-mod0" (x y) (let ((q (fl-div0 x y))) (values q (ieee (- x (* y q))))))

(def-fl "flnumerator" (x)
  (if (fl-finite-p x) (coerce (numerator (rational x)) 'double-float) x))
(def-fl "fldenominator" (x)
  (cond ((fl-finite-p x) (coerce (denominator (rational x)) 'double-float))
	((ps:nan-p x) x)
	(t 1d0)))

;; (VALUES ...): CL's FFLOOR and friends also return the remainder.
(def-fl "flfloor" (x) (if (fl-finite-p x) (values (ffloor x)) x))
(def-fl "flceiling" (x) (if (fl-finite-p x) (values (fceiling x)) x))
(def-fl "fltruncate" (x) (if (fl-finite-p x) (values (ftruncate x)) x))
(def-fl "flround" (x) (if (fl-finite-p x) (values (fround x)) x))

(defun real-result (z)
  "CL returns complexes where R6RS flonum operations want NaN."
  (if (complexp z) ps::+nan+ z))

(def-fl "flexp" (x) (ieee (exp x)))
(defprim "fllog" (x &optional (base nil base-p))
  (check-fl "fllog" x)
  (ieee (cond ((and (zerop x) (not base-p)) ps::-inf+)
	      ((< x 0) ps::+nan+)
	      (base-p (check-fl "fllog" base) (real-result (/ (log x) (log base))))
	      (t (log x)))))
(def-fl "flsin" (x) (ieee (sin x)))
(def-fl "flcos" (x) (ieee (cos x)))
(def-fl "fltan" (x) (ieee (tan x)))
(def-fl "flasin" (x) (ieee (real-result (asin x))))
(def-fl "flacos" (x) (ieee (real-result (acos x))))
(defprim "flatan" (y &optional (x nil x-p))
  (check-fl "flatan" y)
  (ieee (if x-p (progn (check-fl "flatan" x) (atan y x)) (atan y))))
(def-fl "flsqrt" (x) (ieee (if (minusp x) (if (eql x -0d0) x ps::+nan+) (sqrt x))))
(def-fl "flexpt" (x y) (ieee (real-result (expt x y))))
;;; ------------------------------------------------------------------
;;; Generic arithmetic where R6RS asks more than R7RS (11.7.4.3, 11.7.4.4)

(defprim "log" (x &optional (base nil base-p))
  (when (or (eql x 0) (and base-p (eql base 0)))
    (r6rs-assertion-violation "log" "undefined for exact zero" x))
  (if base-p (ps:scheme-log x base) (ps:scheme-log x)))

(defprim "expt" (base power)
  (if (and (eql base 0) (numberp power) (minusp (realpart power)))
      (ps-r7rs:raise-object
       (make-standard-condition "make-implementation-restriction-violation" "expt"
				"exact zero to a negative power" (list base power))
       nil)
      (ps:scheme-expt base power)))

;; The precision, a mantissa width, may be ignored: a flonum's shortest
;; representation reads back as the same flonum.
(defprim "number->string" (z &optional (radix 10) precision)
  (declare (ignore precision))
  (ps:format-scheme-number z radix))

(defprim "integer->char" (n)
  (unless (and (integerp n) (or (<= 0 n #xD7FF) (<= #xE000 n #x10FFFF)))
    (r6rs-assertion-violation "integer->char" "not a Unicode scalar value" n))
  (code-char n))
