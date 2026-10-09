; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-GUILE -*-

;;;; More of libguile's primitives: lists and association lists, sorting,
;;;; numbers, symbols, strings, vectors, bit vectors and arrays, the
;;;; operating system, and the C halves of (ice-9 ports) and friends,
;;;; which Guile's modules install with load-extension.

(in-package "PSEUDOSCHEME-GUILE")

(defun scheme-equal (a b) (ps:scheme-equal-p a b))
(defun scheme-eqv (a b) (or (eql a b) (and (numberp a) (numberp b) (= a b) (eq (type-of a) (type-of b)))))

;;; ------------------------------------------------------------------
;;; Lists

;; assq-ref and the other alist procedures: runtime.lisp
(defguile "sloppy-assq" (key alist)
  (or (find-if (lambda (e) (and (consp e) (eq (car e) key))) alist) ps:false))
(defguile "sloppy-assv" (key alist)
  (or (find-if (lambda (e) (and (consp e) (scheme-eqv (car e) key))) alist) ps:false))
(defguile "sloppy-assoc" (key alist)
  (or (find-if (lambda (e) (and (consp e) (scheme-equal (car e) key))) alist) ps:false))

(defguile "delq" (x list) (remove x list :test #'eq))
(defguile "delv" (x list) (remove x list :test #'scheme-eqv))
(defguile "delete" (x list &optional (eq ps:false))
  (if (truthy eq)
      (remove-if (lambda (y) (truthy (funcall eq x y))) list)
      (remove x list :test #'scheme-equal)))
(defguile "delq!" (x list) (delete x list :test #'eq))
(defguile "delv!" (x list) (delete x list :test #'scheme-eqv))
(defguile "delete!" (x list &optional (eq ps:false))
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

(defun host-call (name &rest args) (apply (psx:host-ref name) args))

(defun exact-zero-p (x) (eql x 0))

(defguile "integer-expt" (n k)
  (unless (integerp k) (wrong-type "integer-expt" 2 k))
  (cond ((zerop k) 1)
	((and (zerop n) (minusp k)) +nan+)
	(t (expt n k))))

(defguile "expt" (z w)
  (cond ((exact-zero-p w) 1)
	((and (zerop z) (realp w) (minusp w)) +nan+)
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

(defguile "/" (x &rest ys)
  (when (if ys (some #'exact-zero-p ys) (exact-zero-p x))
    (numerical-overflow "divide"))
  (apply #'host-call "/" x ys))

(defguile "inexact->exact" (z)
  (when (or (nan-p z) (and (floatp z) (sb-ext:float-infinity-p z)))
    (out-of-range "inexact->exact" z))
  (host-call "exact" z))

(defguile "string->number" (string &optional (radix 10))
  (guile-string->number string radix))

(defun guile-string->number (string radix)
  (unless (stringp string) (wrong-type "string->number" 1 string))
  ;; R5RS's # for a digit not known ("2#" is 20.0), which Guile still
  ;; reads: each # after a digit is a 0, no digit follows one in the
  ;; same digits ("5#.0" isn't a number), and the number is inexact
  (let ((digits (let ((s (copy-seq string)) (after-digit nil) (hashed nil))
		  (dotimes (i (length s) s)
		    (let ((c (char s i)))
		      (cond ((digit-char-p c radix)
			     (when hashed (return-from guile-string->number ps:false))
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
	  (cond ((not (numberp n)) ps:false)
		((search "#e" string :test #'char-equal) n) ; exact, as asked
		((rationalp n) (coerce n 'double-float))
		(t n))))))

(defguile "make-rectangular" (re im)
  (unless (realp re) (wrong-type "make-rectangular" 1 re))
  (unless (realp im) (wrong-type "make-rectangular" 2 im))
  (if (exact-zero-p im)
      re
      (complex (coerce re 'double-float) (coerce im 'double-float))))
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

(defguile "make-symbol" (name) (make-symbol (ps::invert-case name)))
(defguile "symbol-interned?" (s) (bool (symbol-package s)))
(defguile "string-ci->symbol" (s) (ssym (string-downcase s)))
(defguile "symbol-pref" (s) (get s 'guile-pref ps:false))
(defguile "symbol-pset!" (s v) (setf (get s 'guile-pref) v) *unspecified*)
(defguile "symbol-fref" (s) (get s 'guile-fref ps:false))
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
	ps:false)))
(defguile "substring/copy" (s start &optional (end (length s))) (subseq s start end))
;;; libguile's debugging dumps of a string's and a symbol's buffer, as
;;; they are here: a string is its own buffer, never shared, and
;;; mutable; "wide" if a character is past Latin-1.

(defun wide-string-p (s) (some (lambda (c) (> (char-code c) 255)) s))

(defguile "%string-dump" (s)
  (unless (stringp s) (wrong-type "%string-dump" 1 s))
  (flet ((entry (key value) (cons (ssym key) value)))
    (list (entry "string" s) (entry "start" 0) (entry "length" (length s))
	  (entry "shared" ps:false) (entry "read-only" ps:false)
	  (entry "stringbuf" s) (entry "stringbuf-chars" (copy-seq s))
	  (entry "stringbuf-length" (length s)) (entry "stringbuf-shared" ps:false)
	  (entry "stringbuf-mutable" ps:true) (entry "stringbuf-wide" (bool (wide-string-p s))))))

(defguile "%symbol-dump" (sym)
  (unless (ps:scheme-symbol-p sym) (wrong-type "%symbol-dump" 1 sym))
  (let ((name (ps:scheme-symbol-name sym)))
    (flet ((entry (key value) (cons (ssym key) value)))
      (list (entry "symbol" sym) (entry "hash" (sxhash name)) (entry "interned" ps:true)
	    (entry "stringbuf" name) (entry "stringbuf-chars" (copy-seq name))
	    (entry "stringbuf-length" (length name)) (entry "stringbuf-shared" ps:false)
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

(defguile "substring/read-only" (s start &optional (end (length s))) (subseq s start end))
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
(defguile "make-bitvector" (n &optional (fill ps:false))
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
  (or (position (if (truthy b) 1 0) v :start start) ps:false))
(defguile "bit-count" (b v) (count (if (truthy b) 1 0) v))
(defguile "bit-position" (b v start) (or (position (if (truthy b) 1 0) v :start start) ps:false))
(defguile "bit-invert!" (v) (bit-not v v) *unspecified*)

;;; ------------------------------------------------------------------
;;; The operating system

(defun system-error (subr &optional (errno (sb-alien:extern-alien "errno" sb-alien:int)))
  (guile-error (ssym "system-error") subr "~A" (list (sb-int:strerror errno)) (list errno)))

(defguile "getenv" (name) (or (sb-posix:getenv name) ps:false))
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
(defguile "getlogin" () (or (sb-posix:getenv "USER") ps:false))
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
(defguile "file-encoding" (port) (declare (ignore port)) ps:false)

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
(defguile "stat" (path &optional (exception-on-error ps:true))
  (handler-case (stat-vector (cond ((integerp path) (sb-posix:fstat path))
				   ((streamp path) (sb-posix:fstat (port-fdes path "stat")))
				   (t (sb-posix:stat path))))
    (sb-posix:syscall-error (e)
      (if (truthy exception-on-error)
	  (system-error "stat" (sb-posix:syscall-errno e))
	  ps:false))))
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
  (if (directory-stream-entries d) (pop (directory-stream-entries d)) ps:eof-object))
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

(defguile "system" (&optional (command ps:false))
  (if (truthy command)
      (run-and-wait "/bin/sh" (list "-c" command))
      ps:true))
(defguile "system*" (prog &rest args) (run-and-wait prog args))
(defguile "status:exit-val" (status) (if (zerop (logand status #x7f)) (ash status -8) ps:false))
(defguile "status:term-sig" (status) (let ((s (logand status #x7f))) (if (zerop s) ps:false s)))
(defguile "status:stop-sig" (status) (declare (ignore status)) ps:false)
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
(defguile "isatty?" (port) (declare (ignore port)) ps:false)
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
	    (if (cffi:null-pointer-p zone) ps:false (cffi:foreign-string-to-lisp zone)))))

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

(defguile "defined?" (sym &optional (module ps:false))
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
(defguile "equal?" (&rest args) (bool (all-adjacent #'ps:scheme-equal-p args)))

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
	ps:false)))
(defguile "make-guardian" ()
  (let ((objects '()))
    (lambda (&optional (x nil x-p))
      (if x-p (progn (push x objects) *unspecified*) ps:false))))
(defguile "make-stack" (&rest args) (declare (ignore args)) ps:false)
(defguile "stack?" (x) (declare (ignore x)) ps:false)
(defguile "frame?" (x) (declare (ignore x)) ps:false)
(defguile "backtrace" (&rest args) (declare (ignore args)) *unspecified*)
(defguile "display-backtrace" (&rest args) (declare (ignore args)) *unspecified*)
(defguile "display-application" (&rest args) (declare (ignore args)) *unspecified*)
(defguile "display-error" (frame port subr message args rest)
  (declare (ignore frame rest))
  (when (truthy subr) (format port "In procedure ~A: " subr))
  (write-string (apply #'simple-format ps:false message (if (listp args) args '())) port)
  (terpri port)
  *unspecified*)
(defguile "eval-string" (string &optional (module ps:false))
  (with-input-from-string (in string)
    (let ((result *unspecified*))
      (loop for form = (guile-read in)
	    until (eq form ps:eof-object)
	    do (setq result (guile-eval form module)))
      result)))
(defguile "gettext" (msg &rest args) (declare (ignore args)) msg)
(defguile "ngettext" (msg plural n &rest args) (declare (ignore args)) (if (= n 1) msg plural))
(defguile "textdomain" (&rest args) (declare (ignore args)) ps:false)
(defguile "bindtextdomain" (&rest args) (declare (ignore args)) ps:false)
(defguile "bind-textdomain-codeset" (&rest args) (declare (ignore args)) ps:false)
(defguile "setlocale" (&rest args) (declare (ignore args)) "C")
(defguile "%package-data-dir" () (namestring (uiop:pathname-parent-directory-pathname *guile-source-directory*)))
(defguile "%library-dir" () (namestring *guile-source-directory*))	; guile-procedures.txt is here
(defguile "%site-dir" () "")
(defguile "%global-site-dir" () "")
(defguile "%site-ccache-dir" () "")
(defguile "%resolve-variable" (spec module)
  (or (module-variable* module (if (consp spec) (cdr spec) spec)) ps:false))
(defguile "module-import-interface" (module sym)
  ;; the interface MODULE's binding of SYM comes from
  (let ((v (imported-variable module sym)))
    (or (and v (find-if (lambda (iface) (eq (module-variable* iface sym) v))
			(module-slot module +module-uses+)))
	ps:false)))
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
	(cond ((eq handler ps:false)
	       ;; the handler there was before Guile's
	       (multiple-value-bind (original found) (gethash signum *original-interrupt-handlers*)
		 (when found (sb-sys:enable-interrupt signum original)))
	       (remhash signum *signal-handlers*))
	      ((integerp handler)
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
  (itimer-values (multiple-value-list
		  (sb-unix:unix-setitimer (itimer-which which) interval-seconds interval-microseconds
					  value-seconds value-microseconds))))
(defguile "getitimer" (which)
  (itimer-values (multiple-value-list (sb-unix:unix-getitimer (itimer-which which)))))

(defguile "system-async-mark" (proc &optional thread)
  (let ((thread (if (typep thread 'sb-thread:thread) thread sb-thread:*current-thread*)))
    (sb-thread:interrupt-thread thread (lambda () (funcall proc)))
    *unspecified*))

;;; after-gc-hook, run after each garbage collection (by SBCL's
;;; *after-gc-hooks*, in whichever thread SBCL runs them)

(defun run-after-gc-hook ()
  (let ((hook *after-gc-hook*))
    (when (and hook (hook-procedures hook))
      (dolist (p (hook-procedures hook))
	(ignore-errors (funcall p))))))

(pushnew 'run-after-gc-hook sb-ext:*after-gc-hooks*)
(defguile "restore-signals" () *unspecified*)
(defguile "alarm" (n) (declare (ignore n)) 0)
(defguile "load-compiled" (file) (declare (ignore file)) ps:false)

;;; ------------------------------------------------------------------
;;; load-extension: libguile's init functions for modules partly in C.
;;; Each defines its bindings in the current module.

(defvar *extensions* (make-hash-table :test 'equal)
  "C init function name -> function returning ((name . value) ...).")

(defmacro defextension (name &body body)
  `(setf (gethash ,name *extensions*) (lambda () ,@body)))

(defguile "load-extension" (library init)
  (declare (ignore library))
  (let ((ext (gethash init *extensions*)))
    (when ext
      (loop for (name . value) in (funcall ext)
	    do (define-in-module *current-module* (ssym name) value)))
    (fill-unbound-exports init)
    *unspecified*))

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
	    (let ((s (ps:scheme-symbol-name name)))
	      (cond ((gethash s *extension-primitives*)
		     (setf (gvariable-value v) (gethash s *extension-primitives*)))
		    ((gethash s *guile-primitives*)
		     (setf (gvariable-value v) (gethash s *guile-primitives*)))
		    ((host-bound-p s) (setf (gvariable-value v) (psx:host-ref s)))
		    (t (push (cons init s) *missing-extension-names*))))))))))

(defextension "scm_init_ice_9_control"
  (list (cons "suspendable-continuation?" (lambda (tag) (bool (psx::find-prompt tag))))))

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
		(apply (psx:host-ref "sint-list->bytevector") list args)))
	(cons "uint-list->bytevector"
	      (lambda (list &rest args)
		(check-proper-list "uint-list->bytevector" 1 list)
		(apply (psx:host-ref "uint-list->bytevector") list args)))
	(cons "bytevector-fill!"
	      ;; with Guile's optional range
	      (lambda (bv fill &optional (start 0) (end (length bv)))
		(unless (typep bv 'ps-r6rs::octets) (wrong-type "bytevector-fill!" 1 bv))
		(unless (and (integerp fill) (<= -128 fill 255)) (wrong-type "bytevector-fill!" 2 fill))
		(fill bv (ldb (byte 8 0) fill) :start start :end end)
		*unspecified*))
	(cons "bytevector-slice"
	      (lambda (bv offset &optional (size nil size-p))
		(unless (typep bv 'ps-r6rs::octets) (wrong-type "bytevector-slice" 1 bv))
		(unless (and (integerp offset) (<= 0 offset (length bv)))
		  (out-of-range "bytevector-slice" offset))
		(let ((size (if size-p size (- (length bv) offset))))
		  (unless (and (integerp size) (<= 0 size (- (length bv) offset)))
		    (out-of-range "bytevector-slice" size))
		  ;; sharing BV's bytes, as Guile's (its linker writes each
		  ;; section of an image through a slice)
		  (make-array size :element-type '(unsigned-byte 8)
				   :displaced-to bv :displaced-index-offset offset))))
	;; Guile's checks of the size, before the host's
	(cons "bytevector->sint-list" (lambda (bv endianness size) (integer-list-size-check "bytevector->sint-list" bv size)
					(funcall (psx:host-ref "bytevector->sint-list") bv endianness size)))
	(cons "bytevector->uint-list" (lambda (bv endianness size) (integer-list-size-check "bytevector->uint-list" bv size)
					(funcall (psx:host-ref "bytevector->uint-list") bv endianness size)))
	(cons "utf8->string"
	      (lambda (bv &optional (start 0) (end nil))
		(unless (typep bv 'ps-r6rs::octets) (wrong-type "utf8->string" 1 bv))
		(handler-case (sb-ext:octets-to-string bv :external-format '(:utf-8 :replacement nil)
							  :start start :end end)
		  (sb-int:character-decoding-error ()
		    (call-throw (ssym "decoding-error") (list "utf8->string" "input decoding error" +eilseq+ bv))))))
	;; the endianness is optional, big by default
	(cons "utf16->string" (lambda (bv &optional (endianness (ssym "big")) endianness-mandatory)
				(funcall (psx:host-ref "utf16->string") bv endianness
					 (if (truthy endianness-mandatory) ps:true ps:false))))
	(cons "utf32->string" (lambda (bv &optional (endianness (ssym "big")) endianness-mandatory)
				(funcall (psx:host-ref "utf32->string") bv endianness
					 (if (truthy endianness-mandatory) ps:true ps:false))))))

(defun integer-list-size-check (who bv size)
  (unless (typep bv 'ps-r6rs::octets) (wrong-type who 1 bv))
  (unless (and (integerp size) (plusp size)) (out-of-range who size))
  (unless (zerop (mod (length bv) size)) (wrong-type who 3 size)))

(defguile "raise" (signal) (sb-posix:kill (sb-posix:getpid) signal) ps:true)

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
(defguile "getgroups" () (vector (sb-posix:getgid)))
(defguile "getpw" (&optional user)
  (let ((pw (cond ((null user) nil)
		  ((integerp user) (sb-posix:getpwuid user))
		  (t (sb-posix:getpwnam user)))))
    (if pw
	(vector (sb-posix:passwd-name pw) (sb-posix:passwd-passwd pw) (sb-posix:passwd-uid pw)
		(sb-posix:passwd-gid pw) (sb-posix:passwd-gecos pw) (sb-posix:passwd-dir pw)
		(sb-posix:passwd-shell pw))
	ps:false)))
(defguile "parse-path" (path &optional (tail '()))
  (if (and (stringp path) (plusp (length path)))
      (append (funcall (gethash "string-split" *guile-primitives*) path #\:) tail)
      tail))
(defguile "search-path" (path filename &optional (extensions '("")) (require-exts ps:false))
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
	  ps:false))))
(defguile "procedure" (x)
  (if (typep x 'applicable-struct) (svref (astruct-slots x) 0) (wrong-type "procedure" 1 x)))

(defun strftime (format tm)
  (let ((s (svref tm 0)) (m (svref tm 1)) (h (svref tm 2)) (d (svref tm 3))
	(mo (svref tm 4)) (y (+ 1900 (svref tm 5))) (wd (svref tm 6)))
    (with-output-to-string (out)
      (loop with i = 0 while (< i (length format))
	    do (let ((c (char format i)))
		 (if (and (char= c #\%) (< (1+ i) (length format)))
		     (let ((d2 (char format (1+ i))))
		       (case d2
			 (#\Y (format out "~D" y)) (#\y (format out "~2,'0D" (mod y 100)))
			 (#\m (format out "~2,'0D" (1+ mo))) (#\d (format out "~2,'0D" d))
			 (#\e (format out "~2D" d))
			 (#\H (format out "~2,'0D" h)) (#\M (format out "~2,'0D" m))
			 (#\S (format out "~2,'0D" s))
			 (#\a (write-string (subseq (nth wd '("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday")) 0 3) out))
			 (#\A (write-string (nth wd '("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday")) out))
			 (#\b (write-string (subseq (nth mo '("January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December")) 0 3) out))
			 (#\B (write-string (nth mo '("January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December")) out))
			 (#\F (format out "~D-~2,'0D-~2,'0D" y (1+ mo) d))
			 (#\T (format out "~2,'0D:~2,'0D:~2,'0D" h m s))
			 (#\Z (write-string (svref tm 10) out))
			 (#\z (let ((off (- (svref tm 9)))) (format out "~A~2,'0D~2,'0D" (if (minusp off) "-" "+") (floor (abs off) 3600) (mod (floor (abs off) 60) 60))))
			 (#\% (write-char #\% out))
			 (t (write-char c out) (write-char d2 out)))
		       (incf i 2))
		     (progn (write-char c out) (incf i))))))))
(defguile "strftime" (format tm) (strftime format tm))
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
  (let ((result (if (or (null locale) (eq locale ps:false))
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
    (list (cons "program?" (lambda (x) (declare (ignore x)) ps:false))
	  (cons "program-code" #'no-program)
	  (cons "primitive-code?" (lambda (x) (declare (ignore x)) ps:false))
	  (cons "primitive-code-name" (lambda (x) (declare (ignore x)) ps:false))
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
  (let ((end (if (or (null end) (eq end ps:false)) (length s) end)))
    (unless (and (integerp start) (<= 0 start (length s))) (guile-error (ssym "out-of-range") who "Value out of range: ~S" (list start) (list start)))
    (unless (and (integerp end) (<= start end (length s))) (guile-error (ssym "out-of-range") who "Value out of range: ~S" (list end) (list end)))
    end))

(defguile "string-any-c-code" (pred s &optional (start 0) end)	; boot-9's string-any
  (let ((match (char-or-set-matcher "string-any" 1 pred))
	(end (string-bounds "string-any" s start end)))
    (loop for i from start below end
	  for v = (funcall match (char s i))
	  when (truthy* v) return (if (functionp pred) v ps:true)
	  finally (return ps:false))))

(defguile "string-every-c-code" (pred s &optional (start 0) end)	; and string-every
  (let ((match (char-or-set-matcher "string-every" 1 pred))
	(end (string-bounds "string-every" s start end))
	(last ps:true))
    (loop for i from start below end
	  for v = (funcall match (char s i))
	  unless (truthy* v) return ps:false
	  do (setq last (if (functionp pred) v ps:true))
	  finally (return last))))

(defun truthy* (v) (and v (not (eq v ps:false))))

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
	 (lambda (prog args &optional (from ps:false) (to ps:false))
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
;;; --lose-on-corruption, which --script implies.)

(defun check-stack-limit (who x)
  (unless (integerp x) (wrong-type who 1 x))
  (unless (< 0 x (ash 1 62))
    (guile-error (ssym "out-of-range") who "Value out of range: ~S" (list x) (list x)))
  x)

(defguile "call-with-stack-overflow-handler" (limit thunk handler)
  (check-stack-limit "call-with-stack-overflow-handler" limit)
  (handler-case (funcall thunk)
    (storage-condition ()
      (check-stack-limit "call-with-stack-overflow-handler" (funcall handler))
      (guile-error (ssym "stack-overflow") "call-with-stack-overflow-handler" "Stack overflow" '()))))

;; (system vm vm) exports it; it isn't in the root module
(setf (gethash "call-with-stack-overflow-handler" *extension-primitives*)
      (gethash "call-with-stack-overflow-handler" *guile-primitives*))
(remhash "call-with-stack-overflow-handler" *guile-primitives*)

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
      (multiple-value-bind (v alive) (sb-ext:weak-pointer-value c) (if alive v ps:false))
      c))
(defun weak-vector-elements (w) (map 'list #'weak-cell-value (weak-vector-cells w)))
(defguile "make-weak-vector" (n &optional (fill ps:false))
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
  (unless (or (module-p* module) (eq module ps:false)) (wrong-type "module-reverse-lookup" 1 module))
  (unless (gvariable-p variable) (wrong-type "module-reverse-lookup" 2 variable))
  (let ((seen '()))
    (labels ((look-in (m)
	       (unless (member m seen :test #'eq)
		 (push m seen)
		 (let ((obarray (if (module-p* m) (module-slot m +module-obarray+) *obarray*)))
		   (or (loop for (name . v) in (ghash-handles obarray) when (eq v variable) return name)
		       (and (module-p* m)
			    (loop for iface in (module-slot m +module-uses+) thereis (look-in iface))))))))
      (or (look-in module) ps:false))))

;;; POSIX odds and ends
(defguile "ttyname" (port)
  (let ((p (cffi:foreign-funcall "ttyname" :int (port-fdes port "ttyname") :pointer)))
    (if (cffi:null-pointer-p p)
	(system-error "ttyname" (sb-alien:extern-alien "errno" sb-alien:int))
	(cffi:foreign-string-to-lisp p))))
(defguile "utime" (obj &optional actime modtime actimens modtimens flags)
  (declare (ignore actimens modtimens flags))
  (let* ((now (- (get-universal-time) #.(encode-universal-time 0 0 0 1 1 1970 0)))
	 (a (if (integerp actime) actime now)) (m (if (integerp modtime) modtime now))
	 (path (if (stringp obj) obj (port-filename* (->port obj "utime")))))
    (sb-posix:utimes path a m))
  *unspecified*)

;;; spawn: Guile 3.0.9's posix_spawn interface.  ARGS includes argv[0].
(defguile "spawn" (program args &key (environment nil) (input nil) (output nil) (error nil)
			   (search-path? ps:true))
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
    (unless (typep bv 'ps-r6rs::octets) (wrong-type "load-thunk-from-memory" 1 bv))
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
	     (string= (ps:scheme-symbol-name grammar) "strict-infix"))
    (guile-error (ssym "misc-error") "string-join"
		 "strict-infix grammar requires non-empty list" '()))
  (funcall (gethash (cons "(srfi 13)" "string-join") *library-values*) strings delimiter grammar))

;;; The *at procedures: a file named relative to a directory port, by
;;; libc's *at calls.  statat stats the directory's path joined to the
;;; name (F_GETPATH on Darwin, /proc/self/fd elsewhere).

(defun at-directory-fd (who dir)
  "DIR's descriptor; #f is the current directory (AT_FDCWD)."
  (cond ((eq dir ps:false) #+darwin -2 #-darwin -100)
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
