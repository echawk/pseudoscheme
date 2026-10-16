; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

;;;; Strings, vectors, bytes, dates and the rest of #%kernel's data
;;;; primitives, natively: comparisons that take one or more arguments,
;;;; copying and filling with Racket's checks and messages, vector
;;;; operations, Unicode (titlecase, normal forms, grapheme clusters),
;;;; byte-string converters, SHA-1 and SHA-2, stencil vectors, date
;;;; structures and seconds->date, and small system queries.
;;;;
;;;; Loaded after more.lisp: a native defined again here replaces the
;;;; earlier one of its name.  (basics.lisp's *HOST-ALIASES* and
;;;; native.lisp's *HOST-SHARED* name some of the same procedures and win
;;;; over a native: those entries must go for these to be used.)

(in-package "PSEUDOSCHEME-RACKET")

;;; ------------------------------------------------------------------
;;; Argument checks, as Racket's messages go

;;; (OCTETS and MAKE-OCTETS are ports.lisp's.)

(defun contract-error (who expected given)
  (racket-error who "contract violation~%  expected: ~A~%  given: ~A" expected (written given)))

(defun printed-quoted (x) (with-output-to-string (s) (print-quoted x s)))

(defun check-string (who x) (unless (stringp x) (contract-error who "string?" x)) x)
(defun check-char (who x) (unless (characterp x) (contract-error who "char?" x)) x)
(defun check-octets (who x) (unless (typep x 'octets) (contract-error who "bytes?" x)) x)
(defun check-vector (who x) (unless (simple-vector-p x) (contract-error who "vector?" x)) x)
(defun check-nonneg (who x)
  (unless (typep x '(integer 0)) (contract-error who "exact-nonnegative-integer?" x))
  x)

(defun absent-p (x) (or (null x) (eq x ps:false)))

(defun check-range (who start end len kind value)
  "START and END (absent: the ends) as indices into VALUE, of length LEN
and called KIND in messages: (values start end)."
  (let ((start (if (absent-p start) 0 start)))
    (check-nonneg who start)
    (unless (<= start len)
      (racket-error who "starting index is out of range~%  starting index: ~D~%  valid range: [0, ~D]~%  ~A: ~A"
		    start len kind (printed-quoted value)))
    (let ((end (if (absent-p end) len end)))
      (check-nonneg who end)
      (when (> end len)
	(racket-error who "ending index is out of range~%  ending index: ~D~%  valid range: [0, ~D]~%  ~A: ~A"
		      end len kind (printed-quoted value)))
      (when (< end start)
	(racket-error who "ending index is smaller than starting index~%  ending index: ~D~%  starting index: ~D~%  valid range: [0, ~D]~%  ~A: ~A"
		      end start len kind (printed-quoted value)))
      (values start end))))

(defun unicode-call (name fallback &rest args)
  "SB-UNICODE's function NAME on ARGS, or FALLBACK on them if this SBCL lacks it."
  (let ((symbol (find-symbol name "SB-UNICODE")))
    (apply (if (and symbol (fboundp symbol)) symbol fallback) args)))

(defun fresh-string (s)
  "S as a fresh string of its own."
  (let ((r (make-string (length s))))
    (replace r s)
    r))

;;; ------------------------------------------------------------------
;;; Comparisons of one or more arguments

(defun case-folded (s)
  (fresh-string (unicode-call "CASEFOLD" #'string-downcase s)))

(defun string-chain (who test strings fold)
  (dolist (s strings) (check-string who s))
  (let ((strings (if fold (mapcar #'case-folded strings) strings)))
    (bool (loop for tail on strings
		while (cdr tail)
		always (funcall test (first tail) (second tail))))))

(defun char-chain (who test chars)
  (dolist (c chars) (check-char who c))
  (let ((chars (mapcar #'char-downcase chars)))
    (bool (loop for tail on chars
		while (cdr tail)
		always (funcall test (first tail) (second tail))))))

(defun bytes-lessp (a b)
  (let ((m (mismatch a b)))
    (and m (or (= m (length a)) (and (< m (length b)) (< (aref a m) (aref b m)))))))

(defun bytes-chain (who test byteses)
  (dolist (b byteses) (check-octets who b))
  (bool (loop for tail on byteses
	      while (cdr tail)
	      always (funcall test (first tail) (second tail)))))

(macrolet ((strings (&rest specs)
	     `(progn
		,@(loop for (name test fold) in specs
			collect `(defnative ,name (a &rest more) (:leaf t)
				   (string-chain ,name ,test (cons a more) ,fold))))))
  (strings ("string=?" #'string= nil)
	   ("string<?" #'string< nil)
	   ("string>?" #'string> nil)
	   ("string<=?" #'string<= nil)
	   ("string>=?" #'string>= nil)
	   ("string-ci=?" #'string= t)
	   ("string-ci<?" #'string< t)
	   ("string-ci>?" #'string> t)
	   ("string-ci<=?" #'string<= t)
	   ("string-ci>=?" #'string>= t)
	   ;; the locale's collation is by code point here
	   ("string-locale=?" #'string= nil)
	   ("string-locale<?" #'string< nil)
	   ("string-locale>?" #'string> nil)
	   ("string-locale-ci=?" #'string= t)
	   ("string-locale-ci<?" #'string< t)
	   ("string-locale-ci>?" #'string> t)))

(macrolet ((chars (&rest specs)
	     `(progn
		,@(loop for (name test) in specs
			collect `(defnative ,name (a &rest more) (:leaf t)
				   (char-chain ,name ,test (cons a more)))))))
  (chars ("char-ci=?" #'char=)
	 ("char-ci<?" #'char<)
	 ("char-ci>?" #'char>)
	 ("char-ci<=?" #'char<=)
	 ("char-ci>=?" #'char>=)))

(defnative "bytes<?" (a &rest more) (:leaf t) (bytes-chain "bytes<?" #'bytes-lessp (cons a more)))
(defnative "bytes>?" (a &rest more) (:leaf t)
  (bytes-chain "bytes>?" (lambda (x y) (bytes-lessp y x)) (cons a more)))

;;; ------------------------------------------------------------------
;;; Copying and filling, with Racket's arities

(defun check-mutable (who x expected predicate)
  (unless (and (funcall predicate x) (not (immutable-p x)))
    (contract-error who expected x)))

(defun copy-into (who dest dest-start src src-start src-end kind dest-check src-check)
  "Racket's string-copy! / vector-copy!: copy SRC's range into DEST at DEST-START."
  (funcall dest-check)
  (check-nonneg who dest-start)
  (funcall src-check)
  (let ((dest-length (length dest)))
    (unless (<= dest-start dest-length)
      (racket-error who "starting index is out of range~%  starting index: ~D~%  valid range: [0, ~D]~%  ~A: ~A"
		    dest-start dest-length kind (printed-quoted dest)))
    (multiple-value-bind (start end) (check-range who src-start src-end (length src) kind src)
      (when (> (+ dest-start (- end start)) dest-length)
	(racket-error who "not enough room in target ~A~%  target length: ~D~%  needed length: ~D"
		      kind dest-length (+ dest-start (- end start))))
      (replace dest (if (eq dest src) (subseq src start end) src)
	       :start1 dest-start :start2 (if (eq dest src) 0 start) :end2 (if (eq dest src) (- end start) end))
      *void*)))

(defnative "string-copy!" (dest dest-start src &optional src-start src-end) (:leaf t)
  (copy-into "string-copy!" dest dest-start src src-start src-end "string"
	     (lambda () (check-mutable "string-copy!" dest "(and/c string? (not/c immutable?))" #'stringp))
	     (lambda () (check-string "string-copy!" src))))

(defnative "vector-copy!" (dest dest-start src &optional src-start src-end) (:leaf t)
  (copy-into "vector-copy!" dest dest-start src src-start src-end "vector"
	     (lambda () (check-mutable "vector-copy!" dest "(and/c vector? (not/c immutable?))" #'simple-vector-p))
	     (lambda () (check-vector "vector-copy!" src))))

(defnative "string-fill!" (s c) (:leaf t)
  (unless (and (stringp s) (not (immutable-p s))) (contract-error "string-fill!" "mutable-string?" s))
  (check-char "string-fill!" c)
  (fill s c)
  *void*)

(defnative "vector-fill!" (v x) (:leaf t)
  (check-vector "vector-fill!" v)
  (check-mutable "vector-fill!" v "(and/c vector? (not/c immutable?))" #'simple-vector-p)
  (fill v x)
  *void*)

(defnative "string->list" (s) (:leaf t) (check-string "string->list" s) (coerce s 'list))
(defnative "vector->list" (v) (:leaf t) (check-vector "vector->list" v) (coerce v 'list))
(defnative "bytes-copy" (b) (:leaf t) (check-octets "bytes-copy" b) (copy-seq b))

(defun vector-slice (who v start end)
  (check-vector who v)
  (multiple-value-bind (start end) (check-range who start end (length v) "vector" v)
    (subseq v start end)))

(defnatives "#%kernel"
  ("vector-copy" (v &optional start end) (vector-slice "vector-copy" v start end))
  ("vector*-copy" (v &optional start end) (vector-slice "vector*-copy" v start end))
  ("vector-append" (&rest vs)
   (dolist (v vs) (check-vector "vector-append" v))
   (apply #'concatenate 'simple-vector vs))
  ("vector*-append" (&rest vs)
   (dolist (v vs) (check-vector "vector*-append" v))
   (apply #'concatenate 'simple-vector vs))
  ("vector->values" (v &optional start end)
   (values-list (coerce (vector-slice "vector->values" v start end) 'list))))

(defun vector-extended (who v size fill)
  (check-vector who v)
  (check-nonneg who size)
  (when (< size (length v))
    (racket-error who "new length is shorter than existing length~%  new length: ~D~%  existing length: ~D"
		  size (length v)))
  (let ((r (make-array size :initial-element fill)))
    (replace r v)
    r))

(defun vector-with (who v index x)
  (check-vector who v)
  (check-nonneg who index)
  (let ((n (length v)))
    (when (zerop n)
      (racket-error who "index is out of range for empty vector~%  index: ~D~%  vector: ~A" index (printed-quoted v)))
    (unless (< index n)
      (racket-error who "index is out of range~%  index: ~D~%  valid range: [0, ~D]~%  vector: ~A"
		    index (1- n) (printed-quoted v)))
    (let ((r (copy-seq v)))
      (setf (svref r index) x)
      r)))

(defnatives "#%kernel"
  ("vector-extend" (v size &optional (fill 0)) (vector-extended "vector-extend" v size fill))
  ("vector*-extend" (v size &optional (fill 0)) (vector-extended "vector*-extend" v size fill))
  ("vector-set/copy" (v index x) (vector-with "vector-set/copy" v index x))
  ("vector*-set/copy" (v index x) (vector-with "vector*-set/copy" v index x)))

;;; ------------------------------------------------------------------
;;; Strings and Unicode

(defun normalized (who s form)
  (check-string who s)
  (coerce (unicode-call "NORMALIZE-STRING" (lambda (s form) (declare (ignore form)) s) s form) 'simple-string))

(defnatives "#%kernel"
  ("string-normalize-nfc" (s) (normalized "string-normalize-nfc" s :nfc))
  ("string-normalize-nfd" (s) (normalized "string-normalize-nfd" s :nfd))
  ("string-normalize-nfkc" (s) (normalized "string-normalize-nfkc" s :nfkc))
  ("string-normalize-nfkd" (s) (normalized "string-normalize-nfkd" s :nfkd))
  ("string-titlecase" (s)
   (check-string "string-titlecase" s)
   (fresh-string (unicode-call "TITLECASE" #'string-capitalize s)))
  ("string-locale-upcase" (s) (check-string "string-locale-upcase" s) (string-upcase s)))

(defun char-utf-8-size (c)
  (let ((n (char-code c)))
    (cond ((< n #x80) 1) ((< n #x800) 2) ((< n #x10000) 3) (t 4))))

(defnative "string-utf-8-length" (s &optional start end) (:leaf t)
  (check-string "string-utf-8-length" s)
  (multiple-value-bind (start end) (check-range "string-utf-8-length" start end (length s) "byte string" s)
    (loop for i from start below end sum (char-utf-8-size (char s i)))))

;;; Decoding UTF-8 in a byte string: the code point, the next index and
;;; :OK, or :ABORTS (the input ends inside a sequence) or :ERROR.
(defun utf8-decode (b i end)
  (let ((b0 (aref b i)))
    (cond ((< b0 #x80) (values b0 (1+ i) :ok))
	  ((< b0 #xC2) (values nil (1+ i) :error))
	  (t (let ((n (cond ((< b0 #xE0) 2) ((< b0 #xF0) 3) ((< b0 #xF5) 4))))
	       (if (null n)
		   (values nil (1+ i) :error)
		   (let ((code (logand b0 (case n (2 #x1F) (3 #x0F) (t #x07)))))
		     (loop for k from 1 below n
			   do (when (>= (+ i k) end)
				(return-from utf8-decode (values nil end :aborts)))
			      (let ((bk (aref b (+ i k))))
				(when (or (/= (logand bk #xC0) #x80)
					  (and (= k 1)
					       (or (and (= b0 #xE0) (< bk #xA0))
						   (and (= b0 #xED) (> bk #x9F))
						   (and (= b0 #xF0) (< bk #x90))
						   (and (= b0 #xF4) (> bk #x8F)))))
				  (return-from utf8-decode (values nil (1+ i) :error)))
				(setq code (logior (ash code 6) (logand bk #x3F)))))
		     (values code (+ i n) :ok))))))))

(defun utf8-skip (who b skip err-char start end)
  "The index in B after SKIP characters from START, and what's there:
its character (an error as ERR-CHAR if given), or NIL."
  (check-octets who b)
  (check-nonneg who skip)
  (multiple-value-bind (start end) (check-range who start end (length b) "byte string" b)
    (let ((i start))
      (dotimes (k skip)
	(when (>= i end) (return-from utf8-skip (values nil nil)))
	(multiple-value-bind (code next status) (utf8-decode b i end)
	  (declare (ignore code))
	  (cond ((eq status :ok) (setq i next))
		((and (characterp err-char) (eq status :error)) (setq i next))
		((and (characterp err-char) (eq status :aborts)) (setq i end))
		((eq status :aborts) (return-from utf8-skip (values nil nil)))
		(t (return-from utf8-skip (values nil nil))))))
      (values i end))))

(defnative "bytes-utf-8-index" (b skip &optional err-char start end) (:leaf t)
  (multiple-value-bind (i end) (utf8-skip "bytes-utf-8-index" b skip err-char start end)
    (if (and i (< i end)) i ps:false)))

(defnative "bytes-utf-8-ref" (b skip &optional err-char start end) (:leaf t)
  (multiple-value-bind (i end) (utf8-skip "bytes-utf-8-ref" b skip err-char start end)
    (if (and i (< i end))
	(multiple-value-bind (code next status) (utf8-decode b i end)
	  (declare (ignore next))
	  (cond ((eq status :ok) (code-char code))
		((characterp err-char) err-char)
		(t ps:false)))
	ps:false)))

;;; Grapheme clusters (UAX #29), from Racket's own tables: the break
;;; property of each character that isn't Other or a Hangul syllable,
;;; as (start end property) triples, and the Extended_Pictographic ranges.
;;; Properties by index: Other CR LF Control Extend ZWJ Regional_Indicator
;;; Prepend SpacingMark L V T LV LVT.

(defparameter *grapheme-break-ranges*
  (coerce '(0 9 3 10 10 2 11 12 3 13 13 1 
    14 31 3 127 159 3 173 173 3 768 879 4 
    1155 1161 4 1425 1469 4 1471 1471 4 1473 1474 4 
    1476 1477 4 1479 1479 4 1536 1541 7 1552 1562 4 
    1564 1564 3 1611 1631 4 1648 1648 4 1750 1756 4 
    1757 1757 7 1759 1764 4 1767 1768 4 1770 1773 4 
    1807 1807 7 1809 1809 4 1840 1866 4 1958 1968 4 
    2027 2035 4 2045 2045 4 2070 2073 4 2075 2083 4 
    2085 2087 4 2089 2093 4 2137 2139 4 2192 2193 7 
    2199 2207 4 2250 2273 4 2274 2274 7 2275 2306 4 
    2307 2307 8 2362 2362 4 2363 2363 8 2364 2364 4 
    2366 2368 8 2369 2376 4 2377 2380 8 2381 2381 4 
    2382 2383 8 2385 2391 4 2402 2403 4 2433 2433 4 
    2434 2435 8 2492 2492 4 2494 2494 4 2495 2496 8 
    2497 2500 4 2503 2504 8 2507 2508 8 2509 2509 4 
    2519 2519 4 2530 2531 4 2558 2558 4 2561 2562 4 
    2563 2563 8 2620 2620 4 2622 2624 8 2625 2626 4 
    2631 2632 4 2635 2637 4 2641 2641 4 2672 2673 4 
    2677 2677 4 2689 2690 4 2691 2691 8 2748 2748 4 
    2750 2752 8 2753 2757 4 2759 2760 4 2761 2761 8 
    2763 2764 8 2765 2765 4 2786 2787 4 2810 2815 4 
    2817 2817 4 2818 2819 8 2876 2876 4 2878 2879 4 
    2880 2880 8 2881 2884 4 2887 2888 8 2891 2892 8 
    2893 2893 4 2901 2903 4 2914 2915 4 2946 2946 4 
    3006 3006 4 3007 3007 8 3008 3008 4 3009 3010 8 
    3014 3016 8 3018 3020 8 3021 3021 4 3031 3031 4 
    3072 3072 4 3073 3075 8 3076 3076 4 3132 3132 4 
    3134 3136 4 3137 3140 8 3142 3144 4 3146 3149 4 
    3157 3158 4 3170 3171 4 3201 3201 4 3202 3203 8 
    3260 3260 4 3262 3262 8 3263 3264 4 3265 3265 8 
    3266 3266 4 3267 3268 8 3270 3272 4 3274 3277 4 
    3285 3286 4 3298 3299 4 3315 3315 8 3328 3329 4 
    3330 3331 8 3387 3388 4 3390 3390 4 3391 3392 8 
    3393 3396 4 3398 3400 8 3402 3404 8 3405 3405 4 
    3406 3406 7 3415 3415 4 3426 3427 4 3457 3457 4 
    3458 3459 8 3530 3530 4 3535 3535 4 3536 3537 8 
    3538 3540 4 3542 3542 4 3544 3550 8 3551 3551 4 
    3570 3571 8 3633 3633 4 3635 3635 8 3636 3642 4 
    3655 3662 4 3761 3761 4 3763 3763 8 3764 3772 4 
    3784 3790 4 3864 3865 4 3893 3893 4 3895 3895 4 
    3897 3897 4 3902 3903 8 3953 3966 4 3967 3967 8 
    3968 3972 4 3974 3975 4 3981 3991 4 3993 4028 4 
    4038 4038 4 4141 4144 4 4145 4145 8 4146 4151 4 
    4153 4154 4 4155 4156 8 4157 4158 4 4182 4183 8 
    4184 4185 4 4190 4192 4 4209 4212 4 4226 4226 4 
    4228 4228 8 4229 4230 4 4237 4237 4 4253 4253 4 
    4352 4447 9 4448 4519 10 4520 4607 11 4957 4959 4 
    5906 5909 4 5938 5940 4 5970 5971 4 6002 6003 4 
    6068 6069 4 6070 6070 8 6071 6077 4 6078 6085 8 
    6086 6086 4 6087 6088 8 6089 6099 4 6109 6109 4 
    6155 6157 4 6158 6158 3 6159 6159 4 6277 6278 4 
    6313 6313 4 6432 6434 4 6435 6438 8 6439 6440 4 
    6441 6443 8 6448 6449 8 6450 6450 4 6451 6456 8 
    6457 6459 4 6679 6680 4 6681 6682 8 6683 6683 4 
    6741 6741 8 6742 6742 4 6743 6743 8 6744 6750 4 
    6752 6752 4 6754 6754 4 6757 6764 4 6765 6770 8 
    6771 6780 4 6783 6783 4 6832 6877 4 6880 6891 4 
    6912 6915 4 6916 6916 8 6964 6973 4 6974 6977 8 
    6978 6980 4 7019 7027 4 7040 7041 4 7042 7042 8 
    7073 7073 8 7074 7077 4 7078 7079 8 7080 7085 4 
    7142 7142 4 7143 7143 8 7144 7145 4 7146 7148 8 
    7149 7149 4 7150 7150 8 7151 7155 4 7204 7211 8 
    7212 7219 4 7220 7221 8 7222 7223 4 7376 7378 4 
    7380 7392 4 7393 7393 8 7394 7400 4 7405 7405 4 
    7412 7412 4 7415 7415 8 7416 7417 4 7616 7679 4 
    8203 8203 3 8204 8204 4 8205 8205 5 8206 8207 3 
    8232 8238 3 8288 8303 3 8400 8432 4 11503 11505 4 
    11647 11647 4 11744 11775 4 12330 12335 4 12441 12442 4 
    42607 42610 4 42612 42621 4 42654 42655 4 42736 42737 4 
    43010 43010 4 43014 43014 4 43019 43019 4 43043 43044 8 
    43045 43046 4 43047 43047 8 43052 43052 4 43136 43137 8 
    43188 43203 8 43204 43205 4 43232 43249 4 43263 43263 4 
    43302 43309 4 43335 43345 4 43346 43346 8 43347 43347 4 
    43360 43388 9 43392 43394 4 43395 43395 8 43443 43443 4 
    43444 43445 8 43446 43449 4 43450 43451 8 43452 43453 4 
    43454 43455 8 43456 43456 4 43493 43493 4 43561 43566 4 
    43567 43568 8 43569 43570 4 43571 43572 8 43573 43574 4 
    43587 43587 4 43596 43596 4 43597 43597 8 43644 43644 4 
    43696 43696 4 43698 43700 4 43703 43704 4 43710 43711 4 
    43713 43713 4 43755 43755 8 43756 43757 4 43758 43759 8 
    43765 43765 8 43766 43766 4 44003 44004 8 44005 44005 4 
    44006 44007 8 44008 44008 4 44009 44010 8 44012 44012 8 
    44013 44013 4 55216 55238 10 55243 55291 11 64286 64286 4 
    65024 65039 4 65056 65071 4 65279 65279 3 65438 65439 4 
    65520 65531 3 66045 66045 4 66272 66272 4 66422 66426 4 
    68097 68099 4 68101 68102 4 68108 68111 4 68152 68154 4 
    68159 68159 4 68325 68326 4 68900 68903 4 68969 68973 4 
    69291 69292 4 69370 69375 4 69446 69456 4 69506 69509 4 
    69632 69632 8 69633 69633 4 69634 69634 8 69688 69702 4 
    69744 69744 4 69747 69748 4 69759 69761 4 69762 69762 8 
    69808 69810 8 69811 69814 4 69815 69816 8 69817 69818 4 
    69821 69821 7 69826 69826 4 69837 69837 7 69888 69890 4 
    69927 69931 4 69932 69932 8 69933 69940 4 69957 69958 8 
    70003 70003 4 70016 70017 4 70018 70018 8 70067 70069 8 
    70070 70078 4 70079 70079 8 70080 70080 4 70082 70083 7 
    70089 70092 4 70094 70094 8 70095 70095 4 70188 70190 8 
    70191 70193 4 70194 70195 8 70196 70199 4 70206 70206 4 
    70209 70209 4 70367 70367 4 70368 70370 8 70371 70378 4 
    70400 70401 4 70402 70403 8 70459 70460 4 70462 70462 4 
    70463 70463 8 70464 70464 4 70465 70468 8 70471 70472 8 
    70475 70476 8 70477 70477 4 70487 70487 4 70498 70499 8 
    70502 70508 4 70512 70516 4 70584 70584 4 70585 70586 8 
    70587 70592 4 70594 70594 4 70597 70597 4 70599 70601 4 
    70602 70602 8 70604 70605 8 70606 70608 4 70609 70609 7 
    70610 70610 4 70625 70626 4 70709 70711 8 70712 70719 4 
    70720 70721 8 70722 70724 4 70725 70725 8 70726 70726 4 
    70750 70750 4 70832 70832 4 70833 70834 8 70835 70840 4 
    70841 70841 8 70842 70842 4 70843 70844 8 70845 70845 4 
    70846 70846 8 70847 70848 4 70849 70849 8 70850 70851 4 
    71087 71087 4 71088 71089 8 71090 71093 4 71096 71099 8 
    71100 71101 4 71102 71102 8 71103 71104 4 71132 71133 4 
    71216 71218 8 71219 71226 4 71227 71228 8 71229 71229 4 
    71230 71230 8 71231 71232 4 71339 71339 4 71340 71340 8 
    71341 71341 4 71342 71343 8 71344 71351 4 71453 71453 4 
    71454 71454 8 71455 71455 4 71458 71461 4 71462 71462 8 
    71463 71467 4 71724 71726 8 71727 71735 4 71736 71736 8 
    71737 71738 4 71984 71984 4 71985 71989 8 71991 71992 8 
    71995 71998 4 71999 71999 7 72000 72000 8 72001 72001 7 
    72002 72002 8 72003 72003 4 72145 72147 8 72148 72151 4 
    72154 72155 4 72156 72159 8 72160 72160 4 72164 72164 8 
    72193 72202 4 72243 72248 4 72249 72249 8 72251 72254 4 
    72263 72263 4 72273 72278 4 72279 72280 8 72281 72283 4 
    72324 72329 7 72330 72342 4 72343 72343 8 72344 72345 4 
    72544 72544 4 72545 72545 8 72546 72548 4 72549 72549 8 
    72550 72550 4 72551 72551 8 72751 72751 8 72752 72758 4 
    72760 72765 4 72766 72766 8 72767 72767 4 72850 72871 4 
    72873 72873 8 72874 72880 4 72881 72881 8 72882 72883 4 
    72884 72884 8 72885 72886 4 73009 73014 4 73018 73018 4 
    73020 73021 4 73023 73029 4 73030 73030 7 73031 73031 4 
    73098 73102 8 73104 73105 4 73107 73108 8 73109 73109 4 
    73110 73110 8 73111 73111 4 73459 73460 4 73461 73462 8 
    73472 73473 4 73474 73474 7 73475 73475 8 73524 73525 8 
    73526 73530 4 73534 73535 8 73536 73538 4 73562 73562 4 
    78896 78911 3 78912 78912 4 78919 78933 4 90398 90409 4 
    90410 90412 8 90413 90415 4 92912 92916 4 92976 92982 4 
    93539 93539 10 93543 93546 10 94031 94031 4 94033 94087 8 
    94095 94098 4 94180 94180 4 94192 94193 4 113821 113822 4 
    113824 113827 3 118528 118573 4 118576 118598 4 119141 119145 4 
    119149 119154 4 119155 119162 3 119163 119170 4 119173 119179 4 
    119210 119213 4 119362 119364 4 121344 121398 4 121403 121452 4 
    121461 121461 4 121476 121476 4 121499 121503 4 121505 121519 4 
    122880 122886 4 122888 122904 4 122907 122913 4 122915 122916 4 
    122918 122922 4 123023 123023 4 123184 123190 4 123566 123566 4 
    123628 123631 4 124140 124143 4 124398 124399 4 124643 124643 4 
    124646 124646 4 124654 124655 4 124661 124661 4 125136 125142 4 
    125252 125258 4 127462 127487 6 127995 127999 4 917504 917535 3 
    917536 917631 4 917632 917759 3 917760 917999 4 918000 921599 3 )
    'simple-vector))

(defparameter *extended-pictographic-ranges*
  (coerce '(169 169 174 174 8252 8252 8265 8265 8482 8482 8505 8505 
    8596 8601 8617 8618 8986 8987 9000 9000 9167 9167 9193 9203 
    9208 9210 9410 9410 9642 9643 9654 9654 9664 9664 9723 9726 
    9728 9732 9742 9742 9745 9745 9748 9749 9752 9752 9757 9757 
    9760 9760 9762 9763 9766 9766 9770 9770 9774 9775 9784 9786 
    9792 9792 9794 9794 9800 9811 9823 9824 9827 9827 9829 9830 
    9832 9832 9851 9851 9854 9855 9874 9879 9881 9881 9883 9884 
    9888 9889 9895 9895 9898 9899 9904 9905 9917 9918 9924 9925 
    9928 9928 9934 9935 9937 9937 9939 9940 9961 9962 9968 9973 
    9975 9978 9981 9981 9986 9986 9989 9989 9992 9997 9999 9999 
    10002 10002 10004 10004 10006 10006 10013 10013 10017 10017 10024 10024 
    10035 10036 10052 10052 10055 10055 10060 10060 10062 10062 10067 10069 
    10071 10071 10083 10084 10133 10135 10145 10145 10160 10160 10175 10175 
    10548 10549 11013 11015 11035 11036 11088 11088 11093 11093 12336 12336 
    12349 12349 12951 12951 12953 12953 126980 126980 127020 127023 127124 127135 
    127151 127152 127168 127168 127183 127184 127222 127231 127344 127345 127358 127359 
    127374 127374 127377 127386 127406 127461 127489 127503 127514 127514 127535 127535 
    127538 127546 127548 127551 127561 127583 127590 127777 127780 127891 127894 127895 
    127897 127899 127902 127984 127987 127989 127991 127994 128000 128253 128255 128317 
    128329 128334 128336 128359 128367 128368 128371 128378 128391 128391 128394 128397 
    128400 128400 128405 128406 128420 128421 128424 128424 128433 128434 128444 128444 
    128450 128452 128465 128467 128476 128478 128481 128481 128483 128483 128488 128488 
    128495 128495 128499 128499 128506 128591 128640 128709 128715 128722 128725 128741 
    128745 128745 128747 128752 128755 128767 128986 129023 129036 129039 129096 129103 
    129114 129119 129160 129167 129198 129199 129212 129215 129218 129231 129241 129279 
    129292 129338 129340 129349 129351 129535 129624 129631 129646 129791 130048 131069 )
    'simple-vector))

(defparameter *grapheme-break-names*
  #("Other" "CR" "LF" "Control" "Extend" "ZWJ" "Regional_Indicator" "Prepend" "SpacingMark"
    "L" "V" "T" "LV" "LVT"))

(defun range-lookup (table c stride)
  "The index into TABLE (triples or pairs, by STRIDE) of the range holding code C, or NIL."
  (let ((lo 0) (hi (1- (floor (length table) stride))))
    (loop while (<= lo hi)
	  do (let* ((mid (floor (+ lo hi) 2)) (at (* mid stride)))
	       (cond ((< c (svref table at)) (setq hi (1- mid)))
		     ((> c (svref table (1+ at))) (setq lo (1+ mid)))
		     (t (return-from range-lookup at)))))
    nil))

(defun grapheme-break-index (c)
  (let ((code (char-code c)))
    (if (<= #xAC00 code #xD7A3)
	(if (zerop (mod (- code #xAC00) 28)) 12 13)
	(let ((at (range-lookup *grapheme-break-ranges* code 3)))
	  (if at (svref *grapheme-break-ranges* (+ at 2)) 0)))))

(defun extended-pictographic-p (c)
  (and (range-lookup *extended-pictographic-ranges* (char-code c) 2) t))

(defnative "char-grapheme-break-property" (c) (:leaf t)
  (check-char "char-grapheme-break-property" c)
  (rsym (svref *grapheme-break-names* (grapheme-break-index c))))

(defnative "char-extended-pictographic?" (c) (:leaf t)
  (check-char "char-extended-pictographic?" c)
  (bool (extended-pictographic-p c)))

;;; A cluster state: 0 (nothing yet), the previous character's property
;;; index + 1, or 65 (Extended_Pictographic), 66 (that and Extends), 67
;;; (that and a ZWJ), 68 (a pair of Regional_Indicators).
(defun grapheme-start-state (c)
  (if (extended-pictographic-p c) 65 (1+ (grapheme-break-index c))))

(defun grapheme-step (c state)
  "Racket's char-grapheme-step: whether a cluster ended before C (or,
from state 0, with it), and the state after."
  (let ((p (grapheme-break-index c)))
    (cond ((zerop state)
	   (if (member p '(2 3)) (values t 0) (values nil (grapheme-start-state c))))
	  ((and (= state 2) (= p 2)) (values t 0))
	  (t (let ((prev (cond ((<= 1 state 14) (1- state))
			       ((member state '(65 66)) :pict)
			       ((= state 67) :pict-zwj)
			       (t :ri-pair))))
	       (if (and (not (member prev '(1 2 3)))
			(not (member p '(1 2 3)))
			(or (and (eql prev 9) (member p '(9 10 12 13)))
			    (and (member prev '(12 10)) (member p '(10 11)))
			    (and (member prev '(13 11)) (eql p 11))
			    (member p '(4 5 8))
			    (eql prev 7)
			    (and (eq prev :pict-zwj) (extended-pictographic-p c))
			    (and (eql prev 6) (eql p 6))))
		   (values nil (cond ((and (eql p 4) (member state '(65 66))) 66)
				     ((and (eql p 5) (member state '(65 66))) 67)
				     ((eql prev 7) (grapheme-start-state c))
				     ((and (eql prev 6) (eql p 6)) 68)
				     (t (grapheme-start-state c))))
		   (values t (grapheme-start-state c))))))))

(defnative "char-grapheme-step" (c state) (:leaf t)
  (check-char "char-grapheme-step" c)
  (unless (typep state 'fixnum) (contract-error "char-grapheme-step" "grapheme-cluster-state?" state))
  (multiple-value-bind (consumed new-state) (grapheme-step c state)
    (values (bool consumed) new-state)))

(defun grapheme-span (s start end)
  (if (= start end)
      0
      (multiple-value-bind (consumed state) (grapheme-step (char s start) 0)
	(if consumed
	    1
	    (loop for i from (1+ start)
		  do (when (= i end) (return (- i start)))
		     (multiple-value-bind (consumed new-state) (grapheme-step (char s i) state)
		       (setq state new-state)
		       (when consumed
			 (return (if (zerop state) (- (1+ i) start) (- i start))))))))))

(defun grapheme-indices (who s start end)
  (check-string who s)
  (let ((n (length s)))
    (let ((start (if (absent-p start) 0 start)))
      (unless (and (typep start 'fixnum) (<= 0 start n))
	(racket-error who "~A is not a valid start index for ~A" (written start) (written s)))
      (let ((end (if (absent-p end) n end)))
	(unless (and (typep end 'fixnum) (<= start end n))
	  (racket-error who "~A and ~A are not valid start/end indices for ~A" (written start) (written end) (written s)))
	(values start end)))))

(defnative "string-grapheme-span" (s start &optional end) (:leaf t)
  (multiple-value-bind (start end) (grapheme-indices "string-grapheme-span" s start end)
    (grapheme-span s start end)))

(defnative "string-grapheme-count" (s &optional start end) (:leaf t)
  (multiple-value-bind (start end) (grapheme-indices "string-grapheme-count" s start end)
    (loop with count = 0
	  while (< start end)
	  do (incf start (grapheme-span s start end)) (incf count)
	  finally (return count))))

;;; The ranges of characters that behave alike, as Racket's reader of
;;; the Unicode tables makes them: (start end same?), where SAME? means
;;; every character in the range has the same properties (computed from
;;; SBCL's Unicode data, once).
(defun unassigned-char-p (n)
  (eq (sb-unicode:general-category (code-char n)) :cn))

(defun same-char-class-p (a b)
  (flet ((category (c) (sb-unicode:general-category c))
	 (delta (f c) (- (char-code (funcall f c)) (char-code c))))
    (let ((ca (category a)) (cb (category b)))
      (and (eq ca cb)
	   (eq (and (member ca '(:lu :ll :lt :lm :lo :nl)) t) (and (member cb '(:lu :ll :lt :lm :lo :nl)) t))
	   (eq (and (sb-unicode:lowercase-p a) t) (and (sb-unicode:lowercase-p b) t))
	   (eq (and (sb-unicode:uppercase-p a) t) (and (sb-unicode:uppercase-p b) t))
	   (= (delta #'char-upcase a) (delta #'char-upcase b))
	   (= (delta #'char-downcase a) (delta #'char-downcase b))
	   (let ((sa (string a)) (sb (string b)))
	     (eq (string= (normalized "make-known-char-range-list" sa :nfkd) sa)
		 (string= (normalized "make-known-char-range-list" sb :nfkd) sb)))))))

(defvar *known-char-ranges* nil)

(defun compute-known-char-ranges ()
  (let ((result '()) (i 1) (start 0) (same t))
    (block scan
      (loop
	(cond ((= i #xD800)
	       (push (list start (1- i) same) result)
	       (setq start #xE000 i #xE001 same t))
	      ((= i #x110000)
	       (push (list start #x10FFFF same) result)
	       (return-from scan))
	      ((unassigned-char-p i)
	       (push (list start (1- i) same) result)
	       (let ((j (1+ i)))
		 (loop
		   (cond ((= j #xD800) (setq j #xE000))
			 ((= j #x110000) (return-from scan))
			 ((unassigned-char-p j) (incf j))
			 (t (return))))
		 (setq start j i (1+ j) same t)))
	      ((and same (same-char-class-p (code-char i) (code-char (1- i)))) (incf i))
	      (t (setq same nil) (incf i)))))
    (mapcar (lambda (r) (list (first r) (second r) (bool (third r)))) (nreverse result))))

(defnative "make-known-char-range-list" () (:leaf t)
  (or *known-char-ranges* (setq *known-char-ranges* (compute-known-char-ranges))))

;;; ------------------------------------------------------------------
;;; SHA-1 and SHA-2

(defun sha-padded (b start end)
  "B's bytes START to END, padded as SHA's block structure wants."
  (let* ((len (- end start))
	 (total (* 64 (floor (+ len 9 63) 64)))
	 (m (make-octets total)))
    (replace m b :start2 start :end2 end)
    (setf (aref m len) #x80)
    (dotimes (i 8) (setf (aref m (- total 1 i)) (ldb (byte 8 (* 8 i)) (* 8 len))))
    m))

(declaim (inline u32 rol32 ror32))
(defun u32 (x) (ldb (byte 32 0) x))
(defun rol32 (x n) (u32 (logior (ash x n) (ash x (- n 32)))))
(defun ror32 (x n) (rol32 x (- 32 n)))

(defun block-word (m at)
  (logior (ash (aref m at) 24) (ash (aref m (+ at 1)) 16) (ash (aref m (+ at 2)) 8) (aref m (+ at 3))))

(defun words->octets (words n)
  (let ((r (make-octets (* 4 n))))
    (dotimes (i n r)
      (dotimes (k 4) (setf (aref r (+ (* 4 i) k)) (ldb (byte 8 (* 8 (- 3 k))) (nth i words)))))))

(defun sha1-octets (b start end)
  (let ((m (sha-padded b start end))
	(h (list #x67452301 #xEFCDAB89 #x98BADCFE #x10325476 #xC3D2E1F0))
	(w (make-array 80)))
    (loop for base from 0 below (length m) by 64
	  do (dotimes (i 16) (setf (svref w i) (block-word m (+ base (* 4 i)))))
	     (loop for i from 16 below 80
		   do (setf (svref w i)
			    (rol32 (logxor (svref w (- i 3)) (svref w (- i 8)) (svref w (- i 14)) (svref w (- i 16))) 1)))
	     (destructuring-bind (a b c d e) h
	       (dotimes (i 80)
		 (multiple-value-bind (f k)
		     (cond ((< i 20) (values (logior (logand b c) (logand (logxor b #xFFFFFFFF) d)) #x5A827999))
			   ((< i 40) (values (logxor b c d) #x6ED9EBA1))
			   ((< i 60) (values (logior (logand b c) (logand b d) (logand c d)) #x8F1BBCDC))
			   (t (values (logxor b c d) #xCA62C1D6)))
		   (let ((temp (u32 (+ (rol32 a 5) f e k (svref w i)))))
		     (setq e d d c c (rol32 b 30) b a a temp))))
	       (setq h (mapcar (lambda (x y) (u32 (+ x y))) h (list a b c d e)))))
    (words->octets h 5)))

(defun first-primes (n)
  (let ((primes '()))
    (loop for k from 2
	  while (< (length primes) n)
	  do (when (loop for p in primes never (zerop (mod k p))) (setq primes (append primes (list k)))))
    primes))

(defun integer-cube-root (n)
  (let ((x (ash 1 (1+ (floor (integer-length n) 3)))))
    (loop (let ((y (floor (+ (* 2 x) (floor n (* x x))) 3)))
	    (if (>= y x) (return x) (setq x y))))))

(defvar *sha256-constants*
  (map 'simple-vector (lambda (p) (ldb (byte 32 0) (integer-cube-root (* p (expt 2 96))))) (first-primes 64)))

(defun sha256-octets (b start end kind)
  "SHA-256, or SHA-224 for KIND :224: the initial hash is the fractional parts
of the square roots of the first (or the ninth to sixteenth) primes."
  (let* ((m (sha-padded b start end))
	 (primes (first-primes 16))
	 (h (if (eq kind :224)
		(mapcar (lambda (p) (ldb (byte 32 0) (isqrt (* p (expt 2 128))))) (nthcdr 8 primes))
		(mapcar (lambda (p) (ldb (byte 32 0) (isqrt (* p (expt 2 64))))) (subseq primes 0 8))))
	 (w (make-array 64)))
    (loop for base from 0 below (length m) by 64
	  do (dotimes (i 16) (setf (svref w i) (block-word m (+ base (* 4 i)))))
	     (loop for i from 16 below 64
		   do (let* ((w15 (svref w (- i 15))) (w2 (svref w (- i 2)))
			     (s0 (logxor (ror32 w15 7) (ror32 w15 18) (ash w15 -3)))
			     (s1 (logxor (ror32 w2 17) (ror32 w2 19) (ash w2 -10))))
			(setf (svref w i) (u32 (+ (svref w (- i 16)) s0 (svref w (- i 7)) s1)))))
	     (destructuring-bind (a b c d e f g hh) h
	       (dotimes (i 64)
		 (let* ((s1 (logxor (ror32 e 6) (ror32 e 11) (ror32 e 25)))
			(ch (logxor (logand e f) (logand (logxor e #xFFFFFFFF) g)))
			(t1 (u32 (+ hh s1 ch (svref *sha256-constants* i) (svref w i))))
			(s0 (logxor (ror32 a 2) (ror32 a 13) (ror32 a 22)))
			(maj (logxor (logand a b) (logand a c) (logand b c)))
			(t2 (u32 (+ s0 maj))))
		   (setq hh g g f f e e (u32 (+ d t1)) d c c b b a a (u32 (+ t1 t2)))))
	       (setq h (mapcar (lambda (x y) (u32 (+ x y))) h (list a b c d e f g hh)))))
    (words->octets h (if (eq kind :224) 7 8))))

(defun sha-input (who b start end)
  (unless (typep b 'octets)
    (racket-error who "contract violation~%  expected: (lambda (p) (or (bytes? p) (input-port? p)))~%  given: ~A" (written b)))
  (check-range who start end (length b) "byte string" b))

(defnatives "#%kernel"
  ("sha1-bytes" (b &optional start end)
   (multiple-value-bind (start end) (sha-input "sha1-bytes" b start end) (sha1-octets b start end)))
  ("sha224-bytes" (b &optional start end)
   (multiple-value-bind (start end) (sha-input "sha224-bytes" b start end) (sha256-octets b start end :224)))
  ("sha256-bytes" (b &optional start end)
   (multiple-value-bind (start end) (sha-input "sha256-bytes" b start end) (sha256-octets b start end :256))))

;;; ------------------------------------------------------------------
;;; Byte-string converters: UTF-8 (strict, or permissive as a source),
;;; UTF-16 and UTF-32 (with byte order marks as iconv has them), Latin-1
;;; and ASCII.  A converter keeps no input between calls, as Racket's
;;; reports what it didn't consume.

(defstruct (bconv (:constructor make-bconv (from to)))
  from to
  (source-order :be)			; UTF-16/32 source: from its byte order mark
  (source-started nil)
  (bom-pending nil)			; UTF-16/32 target: write the mark first
  (closed nil))

(defmethod print-object ((c bconv) stream) (format stream "#<bytes-converter>"))

(defun converter-kind (name source-p)
  (let ((name (string-upcase name)))
    (cond ((member name '("" "UTF-8") :test #'string=) :utf8)
	  ((and source-p (string= name "UTF-8-PERMISSIVE")) :utf8-permissive)
	  ((string= name "UTF-16") :utf16)
	  ((string= name "UTF-16BE") :utf16be)
	  ((string= name "UTF-16LE") :utf16le)
	  ((string= name "UTF-32") :utf32)
	  ((string= name "UTF-32BE") :utf32be)
	  ((string= name "UTF-32LE") :utf32le)
	  ((member name '("ISO-8859-1" "LATIN1" "LATIN-1") :test #'string=) :latin1)
	  ((member name '("ASCII" "US-ASCII") :test #'string=) :ascii)
	  (t nil))))

(defnative "bytes-open-converter" (from to) (:leaf t)
  (check-string "bytes-open-converter" from)
  (check-string "bytes-open-converter" to)
  (let ((f (converter-kind from t)) (k (converter-kind to nil)))
    (if (and f k)
	(let ((c (make-bconv f k)))
	  (setf (bconv-bom-pending c) (and (member k '(:utf16 :utf32)) t))
	  c)
	ps:false)))

(defnative "bytes-converter?" (x) (:leaf t) (bool (bconv-p x)))

(defun check-converter (who c)
  (unless (bconv-p c) (contract-error who "bytes-converter?" c))
  (when (bconv-closed c)
    (racket-error who "converter is closed~%  converter: ~A" (written c))))

(defnative "bytes-close-converter" (c) (:leaf t)
  (unless (bconv-p c) (contract-error "bytes-close-converter" "bytes-converter?" c))
  (setf (bconv-closed c) t)
  *void*)

(defun source-kind (c)
  "C's source encoding, with the byte order a mark chose."
  (let ((from (bconv-from c)) (order (bconv-source-order c)))
    (case from
      (:utf16 (if (eq order :be) :utf16be :utf16le))
      (:utf32 (if (eq order :be) :utf32be :utf32le))
      (t from))))

(defun decode-source (c b i end)
  "The next character code of B at I, the index after it, and :OK, :ABORTS or :ERROR."
  (let ((kind (source-kind c)))
    (ecase kind
      (:utf8 (utf8-decode b i end))
      (:utf8-permissive
       (multiple-value-bind (code next status) (utf8-decode b i end)
	 (if (eq status :error) (values #xFFFD next :ok) (values code next status))))
      ((:utf16be :utf16le)
       (if (> (+ i 2) end)
	   (values nil end :aborts)
	   (flet ((unit (at) (if (eq kind :utf16be)
				 (logior (ash (aref b at) 8) (aref b (1+ at)))
				 (logior (ash (aref b (1+ at)) 8) (aref b at)))))
	     (let ((u (unit i)))
	       (cond ((<= #xD800 u #xDBFF)
		      (if (> (+ i 4) end)
			  (values nil end :aborts)
			  (let ((u2 (unit (+ i 2))))
			    (if (<= #xDC00 u2 #xDFFF)
				(values (+ #x10000 (ash (- u #xD800) 10) (- u2 #xDC00)) (+ i 4) :ok)
				(values nil (+ i 2) :error)))))
		     ((<= #xDC00 u #xDFFF) (values nil (+ i 2) :error))
		     (t (values u (+ i 2) :ok)))))))
      ((:utf32be :utf32le)
       (if (> (+ i 4) end)
	   (values nil end :aborts)
	   (let ((u (if (eq kind :utf32be)
			(block-word b i)
			(logior (aref b i) (ash (aref b (+ i 1)) 8) (ash (aref b (+ i 2)) 16) (ash (aref b (+ i 3)) 24)))))
	     (if (or (> u #x10FFFF) (<= #xD800 u #xDFFF))
		 (values nil (+ i 4) :error)
		 (values u (+ i 4) :ok)))))
      (:latin1 (values (aref b i) (1+ i) :ok))
      (:ascii (if (< (aref b i) 128) (values (aref b i) (1+ i) :ok) (values nil (1+ i) :error))))))

(defun encode-target (kind code)
  "CODE as a list of bytes in encoding KIND, or NIL if it has none there."
  (flet ((u16 (u big) (if big (list (ldb (byte 8 8) u) (ldb (byte 8 0) u)) (list (ldb (byte 8 0) u) (ldb (byte 8 8) u))))
	 (u32b (u big) (let ((l (list (ldb (byte 8 24) u) (ldb (byte 8 16) u) (ldb (byte 8 8) u) (ldb (byte 8 0) u))))
			 (if big l (reverse l)))))
    (ecase kind
      (:utf8 (cond ((< code #x80) (list code))
		   ((< code #x800) (list (logior #xC0 (ash code -6)) (logior #x80 (logand code #x3F))))
		   ((< code #x10000) (list (logior #xE0 (ash code -12)) (logior #x80 (logand (ash code -6) #x3F))
					   (logior #x80 (logand code #x3F))))
		   (t (list (logior #xF0 (ash code -18)) (logior #x80 (logand (ash code -12) #x3F))
			    (logior #x80 (logand (ash code -6) #x3F)) (logior #x80 (logand code #x3F))))))
      ((:utf16 :utf16be :utf16le)
       (let ((big (not (eq kind :utf16le))))
	 (if (< code #x10000)
	     (u16 code big)
	     (let ((v (- code #x10000)))
	       (append (u16 (+ #xD800 (ash v -10)) big) (u16 (+ #xDC00 (logand v #x3FF)) big))))))
      ((:utf32 :utf32be :utf32le) (u32b code (not (eq kind :utf32le))))
      (:latin1 (and (< code 256) (list code)))
      (:ascii (and (< code 128) (list code))))))

(defun byte-order-mark (kind)
  (case kind (:utf16 '(#xFE #xFF)) (:utf32 '(0 0 #xFE #xFF))))

(defun detect-source-mark (c b i end)
  "A byte order mark at the start of a UTF-16 or UTF-32 source: its length."
  (let ((from (bconv-from c)))
    (setf (bconv-source-started c) t)
    (flet ((is (&rest bytes) (and (<= (+ i (length bytes)) end)
				  (loop for x in bytes for k from i always (= x (aref b k))))))
      (cond ((and (eq from :utf16) (is #xFF #xFE)) (setf (bconv-source-order c) :le) 2)
	    ((and (eq from :utf16) (is #xFE #xFF)) (setf (bconv-source-order c) :be) 2)
	    ((and (eq from :utf32) (is #xFF #xFE 0 0)) (setf (bconv-source-order c) :le) 4)
	    ((and (eq from :utf32) (is 0 0 #xFE #xFF)) (setf (bconv-source-order c) :be) 4)
	    (t 0)))))

(defun convert-bytes (c b s e dest ds de)
  "Racket's bytes-convert: (values converted-or-count consumed status)."
  (let ((out (and (null dest) (make-array 16 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
	(pos ds) (i s) (status :complete)
	(marked 0))
    (when (and (member (bconv-from c) '(:utf16 :utf32)) (not (bconv-source-started c)) (< i e))
      (setq marked (detect-source-mark c b i e))
      (incf i marked))
    (loop
      (when (>= i e) (setq status :complete) (return))
      (multiple-value-bind (code next state) (decode-source c b i e)
	(unless (eq state :ok) (setq status state) (return))
	(let* ((mark (and (bconv-bom-pending c) (byte-order-mark (bconv-to c))))
	       (bytes (encode-target (bconv-to c) code)))
	  (cond ((null bytes) (setq status :error) (return))
		((and dest (> (+ pos (length mark) (length bytes)) de)) (setq status :continues) (return))
		(t (dolist (x (append mark bytes))
		     (if out (vector-push-extend x out) (progn (setf (aref dest pos) x) (incf pos))))
		   (setf (bconv-bom-pending c) nil)
		   (setq i next))))))
    (values (if out (coerce out 'octets) (- pos ds))
	    (- i s)
	    (rsym (string-downcase (symbol-name status))))))

(defnative "bytes-convert" (c b &optional (start 0) end dest (dest-start 0) dest-end) (:leaf t)
  (check-converter "bytes-convert" c)
  (check-octets "bytes-convert" b)
  (multiple-value-bind (start end) (check-range "bytes-convert" start end (length b) "byte string" b)
    (when (absent-p dest) (setq dest nil))
    (when dest (check-octets "bytes-convert" dest))
    (multiple-value-bind (ds de)
	(if dest
	    (check-range "bytes-convert" dest-start dest-end (length dest) "byte string" dest)
	    (values 0 0))
      (convert-bytes c b start end dest ds de))))

(defnative "bytes-convert-end" (c &optional dest (dest-start 0) dest-end) (:leaf t)
  (check-converter "bytes-convert-end" c)
  (if (absent-p dest)
      (values (make-octets 0) (rsym "complete"))
      (progn (check-octets "bytes-convert-end" dest)
	     (check-range "bytes-convert-end" dest-start dest-end (length dest) "byte string" dest)
	     (values 0 (rsym "complete")))))

;;; ------------------------------------------------------------------
;;; Stencil vectors

(defstruct (stencil (:constructor make-stencil (mask items)))
  (mask 0 :type integer) (items #() :type simple-vector))

(defmethod print-object ((s stencil) stream)
  (format stream "#<stencil ~D~:[:~;~]~{ ~A~}>" (stencil-mask s) (zerop (length (stencil-items s)))
	  (map 'list (lambda (x) (with-output-to-string (o) (print-value x o :write))) (stencil-items s))))

(defconstant +stencil-mask-width+ 58)

(defun check-stencil (who x) (unless (stencil-p x) (contract-error who "stencil-vector?" x)) x)

(defun check-stencil-mask (who mask)
  (unless (typep mask `(integer 0 ,(1- (ash 1 +stencil-mask-width+))))
    (contract-error who "(integer-in 0 (sub1 (expt 2 (stencil-vector-mask-width))))" mask)))

(defnative "stencil-vector" (mask &rest items) (:leaf t)
  (check-stencil-mask "stencil-vector" mask)
  (unless (= (logcount mask) (length items))
    (racket-error "stencil-vector" "mask does not match given number of items~%  mask: ~D~%  given items: ~D"
		  mask (length items)))
  (make-stencil mask (coerce items 'simple-vector)))

(defnatives "#%kernel"
  ("stencil-vector?" (x) (bool (stencil-p x)))
  ("stencil-vector-length" (s) (length (stencil-items (check-stencil "stencil-vector-length" s))))
  ("stencil-vector-mask" (s) (stencil-mask (check-stencil "stencil-vector-mask" s)))
  ("stencil-vector-mask-width" () +stencil-mask-width+)
  ("stencil-vector-ref" (s i)
   (check-stencil "stencil-vector-ref" s)
   (svref (stencil-items s) (stencil-index "stencil-vector-ref" s i)))
  ("stencil-vector-set!" (s i x)
   (check-stencil "stencil-vector-set!" s)
   (setf (svref (stencil-items s) (stencil-index "stencil-vector-set!" s i)) x)
   *void*))

(defun stencil-index (who s i)
  (check-nonneg who i)
  (let ((n (length (stencil-items s))))
    (unless (< i n)
      (racket-error who "index is out of range~%  index: ~D~%  valid range: [0, ~D]~%  stencil vector: ~A"
		    i (1- n) (written s)))
    i))

(defnative "stencil-vector-update" (s remove add &rest items) (:leaf t)
  (check-stencil "stencil-vector-update" s)
  (check-stencil-mask "stencil-vector-update" remove)
  (check-stencil-mask "stencil-vector-update" add)
  (let ((mask (stencil-mask s)))
    (unless (zerop (logandc2 remove mask))
      (racket-error "stencil-vector-update" "mask of stencil vector does not have all bits in removal mask~%  stencil vector: ~A~%  removal mask: ~D"
		    (written s) remove))
    (let ((kept (logandc2 mask remove)))
      (unless (zerop (logand kept add))
	(racket-error "stencil-vector-update" "mask of stencil vector already has bits in addition mask~%  stencil vector: ~A~%  addition mask: ~D"
		      (written s) add))
      (unless (= (logcount add) (length items))
	(racket-error "stencil-vector-update" "mask does not match given number of items~%  mask: ~D~%  given items: ~D"
		      add (length items)))
      (let ((new-mask (logior kept add)) (result '()) (items items))
	(dotimes (bit +stencil-mask-width+)
	  (cond ((logbitp bit add) (push (pop items) result))
		((logbitp bit kept)
		 (push (svref (stencil-items s) (logcount (ldb (byte bit 0) mask))) result))))
	(make-stencil new-mask (coerce (nreverse result) 'simple-vector))))))

;;; ------------------------------------------------------------------
;;; Pseudo-random generators' state vectors (MRG32k3a's six words).  The
;;; generators here are SBCL random states: a vector is drawn from one,
;;; and the generator reseeded from it so a copy made from the vector
;;; produces what the original goes on to.

(defun prng-vector-p (x)
  (and (simple-vector-p x) (= (length x) 6)
       (every (lambda (n) (typep n '(integer 0))) x)
       (loop for i below 3 always (< (svref x i) 4294967087))
       (loop for i from 3 below 6 always (< (svref x i) 4294944443))
       (some #'plusp (subseq x 0 3))
       (some #'plusp (subseq x 3 6))))

(defun prng-state-from-vector (v)
  (sb-ext:seed-random-state (reduce (lambda (acc n) (+ (* acc 4294967311) n)) v :initial-value 0)))

(defun check-prng (who x) (unless (prng-p x) (contract-error who "pseudo-random-generator?" x)) x)

(defun check-prng-vector (who v)
  (unless (prng-vector-p v)
    (racket-error who "not a valid pseudo-random generator state vector ~A" (printed-quoted v))))

(defnatives "#%kernel"
  ("pseudo-random-generator-vector?" (x) (bool (prng-vector-p x)))
  ("pseudo-random-generator->vector" (g)
   (check-prng "pseudo-random-generator->vector" g)
   (let* ((*random-state* (make-random-state (prng-state g)))
	  (v (vector (random 4294967087) (random 4294967087) (1+ (random 4294967086))
		     (random 4294944443) (random 4294944443) (1+ (random 4294944442)))))
     (setf (prng-state g) (prng-state-from-vector v))
     v))
  ("vector->pseudo-random-generator" (v)
   (check-prng-vector "vector->pseudo-random-generator" v)
   (make-prng (prng-state-from-vector v)))
  ("vector->pseudo-random-generator!" (g v)
   (check-prng "vector->pseudo-random-generator!" g)
   (check-prng-vector "vector->pseudo-random-generator!" v)
   (setf (prng-state g) (prng-state-from-vector v))
   *void*))

;;; ------------------------------------------------------------------
;;; Numbers, byte strings, and the system

(defnative "real->double-flonum" (x) (:leaf t)
  (unless (realp x) (contract-error "real->double-flonum" "real?" x))
  (handler-case (float x 1d0)
    (error () (if (plusp x) sb-ext:double-float-positive-infinity sb-ext:double-float-negative-infinity))))

(defnative "shared-bytes" (&rest bytes) (:leaf t)
  (make-array (length bytes) :element-type '(unsigned-byte 8) :initial-contents bytes))
(defnative "make-shared-bytes" (n &optional (fill 0)) (:leaf t)
  (check-nonneg "make-shared-bytes" n)
  (make-array n :element-type '(unsigned-byte 8) :initial-element fill))

(defstruct (phantom-bytes (:constructor make-phantom-bytes* (size))) size)

(defnatives "#%kernel"
  ("make-phantom-bytes" (n) (check-nonneg "make-phantom-bytes" n) (make-phantom-bytes* n))
  ("phantom-bytes?" (x) (bool (phantom-bytes-p x)))
  ("set-phantom-bytes!" (p n)
   (unless (phantom-bytes-p p) (contract-error "set-phantom-bytes!" "phantom-bytes?" p))
   (check-nonneg "set-phantom-bytes!" n)
   (setf (phantom-bytes-size p) n)
   *void*))

(defnative "vector-set-performance-stats!" (v &optional thread) (:leaf t)
  (declare (ignore thread))
  (check-vector "vector-set-performance-stats!" v)
  (let ((stats (list (floor (get-internal-run-time) (/ internal-time-units-per-second 1000))
		      (floor (get-internal-real-time) (/ internal-time-units-per-second 1000))
		      (floor sb-ext:*gc-run-time* (/ internal-time-units-per-second 1000)))))
    (dotimes (i (min 12 (length v)))
      (setf (svref v i) (or (nth i stats) 0))))
  *void*)

(defnative "system-language+country" () (:leaf t)
  ;; "en_US" from the locale's environment variables
  (let* ((locale (or (find-if (lambda (s) (and s (plusp (length s))))
			      (mapcar #'uiop:getenv '("LC_ALL" "LC_MESSAGES" "LANG")))
		     "en_US"))
	 (cut (position-if (lambda (c) (member c '(#\. #\@))) locale)))
    (if (member locale '("C" "POSIX") :test #'string=)
	"en_US"
	(fresh-string (if cut (subseq locale 0 cut) locale)))))

(defnative "filesystem-root-list" () (:leaf t) (list (make-rpath (fresh-string "/"))))

(defnative "string->path-element" (s &optional false-on-non-element?) (:leaf t)
  (check-string "string->path-element" s)
  (cond ((zerop (length s)) (racket-error "string->path-element" "path string is empty"))
	((or (find #\/ s) (find (code-char 0) s) (string= s ".") (string= s ".."))
	 (if (and false-on-non-element? (truthy false-on-non-element?))
	     ps:false
	     (racket-error "string->path-element"
			   "cannot be converted to a path element~%  path: ~A~%  explanation: path can be split, is not relative, or names a special element"
			   (written s))))
	(t (make-rpath (fresh-string s)))))

(defnative "build-path/convention-type" (type base &rest subs) (:leaf t)
  (let ((kind (and (symbolp type) (rname type))))
    (unless (member kind '("unix" "windows") :test #'equal)
      (contract-error "build-path/convention-type" "(or/c 'unix 'windows)" type))
    (if (string= kind "unix")
	(apply (native-function (gethash "build-path" *natives*)) base subs)
	(make-rpath
	 (format nil "~{~A~^\\~}"
		 (mapcar (lambda (x)
			   (cond ((rpath-p x) (rpath-string x))
				 ((and (symbolp x) (string= (rname x) "up")) "..")
				 ((and (symbolp x) (string= (rname x) "same")) ".")
				 ((stringp x)
				  (racket-error "build-path/convention-type"
						"specified convention incompatible with string path element~%  path element: ~A~%  convention: ~A"
						(written x) (written type)))
				 (t (contract-error "build-path/convention-type"
						    "(or/c path-string? path-for-some-system? 'up 'same)" x))))
			 (cons base subs)))))))

;;; ------------------------------------------------------------------
;;; Dates: struct date (second minute hour day month year week-day
;;; year-day dst? time-zone-offset) and its subtype date* (nanosecond
;;; time-zone-name), transparent, as Racket's.

(defun check-integer-in (who lo hi x)
  (unless (and (integerp x) (<= lo x hi))
    (contract-error who (format nil "(integer-in ~D ~D)" lo hi) x)))

(defun check-boolean (who x)
  (unless (or (eq x ps:true) (eq x ps:false)) (contract-error who "boolean?" x)))

(defun date-guard (&rest args)
  (destructuring-bind (second minute hour day month year week-day year-day dst? offset who) args
    (let ((who (rname who)))
      (check-integer-in who 0 60 second)
      (check-integer-in who 0 59 minute)
      (check-integer-in who 0 23 hour)
      (check-integer-in who 1 31 day)
      (check-integer-in who 1 12 month)
      (unless (integerp year) (contract-error who "exact-integer?" year))
      (check-integer-in who 0 6 week-day)
      (check-integer-in who 0 365 year-day)
      (check-boolean who dst?)
      (unless (integerp offset) (contract-error who "exact-integer?" offset))
      (values second minute hour day month year week-day year-day dst? offset))))

(defun date*-guard (&rest args)
  (destructuring-bind (second minute hour day month year week-day year-day dst? offset nanosecond zone who) args
    (let ((who (rname who)))
      (check-integer-in who 0 999999999 nanosecond)
      (unless (stringp zone) (contract-error who "string?" zone))
      (values second minute hour day month year week-day year-day dst? offset nanosecond
	      (immutable! (fresh-string zone))))))

(defvar *date-type* nil)
(defvar *date*-type* nil)
(defvar *date-constructor* nil)
(defvar *date*-constructor* nil)

(defun define-date-types ()
  (multiple-value-bind (type constructor predicate)
      (make-struct-type* (rsym "date") ps:false 10 0 ps:false '() ps:false ps:false
			 '(0 1 2 3 4 5 6 7 8 9) #'date-guard)
    (setq *date-type* type *date-constructor* constructor)
    (define-primitive "#%kernel" "struct:date" type)
    (define-primitive "#%kernel" "date" constructor)
    (define-primitive "#%kernel" "date?" predicate))
  (multiple-value-bind (type constructor predicate)
      (make-struct-type* (rsym "date*") *date-type* 2 0 ps:false '() ps:false ps:false
			 '(0 1) #'date*-guard)
    (setq *date*-type* type *date*-constructor* constructor)
    (define-primitive "#%kernel" "struct:date*" type)
    (define-primitive "#%kernel" "date*" constructor)
    (define-primitive "#%kernel" "date*?" predicate))
  (loop for (name . i) in '(("second" . 0) ("minute" . 1) ("hour" . 2) ("day" . 3) ("month" . 4)
			    ("year" . 5) ("week-day" . 6) ("year-day" . 7) ("dst?" . 8)
			    ("time-zone-offset" . 9))
	do (let ((who (format nil "date-~A" name)) (i i))
	     (define-primitive "#%kernel" who
	       (lambda (x)
		 (unless (and (struct-instance-p x) (subtype-p (struct-of-type x) *date-type*))
		   (contract-error who "date?" x))
		 (svref (struct-fields x) i)))))
  (loop for (name . i) in '(("nanosecond" . 10) ("time-zone-name" . 11))
	do (let ((who (format nil "date*-~A" name)) (i i))
	     (define-primitive "#%kernel" who
	       (lambda (x)
		 (unless (and (struct-instance-p x) (subtype-p (struct-of-type x) *date*-type*))
		   (contract-error who "date*?" x))
		 (svref (struct-fields x) i))))))

(define-date-types)

(defun civil-from-days (days)
  "The proleptic Gregorian year, month and day of DAYS since 1970-01-01."
  (let* ((z (+ days 719468))
	 (era (floor z 146097))
	 (doe (- z (* era 146097)))
	 (yoe (floor (+ (- doe (floor doe 1460)) (floor doe 36524) (- (floor doe 146096))) 365))
	 (doy (- doe (+ (* 365 yoe) (floor yoe 4) (- (floor yoe 100)))))
	 (mp (floor (+ (* 5 doy) 2) 153))
	 (day (1+ (- doy (floor (+ (* 153 mp) 2) 5))))
	 (month (if (< mp 10) (+ mp 3) (- mp 9))))
    (values (+ yoe (* era 400) (if (<= month 2) 1 0)) month day)))

(defun days-from-civil (y m d)
  (let* ((y (if (<= m 2) (1- y) y))
	 (era (floor y 400))
	 (yoe (- y (* era 400)))
	 (doy (+ (floor (+ (* 153 (+ m (if (> m 2) -3 9))) 2) 5) (1- d)))
	 (doe (+ (* yoe 365) (floor yoe 4) (- (floor yoe 100)) doy)))
    (+ (* era 146097) doe -719468)))

(defun utc-fields (secs)
  "UTC's second, minute, hour, day, month, year, week day and year day of SECS."
  (multiple-value-bind (days rest) (floor secs 86400)
    (multiple-value-bind (year month day) (civil-from-days days)
      (values (mod rest 60) (mod (floor rest 60) 60) (floor rest 3600) day month year
	      (mod (+ days 4) 7) (- days (days-from-civil year 1 1))))))

(defun c-string-at (sap)
  (with-output-to-string (s)
    (loop for i from 0
	  for byte = (sb-sys::sap-ref-8 sap i)
	  until (zerop byte)
	  do (write-char (code-char byte) s))))

(defun local-fields (secs)
  "The local time of SECS from C's localtime_r: second, minute, hour,
day, month, year, week day, year day, dst?, offset and zone name; or NIL."
  (sb-alien::with-alien ((tt sb-alien::long secs)
			 (tm (sb-alien::array sb-alien::unsigned-char 96)))
    (let ((result (sb-alien::alien-funcall
		   (sb-alien::extern-alien "localtime_r"
					   (function sb-alien::system-area-pointer
						     sb-alien::system-area-pointer sb-alien::system-area-pointer))
		   (sb-alien::alien-sap (sb-alien::addr tt))
		   (sb-alien::alien-sap tm))))
      (unless (zerop (sb-sys::sap-int result))
	(flet ((int (offset) (sb-sys::signed-sap-ref-32 result offset)))
	  (values (int 0) (int 4) (int 8) (int 12) (1+ (int 16)) (+ 1900 (int 20)) (int 24) (int 28)
		  (plusp (int 32))
		  (sb-sys::signed-sap-ref-64 result 40)
		  (c-string-at (sb-sys::sap-ref-sap result 48))))))))

(defnative "seconds->date" (secs &optional (local-time? ps:true)) (:leaf t)
  (unless (realp secs) (contract-error "seconds->date" "real?" secs))
  (when (and (floatp secs) (or (sb-ext:float-infinity-p secs) (sb-ext:float-nan-p secs)))
    (racket-error "seconds->date" "integer is out-of-range~%  integer: ~A" (written secs)))
  (let* ((exact (rational secs))
	 (whole (floor exact))
	 (nanos (floor (* (- exact whole) 1000000000))))
    (when (> (abs whole) 67768036191676799)
      (racket-error "seconds->date" "integer is out-of-range~%  integer: ~D" whole))
    (multiple-value-bind (second minute hour day month year week-day year-day dst? offset zone)
	(if (truthy local-time?)
	    (local-fields whole)
	    (multiple-value-bind (s mi h d mo y wd yd) (utc-fields whole)
	      (values s mi h d mo y wd yd nil 0 "UTC")))
      (unless second
	(racket-error "seconds->date" "integer is out-of-range~%  integer: ~D" whole))
      (funcall *date*-constructor* second minute hour day month year week-day year-day (bool dst?) offset
	       nanos zone))))

;;; length: of a proper list only (the host's takes any sequence, and
;;; loops on a cycle)
(defnative "length" (l) (:leaf t)
  ;; list-length: an error for an improper list, NIL for a cycle (which
  ;; Racket shows in graph notation)
  (let ((n (and (listp l) (handler-case (list-length l) (error () :improper)))))
    (cond ((integerp n) n)
	  ((null n) (if (listp l)
			(racket-error "length" "~A is circular" (written l))
			(arg-error "length" "list?" l)))
	  (t (arg-error "length" "list?" l)))))
