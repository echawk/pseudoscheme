; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

;;;; More of #%kernel, #%unsafe and #%flfxnum, the simple ones racket/base
;;;; uses as it loads: unsafe operations, flonum and fixnum vectors and
;;;; operations, characters, placeholders, byte encodings of numbers,
;;;; files, time, and procedures.  Atomic sections are no-ops: there is
;;;; one Racket thread (control.lisp).

(in-package "PSEUDOSCHEME-RACKET")

(defmacro defnatives (instance &body definitions)
  "Several DEFNATIVEs in INSTANCE, all leaves: (name lambda-list body...)."
  `(progn ,@(loop for (name lambda-list . body) in definitions
		  collect `(defnative ,name ,lambda-list (:instance ,instance :leaf t) ,@body))))

;;; ------------------------------------------------------------------
;;; Lists, pairs, vectors, strings, bytes

(defnatives "#%unsafe"
  ("unsafe-list-ref" (l i) (nth i l))
  ("unsafe-list-tail" (l i) (nthcdr i l))
  ("unsafe-mcar" (p) (mpair-car p))
  ("unsafe-mcdr" (p) (mpair-cdr p))
  ("unsafe-set-mcar!" (p v) (setf (mpair-car p) v) *void*)
  ("unsafe-set-mcdr!" (p v) (setf (mpair-cdr p) v) *void*)
  ("unsafe-set-immutable-car!" (p v) (setf (car p) v) *void*)
  ("unsafe-set-immutable-cdr!" (p v) (setf (cdr p) v) *void*)
  ("unsafe-char=?" (a b) (bool (char= a b)))
  ("unsafe-char<?" (a b) (bool (char< a b)))
  ("unsafe-char<=?" (a b) (bool (char<= a b)))
  ("unsafe-char>?" (a b) (bool (char> a b)))
  ("unsafe-char>=?" (a b) (bool (char>= a b)))
  ("unsafe-char->integer" (c) (char-code c))
  ("unsafe-vector-copy" (v &optional (start 0) end) (subseq v start end))
  ("unsafe-vector*-copy" (v &optional (start 0) end) (subseq v start end))
  ("unsafe-vector-append" (&rest vs) (apply #'concatenate 'simple-vector vs))
  ("unsafe-vector*-append" (&rest vs) (apply #'concatenate 'simple-vector vs))
  ("unsafe-vector-set/copy" (v i x) (let ((c (copy-seq v))) (setf (svref c i) x) c))
  ("unsafe-vector*-set/copy" (v i x) (let ((c (copy-seq v))) (setf (svref c i) x) c))
  ("unsafe-vector*-cas!" (v i old new) (bool (eq (sb-ext:compare-and-swap (svref v i) old new) old)))
  ("unsafe-vector*->immutable-vector!" (v) (immutable! v))
  ("unsafe-string->immutable-string!" (s) (immutable! s))
  ("unsafe-bytes->immutable-bytes!" (b) (immutable! b))
  ("unsafe-bytes-copy!" (dest dest-start src &optional (src-start 0) end)
   (replace dest src :start1 dest-start :start2 src-start :end2 end) *void*)
  ("unsafe-struct*-type" (s) (struct-of-type s))
  ("unsafe-make-srcloc" (source line column position span)
   (funcall (primitive "srcloc") source line column position span))
  ("unsafe-start-atomic" () *void*)
  ("unsafe-end-atomic" () *void*)
  ("unsafe-start-breakable-atomic" () *void*)
  ("unsafe-end-breakable-atomic" () *void*)
  ("unsafe-start-uninterruptible" () *void*)
  ("unsafe-end-uninterruptible" () *void*)
  ("unsafe-in-atomic?" () ps:false)
  ("unsafe-add-collect-callbacks" (&rest args) (declare (ignore args)) *void*)
  ("unsafe-remove-collect-callbacks" (&rest args) (declare (ignore args)) *void*)
  ("unsafe-add-global-finalizer" (&rest args) (declare (ignore args)) *void*)
  ("unsafe-assert-unreachable" () (racket-error "unsafe-assert-unreachable" "reached"))
  ("unsafe-make-struct-type-property/guard-calls-no-arguments" (name &optional (guard ps:false) (supers '()) &rest more)
   (declare (ignore more))
   (funcall (native-function (gethash "make-struct-type-property" *natives*)) name guard supers)))

(defnatives "#%kernel"
  ("quotient/remainder" (n m)
   (values (funcall (psx:host-ref "quotient") n m) (funcall (psx:host-ref "remainder") n m)))
  ("list-pair?" (x) (bool (and (consp x) (handler-case (list-length x) (error () nil)))))
  ("bytes->list" (b) (coerce b 'list))
  ("list->bytes" (l) (make-array (length l) :element-type '(unsigned-byte 8) :initial-contents l))
  ("bytes>?" (a b) (bool (let ((m (mismatch a b)))
			   (and m (< m (length a)) (or (= m (length b)) (> (aref a m) (aref b m)))))))
  ("keyword->immutable-string" (k) (immutable! (copy-seq (rkeyword-name k))))
  ("interned-char?" (c) (bool (characterp c)))
  ("hash-clear" (h) (if (ihash-p h) (empty-ihash (ihash-kind h)) (progn (clrhash (rhash-table h)) h)))
  ("immutable-hash?" (x) (bool (ihash-p x)))
  ("mutable-hash?" (x) (bool (rhash-p x)))
  ("immutable-string?" (x) (bool (and (stringp x) (immutable-p x))))
  ("mutable-string?" (x) (bool (and (stringp x) (not (immutable-p x)))))
  ("immutable-bytes?" (x) (bool (and (typep x '(simple-array (unsigned-byte 8) (*))) (immutable-p x))))
  ("mutable-bytes?" (x) (bool (and (typep x '(simple-array (unsigned-byte 8) (*))) (not (immutable-p x)))))
  ("immutable-vector?" (x) (bool (and (simple-vector-p x) (immutable-p x))))
  ("mutable-vector?" (x) (bool (and (simple-vector-p x) (not (immutable-p x)))))
  ("immutable-box?" (x) (bool (and (rbox-p x) (immutable-p x))))
  ("mutable-box?" (x) (bool (and (rbox-p x) (not (immutable-p x)))))
  ("black-box" (x) x)
  ("assert-unreachable" () (racket-error "assert-unreachable" "reached"))
  ("path<?" (a &rest more)
   (bool (loop for x = a then y for y in more always (string< (rpath-string x) (rpath-string y)))))
  ("path-element->string" (p) (rpath-string p)))

;;; ------------------------------------------------------------------
;;; Characters

(defnatives "#%kernel"
  ("char-blank?" (c) (bool (or (char= c #\Space) (char= c #\Tab) (eq (sb-unicode:general-category c) :zs))))
  ("char-graphic?" (c) (bool (member (sb-unicode:general-category c)
				     '(:lu :ll :lt :lm :lo :mn :mc :me :nd :nl :no :pc :pd :ps :pe :pi :pf :po :sm :sc :sk :so))))
  ("char-lower-case?" (c) (bool (sb-unicode:lowercase-p c)))
  ("char-upper-case?" (c) (bool (sb-unicode:uppercase-p c)))
  ("char-title-case?" (c) (bool (eq (sb-unicode:general-category c) :lt)))
  ("char-punctuation?" (c) (bool (member (sb-unicode:general-category c) '(:pc :pd :ps :pe :pi :pf :po))))
  ("char-symbolic?" (c) (bool (member (sb-unicode:general-category c) '(:sm :sc :sk :so))))
  ("char-iso-control?" (c) (bool (or (<= 0 (char-code c) 31) (<= 127 (char-code c) 159))))
  ("char-foldcase" (c) (char-downcase c))
  ("char-titlecase" (c) (char-upcase c))
  ("char-ci<?" (a b) (bool (char-lessp a b)))
  ("char-ci<=?" (a b) (bool (char-not-greaterp a b)))
  ("char-ci>?" (a b) (bool (char-greaterp a b)))
  ("char-ci>=?" (a b) (bool (char-not-lessp a b))))

;;; ------------------------------------------------------------------
;;; Numbers, flonums, fixnums

(defun fl (x) (coerce x 'double-float))

(defnatives "#%flfxnum"
  ("->fl" (n) (fl n))
  ("fl->exact-integer" (x) (values (truncate x)))
  ("fl->fx" (x) (values (truncate x)))
  ("flsin" (x) (sin x)) ("flcos" (x) (cos x)) ("fltan" (x) (tan x))
  ("flasin" (x) (fl (asin x))) ("flacos" (x) (fl (acos x)))
  ("flatan" (x &optional y) (if y (atan x y) (atan x)))
  ("flexp" (x) (exp x)) ("fllog" (x) (if (plusp x) (log x) (fl (realpart (log (complex x 0d0))))))
  ("flexpt" (a b) (fl (expt a b)))
  ("flreal-part" (z) (fl (realpart z))) ("flimag-part" (z) (fl (imagpart z)))
  ("make-flrectangular" (a b) (complex a b))
  ("flsingle" (x) (fl (coerce x 'single-float)))
  ("fxpopcount" (n) (logcount n)) ("fxpopcount32" (n) (logcount n)) ("fxpopcount16" (n) (logcount n))
  ("fxrshift/logical" (n k) (ash (ldb (byte 61 0) n) (- k)))
  ("fx+/wraparound" (a b) (wrap-fixnum (+ a b)))
  ("fx-/wraparound" (a &optional b) (wrap-fixnum (if b (- a b) (- a))))
  ("fx*/wraparound" (a b) (wrap-fixnum (* a b)))
  ("fxlshift/wraparound" (a k) (wrap-fixnum (ash a k)))
  ("flvector" (&rest xs) (make-array (length xs) :element-type 'double-float :initial-contents (mapcar #'fl xs)))
  ("make-flvector" (n &optional (x 0d0)) (make-array n :element-type 'double-float :initial-element (fl x)))
  ("flvector?" (x) (bool (typep x '(simple-array double-float (*)))))
  ("flvector-length" (v) (length v))
  ("flvector-ref" (v i) (aref v i))
  ("flvector-set!" (v i x) (setf (aref v i) x) *void*)
  ("fxvector" (&rest xs) (make-fxvector* (coerce xs 'simple-vector)))
  ("make-fxvector" (n &optional (x 0)) (make-fxvector* (make-array n :initial-element x)))
  ("fxvector?" (x) (bool (fxvector-p x)))
  ("fxvector-length" (v) (length (fxvector-items v)))
  ("fxvector-ref" (v i) (svref (fxvector-items v) i))
  ("fxvector-set!" (v i x) (setf (svref (fxvector-items v) i) x) *void*))

(defstruct (fxvector (:constructor make-fxvector* (items))) (items #() :type simple-vector))

(defun wrap-fixnum (n)
  "N wrapped to Racket CS's fixnum range (61 bits)."
  (let ((m (ldb (byte 61 0) n))) (if (logbitp 60 m) (- m (ash 1 61)) m)))

(define-primitive "#%flfxnum" "most-positive-fixnum" (1- (ash 1 60)))
(define-primitive "#%flfxnum" "most-negative-fixnum" (- (ash 1 60)))

(defnatives "#%unsafe"
  ("unsafe-fx->fl" (n) (fl n)) ("unsafe-fl->fx" (x) (values (truncate x)))
  ("unsafe-flabs" (x) (abs x)) ("unsafe-flsqrt" (x) (sqrt x))
  ("unsafe-flmin" (a &rest r) (reduce #'min r :initial-value a))
  ("unsafe-flmax" (a &rest r) (reduce #'max r :initial-value a))
  ("unsafe-fxabs" (n) (abs n))
  ("unsafe-fxmin" (a &rest r) (reduce #'min r :initial-value a))
  ("unsafe-fxmax" (a &rest r) (reduce #'max r :initial-value a))
  ("unsafe-fx+/wraparound" (a b) (wrap-fixnum (+ a b)))
  ("unsafe-fx-/wraparound" (a b) (wrap-fixnum (- a b)))
  ("unsafe-fx*/wraparound" (a b) (wrap-fixnum (* a b)))
  ("unsafe-fxlshift/wraparound" (a k) (wrap-fixnum (ash a k)))
  ("unsafe-fxrshift/logical" (n k) (ash (ldb (byte 61 0) n) (- k)))
  ("unsafe-fxpopcount" (n) (logcount n))
  ("unsafe-flvector-length" (v) (length v)) ("unsafe-flvector-ref" (v i) (aref v i))
  ("unsafe-flvector-set!" (v i x) (setf (aref v i) x) *void*)
  ("unsafe-fxvector-length" (v) (length (fxvector-items v)))
  ("unsafe-fxvector-ref" (v i) (svref (fxvector-items v) i))
  ("unsafe-fxvector-set!" (v i x) (setf (svref (fxvector-items v) i) x) *void*))

(defnatives "#%kernel"
  ("magnitude" (z) (abs z))
  ("angle" (z) (if (and (rationalp z) (>= z 0)) 0 (phase z)))
  ("inexact-real?" (x) (bool (floatp x)))
  ("integer-sqrt/remainder" (n) (let ((r (isqrt n))) (values r (- n (* r r)))))
  ("bitwise-bit-field" (n start end) (ldb (byte (- end start) start) n))
  ("bitwise-first-bit-set" (n) (if (zerop n) -1 (1- (integer-length (logand n (- n))))))
  ("integer->integer-bytes" (n size signed? &optional (big-endian? ps:false) dest (start 0))
   (let ((b (or dest (make-array size :element-type '(unsigned-byte 8))))
	 (u (ldb (byte (* 8 size) 0) n)))
     (declare (ignore signed?))
     (dotimes (i size b)
       (setf (aref b (+ start (if (truthy big-endian?) (- size 1 i) i))) (ldb (byte 8 (* 8 i)) u)))))
  ("integer-bytes->integer" (b signed? &optional (big-endian? ps:false) (start 0) end)
   (let* ((end (or end (length b))) (size (- end start)) (u 0))
     (dotimes (i size)
       (setf (ldb (byte 8 (* 8 i)) u) (aref b (+ start (if (truthy big-endian?) (- size 1 i) i)))))
     (if (and (truthy signed?) (logbitp (1- (* 8 size)) u)) (- u (ash 1 (* 8 size))) u)))
  ("real->floating-point-bytes" (x size &optional (big-endian? ps:false) dest (start 0))
   (let ((bits (if (= size 4)
		   (ldb (byte 32 0) (sb-kernel:single-float-bits (coerce x 'single-float)))
		   (let ((d (fl x)))
		     (logior (ash (ldb (byte 32 0) (sb-kernel:double-float-high-bits d)) 32)
			     (sb-kernel:double-float-low-bits d)))))
	 (b (or dest (make-array size :element-type '(unsigned-byte 8)))))
     (dotimes (i size b)
       (setf (aref b (+ start (if (truthy big-endian?) (- size 1 i) i))) (ldb (byte 8 (* 8 i)) bits)))))
  ("floating-point-bytes->real" (b &optional (big-endian? ps:false) (start 0) end)
   (let* ((end (or end (length b))) (size (- end start)) (u 0))
     (dotimes (i size)
       (setf (ldb (byte 8 (* 8 i)) u) (aref b (+ start (if (truthy big-endian?) (- size 1 i) i)))))
     (if (= size 4)
	 (fl (sb-kernel:make-single-float (if (logbitp 31 u) (- u (ash 1 32)) u)))
	 (sb-kernel:make-double-float (let ((h (ldb (byte 32 32) u))) (if (logbitp 31 h) (- h (ash 1 32)) h))
				      (ldb (byte 32 0) u))))))

;;; ------------------------------------------------------------------
;;; Placeholders and reader graphs

(defstruct (placeholder (:constructor make-placeholder* (value))) value)
(defstruct (hash-placeholder (:constructor make-hash-placeholder* (kind assocs))) kind assocs)

(defnatives "#%kernel"
  ("make-placeholder" (v) (make-placeholder* v))
  ("placeholder?" (x) (bool (placeholder-p x)))
  ("placeholder-set!" (p v) (setf (placeholder-value p) v) *void*)
  ("placeholder-get" (p) (placeholder-value p))
  ("make-hash-placeholder" (assocs) (make-hash-placeholder* :equal assocs))
  ("make-hasheq-placeholder" (assocs) (make-hash-placeholder* :eq assocs))
  ("make-hasheqv-placeholder" (assocs) (make-hash-placeholder* :eqv assocs))
  ("make-hashalw-placeholder" (assocs) (make-hash-placeholder* :equal assocs))
  ("hash-placeholder?" (x) (bool (hash-placeholder-p x)))
  ("make-reader-graph" (v) (resolve-placeholders v (make-hash-table :test 'eq))))

(defun resolve-placeholders (v seen)
  "V with its placeholders replaced by their values, sharing and cycles kept."
  (labels ((walk (x)
	     (cond ((placeholder-p x) (walk (placeholder-value x)))
		   ((gethash x seen))
		   ((consp x)
		    (let ((c (cons nil nil)))
		      (setf (gethash x seen) c)
		      (setf (car c) (walk (car x)) (cdr c) (walk (cdr x)))
		      c))
		   ((simple-vector-p x)
		    (let ((c (make-array (length x))))
		      (setf (gethash x seen) c)
		      (dotimes (i (length x) c) (setf (svref c i) (walk (svref x i))))))
		   ((rbox-p x)
		    (let ((c (make-rbox)))
		      (setf (gethash x seen) c (rbox-value c) (walk (rbox-value x)))
		      c))
		   ((hash-placeholder-p x)
		    (ihash-from-assocs (hash-placeholder-kind x)
				       (mapcar (lambda (a) (cons (walk (car a)) (walk (cdr a))))
					       (hash-placeholder-assocs x))))
		   (t x))))
    (walk v)))

;;; ------------------------------------------------------------------
;;; Files, time, the process

(defnatives "#%kernel"
  ("delete-file" (p) (delete-file (path-string "delete-file" p)) *void*)
  ("delete-directory" (p) (sb-posix:rmdir (path-string "delete-directory" p)) *void*)
  ("make-directory" (p &optional perms) (declare (ignore perms))
   (sb-posix:mkdir (path-string "make-directory" p) #o777) *void*)
  ("copy-file" (from to &optional exists-ok?) (declare (ignore exists-ok?))
   (uiop:copy-file (path-string "copy-file" from) (path-string "copy-file" to)) *void*)
  ("rename-file-or-directory" (from to &optional exists-ok?) (declare (ignore exists-ok?))
   (sb-posix:rename (path-string "rename-file-or-directory" from) (path-string "rename-file-or-directory" to)) *void*)
  ("file-or-directory-type" (p &optional must-exist?)
   (let ((st (ignore-errors (sb-posix:lstat (path-string "file-or-directory-type" p)))))
     (cond ((null st) (if (truthy (or must-exist? ps:false))
			  (racket-error "file-or-directory-type" "access failed~%  path: ~A" (path-string "" p))
			  ps:false))
	   ((sb-posix:s-islnk (sb-posix:stat-mode st)) (rsym "link"))
	   ((sb-posix:s-isdir (sb-posix:stat-mode st)) (rsym "directory"))
	   (t (rsym "file")))))
  ("current-seconds" () (- (get-universal-time) #.(encode-universal-time 0 0 0 1 1 1970 0)))
  ("current-milliseconds" () (floor (get-internal-real-time) (/ internal-time-units-per-second 1000)))
  ("current-process-milliseconds" (&optional thread) (declare (ignore thread))
   (floor (get-internal-run-time) (/ internal-time-units-per-second 1000)))
  ("collect-garbage" (&optional mode) (declare (ignore mode)) (sb-ext:gc :full t) *void*))

(defnative "exit" (&optional (v ps:true)) ()
  (finish-output)
  (uiop:quit (cond ((and (integerp v) (<= 0 v 255)) v) (t 0))))

;;; ------------------------------------------------------------------
;;; Procedures

(defnatives "#%kernel"
  ("primitive?" (x) (declare (ignore x)) ps:false)
  ("primitive-closure?" (x) (declare (ignore x)) ps:false)
  ("procedure-realm" (f) (declare (ignore f)) (rsym "racket"))
  ("procedure->method" (f) f)
  ("procedure-closure-contents-eq?" (a b) (bool (eq a b)))
  ("parameter-procedure=?" (a b) (bool (eq a b))))

(defnative "make-derived-parameter" (p guard wrap &optional name realm) (:leaf t)
  (declare (ignore name realm))
  ;; a procedure over P's value: set through GUARD, read through WRAP
  (let ((data (gethash p *parameters*)))
    (let ((derived (lambda (&optional (v nil v-p))
		     (if v-p (funcall p (funcall guard v)) (funcall wrap (funcall p))))))
      (setf (gethash derived *parameters*) data)
      derived)))

(defnative "call-with-current-continuation" (proc &optional prompt-tag) ()
  (declare (ignore prompt-tag))
  (funcall (psx:host-ref "call-with-current-continuation") proc))
(defnative "continuation?" (x) (:leaf t) (declare (ignore x)) ps:false)

;;; ------------------------------------------------------------------
;;; Load forms for the rest of the values compiled code may hold as
;;; literals (cache.lisp): prefab structs (the expander's serialized
;;; syntax uses them) and compiled regexps.  Other structs can't be.

(defun prefab-instance (key count fields)
  (make-instance-of (prefab-struct-type key count) fields))

(defun srcloc-instance (fields) (make-instance-of *srcloc-type* fields))

(defmethod make-load-form ((x rstruct) &optional environment)
  (declare (ignore environment))
  (let ((type (rstruct-type x)))
    (cond ((struct-type-prefab-key type)
	   `(prefab-instance ',(struct-type-prefab-key type) ,(length (rstruct-fields x)) ',(rstruct-fields x)))
	  ((eq type *srcloc-type*) `(srcloc-instance ',(rstruct-fields x)))
	  (t (error "~S can't be in a compiled file: not a prefab struct" x)))))

(defmethod make-load-form ((x rregexp) &optional environment)
  (declare (ignore environment))
  `(make-rregexp ',(rregexp-source x) ',(rregexp-px x)))

(defnative "system-big-endian?" () (:leaf t) (bool (eq sb-c::*backend-byte-order* :big-endian)))

;;; The locale's encoding is UTF-8 here
(defnative "bytes->string/locale" (b &optional (err-char ps:false) (start 0) end) (:leaf t)
  (declare (ignore err-char))
  (sb-ext:octets-to-string b :external-format '(:utf-8 :replacement #\?) :start start :end end))
(defnative "locale-string-encoding" () (:leaf t) "UTF-8")
