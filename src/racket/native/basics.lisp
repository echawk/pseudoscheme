; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

;;;; The simple primitives: Racket names for host procedures (unsafe-car
;;;; is car here; fx= is fx=?), constants, and small natives -- numbers,
;;;; characters, strings and bytes, symbols and keywords, boxes, weak
;;;; boxes, srclocs, printing with format, argument errors, inspectors
;;;; and system-type.  The order here is the expander's: what it uses
;;;; most (tests/racket/coverage.lisp --expander) first.

(in-package "PSEUDOSCHEME-RACKET")

;;; ------------------------------------------------------------------
;;; Host procedures under Racket's names
;;;
;;; The compiler calls the host's procedure for these, as for
;;; *HOST-SHARED*'s, so the translator can open-code them.  An unsafe
;;; operation is the safe one: the host checks, which is allowed.

(defparameter *host-aliases*
  '(("#%unsafe" ("unsafe-car" . "car") ("unsafe-cdr" . "cdr") ("unsafe-cons-list" . "cons")
     ("unsafe-fx+" . "fx+") ("unsafe-fx-" . "fx-") ("unsafe-fx*" . "fx*")
     ("unsafe-fx=" . "fx=?") ("unsafe-fx<" . "fx<?") ("unsafe-fx>" . "fx>?")
     ("unsafe-fx<=" . "fx<=?") ("unsafe-fx>=" . "fx>=?")
     ("unsafe-fxand" . "fxand") ("unsafe-fxior" . "fxior") ("unsafe-fxxor" . "fxxor")
     ("unsafe-fxnot" . "fxnot")
     ("unsafe-fxlshift" . "fxarithmetic-shift-left") ("unsafe-fxrshift" . "fxarithmetic-shift-right")
     ("unsafe-fxquotient" . "quotient") ("unsafe-fxremainder" . "remainder")
     ("unsafe-fxmodulo" . "modulo")
     ("unsafe-fl+" . "fl+") ("unsafe-fl-" . "fl-") ("unsafe-fl*" . "fl*") ("unsafe-fl/" . "fl/")
     ("unsafe-fl=" . "fl=?") ("unsafe-fl<" . "fl<?") ("unsafe-fl>" . "fl>?")
     ("unsafe-fl<=" . "fl<=?") ("unsafe-fl>=" . "fl>=?")
     ("unsafe-vector-ref" . "vector-ref") ("unsafe-vector-set!" . "vector-set!")
     ("unsafe-vector-length" . "vector-length") ("unsafe-vector*-ref" . "vector-ref")
     ("unsafe-vector*-set!" . "vector-set!") ("unsafe-vector*-length" . "vector-length")
     ("unsafe-string-ref" . "string-ref") ("unsafe-string-set!" . "string-set!")
     ("unsafe-string-length" . "string-length")
     ("unsafe-bytes-ref" . "bytevector-u8-ref") ("unsafe-bytes-set!" . "bytevector-u8-set!")
     ("unsafe-bytes-length" . "bytevector-length"))
    ("#%kernel" ("list*" . "cons*") ("vector*-ref" . "vector-ref") ("vector*-set!" . "vector-set!")
     ("vector*-length" . "vector-length")
     ("bytes?" . "bytevector?") ("bytes-length" . "bytevector-length")
     ("bytes-ref" . "bytevector-u8-ref") ("bytes-set!" . "bytevector-u8-set!")
     ("make-bytes" . "make-bytevector") ("bytes-copy" . "bytevector-copy")
     ("bytes-copy!" . "bytevector-copy!") ("bytes-append" . "bytevector-append")
     ("bytes-fill!" . "bytevector-fill!")
     ("vector-copy!" . "vector-copy!") ("string-copy!" . "string-copy!")
     ("eof-object?" . "eof-object?") ("char<=?" . "char<=?") ("char>=?" . "char>=?")
     ("char-ci=?" . "char-ci=?") ("string-ci=?" . "string-ci=?")
     ("exact-integer?" . "exact-integer?") ("arithmetic-shift" . "bitwise-arithmetic-shift")
     ("bitwise-and" . "bitwise-and") ("bitwise-ior" . "bitwise-ior") ("bitwise-xor" . "bitwise-xor")
     ("bitwise-not" . "bitwise-not") ("bitwise-bit-set?" . "bitwise-bit-set?")
     ("integer-sqrt" . "exact-integer-sqrt")
     ("infinite?" . "infinite?") ("nan?" . "nan?")
     ("vector-fill!" . "vector-fill!") ("string-fill!" . "string-fill!")
     ("string->bytes/utf-8" . "string->utf8") ("bytes->string/utf-8" . "utf8->string")
     ("write-string" . "write-string") ("write-char" . "write-char") ("display" . "display")
     ("write" . "write") ("newline" . "newline") ("read-char" . "read-char")
     ("peek-char" . "peek-char") ("read-byte" . "read-u8") ("peek-byte" . "peek-u8")
     ("current-input-port" . "current-input-port") ("current-output-port" . "current-output-port")
     ("current-error-port" . "current-error-port") ("flush-output" . "flush-output-port")
     ("eof" . "eof-object") ("vector-copy" . "vector-copy") ("vector-map" . "vector-map")
     ("vector-for-each" . "vector-for-each") ("string-for-each" . "string-for-each"))
    ("#%flfxnum" ("fx+" . "fx+") ("fx-" . "fx-") ("fx*" . "fx*") ("fx=" . "fx=?") ("fx<" . "fx<?")
     ("fx>" . "fx>?") ("fx<=" . "fx<=?") ("fx>=" . "fx>=?") ("fxand" . "fxand") ("fxior" . "fxior")
     ("fxxor" . "fxxor") ("fxnot" . "fxnot") ("fxlshift" . "fxarithmetic-shift-left")
     ("fxrshift" . "fxarithmetic-shift-right") ("fxquotient" . "quotient")
     ("fxremainder" . "remainder") ("fxmodulo" . "modulo")
     ("fxmin" . "fxmin") ("fxmax" . "fxmax")
     ("fl+" . "fl+") ("fl-" . "fl-") ("fl*" . "fl*") ("fl/" . "fl/") ("fl=" . "fl=?")
     ("fl<" . "fl<?") ("fl>" . "fl>?") ("fl<=" . "fl<=?") ("fl>=" . "fl>=?")
     ("flabs" . "flabs") ("flsqrt" . "flsqrt") ("flfloor" . "flfloor") ("flceiling" . "flceiling")
     ("flround" . "flround") ("fltruncate" . "fltruncate") ("flmin" . "flmin") ("flmax" . "flmax")
     ("fx->fl" . "fixnum->flonum") ("fixnum?" . "fixnum?") ("flonum?" . "flonum?")))
  "Per instance, (Racket name . host name): the same procedure.")

(defun host-alias (name)
  "The host name for Racket primitive NAME, if it's an alias."
  (loop for (nil . pairs) in *host-aliases*
	do (let ((p (assoc name pairs :test #'string=)))
	     (when p (return (cdr p))))))

(defun install-host-aliases ()
  (loop for (instance . pairs) in *host-aliases*
	do (loop for (name . host) in pairs
		 do (let ((f (ignore-errors (psx:host-ref host))))
		      (if (functionp f)
			  (define-primitive instance name f)
			  (warn "--racket: *host-aliases* names ~A, which the host doesn't have" host))))))

;;; ------------------------------------------------------------------
;;; Constants

(defun ascii-bytes (s) (map '(simple-array (unsigned-byte 8) (*)) #'char-code s))

(defvar *unsafe-undefined* (make-symbol "unsafe-undefined")
  "#%unsafe's unsafe-undefined: the value of a letrec variable not yet
assigned, which schemify's and the expander's checks compare with.")

(define-primitive "#%kernel" "null" '())
(define-primitive "#%unsafe" "unsafe-undefined" *unsafe-undefined*)

(defnative "check-not-unsafe-undefined" (v name) (:instance "#%unsafe" :leaf t)
  (if (eq v *unsafe-undefined*)
      (racket-error (rname name) "undefined;~% cannot use before initialization")
      v))
(defnative "check-not-unsafe-undefined/assign" (v name) (:instance "#%unsafe" :leaf t)
  (if (eq v *unsafe-undefined*)
      (racket-error (rname name) "assignment disallowed;~% cannot assign before initialization")
      v))

;;; ------------------------------------------------------------------
;;; Numbers

(defnative "exact-nonnegative-integer?" (x) (:leaf t) (bool (and (integerp x) (>= x 0))))
(defnative "exact-positive-integer?" (x) (:leaf t) (bool (and (integerp x) (> x 0))))
(defnative "fixnum-for-every-system?" (x) (:leaf t) (bool (and (integerp x) (<= (- (ash 1 29)) x (1- (ash 1 29))))))
(defnative "integer-length" (n) (:leaf t) (integer-length n))
(defnative "fxabs" (n) (:instance "#%flfxnum" :leaf t) (abs n))
(defnative "random" (&optional (k nil k-p) prng) (:leaf t)
  (declare (ignore prng))
  (if k-p (random k) (random 1d0)))

;;; ------------------------------------------------------------------
;;; Symbols and keywords

(defnative "symbol<?" (a &rest more) (:leaf t)
  (bool (loop for x = a then y for y in more always (string< (rname x) (rname y)))))
(defnative "symbol-interned?" (s) (:leaf t) (bool (symbol-package s)))
(defnative "symbol-unreadable?" (s) (:leaf t) (declare (ignore s)) ps:false)
(defnative "string->uninterned-symbol" (s) (:leaf t)
  (make-symbol (symbol-name (rsym s))))
(defnative "string->unreadable-symbol" (s) (:leaf t) (rsym s))
(defnative "gensym" (&optional (base "g")) (:leaf t)
  (make-symbol (symbol-name (rsym (format nil "~A~D" (if (symbolp base) (rname base) base)
					  (incf *gensym-counter*))))))
(defnative "keyword?" (x) (:leaf t) (bool (rkeyword-p x)))
(defnative "keyword->string" (k) (:leaf t) (copy-seq (rkeyword-name k)))
(defnative "string->keyword" (s) (:leaf t) (rkeyword (coerce s 'simple-string)))
(defnative "keyword<?" (a &rest more) (:leaf t)
  (bool (loop for x = a then y for y in more always (string< (rkeyword-name x) (rkeyword-name y)))))

;;; ------------------------------------------------------------------
;;; Strings, characters and bytes

(defnative "bytes" (&rest bytes) (:leaf t)
  (make-array (length bytes) :element-type '(unsigned-byte 8) :initial-contents bytes))
(defnative "subbytes" (b start &optional (end (length b))) (:leaf t) (subseq b start end))
(defnative "bytes=?" (a &rest more) (:leaf t)
  (bool (every (lambda (b) (equalp a b)) more)))
(defnative "bytes->immutable-bytes" (b) (:leaf t) b)
(defnative "string->immutable-string" (s) (:leaf t) s)
(defnative "string->bytes/latin-1" (s &optional err start end) (:leaf t)
  (declare (ignore err))
  (map '(simple-array (unsigned-byte 8) (*)) #'char-code (subseq s (or start 0) end)))
(defnative "bytes->string/latin-1" (b &optional err start end) (:leaf t)
  (declare (ignore err))
  (map 'string #'code-char (subseq b (or start 0) end)))
(defnative "string->bytes/locale" (s &optional err start end) (:leaf t)
  (declare (ignore err))
  (sb-ext:string-to-octets (subseq s (or start 0) end) :external-format :utf-8))
(defnative "char-general-category" (c) (:leaf t)
  (rsym (string-downcase (symbol-name (sb-unicode:general-category c)))))
(defnative "vector->immutable-vector" (v) (:leaf t) v)
(defnative "immutable?" (x) (:leaf t) (bool (ihash-p x)))

;;; ------------------------------------------------------------------
;;; Boxes, weak boxes, place-locals

(defnative "box-immutable" (x) (:leaf t) (make-rbox :value x))
(defnative "unbox*" (b) (:leaf t) (rbox-value b))
(defnative "set-box*!" (b x) (:leaf t) (setf (rbox-value b) x) *void*)
(defnative "unsafe-unbox" (b) (:instance "#%unsafe" :leaf t) (rbox-value b))
(defnative "unsafe-unbox*" (b) (:instance "#%unsafe" :leaf t) (rbox-value b))
(defnative "unsafe-set-box!" (b x) (:instance "#%unsafe" :leaf t) (setf (rbox-value b) x) *void*)
(defnative "unsafe-set-box*!" (b x) (:instance "#%unsafe" :leaf t) (setf (rbox-value b) x) *void*)
(defnative "box-cas!" (b old new) (:leaf t)
  (bool (eq (sb-ext:compare-and-swap (rbox-value b) old new) old)))
(defnative "unsafe-box*-cas!" (b old new) (:instance "#%unsafe" :leaf t)
  (bool (eq (sb-ext:compare-and-swap (rbox-value b) old new) old)))

(defnative "make-weak-box" (v) (:leaf t) (sb-ext:make-weak-pointer v))
(defnative "weak-box?" (x) (:leaf t) (bool (sb-ext:weak-pointer-p x)))
(defnative "weak-box-value" (b &optional (gone ps:false)) (:leaf t)
  (multiple-value-bind (v alive) (sb-ext:weak-pointer-value b)
    (if alive v gone)))

;;; One place: a place-local is a box
(defnative "unsafe-make-place-local" (v) (:instance "#%unsafe" :leaf t) (make-rbox :value v))
(defnative "unsafe-place-local-ref" (b) (:instance "#%unsafe" :leaf t) (rbox-value b))
(defnative "unsafe-place-local-set!" (b v) (:instance "#%unsafe" :leaf t) (setf (rbox-value b) v) *void*)

;;; ------------------------------------------------------------------
;;; Struct-type properties the expander's structs carry, and srcloc

(defmacro define-property (name)
  `(define-primitive "#%kernel" ,name (%make-struct-type-property (rsym ,name) nil '())))

(define-property "prop:authentic")
(define-property "prop:sealed")
(define-property "prop:custom-write")
(define-property "prop:custom-print-quotable")
(define-property "prop:object-name")
(define-property "prop:evt")
(define-property "prop:input-port")
(define-property "prop:output-port")
(define-property "prop:exn:srclocs")
(define-property "prop:arity-string")
(define-property "prop:checked-procedure")
(define-property "prop:impersonator-of")
(define-property "prop:incomplete-arity")
(define-property "prop:method-arity-error")
(define-property "prop:procedure-accessor")
(define-property "prop:serializable")

(multiple-value-bind (type constructor predicate accessor)
    (make-struct-type* (rsym "srcloc") ps:false 5 0 ps:false '() ps:false ps:false '(0 1 2 3 4))
  (define-primitive "#%kernel" "struct:srcloc" type)
  (define-primitive "#%kernel" "srcloc" constructor)
  (define-primitive "#%kernel" "make-srcloc" constructor)
  (define-primitive "#%kernel" "srcloc?" predicate)
  (loop for field in '("source" "line" "column" "position" "span")
	for i from 0
	do (define-primitive "#%kernel" (format nil "srcloc-~A" field)
	     (let ((i i)) (lambda (s) (funcall accessor s i))))))

;;; ------------------------------------------------------------------
;;; Printing

(defnative "format" (control &rest args) () (apply #'racket-format control args))
(defnative "fprintf" (port control &rest args) ()
  (funcall (psx:host-ref "write-string") (apply #'racket-format control args) port)
  *void*)
(defnative "printf" (control &rest args) ()
  (funcall (psx:host-ref "write-string") (apply #'racket-format control args))
  *void*)
(defnative "eprintf" (control &rest args) ()
  (funcall (psx:host-ref "write-string") (apply #'racket-format control args)
	   (funcall (psx:host-ref "current-error-port")))
  *void*)

(defvar *error-print-width* (make-parameter* 256))
(define-primitive "#%kernel" "error-print-width" *error-print-width*)

;;; ------------------------------------------------------------------
;;; Errors with fields

(defun written (v) (with-output-to-string (s) (funcall (psx:host-ref "write") v s)))

(defnative "raise-arguments-error" (who message &rest fields-and-values) ()
  (do-raise (make-exn "exn:fail:contract"
		      (with-output-to-string (s)
			(format s "~A: ~A" (rname who) message)
			(loop for (field v) on fields-and-values by #'cddr
			      do (format s "~%  ~A: ~A" field (written v)))))))

(defnative "raise-result-error" (who expected value &rest more) ()
  (declare (ignore more))
  (do-raise (make-exn "exn:fail:contract"
		      (format nil "~A: contract violation~%  expected: ~A~%  result: ~A"
			      (rname who) expected (written value)))))

(defnative "raise-range-error" (who type-description index-prefix index in-value
				    lower-bound upper-bound &optional alt-lower-bound) ()
  (declare (ignore alt-lower-bound))
  (do-raise (make-exn "exn:fail:contract"
		      (format nil "~A: ~Aindex is out of range~%  ~Aindex: ~A~%  valid range: [~A, ~A]~%  ~A: ~A"
			      (rname who) index-prefix index-prefix index lower-bound upper-bound
			      type-description (written in-value)))))

;;; ------------------------------------------------------------------
;;; Inspectors, procedures, the system

(defvar *current-code-inspector* (make-parameter* *root-inspector*))
(define-primitive "#%kernel" "current-code-inspector" *current-code-inspector*)

(defnative "procedure-arity-includes?" (f k &optional incomplete-ok?) (:leaf t)
  (declare (ignore incomplete-ok?))
  (let ((mask (ignore-errors (arity-mask f))))
    (bool (or (null mask) (logbitp k mask)))))
(defnative "procedure-arity-mask" (f) (:leaf t)
  (or (ignore-errors (arity-mask f)) -1))

(defnative "object-name" (x) (:leaf t)
  (cond ((struct-instance-p x) (struct-type-name (struct-of-type x)))
	((struct-type-p x) (struct-type-name x))
	((functionp x)
	 (let ((name (ignore-errors (sb-kernel:%fun-name x))))
	   (if (and name (symbolp name) (not (null name))) (rsym (string-downcase (symbol-name name))) ps:false)))
	(t ps:false)))

(defnative "version" () (:leaf t) "9.3")
(defnative "banner" () (:leaf t) "Welcome to Racket v9.3 [pseudoscheme].
")

(defnative "system-type" (&optional (mode (rsym "os"))) (:leaf t)
  (let ((m (rname mode)))
    (cond ((string= m "os") (rsym (if (uiop:os-windows-p) "windows" (if (uiop:os-macosx-p) "macosx" "unix"))))
	  ((string= m "os*") (rsym (cond ((uiop:os-macosx-p) "macosx") ((uiop:featurep :linux) "linux")
					 ((uiop:os-windows-p) "windows") (t "unix"))))
	  ((string= m "arch") (rsym (if (member :arm64 *features*) "aarch64" "x86_64")))
	  ((string= m "vm") (rsym *vm-name*))
	  ((string= m "gc") (rsym "cs"))
	  ((string= m "word") 64)
	  ((string= m "link") (rsym (if (uiop:os-macosx-p) "framework" "shared")))
	  ((string= m "so-suffix") (ascii-bytes (if (uiop:os-macosx-p) ".dylib" ".so")))
	  ((string= m "so-mode") (rsym "local"))
	  ((string= m "fs-change") (vector ps:false ps:false ps:false ps:false))
	  ;; the machine compiled code is for: ours (BC answers racket)
	  ((string= m "target-machine") (rsym *vm-name*))
	  ((string= m "machine") (format nil "~A ~A" (software-type) (machine-type)))
	  ((string= m "library-subpath") (ascii-bytes (format nil "~A-~A"
							       (if (member :arm64 *features*) "aarch64" "x86_64")
							       (if (uiop:os-macosx-p) "macosx" "linux"))))
	  (t (racket-error "system-type" "contract violation~%  given: ~A" m)))))

;;; ------------------------------------------------------------------
;;; Ephemerons, lists, vectors, more numbers

(defstruct (ephemeron (:constructor make-ephemeron* (key value))) key value)
(defnative "make-ephemeron" (key v) (:leaf t)
  ;; the value is held strongly: SBCL has no ephemerons
  (make-ephemeron* (sb-ext:make-weak-pointer key) v))
(defnative "ephemeron?" (x) (:leaf t) (bool (ephemeron-p x)))
(defnative "ephemeron-value" (e &optional (gone ps:false) retain) (:leaf t)
  (declare (ignore retain))
  (if (nth-value 1 (sb-ext:weak-pointer-value (ephemeron-key e))) (ephemeron-value e) gone))

(defnative "andmap" (f l &rest ls) ()
  (block andmap
    (let ((result ps:true))
      (apply #'mapc (lambda (&rest xs)
		      (setq result (apply f xs))
		      (unless (truthy result) (return-from andmap ps:false)))
	     l ls)
      result)))
(defnative "ormap" (f l &rest ls) ()
  (block ormap
    (apply #'mapc (lambda (&rest xs)
		    (let ((r (apply f xs))) (when (truthy r) (return-from ormap r))))
	   l ls)
    ps:false))
(defnative "caadr" (x) (:leaf t) (caadr x))
(defnative "cadar" (x) (:leaf t) (cadar x))
(defnative "caddar" (x) (:leaf t) (car (cddar x)))
(defnative "cadadr" (x) (:leaf t) (cadadr x))
(defnative "set-mcar!" (p v) (:leaf t) (setf (mpair-car p) v) *void*)
(defnative "set-mcdr!" (p v) (:leaf t) (setf (mpair-cdr p) v) *void*)
(defnative "vector-immutable" (&rest xs) (:leaf t) (coerce xs 'simple-vector))
(defnative "vector-cas!" (v i old new) (:leaf t)
  (bool (eq (sb-ext:compare-and-swap (svref v i) old new) old)))
(defnative "byte?" (x) (:leaf t) (bool (typep x '(unsigned-byte 8))))
(defnative "bytes<?" (a b) (:leaf t)
  (bool (let ((m (mismatch a b))) (and m (or (= m (length a)) (and (< m (length b)) (< (aref a m) (aref b m))))))))
(defnative "string-foldcase" (s) (:leaf t) (string-downcase s))
(defnative "string-locale-downcase" (s) (:leaf t) (string-downcase s))
(defnative "string-append-immutable" (&rest ss) (:leaf t) (apply #'concatenate 'string ss))
(defnative "symbol->immutable-string" (s) (:leaf t) (rname s))
(defnative "char-utf-8-length" (c) (:leaf t)
  (let ((n (char-code c))) (cond ((< n #x80) 1) ((< n #x800) 2) ((< n #x10000) 3) (t 4))))
(defnative "make-rectangular" (a b) (:leaf t) (complex a b))
(defnative "make-polar" (m a) (:leaf t) (* m (cis a)))
(defnative "real-part" (z) (:leaf t) (realpart z))
(defnative "imag-part" (z) (:leaf t) (imagpart z))
(defnative "single-flonum?" (x) (:leaf t) (declare (ignore x)) ps:false)
(defnative "single-flonum-available?" () (:leaf t) ps:false)
(defnative "real->single-flonum" (x) (:leaf t)
  (racket-error "real->single-flonum" "unsupported~%  given: ~A" x))
(defnative "extflonum?" (x) (:instance "#%extfl" :leaf t) (declare (ignore x)) ps:false)
(defnative "extflonum-available?" () (:instance "#%extfl" :leaf t) ps:false)
(defnative "current-inexact-monotonic-milliseconds" () (:leaf t)
  (coerce (/ (get-internal-real-time) (/ internal-time-units-per-second 1000)) 'double-float))
(defnative "current-inexact-milliseconds" () (:leaf t)
  (multiple-value-bind (s us) (sb-ext:get-time-of-day) (+ (* s 1000d0) (/ us 1000d0))))
(defnative "current-gc-milliseconds" () (:leaf t) sb-ext:*gc-run-time*)
(defnative "current-memory-use" (&optional mode) (:leaf t) (declare (ignore mode)) (sb-kernel:dynamic-usage))
(defnative "eval-jit-enabled" () (:leaf t) ps:true)
(defnative "unquoted-printing-string" (s) (:leaf t) s)
(defnative "datum-intern-literal" (v) (:leaf t) v)

;;; current-print: its default prints a value that isn't #<void> and a
;;; newline, as Racket's (with write; print's quoting comes later)
(define-primitive "#%kernel" "current-print"
  (make-parameter* (lambda (v)
		     (unless (eq v *void*)
		       (funcall (psx:host-ref "write") v)
		       (funcall (psx:host-ref "newline")))
		     *void*)))

;;; arity-at-least, and procedure-arity from the arity mask
(defvar *arity-at-least* nil)
(defun arity-at-least (n) (funcall (second *arity-at-least*) n))

(multiple-value-bind (type constructor predicate accessor)
    (make-struct-type* (rsym "arity-at-least") ps:false 1 0 ps:false '() ps:false ps:false '(0))
  (setq *arity-at-least* (list type constructor predicate accessor))
  (define-primitive "#%kernel" "struct:arity-at-least" type)
  (define-primitive "#%kernel" "arity-at-least" constructor)
  (define-primitive "#%kernel" "make-arity-at-least" constructor)
  (define-primitive "#%kernel" "arity-at-least?" predicate)
  (define-primitive "#%kernel" "arity-at-least-value" (lambda (a) (funcall accessor a 0))))

(defun mask->arity (mask)
  "procedure-arity's form of arity mask MASK."
  (let ((arities (if (minusp mask)
		     (let ((rest (integer-length (lognot mask))))
		       (append (loop for i below rest when (logbitp i mask) collect i)
			       (list (arity-at-least rest))))
		     (loop for i below (integer-length mask) when (logbitp i mask) collect i))))
    (if (and arities (null (cdr arities))) (car arities) arities)))

(defnative "procedure-arity" (f) (:leaf t) (mask->arity (or (ignore-errors (arity-mask f)) -1)))
(defnative "procedure-arity?" (x) (:leaf t)
  (bool (or (and (integerp x) (>= x 0)) (truthy (funcall (third *arity-at-least*) x)) (listp x))))
