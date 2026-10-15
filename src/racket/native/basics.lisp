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
     ("make-bytes" . "make-bytevector")
     ("bytes-append" . "bytevector-append")
     ("bytes-fill!" . "bytevector-fill!")
     ("eof-object?" . "eof-object?") ("char<=?" . "char<=?") ("char>=?" . "char>=?")
     ("exact-integer?" . "exact-integer?") ("arithmetic-shift" . "bitwise-arithmetic-shift")
     ("bitwise-and" . "bitwise-and") ("bitwise-ior" . "bitwise-ior") ("bitwise-xor" . "bitwise-xor")
     ("bitwise-not" . "bitwise-not") ("bitwise-bit-set?" . "bitwise-bit-set?")
     ("integer-sqrt" . "exact-integer-sqrt")
     ("infinite?" . "infinite?") ("nan?" . "nan?")
     ("vector-map" . "vector-map")
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
;;; Immutable strings, byte strings, vectors and boxes: the same types as
;;; mutable ones.  A string, byte string or vector carries the Chez mode's
;;; flag in its header (src/chez/host.lisp), or the one SBCL's fasl
;;; loader gives constants, so that literals stay immutable in compiled
;;; code the cache loads; anything else is recorded here.

(defvar *immutables* (trivial-garbage:make-weak-hash-table :weakness :key :test 'eq :synchronized t))

(defconstant +immutable-header-flags+
  (ash (logior sb-vm:+vector-shareable-nonstd+ sb-vm:+vector-shareable+) 8))

(defun immutable! (x)
  (if (typep x '(simple-array * (*)))
      (ps-r6rs::mark-immutable x)
      (setf (gethash x *immutables*) t))
  x)

(defun immutable-p (x)
  (if (typep x '(simple-array * (*)))
      (logtest +immutable-header-flags+ (sb-kernel:get-header-data x))
      (values (gethash x *immutables*))))

;;; ------------------------------------------------------------------
;;; Constants

(defun ascii-bytes (s) (map '(simple-array (unsigned-byte 8) (*)) #'char-code s))

(defvar *unsafe-undefined* (make-symbol "unsafe-undefined")
  "#%unsafe's unsafe-undefined: the value of a letrec variable not yet
assigned, which schemify's and the expander's checks compare with.")

(define-primitive "#%kernel" "null" '())
(define-primitive "#%kernel" "eof" ps:eof-object)	; a value, not a procedure
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
;;; Pseudo-random generators: SBCL's random states
(defstruct (prng (:constructor make-prng (state))) state)
(defvar *current-prng* (make-parameter* (make-prng (make-random-state t))))
(define-primitive "#%kernel" "current-pseudo-random-generator" *current-prng*)
(defnative "make-pseudo-random-generator" () (:leaf t) (make-prng (make-random-state t)))
(defnative "pseudo-random-generator?" (x) (:leaf t) (bool (prng-p x)))
(defnative "random-seed" (k) (:leaf t)
  (setf (prng-state (funcall *current-prng*)) (sb-ext:seed-random-state k))
  *void*)
(defnative "random" (&rest args) (:leaf t)
  ;; (random), (random k), (random min max), each with a generator last
  (let* ((g (if (and args (prng-p (car (last args)))) (car (last args)) (funcall *current-prng*)))
	 (args (if (and args (prng-p (car (last args)))) (butlast args) args))
	 (*random-state* (prng-state g)))
    (case (length args)
      (0 (random 1d0))
      (1 (random (first args)))
      (t (+ (first args) (random (- (second args) (first args))))))))

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
(defnative "bytes->immutable-bytes" (b) (:leaf t) (if (immutable-p b) b (immutable! (copy-seq b))))
(defnative "string->immutable-string" (s) (:leaf t) (if (immutable-p s) s (immutable! (copy-seq s))))
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
(defnative "vector->immutable-vector" (v) (:leaf t) (if (immutable-p v) v (immutable! (copy-seq v))))
(defnative "immutable?" (x) (:leaf t) (bool (or (ihash-p x) (immutable-p x))))

;;; ------------------------------------------------------------------
;;; Boxes, weak boxes, place-locals

(defnative "box-immutable" (x) (:leaf t) (immutable! (make-rbox :value x)))
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

(defvar *srcloc-type* nil "#%kernel's struct:srcloc.")

(multiple-value-bind (type constructor predicate accessor)
    (make-struct-type* (rsym "srcloc") ps:false 5 0 ps:false '() ps:false ps:false '(0 1 2 3 4))
  (setq *srcloc-type* type)
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
  (port-write-string (apply #'racket-format control args) port)
  *void*)
(defnative "bytes-copy!" (dest dest-start src &optional (src-start 0) (src-end (length src))) (:leaf t)
  (unless (<= 0 src-start src-end (length src))
    (racket-error "bytes-copy!" "index is out of range~%  starting index: ~D~%  ending index: ~D" src-start src-end))
  (unless (<= 0 dest-start (- (length dest) (- src-end src-start)))
    (racket-error "bytes-copy!" "not enough room in target bytes~%  target starting index: ~D" dest-start))
  (replace dest src :start1 dest-start :start2 src-start :end2 src-end)
  *void*)

(defnative "printf" (control &rest args) ()
  (port-write-string (apply #'racket-format control args) (current-port :output))
  *void*)
(defnative "eprintf" (control &rest args) ()
  (port-write-string (apply #'racket-format control args) (current-port :error))
  *void*)

(defvar *error-print-width* (make-parameter* 256))
(define-primitive "#%kernel" "error-print-width" *error-print-width*)

;;; ------------------------------------------------------------------
;;; Errors with fields

(defun written (v)
  "V as Racket's error messages show it: through the
error-value->string-handler (handlers.lisp), printed as print does."
  (error-value->string v))

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

(defvar *renamed-procedures* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "procedure-rename's procedures and their names (handlers.lisp).")

(defnative "object-name" (x) (:leaf t)
  (cond ((rregexp-p x) (rregexp-source x))
	((regexp-literal-p x) (regexp-literal-source x))
	((rinport-p x) (rinport-name x))
	((routport-p x) (routport-name x))
	((struct-instance-p x) (struct-type-name (struct-of-type x)))
	((struct-type-p x) (struct-type-name x))
	((and (functionp x) (gethash x *renamed-procedures*)))
	((functionp x)
	 (let ((name (ignore-errors (sb-kernel:%fun-name x))))
	   ;; the host's procedures defined by DEFUNE are named .NAME
	   (if (and name (symbolp name) (not (null name)))
	       (rsym (string-left-trim "." (string-downcase (symbol-name name))))
	       ps:false)))
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
	  ((string= m "cross") (rsym "infer"))
	  ((string= m "machine-name") (machine-instance))
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
(defnative "vector-immutable" (&rest xs) (:leaf t) (immutable! (coerce xs 'simple-vector)))
(defnative "vector-cas!" (v i old new) (:leaf t)
  (bool (eq (sb-ext:compare-and-swap (svref v i) old new) old)))
(defnative "byte?" (x) (:leaf t) (bool (typep x '(unsigned-byte 8))))
(defnative "bytes<?" (a b) (:leaf t)
  (bool (let ((m (mismatch a b))) (and m (or (= m (length a)) (and (< m (length b)) (< (aref a m) (aref b m))))))))
(defnative "string-foldcase" (s) (:leaf t) (string-downcase s))
(defnative "string-locale-downcase" (s) (:leaf t) (string-downcase s))
(defnative "string-append-immutable" (&rest ss) (:leaf t) (immutable! (apply #'concatenate 'string ss)))
(defnative "symbol->immutable-string" (s) (:leaf t) (immutable! (copy-seq (rname s))))
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
(define-primitive "#%kernel" "eval-jit-enabled" (make-parameter* ps:true))
(defnative "unquoted-printing-string" (s) (:leaf t) s)
(defvar *interned-literals* (make-hash-table :test 'equal :weakness :value :synchronized t))
(defnative "datum-intern-literal" (v) (:leaf t)
  ;; a string or bytes: an immutable one, the same for equal ones
  (if (or (stringp v) (typep v '(simple-array (unsigned-byte 8) (*))))
      (let ((key (if (stringp v) (coerce v 'simple-string) (cons :bytes (coerce v 'list)))))
	(or (gethash key *interned-literals*)
	    (setf (gethash key *interned-literals*) (immutable! (copy-seq v)))))
      v))



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

;;; cache-configuration: a configuration value per index, computed once
(defvar *configuration* (make-hash-table :synchronized t))
(defnative "cache-configuration" (index thunk) ()
  (multiple-value-bind (v found) (gethash index *configuration*)
    (if found v (setf (gethash index *configuration*) (funcall thunk)))))

;;; c[ad]{2,4}r, every one: composed from car and cdr, right to left
(labels ((combos (n) (if (= n 0) '("") (loop for c in '("a" "d") append (mapcar (lambda (s) (concatenate 'string c s)) (combos (1- n)))))))
  (loop for n from 2 to 4
	do (dolist (letters (combos n))
	     (let ((name (format nil "c~Ar" letters))
		   (ops (reverse (coerce letters 'list))))
	       (unless (gethash name *natives*)
		 (setf (gethash name *natives*)
		       (make-native name
				    (lambda (x)
				      (dolist (op ops x)
					(unless (consp x)
					  (racket-error name "contract violation~%  given: ~A" (written x)))
					(setq x (if (char= op #\a) (car x) (cdr x)))))
				    :leaf t)))))))

;;; UTF-8, with Racket's arguments: an error character (or byte) and a range
(defnative "bytes->string/utf-8" (b &optional (err-char ps:false) (start 0) end) (:leaf t)
  (declare (ignore err-char))
  (sb-ext:octets-to-string b :external-format '(:utf-8 :replacement #\?) :start start :end end))
(defnative "string->bytes/utf-8" (s &optional (err-byte ps:false) (start 0) end) (:leaf t)
  (declare (ignore err-byte))
  (sb-ext:string-to-octets s :external-format :utf-8 :start start :end end))
(defnative "bytes-utf-8-length" (b &optional (err-char ps:false) (start 0) end) (:leaf t)
  (declare (ignore err-char))
  (length (sb-ext:octets-to-string b :external-format '(:utf-8 :replacement #\?) :start start :end end)))
