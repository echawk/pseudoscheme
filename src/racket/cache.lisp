; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-RACKET -*-

;;;; Compiled linklets, for all three strategies (docs/racket.md,
;;;; "Compiled code"):
;;;;   - what a .zo holds: each linklet's S-expression after lowering
;;;;     (A, C: schemify's output; B: after our passes 1 to 3), which is
;;;;     machine-independent, so Racket's compilation manager and raco
;;;;     make work unchanged;
;;;;   - the SBCL fasl: in a cache directory (racket/ beside the library
;;;;     cache, src/library-cache.lisp), keyed by the linklet's source
;;;;     text, the build signature, the dates of src/racket/'s files, the
;;;;     continuations mode and the strategy.
;;;;
;;;; Written so far: the fasl of a big (split) linklet with a source text
;;;; to key it by, which is the expander's: compiling it takes 25 s,
;;;; loading its fasl a few.  The fasl, loaded, hands each top-level
;;;; form's function to RACKET:NOTE-SPLIT-FUNCTION.  The load forms of
;;;; the Racket values a literal can be are with their types (reader.lisp,
;;;; linklet/api.lisp).

(in-package "QUASISCHEME-RACKET")

(defstruct (compiled-linklet (:constructor make-compiled-linklet (form &key name strategy)))
  form					; the lowered S-expression
  name
  strategy				; "cs" / "linklet" / "hybrid"
  (imports '())				; per import set, the external names
  (exports '())				; the external names of the exports
  (export-internals '())		; ... and their names inside the linklet
  (safe-exports '())			; the exports that are procedures that can't capture
  (source-key nil)			; a string or a digest identifying its source, for the cache
  (source-form nil)			; the linklet as given, for .zo files (or a function that reads it: LINKLET-SOURCE-FORM)
  (imports-known nil)			; what it was compiled knowing of its imports
  (shape nil)				; (:whole) or (:split cells forms), from its meta file (FORM :deferred)
  (function nil))			; once loaded: the instantiation function

(defvar *racket-signature* nil)

(defun racket-signature ()
  "What compiled linklets depend on besides their source
(COMPUTE-RACKET-SIGNATURE), computed once per process."
  (or *racket-signature* (setq *racket-signature* (compute-racket-signature))))

(pushnew (lambda () (setq *racket-signature* nil)) sb-ext:*init-hooks*)

(defun compute-racket-signature ()
  "What compiled linklets depend on: the build signature, the
continuations mode, the compiler's and this file's dates, and how
primitive names lower (*HOST-SHARED*, *HOST-ALIASES*).  Not the natives
themselves: compiled code calls every primitive by its name, a host
global (native.lisp), whichever native is behind it."
  (let ((root (asdf:system-source-directory :quasischeme)))
    (format nil "~A ~A~{ ~A~} ~36R" (qsx::build-signature) qsx::*full-continuations*
	    ;; native.lisp too: which natives are primitives that call no
	    ;; procedure changes what the continuation transformation makes
	    (loop for file in '("src/racket/linklet/compiler.lisp" "src/racket/cache.lisp"
				"src/racket/native/native.lisp")
		  collect (file-write-date (merge-pathnames file root)))
	    (qsx::fnv-1a (prin1-to-string (list *host-shared* *host-aliases*))))))

(defun linklet-key (compiled)
  "The cache's key for COMPILED, if it has a source key."
  (let ((source (compiled-linklet-source-key compiled)))
    (and source
	 (format nil "~36R-~36R"
		 (if (integerp source) source (qsx::fnv-1a source))
		 (qsx::fnv-1a (format nil "~A ~A" (racket-signature) (compiled-linklet-strategy compiled)))))))

(defvar *racket-cache-directory* nil
  "RACKET-CACHE-DIRECTORY's answer, once per process: finding the cache's
directory (truenames, a stat per component) for each linklet was a sixth
of starting racket.")

(pushnew (lambda () (setq *racket-cache-directory* nil)) sb-ext:*init-hooks*)

(defun racket-cache-directory ()
  "This signature's generation of the Racket cache (qsx::CACHE-GENERATION)."
  (or *racket-cache-directory*
      (setq *racket-cache-directory*
	    (qsx::cache-generation (uiop:merge-pathnames* "../racket/" (qsx::library-cache-directory))
				   (racket-signature)))))

(defun linklet-fasl (key)
  (merge-pathnames (make-pathname :name key :type "fasl") (racket-cache-directory)))

(defvar *loading-split-functions* nil
  "While a split linklet's fasl loads: the vector its functions go in.")

(defun write-split-fasl (path forms)
  "FORMS, a split linklet's (lambda (instance cells) ...) forms, compiled
into the fasl at PATH.  The compiler's notes, and the summary of a
compilation unit a form too big for SBCL aborted (compiled again with
compact frames), aren't the program's output."
  (let ((*error-output* (make-broadcast-stream))
	(qsx::*accept-compile-failure* t))
    (write-split-fasl-1 path forms)))

(defun write-split-fasl-1 (path forms)
  (qsx::write-compiled-file
   path
   (loop for form in forms
	 for i from 0
	 append (qsx::translate-top-level
		 (list (core "racket:note-split-function") (quoted i) form)))
   :source "a Racket linklet"))

(defun open-cache-file (path element-type)
  "An input stream of PATH, a cache file, or NIL if it can't be opened:
one open(2), not OPEN's access, lstat and realpath first, for each of
a few thousand cache files at start."
  (let ((fd (sb-unix:unix-open (sb-ext:native-namestring path) sb-unix:o_rdonly 0)))
    (and fd (sb-sys:make-fd-stream fd :input t :element-type element-type :buffering :full
				      :external-format :utf-8 :file path :auto-close t))))

(defmacro with-cache-file ((var path &optional (element-type ''(unsigned-byte 8))) &body body)
  "BODY with VAR an input stream of cache file PATH, or NIL if there's none."
  `(let ((,var (open-cache-file ,path ,element-type)))
     (unwind-protect (progn ,@body)
       (when ,var (close ,var)))))

(defun load-split-fasl (path n)
  "The N functions of the split linklet whose fasl is at PATH."
  (let ((*loading-split-functions* (make-array n :initial-element nil)))
    ;; from a stream: loading by name looks the file up (stat, a dozen
    ;; times a path) for each of a few thousand fasls
    (with-cache-file (in path)
      (unless in (error "~A isn't there" path))
      (handler-bind ((warning #'muffle-warning))
	(load in)))
    (when (find nil *loading-split-functions*)
      (error "~A doesn't hold all ~D forms" path n))
    (coerce *loading-split-functions* 'list)))

(defun fail-marker (fasl)
  "The file whose presence says that compiling FASL's forms failed (a
form too big for SBCL, say), so that the next process doesn't try
again: that costs seconds, twice (with compact frames)."
  (make-pathname :type "fail" :defaults fasl))

(defun write-fasl-or-mark (fasl forms)
  "Write FASL from FORMS; if compiling them fails, leave a fail marker
beside it and return NIL."
  (cond ((probe-file (fail-marker fasl)) nil)
	((ignore-errors (ensure-directories-exist fasl) (write-split-fasl fasl forms) t) t)
	(t (ignore-errors (with-open-file (out (fail-marker fasl) :direction :output :if-exists :supersede)
			    (declare (ignorable out))))
	   nil)))

(defun compile-into-cache-p ()
  "Whether a linklet not in the cache is compiled into it: not when
QUASISCHEME_EVALUATOR=interpret (src/psyntax.lisp, *EVALUATOR*), which
runs it in SBCL's interpreter instead (sb-fasteval, if SBCL has it)."
  (not (eq (qsx::evaluator) :interpret)))

;;; A linklet's meta file, beside its fasl: what compile-linklet would
;;; lower the linklet again to learn (its imports, exports and safe
;;; exports, and whether it's split), so that a linklet whose machine
;;; code is cached isn't lowered at all (LINKLET-FROM-META): at start,
;;; lowering racket/base's few hundred linklets was half the time.

(defun linklet-meta-file (key)
  (merge-pathnames (make-pathname :name key :type "meta") (racket-cache-directory)))

(defun linklet-shape (compiled)
  (or (compiled-linklet-shape compiled)
      (let ((form (compiled-linklet-form compiled)))
	(if (and (consp form) (eq (car form) (qsx::sym "racket:split")))
	    (list :split (cadr form) (length (cddr form)))
	    (list :whole)))))

(defun meta-symbols-p (x)
  "Whether X's symbols all read back as themselves (interned)."
  (cond ((symbolp x) (or (null x) (symbol-package x)))
	((consp x) (and (meta-symbols-p (car x)) (meta-symbols-p (cdr x))))
	(t (or (stringp x) (integerp x)))))

(defun write-linklet-meta (compiled)
  (let ((key (linklet-key compiled)))
    (when key
      (let ((file (linklet-meta-file key))
	    (meta (list (compiled-linklet-imports compiled) (compiled-linklet-exports compiled)
			(compiled-linklet-export-internals compiled) (compiled-linklet-safe-exports compiled)
			(linklet-shape compiled))))
	(when (and (meta-symbols-p meta) (not (probe-file file)))
	  (ignore-errors
	   (ensure-directories-exist file)
	   (let ((tmp (make-pathname :type "metatmp" :defaults file)))
	     (with-open-file (out tmp :direction :output :if-exists :supersede)
	       (with-standard-io-syntax
		 (let ((*package* (find-package "SCHEME")) (*print-readably* t))
		   (prin1 meta out))))
	     (rename-file tmp file))))))))

(defun linklet-from-meta (compiled)
  "COMPILED (its FORM :deferred, source key set) filled from its meta
file, if that's in the cache; else NIL."
  (let ((key (linklet-key compiled)))
    ;; (its fasl not looked for: if it's missing, it's compiled again
    ;; from the form lowered then, which the meta file describes)
    (when key
      (let ((meta (ignore-errors
		   (with-cache-file (in (linklet-meta-file key) 'character)
		     (and in (with-standard-io-syntax
			       (let ((*package* (find-package "SCHEME")) (*read-eval* nil))
				 (read in))))))))
	(when (and (consp meta) (= (length meta) 5))
	  (destructuring-bind (imports exports internals safe shape) meta
	    (setf (compiled-linklet-imports compiled) imports
		  (compiled-linklet-exports compiled) exports
		  (compiled-linklet-export-internals compiled) internals
		  (compiled-linklet-safe-exports compiled) safe
		  (compiled-linklet-shape compiled) shape)
	    compiled))))))

(defun linklet-source-form (compiled)
  "COMPILED's linklet as given: made now if what was kept is a function
that makes it (the expander's, INSTANTIATE-EXPANDER; a .zo's, which
READ-LINKLET-BUNDLE reads only if it's lowered or written again)."
  (let ((form (compiled-linklet-source-form compiled)))
    (if (functionp form)
	(setf (compiled-linklet-source-form compiled) (funcall form))
	form)))

(defun linklet-form (compiled)
  "COMPILED's lowered form: lowered now if its meta file stood for it."
  (when (eq (compiled-linklet-form compiled) :deferred)
    (setf (compiled-linklet-form compiled)
	  (compiled-linklet-form
	   (compile-linklet-form (linklet-source-form compiled)
				 :name (compiled-linklet-name compiled)
				 :imports-known (compiled-linklet-imports-known compiled)))))
  (compiled-linklet-form compiled))

(defun whole-function (compiled)
  "The instantiation function of COMPILED, not split: from its fasl if
there is one, else compiled now, into the cache if COMPILED has a key.
NIL if it has a key and SBCL can't compile it (WHOLE-OR-SPLIT-FUNCTION)."
  (let* ((key (linklet-key compiled))
	 (fasl (and key (linklet-fasl key))))
    (cond ((not fasl) (qsx:host-eval (linklet-form compiled)))
	  ((car (ignore-errors (load-split-fasl fasl 1))))
	  ((not (compile-into-cache-p)) (qsx:host-eval (linklet-form compiled)))
	  ((write-fasl-or-mark fasl (list (linklet-form compiled)))
	   (car (ignore-errors (load-split-fasl fasl 1)))))))

(defun split-functions (compiled)
  "The functions of split linklet COMPILED's forms: from its fasl if
there is one, else compiled now, into the cache if COMPILED has a key."
  (let* ((n (third (linklet-shape compiled)))
	 (key (linklet-key compiled))
	 (fasl (and key (linklet-fasl key))))
    (or (and fasl (ignore-errors (load-split-fasl fasl n)))
	(and fasl (compile-into-cache-p)
	     (write-fasl-or-mark fasl (cddr (linklet-form compiled)))
	     (ignore-errors (load-split-fasl fasl n)))
	(mapcar #'qsx:host-eval (cddr (linklet-form compiled))))))

;;; ------------------------------------------------------------------
;;; .zo files: write-linklet-bundle-hash and read-linklet-bundle-hash
;;;
;;; After the expander's header: a 4-byte length (little-endian) of the
;;; rest, a 4-byte length of the bundle, then the bundle as Racket data in
;;; UTF-8, (#%quasischeme-bundle (key . value) ...), then its linklets'
;;; forms.  A linklet is (#%quasischeme-linklet name start end key): its
;;; form as the expander gave it, decorrelated, is the text from START to
;;; END of what follows the bundle, read only if it's needed (its machine
;;; code or meta file is missing): at start, reading every linklet's form
;;; again was half of loading racket's .zo files.  KEY is its cache key
;;; (a number): reading it compiles it again, which finds its machine
;;; code in the fasl cache under that key.  (Hashing what is read
;;; back wouldn't do: it isn't the form that was hashed, so the code
;;; compiled as the .zo was written would be compiled again.)  Our
;;; values without a Racket syntax are tagged lists: (#%quasischeme-void), (#%quasischeme-path "..."),
;;; (#%quasischeme-srcloc source line column position span), and
;;; (#%quasischeme-prefab key field ...) for a prefab struct (the
;;; expander's serialized syntax holds shifted-to-label-phase ones).

(defvar *zo-forms* nil
  "While WRITE-LINKLET-BUNDLE encodes a bundle: an adjustable byte vector,
its linklets' forms as text, one after another.")

(defun zo-text-bytes (x)
  (bytes-of (with-output-to-string (s) (print-value x s :write))))

(defun zo-encode (x)
  (cond ((compiled-linklet-p x)
	 ;; with its cache key, a digest of the linklet before this round
	 ;; trip: what it was compiled under, so reading it finds that code
	 (let* ((key (compiled-linklet-source-key x))
		(text (zo-text-bytes (zo-encode (correlated->datum (linklet-source-form x)))))
		(start (fill-pointer *zo-forms*)))
	   (loop for b across text do (vector-push-extend b *zo-forms*))
	   (list* (rsym "#%quasischeme-linklet") (or (compiled-linklet-name x) qs:false)
		  start (fill-pointer *zo-forms*)
		  (and (integerp key) (list key)))))
	((eq x *void*) (list (rsym "#%quasischeme-void")))
	((eq x qs:eof-object) (list (rsym "#%quasischeme-eof")))
	((rpath-p x) (list (rsym "#%quasischeme-path") (rpath-string x)))
	;; a mutable hash table or an extflonum a macro quoted (Racket's fasl
	;; writes them too)
	((rhash-p x)
	 (list* (rsym "#%quasischeme-mutable-hash") (rsym (string-downcase (symbol-name (rhash-kind x))))
		(bool (rhash-weak x))
		(map 'list (lambda (e) (cons (zo-encode (car e)) (zo-encode (cdr e)))) (entries x))))
	((extflonum-p x) (list (rsym "#%quasischeme-extflonum") (extflonum-str x)))
	;; a srcloc (the expander's serialized syntax holds them)
	((and (struct-instance-p x) (eq (struct-of-type x) *srcloc-type*))
	 (cons (rsym "#%quasischeme-srcloc") (map 'list #'zo-encode (struct-fields x))))
	;; any other prefab struct
	((and (struct-instance-p x) (struct-type-prefab-key (struct-of-type x)))
	 (list* (rsym "#%quasischeme-prefab")
		(zo-encode (struct-type-prefab-key (struct-of-type x)))
		(map 'list #'zo-encode (struct-fields x))))
	((correlated-p x) (zo-encode (correlated->datum x)))
	;; the regexp layer's regexps, too (regexp-layer.lisp)
	((layer-regexp-literal x))
	((rregexp-p x)
	 ;; as the #rx or #px literal it prints as
	 (make-regexp-literal (if (rregexp-px x) :px :rx) (rregexp-source x)))
	((consp x) (let ((a (zo-encode (car x))) (d (zo-encode (cdr x))))
		     (if (and (eq a (car x)) (eq d (cdr x))) x (cons a d))))
	((simple-vector-p x) (map 'simple-vector #'zo-encode x))
	((ihash-p x) (ihash-from-assocs (ihash-kind x)
					(map 'list (lambda (e) (cons (zo-encode (car e)) (zo-encode (cdr e))))
					     (ihash-entries x))))
	((or (symbolp x) (numberp x) (stringp x) (characterp x) (rkeyword-p x) (rbox-p x)
	     (typep x '(simple-array (unsigned-byte 8) (*))) (regexp-literal-p x))
	 x)
	(t (error "can't put ~S in a .zo" x))))

(defvar *zo-bytes* nil
  "While READ-LINKLET-BUNDLE decodes a bundle: (bytes . start), where its
linklets' forms are.")

(defun zo-form-reader (start end)
  "A function that reads the linklet form from START to END of the
bundle being decoded (*ZO-BYTES*)."
  (let ((bytes (car *zo-bytes*))
	(base (cdr *zo-bytes*)))
    (lambda ()
      (zo-decode (with-input-from-string (in (sb-ext:octets-to-string bytes :external-format :utf-8
									      :start (+ base start) :end (+ base end)))
		   (read-racket in))))))

(defun zo-decode (x)
  (cond ((and (consp x) (symbolp (car x)))
	 (let ((head (rname (car x))))
	   (cond ((string= head "#%quasischeme-linklet")
		  (destructuring-bind (name start end &optional key) (cdr x)
		    (compile-linklet (zo-form-reader start end) (zo-decode name) key)))
		 ((string= head "#%quasischeme-void") *void*)
		 ((string= head "#%quasischeme-eof") qs:eof-object)
		 ((string= head "#%quasischeme-path") (make-rpath (second x)))
		 ((string= head "#%quasischeme-mutable-hash")
		  (fill-rhash (make-rhash (intern (string-upcase (rname (second x))) "KEYWORD") (truthy (third x)))
			      (mapcar (lambda (e) (cons (zo-decode (car e)) (zo-decode (cdr e)))) (cdddr x))))
		 ((string= head "#%quasischeme-extflonum") (make-extflonum (second x)))
		 ((string= head "#%quasischeme-srcloc")
		  (apply (primitive "srcloc") (mapcar #'zo-decode (cdr x))))
		 ((string= head "#%quasischeme-prefab")
		  (apply (primitive "make-prefab-struct") (mapcar #'zo-decode (cdr x))))
		 (t (cons (zo-decode (car x)) (zo-decode (cdr x)))))))
	((consp x) (cons (zo-decode (car x)) (zo-decode (cdr x))))
	((simple-vector-p x) (map 'simple-vector #'zo-decode x))
	((ihash-p x) (ihash-from-assocs (ihash-kind x)
					(map 'list (lambda (e) (cons (zo-decode (car e)) (zo-decode (cdr e))))
					     (ihash-entries x))))
	(t x)))

(defun write-linklet-bundle (bundle port)
  "write-linklet-bundle-hash: BUNDLE (a hash: phase or key -> linklet or
data) to PORT, an output port of ours."
  (flet ((size (n)
	   (make-array 4 :element-type '(unsigned-byte 8)
			 :initial-contents (loop for i below 4 collect (ldb (byte 8 (* 8 i)) n)))))
    (let* ((*zo-forms* (make-array 4096 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
	   (bytes (zo-text-bytes (cons (rsym "#%quasischeme-bundle")
				       (map 'list (lambda (e) (cons (car e) (zo-encode (cdr e))))
					    (entries bundle))))))
      (out-bytes port (size (+ 4 (length bytes) (length *zo-forms*))))
      (out-bytes port (size (length bytes)))
      (out-bytes port bytes)
      (out-bytes port (coerce *zo-forms* '(simple-array (unsigned-byte 8) (*))))
      *void*)))

(defun read-linklet-bundle (port)
  "read-linklet-bundle-hash: the bundle WRITE-LINKLET-BUNDLE wrote, from
PORT, an input port of ours."
  (flet ((size () (loop for i below 4 sum (ash (inport-read-byte port) (* 8 i)))))
    (let* ((total (size))
	   (n (size))
	   (start (rinport-pos port))
	   (bytes (rinport-bytes port))
	   (text (sb-ext:octets-to-string bytes :external-format :utf-8 :start start :end (+ start n))))
      (setf (rinport-pos port) (+ start (- total 4)))
      (let ((datum (with-input-from-string (in text) (read-racket in)))
	    (*zo-bytes* (cons bytes (+ start n))))
	(ihash-from-assocs :eq (mapcar (lambda (e) (cons (car e) (zo-decode (cdr e)))) (cdr datum)))))))

;;; (system-type 'vm) answers this, so that Racket code that checks for
;;; chez-scheme doesn't take paths meant for Racket CS's machine code.
(defparameter *vm-name* "quasischeme")
