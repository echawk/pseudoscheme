; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

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

(in-package "PSEUDOSCHEME-RACKET")

(defstruct (compiled-linklet (:constructor make-compiled-linklet (form &key name strategy)))
  form					; the lowered S-expression
  name
  strategy				; "cs" / "linklet" / "hybrid"
  (imports '())				; per import set, the external names
  (exports '())				; the external names of the exports
  (export-internals '())		; ... and their names inside the linklet
  (source-key nil)			; a string or a digest identifying its source, for the cache
  (source-form nil)			; the linklet as given, for .zo files
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
  (let ((root (asdf:system-source-directory :pseudoscheme)))
    (format nil "~A ~A~{ ~A~} ~36R" (psx::build-signature) psx::*full-continuations*
	    (loop for file in '("src/racket/linklet/compiler.lisp" "src/racket/cache.lisp")
		  collect (file-write-date (merge-pathnames file root)))
	    (psx::fnv-1a (prin1-to-string (list *host-shared* *host-aliases*))))))

(defun linklet-key (compiled)
  "The cache's key for COMPILED, if it has a source key."
  (let ((source (compiled-linklet-source-key compiled)))
    (and source
	 (format nil "~36R-~36R"
		 (if (integerp source) source (psx::fnv-1a source))
		 (psx::fnv-1a (format nil "~A ~A" (racket-signature) (compiled-linklet-strategy compiled)))))))

(defun racket-cache-directory ()
  "This signature's generation of the Racket cache (psx::CACHE-GENERATION)."
  (psx::cache-generation (uiop:merge-pathnames* "../racket/" (psx::library-cache-directory))
			 (racket-signature)))

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
	(psx::*accept-compile-failure* t))
    (write-split-fasl-1 path forms)))

(defun write-split-fasl-1 (path forms)
  (psx::write-compiled-file
   path
   (loop for form in forms
	 for i from 0
	 append (psx::translate-top-level
		 (list (core "racket:note-split-function") (quoted i) form)))
   :source "a Racket linklet"))

(defun load-split-fasl (path n)
  "The N functions of the split linklet whose fasl is at PATH."
  (let ((*loading-split-functions* (make-array n :initial-element nil)))
    ;; from a stream: loading by name looks the file up (stat, a dozen
    ;; times a path) for each of a few thousand fasls
    (with-open-file (in path :element-type '(unsigned-byte 8))
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

(defun whole-function (compiled)
  "The instantiation function of COMPILED, not split: from its fasl if
there is one, else compiled now, into the cache if COMPILED has a key."
  (let* ((form (compiled-linklet-form compiled))
	 (key (linklet-key compiled))
	 (fasl (and key (linklet-fasl key))))
    (or (and fasl (car (ignore-errors (load-split-fasl fasl 1))))
	(and fasl
	     (write-fasl-or-mark fasl (list form))
	     (car (ignore-errors (load-split-fasl fasl 1))))
	(psx:host-eval form))))

(defun split-functions (compiled)
  "The functions of split linklet COMPILED's forms: from its fasl if
there is one, else compiled now, into the cache if COMPILED has a key."
  (let* ((form (compiled-linklet-form compiled))
	 (n (length (cddr form)))
	 (key (linklet-key compiled))
	 (fasl (and key (linklet-fasl key))))
    (or (and fasl (ignore-errors (load-split-fasl fasl n)))
	(and fasl
	     (write-fasl-or-mark fasl (cddr form))
	     (ignore-errors (load-split-fasl fasl n)))
	(mapcar #'psx:host-eval (cddr form)))))

;;; ------------------------------------------------------------------
;;; .zo files: write-linklet-bundle-hash and read-linklet-bundle-hash
;;;
;;; After the expander's header: a 4-byte length (little-endian), then the
;;; bundle as Racket data in UTF-8, (#%pseudoscheme-bundle (key . value)
;;; ...).  A linklet is (#%pseudoscheme-linklet name form), its form as
;;; the expander gave it, decorrelated, and its cache key (a number):
;;; reading it compiles it again, which lowers it (fast) and finds its
;;; machine code in the fasl cache under that key.  (Hashing what is read
;;; back wouldn't do: it isn't the form that was hashed, so the code
;;; compiled as the .zo was written would be compiled again.)  Our
;;; values without a Racket syntax are tagged lists: (#%pseudoscheme-void), (#%pseudoscheme-path "..."),
;;; (#%pseudoscheme-srcloc source line column position span), and
;;; (#%pseudoscheme-prefab key field ...) for a prefab struct (the
;;; expander's serialized syntax holds shifted-to-label-phase ones).

(defun zo-encode (x)
  (cond ((compiled-linklet-p x)
	 ;; with its cache key, a digest of the linklet before this round
	 ;; trip: what it was compiled under, so reading it finds that code
	 (let ((key (compiled-linklet-source-key x)))
	   (list* (rsym "#%pseudoscheme-linklet") (or (compiled-linklet-name x) ps:false)
		  (zo-encode (correlated->datum (compiled-linklet-source-form x)))
		  (and (integerp key) (list key)))))
	((eq x *void*) (list (rsym "#%pseudoscheme-void")))
	((eq x ps:eof-object) (list (rsym "#%pseudoscheme-eof")))
	((rpath-p x) (list (rsym "#%pseudoscheme-path") (rpath-string x)))
	;; a srcloc (the expander's serialized syntax holds them)
	((and (struct-instance-p x) (eq (struct-of-type x) *srcloc-type*))
	 (cons (rsym "#%pseudoscheme-srcloc") (map 'list #'zo-encode (struct-fields x))))
	;; any other prefab struct
	((and (struct-instance-p x) (struct-type-prefab-key (struct-of-type x)))
	 (list* (rsym "#%pseudoscheme-prefab")
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

(defun zo-decode (x)
  (cond ((and (consp x) (symbolp (car x)))
	 (let ((head (rname (car x))))
	   (cond ((string= head "#%pseudoscheme-linklet")
		  (compile-linklet (zo-decode (third x)) (zo-decode (second x)) (fourth x)))
		 ((string= head "#%pseudoscheme-void") *void*)
		 ((string= head "#%pseudoscheme-eof") ps:eof-object)
		 ((string= head "#%pseudoscheme-path") (make-rpath (second x)))
		 ((string= head "#%pseudoscheme-srcloc")
		  (apply (primitive "srcloc") (mapcar #'zo-decode (cdr x))))
		 ((string= head "#%pseudoscheme-prefab")
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
  (let* ((text (with-output-to-string (s)
		 (print-value (cons (rsym "#%pseudoscheme-bundle")
				    (map 'list (lambda (e) (cons (car e) (zo-encode (cdr e))))
					 (entries bundle)))
			      s :write)))
	 (bytes (bytes-of text)))
    (out-bytes port (let ((n (length bytes)))
		      (make-array 4 :element-type '(unsigned-byte 8)
				    :initial-contents (loop for i below 4 collect (ldb (byte 8 (* 8 i)) n)))))
    (out-bytes port bytes)
    *void*))

(defun read-linklet-bundle (port)
  "read-linklet-bundle-hash: the bundle WRITE-LINKLET-BUNDLE wrote, from
PORT, an input port of ours."
  (let* ((n (loop for i below 4 sum (ash (inport-read-byte port) (* 8 i))))
	 (start (rinport-pos port))
	 (text (sb-ext:octets-to-string (rinport-bytes port) :external-format :utf-8
					:start start :end (+ start n))))
    (setf (rinport-pos port) (+ start n))
    (let ((datum (with-input-from-string (in text) (read-racket in))))
      (ihash-from-assocs :eq (mapcar (lambda (e) (cons (car e) (zo-decode (cdr e)))) (cdr datum))))))

;;; (system-type 'vm) answers this, so that Racket code that checks for
;;; chez-scheme doesn't take paths meant for Racket CS's machine code.
(defparameter *vm-name* "pseudoscheme")
