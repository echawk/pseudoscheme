; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

;;;; The native runtime: Racket's primitives written in Lisp, for SBCL.
;;;;
;;;; Two strategies use it:
;;;;   B  as rumble's replacement: each native procedure is a primitive
;;;;      of its instance (#%kernel unless said otherwise), and a host
;;;;      global named racket:NAME, which compiled linklets call;
;;;;   C  as overrides of rumble: a native procedure that has a :RUMBLE
;;;;      name replaces rumble's procedure of that name after rumble
;;;;      loads (hybrid/overrides.lisp), so schemify's output and
;;;;      rumble's own callers get it.
;;;; So one definition serves both, and the two stay comparable.
;;;;
;;;; A primitive whose Racket meaning is the host's own (car, +, ...) is
;;;; in *HOST-SHARED* rather than written again: compiled code calls the
;;;; host's, which the translator open-codes.

(in-package "PSEUDOSCHEME-RACKET")

(defstruct (native (:constructor make-native (name function &key instance rumble leaf)))
  name function (instance "#%kernel") rumble
  leaf)					; calls no Racket procedure: not a continuation capture site

(defvar *natives* (make-hash-table :test 'equal) "Racket name -> NATIVE.")

(defmacro defnative (name lambda-list (&key (instance "#%kernel") rumble leaf) &body body)
  "Define native procedure NAME (Racket's name) in INSTANCE.  RUMBLE is
the name rumble defines it under, if C may override it.  LEAF: it calls
no Racket procedure (so the continuation transformation needn't treat a
call of it as a capture site)."
  `(setf (gethash ,name *natives*)
	 (make-native ,name (lambda ,lambda-list ,@body) :instance ,instance :rumble ,rumble :leaf ,leaf)))

(defun native-host-name (name)
  "The host global compiled code refers to primitive NAME by, if it has
one: every primitive the instances have that isn't host-shared."
  (and (nth-value 1 (primitive name)) (concatenate 'string "racket:" name)))

(defparameter *host-shared*
  '(;; pairs and lists
    "car" "cdr" "cons" "caar" "cadr" "cdar" "cddr" "caddr" "cdddr" "cadddr"
    "list" "append" "reverse" "length" "list-ref" "list-tail"
    "memq" "memv" "assq" "assv" "null?" "pair?" "list?"
    ;; equivalence, booleans
    "eq?" "eqv?" "not" "boolean?"
    ;; numbers
    "+" "-" "*" "/" "=" "<" ">" "<=" ">=" "zero?" "positive?" "negative?" "odd?" "even?"
    "abs" "max" "min" "quotient" "remainder" "modulo" "gcd" "lcm" "expt" "exp" "log" "sqrt"
    "sin" "cos" "tan" "asin" "acos" "atan" "floor" "ceiling" "round" "truncate"
    "number?" "integer?" "rational?" "real?" "complex?" "exact?" "inexact?"
    "exact->inexact" "inexact->exact" "string->number"
    "numerator" "denominator"
    ;; characters, strings, symbols
    "char?" "char=?" "char<?" "char>?" "char->integer" "integer->char" "char-upcase" "char-downcase"
    "char-alphabetic?" "char-numeric?" "char-whitespace?"
    "string?" "string" "make-string" "string-length" "string-ref" "string-set!" "substring"
    "string-append" "string=?" "string<?" "string-copy" "string->list" "list->string"
    "string->symbol" "symbol->string" "symbol?" "string-upcase" "string-downcase"
    ;; vectors, bytevectors (Racket's bytes are the host's bytevectors)
    "vector" "make-vector" "vector?" "vector-ref" "vector-set!" "vector-length"
    "vector->list" "list->vector" "vector-fill!"
    ;; control
    "procedure?" "apply" "values" "call-with-values" "map" "for-each"
    "dynamic-wind")
  "Racket primitives that are the host's procedures of the same name and
behaviour.")

(defun install-natives ()
  "B: make each native procedure a primitive of its instance, and a host
global; the host-shared ones primitives too."
  (dolist (name *host-shared*)
    (let ((f (ignore-errors (psx:host-ref name))))
      (if (functionp f)
	  (define-primitive "#%kernel" name f)
	  (warn "--racket: *host-shared* names ~A, which the host doesn't have" name))))
  (let ((leaves '()))
    (maphash (lambda (name n)
	       (define-primitive (native-instance n) name (native-function n))
	       (when (native-leaf n) (push (concatenate 'string "racket:" name) leaves)))
	     *natives*)
    (psx::register-primitive-names leaves))
  (install-host-aliases)
  (install-compiler-helpers))

(defun install-host-globals ()
  "Each instance's primitives as host globals racket:NAME, for compiled
linklets (after every instance is filled: #%linklet's too)."
  (maphash (lambda (instance table)
	     (declare (ignore instance))
	     (maphash (lambda (name value)
			(unless (or (member name *host-shared* :test #'string=) (host-alias name))
			  (psx:host-set! (concatenate 'string "racket:" name) value)))
		      table))
	   *primitive-instances*))

;;; ------------------------------------------------------------------
;;; What compiled linklets call (compiler.lisp)

(defun install-compiler-helpers ()
  (psx:defhost "racket:variable-ref" (v) (variable-ref v))
  (psx:defhost "racket:variable-set!" (v value) (variable-set v value))
  (psx:defhost "racket:variable-reference" (instance name)
    (make-variable-reference instance name))
  (psx:defhost "racket:unknown-primitive" (name)
    (racket-error (rname name) "undefined;~% cannot reference an identifier before its definition"))
  (psx:defhost "racket:assign-error" (name)
    (racket-error "set!" "assignment disallowed;~% cannot assign a primitive or imported variable: ~A" (rname name)))
  (psx:defhost "racket:arity-error" (args)
    (racket-error "#<procedure>" "arity mismatch;~% the expected number of arguments does not match the given number~%  given: ~D" (length args)))
  (psx:defhost "racket:note-split-function" (i f)
    (setf (svref *loading-split-functions* i) f))
  (psx:defhost "racket:call-with-mark" (key value thunk)
    (ps-r6rs::call-with-mark key value thunk))
  (psx::register-primitive-names
   '("racket:variable-ref" "racket:variable-set!" "racket:variable-reference"
     "racket:unknown-primitive" "racket:assign-error" "racket:arity-error")))

;;; ------------------------------------------------------------------
;;; Numbers and the like, where Racket's differ from the host's

;;; Racket's pairs are immutable; mutable pairs and boxes are their own types
(defstruct (rbox (:constructor make-rbox (&key value))) value)
(defstruct (mpair (:constructor make-mpair (&key car cdr))) car cdr)

(defnative "add1" (z) (:leaf t) (ps:scheme+ z 1))
(defnative "sub1" (z) (:leaf t) (ps:scheme- z 1))
(defnative "void" (&rest args) (:leaf t) (declare (ignore args)) *void*)
(defnative "void?" (x) (:leaf t) (bool (eq x *void*)))
(defnative "box" (x) (:leaf t) (make-rbox :value x))
(defnative "unbox" (b) (:leaf t) (rbox-value b))
(defnative "set-box!" (b x) (:leaf t) (setf (rbox-value b) x) *void*)
(defnative "box?" (x) (:leaf t) (bool (rbox-p x)))
(defnative "mcons" (a d) (:leaf t) (make-mpair :car a :cdr d))
(defnative "mcar" (p) (:leaf t) (mpair-car p))
(defnative "mcdr" (p) (:leaf t) (mpair-cdr p))
(defnative "mpair?" (x) (:leaf t) (bool (mpair-p x)))


