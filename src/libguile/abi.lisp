; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-LIBGUILE -*-

;;;; GNU Guile 3.0.11's ABI facts, as tests/libguile/probe.c measured
;;;; them on a real Guile build (aarch64-linux and any 64-bit target
;;;; Guile supports: the layouts don't depend on the machine beyond the
;;;; word size).  tests/libguile/run.sh runs the probe again and
;;;; compares.  Words are 64 bits.

(in-package "QUASISCHEME-LIBGUILE")

;;; Immediates
(defconstant +false+ #x4)
(defconstant +elisp-nil+ #x104)
(defconstant +null+ #x304)
(defconstant +true+ #x404)
(defconstant +unspecified+ #x804)
(defconstant +undefined+ #x904)
(defconstant +eof+ #xa04)
;; a fixnum n is n<<2 | 2, 62 bits; a character c is c<<8 | #x0c
(defconstant +fixnum-tag+ 2)
(defconstant +most-positive-fixnum+ #x1fffffffffffffff)
(defconstant +char-tag+ #x0c)

;;; The first word of each kind of heap object (its type tag), and
;;; where its other words are.  A pair's first word is its car, an SCM,
;;; whose low bit is clear; every other object's first word has the low
;;; bit set.  A struct's first word is its vtable's address + 1.
(defconstant +tc7-symbol+ #x05)
(defconstant +tc7-variable+ #x07)	; word 1: the value
(defconstant +tc7-vector+ #x0d)		; length << 8; words 1..n: the elements
(defconstant +tc7-string+ #x15)		; words: stringbuf, start, length
(defconstant +tc7-number+ #x17)
(defconstant +tc16-bignum+ #x117)
(defconstant +tc16-flonum+ #x217)	; word 1: the double
(defconstant +tc16-complex+ #x317)	; words 1, 2: the real and imaginary doubles
(defconstant +tc16-fraction+ #x417)	; words 1, 2: numerator, denominator
(defconstant +tc7-hashtable+ #x1d)
(defconstant +tc7-pointer+ #x1f)	; word 1: the address
(defconstant +tc7-fluid+ #x25)
(defconstant +tc7-keyword+ #x35)
(defconstant +tc7-program+ #x45)
(defconstant +tc7-bytevector+ #x4d)	; words: length, contents pointer, parent
(defconstant +bytevector-u8-flags+ #x8100) ; what a plain bytevector's first word adds
(defconstant +tc7-smob+ #x77)		; tag | smob number << 8 | flags << 16
(defconstant +tc7-port+ #x7d)
(defconstant +tc3-struct+ 1)

;;; The smob descriptor table, scm_smobs: 256 descriptors of 64 bytes;
;;; inline scm_new_smob reads mark and free
(defconstant +smob-descriptor-size+ 64)
(defconstant +max-smobs+ 256)
(defparameter *smob-descriptor-offsets*
  '(:name 0 :size 8 :mark 16 :free 24 :print 32 :equalp 40 :apply 48))

;;; Structures callers allocate for our functions to fill
(defconstant +sizeof-array-handle+ 104)
(defconstant +sizeof-c-hook+ 24)

;;; Flags
(defconstant +wind-explicitly+ #x10) ; SCM_F_WIND_EXPLICITLY, SCM_F_DYNWIND_REWINDABLE
