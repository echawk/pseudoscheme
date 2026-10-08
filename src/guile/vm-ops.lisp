; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-GUILE -*-

;;;; Guile's VM instructions: their table, and decoding them
;;;;
;;;; The table is Guile's own, as `(instruction-list)' returns it: each
;;;; entry is (name opcode kind word-format ...), where kind is ! (no
;;;; result) or <- (a result in the first operand), and each word format
;;;; names the fields of one 32-bit word, packed from the low bits.  The
;;;; first word's low 8 bits are the opcode (the X8 that starts its
;;;; format).  See the manual's "Instruction Set" (docs/guile-vm.md).

(in-package "PSEUDOSCHEME-GUILE")

(defstruct (vm-op (:constructor make-vm-op (name opcode kind formats)))
  name opcode kind formats
  (fields '()))				; the field kinds, one per operand, in order

(defvar *vm-ops* nil "Opcode -> vm-op, from Guile's instruction table.")
(defvar *vm-ops-by-name* (make-hash-table :test 'equal))

(defparameter *word-fields*
  '(("X32") ("X8_S24" :u24) ("X8_F24" :u24) ("X8_C24" :u24) ("X8_L24" :s24)
    ("X8_S12_S12" :u12 :u12) ("X8_F12_F12" :u12 :u12) ("X8_C12_C12" :u12 :u12)
    ("X8_S12_C12" :u12 :u12) ("X8_S12_Z12" :u12 :s12)
    ("X8_S8_S8_S8" :u8 :u8 :u8) ("X8_S8_S8_C8" :u8 :u8 :u8) ("X8_S8_C8_S8" :u8 :u8 :u8)
    ("X8_S8_ZI16" :u8 :s16) ("X8_S8_I16" :u8 :u16)
    ("C8_S24" :c8 :c24) ("C8_C24" :c8 :c24) ("C16_C16" :c16a :c16b)
    ("B1_X7_F24" :b1 :c24) ("B1_X7_C24" :b1 :c24)
    ("C32" :u32) ("I32" :u32) ("N32" :s32) ("R32" :s32) ("L32" :s32) ("LO32" :s32)
    ("A32" :u32) ("B32" :u32) ("AU32" :u32) ("BU32" :u32) ("AS32" :u32) ("BS32" :u32)
    ("AF32" :u32) ("BF32" :u32)
    ("V32_X8_L24" :table))
  "Each word format's fields, from the low bits.  A first word's format
begins with X8 (the opcode), which isn't a field.")

(defun word-format-fields (format)
  (let ((entry (assoc format *word-fields* :test #'string=)))
    (unless entry (error "Unknown Guile VM word format ~A" format))
    (cdr entry)))

(defun load-vm-ops ()
  "Fill *VM-OPS* from Guile's instruction table."
  (let ((ops (make-array 256 :initial-element nil)))
    (clrhash *vm-ops-by-name*)
    (dolist (entry (instruction-list))
      (destructuring-bind (name opcode kind &rest formats) entry
	(let* ((formats (mapcar #'ps:scheme-symbol-name formats))
	       (op (make-vm-op (ps:scheme-symbol-name name) opcode (ps:scheme-symbol-name kind) formats)))
	  (setf (vm-op-fields op) (mapcan (lambda (f) (copy-list (word-format-fields f))) formats))
	  (setf (svref ops opcode) op
		(gethash (vm-op-name op) *vm-ops-by-name*) op))))
    (setq *vm-ops* ops)))

(defun vm-op (opcode)
  (unless *vm-ops* (load-vm-ops))
  (svref *vm-ops* opcode))

;;; Code is an image's bytes, addressed by 32-bit word index.

(declaim (inline code-word))
(defun code-word (bytes index)
  "The little-endian 32-bit word at word INDEX of BYTES."
  (let ((at (* 4 index)))
    (logior (aref bytes at) (ash (aref bytes (+ at 1)) 8)
	    (ash (aref bytes (+ at 2)) 16) (ash (aref bytes (+ at 3)) 24))))

(defun sign-extend (n bits)
  (if (logbitp (1- bits) n) (- n (ash 1 bits)) n))

(defun decode-instruction (bytes ip)
  "The instruction at word IP of BYTES: its vm-op, its operands (a list,
in the order of its fields), and the IP after it."
  (let* ((first (code-word bytes ip))
	 (op (or (vm-op (ldb (byte 8 0) first))
		 (error "Unknown Guile VM opcode ~D at ~D" (ldb (byte 8 0) first) ip)))
	 (operands '())
	 (at ip))
    (dolist (format (vm-op-formats op))
      (let ((word (code-word bytes at))
	    (fields (word-format-fields format)))
	(incf at)
	(flet ((field (kind)
		 (ecase kind
		   (:u24 (ldb (byte 24 8) word))
		   (:s24 (sign-extend (ldb (byte 24 8) word) 24))
		   (:u12 nil) (:s12 nil) (:u8 nil) (:s16 nil) (:u16 nil)
		   (:c8 (ldb (byte 8 0) word))
		   (:c24 (ldb (byte 24 8) word))
		   (:c16a (ldb (byte 16 0) word))
		   (:c16b (ldb (byte 16 16) word))
		   (:b1 (ldb (byte 1 0) word))
		   (:u32 word)
		   (:s32 (sign-extend word 32)))))
	  ;; the sub-word fields of a first word follow its X8, packed
	  (let ((shift 8))
	    (dolist (kind fields)
	      (case kind
		((:u12 :s12) (let ((v (ldb (byte 12 shift) word)))
			       (push (if (eq kind :s12) (sign-extend v 12) v) operands)
			       (incf shift 12)))
		((:u8) (push (ldb (byte 8 shift) word) operands) (incf shift 8))
		((:s16 :u16) (let ((v (ldb (byte 16 shift) word)))
			       (push (if (eq kind :s16) (sign-extend v 16) v) operands)
			       (incf shift 16)))
		(:table
		 ;; a count, then that many X8_L24 words of offsets
		 (let ((n word))
		   (push n operands)
		   (push (loop repeat n
			       collect (prog1 (sign-extend (ldb (byte 24 8) (code-word bytes at)) 24)
					 (incf at)))
			 operands)))
		(t (push (field kind) operands))))))))
    (values op (nreverse operands) at)))

(defun disassemble-words (bytes start end)
  "The instructions from word START below END, as (ip name operand ...)."
  (loop with ip = start
	while (< ip end)
	collect (multiple-value-bind (op operands next) (decode-instruction bytes ip)
		  (prog1 (list* ip (vm-op-name op) operands)
		    (setq ip next)))))
