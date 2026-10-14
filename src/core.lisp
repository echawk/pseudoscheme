; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PS; -*-
; File core.lisp / Copyright (c) 1991 Jonathan Rees / See file COPYING

;;;; Pseudoscheme run-time system

(in-package "PS")

; The Scheme booleans
;   - must be self-evaluating
;   - must have invertible read/print syntax
;   - must be uniquely created
;   - can't be symbols without slowing down Scheme's SYMBOL? predicate
;   - similarly for numbers, pairs, etc.
; What values are self-evaluating Common Lisp objects with a read/print
; syntax that aren't used for anything in Scheme?  ...
; There aren't any.
; So, we use symbols, and slow down the SYMBOL? predicate.

(defparameter false 'false)  ;You can set this to 'nil if you want
(defparameter true  't)

(declaim (inline truep true? scheme-symbol-p))

; Convert Scheme boolean to Lisp boolean.
;  E.g. (cl:if (truep foo) ...)

(defun truep (scheme-test)
  (not (eq scheme-test false)))

; Convert Lisp boolean to Scheme boolean.
;  E.g. (cons (true? (cl:numberp x)) ...)
; This assumes that the argument is never the empty list.

(defun true? (cl-test) (or cl-test false))

(defun scheme-symbol-p (x)
  (declare (optimize (safety 0)))	;compilers are stupid
  (and (symbolp x) (not (eq (car (symbol-plist x)) 'not-a-symbol))))

(setf (get true  'not-a-symbol) t)
(setf (get false 'not-a-symbol) t)
(setf (get nil   'not-a-symbol) t)	;used for Scheme's empty list

;

(defparameter scheme-package (find-package "SCHEME"))

; Symbol case.  Scheme symbols are CL symbols in the SCHEME package,
; named by INVERTING case, as CL's :INVERT readtable case does: a name
; written all in lower case is the upper-case CL symbol (so `car' is
; SCHEME::CAR, which is what the translator's own sources -- read by the
; host's upcasing reader -- and every .pso file already use), one
; written all in upper case is the lower-case CL symbol, and a name of
; mixed case is kept as is.  Inversion is its own inverse, so this is a
; bijection: Scheme symbols are case-sensitive, as R6RS and R7RS
; require, with no change to how standard names are spelled in CL.
; R5RS-style case folding is a reader mode (*FOLD-CASE*, also set by
; R7RS's #!fold-case).

(defun invert-case (string)
  (let ((upper nil) (lower nil))
    (loop for c across string
	  do (cond ((upper-case-p c) (setq upper t))
		   ((lower-case-p c) (setq lower t))))
    (cond ((and upper lower) string)
	  (upper (string-downcase string))
	  (lower (string-upcase string))
	  (t string))))

(defun scheme-symbol-name (symbol)
  "The Scheme name of SYMBOL (SYMBOL->STRING)."
  (invert-case (symbol-name symbol)))

(defun intern-scheme-symbol (string)
  "The Scheme symbol named STRING (STRING->SYMBOL)."
  (values (intern (invert-case string) scheme-package)))

;;; Collecting symbols.  A package never lets go of a symbol, but the
;;; symbols Scheme code makes and drops -- gensyms, macro expanders'
;;; renamings, names read once -- are garbage in Chez and Guile, whose
;;; symbol tables are weak.  A symbol with no value, no function and no
;;; property list means only its name: if nothing refers to it, uninterning
;;; it can't be seen, as the next STRING->SYMBOL of that name makes an
;;; equivalent one.  So now and then SWEEP-SYMBOLS uninterns every such
;;; symbol, collects, and interns again those still referred to.

(defvar *symbol-sweep-threshold* 100000
  "How many symbols SCHEME may gain before MAYBE-SWEEP-SYMBOLS sweeps;
PSEUDOSCHEME_SYMBOL_SWEEP in the environment, if set (0 sweeps every
time: for testing).")

(defvar *symbols-after-sweep* nil "SCHEME's symbol count after the last sweep.")

(defun sweep-symbols (&optional (package scheme-package))
  "Unintern PACKAGE's symbols that are only names and that nothing refers
to.  The number uninterned."
  #+sbcl
  (let ((pointers '()))
    (flet ((only-a-name-p (s)
	     (and (eq (symbol-package s) package) (not (boundp s)) (null (sb-kernel:symbol-%info s)))))
      (let ((candidates '()))
	(do-symbols (s package)
	  (when (only-a-name-p s) (push s candidates)))
	(loop for s in candidates
	      do (unintern s package)
		 (push (sb-ext:make-weak-pointer s) pointers))
	(setq candidates nil))
      (sb-ext:gc :full t)
      (let ((count 0))
	(dolist (w pointers)
	  (let ((s (sb-ext:weak-pointer-value w)))
	    (if s (import s package) (incf count))))
	count)))
  #-sbcl (progn package 0))

(defun maybe-sweep-symbols ()
  "SWEEP-SYMBOLS, if SCHEME has gained *SYMBOL-SWEEP-THRESHOLD* symbols
since the last sweep.  For when a file or library is done."
  #+sbcl
  (let ((n (sb-int:package-internal-symbol-count scheme-package)))
    (cond ((null *symbols-after-sweep*) (setq *symbols-after-sweep* n))
	  ((> (- n *symbols-after-sweep*)
	      (or (ignore-errors (parse-integer (sb-ext:posix-getenv "PSEUDOSCHEME_SYMBOL_SWEEP")))
		  *symbol-sweep-threshold*))
	   (sweep-symbols)
	   (setq *symbols-after-sweep* (sb-int:package-internal-symbol-count scheme-package))))))

(defun list->bytevector (list)
  "For the reader's #u8(...) / #vu8(...)."
  (dolist (b list)
    (unless (typep b '(unsigned-byte 8))
      (scheme-error "bytevector literal: not a byte: ~S" b)))
  (make-array (length list) :element-type '(unsigned-byte 8) :initial-contents list))

;;; Chez Scheme's boxes, #&x (src/chez/), and fxvectors, #vfx(...), a
;;; vector of fixnums.  Chez's flvectors are SRFI 4's f64vectors here.

;;; Source annotations, as Chez Scheme has them: an annotation stands
;;; for its expression, from SOURCE (a source object: a source file
;;; descriptor and character positions), with STRIPPED the datum it
;;; stands for without annotations.  psyntax looks through them; the
;;; reader makes them while *ANNOTATING-SFD* is a descriptor.

(defstruct (annotation (:constructor make-annotation (expression source stripped options)))
  expression source stripped options)
(defstruct (sfd (:constructor make-sfd (path checksum))) path checksum)
(defstruct (source-object (:constructor make-source-object (sfd bfp efp &optional line column)))
  sfd bfp efp line column)
(defmethod make-load-form ((x annotation) &optional env) (make-load-form-saving-slots x :environment env))
(defmethod make-load-form ((x sfd) &optional env) (make-load-form-saving-slots x :environment env))
(defmethod make-load-form ((x source-object) &optional env) (make-load-form-saving-slots x :environment env))

(defvar *annotations-made* nil "Has an annotation been made?  (Else stripping is a no-op.)")
(defvar *annotating-sfd* nil "While the reader annotates: the descriptor of what it reads.")

;; Lisp booleans: read.pso tests them with (ps:true? ...), which the
;; translator compiles as a plain Lisp test
(defun annotating-p () (and *annotating-sfd* t))
(defun annotated-p (x) (annotation-p x))
(defun call-without-annotating (thunk) (let ((*annotating-sfd* nil)) (funcall thunk)))

(defun reader-position (port)
  (or (ignore-errors (file-position port)) 0))

(defun stripped (x)
  "X with the annotations at its top (in a list or vector the reader just
made of annotated parts) replaced by their stripped data."
  (cond ((annotation-p x) (annotation-stripped x))
	((consp x)
	 (let ((a (stripped (car x))) (d (stripped (cdr x))))
	   (if (and (eq a (car x)) (eq d (cdr x))) x (cons a d))))
	((simple-vector-p x) (if (some #'annotation-p x) (map 'simple-vector #'stripped x) x))
	(t x)))

(defun reader-annotate (datum port start)
  (setq *annotations-made* t)
  (make-annotation datum (make-source-object *annotating-sfd* start (reader-position port))
		   (stripped datum) :debug-profile))

(defun file-checksum (path)
  "Chez's source file descriptor checksum, here a hash of the file's bytes."
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((h 0))
      (loop for b = (read-byte in nil) while b
	    do (setq h (logand (+ (* h 31) b) #xffffffff)))
      h)))

(defstruct (box (:constructor make-box (contents)))
  contents)

(defun make-gensym-from-name (name)
  "For the reader's #{name unique}: an uninterned symbol."
  (make-symbol (invert-case name)))

;;; Chez Scheme's stencil vectors, #<mask>vs(...): a mask of up to 58
;;; bits and an element for each bit set, in order from the lowest.

(defstruct (stencil-vector (:constructor %make-stencil-vector (mask elements)))
  (mask 0 :type (integer 0)) (elements #() :type simple-vector))

(defconstant stencil-vector-mask-width 58)

(defun make-stencil-vector (mask elements)
  "For the reader's #<mask>vs(...) and Chez's stencil-vector."
  (unless (and (integerp mask) (<= 0 mask) (< mask (ash 1 stencil-vector-mask-width)))
    (scheme-error "stencil-vector: invalid mask ~S" mask))
  (unless (= (logcount mask) (length elements))
    (scheme-error "stencil-vector: mask ~S doesn't match ~S elements" mask (length elements)))
  (%make-stencil-vector mask (coerce elements 'simple-vector)))

(defun fxvector-p (x)
  (typep x '(simple-array fixnum (*))))

(defun list->fxvector (list)
  "For the reader's #vfx(...) and Chez's list->fxvector."
  (dolist (x list)
    (unless (typep x 'fixnum)
      (scheme-error "fxvector: not a fixnum: ~S" x)))
  (make-array (length list) :element-type 'fixnum :initial-contents list))

;;; Homogeneous numeric vectors (SRFI 4, SRFI 160) are CL specialized
;;; vectors: a u8vector is a bytevector, an s16vector a (simple-array
;;; (signed-byte 16) (*)), an f64vector a (simple-array double-float
;;; (*)).  The reader reads #s16(1 2 3) and the writer writes it.

(defparameter numeric-vector-types
  '(("u8" . (unsigned-byte 8)) ("s8" . (signed-byte 8))
    ("u16" . (unsigned-byte 16)) ("s16" . (signed-byte 16))
    ("u32" . (unsigned-byte 32)) ("s32" . (signed-byte 32))
    ("u64" . (unsigned-byte 64)) ("s64" . (signed-byte 64))
    ("f32" . single-float) ("f64" . double-float)
    ("c64" . (complex single-float)) ("c128" . (complex double-float)))
  "SRFI 4/160 tags and the element types of their vectors.")

(defun numeric-vector-element (type x)
  "X as an element of a vector of TYPE (from NUMERIC-VECTOR-TYPES), or
NIL if it can't be one: reals become floats in a float vector."
  (cond ((typep x type) x)
	((not (numberp x)) nil)
	((and (member type '(single-float double-float)) (realp x)) (coerce x type))
	((and (consp type) (eq (car type) 'complex))
	 (let ((part (cadr type)))
	   (complex (coerce (realpart x) part) (coerce (imagpart x) part))))))

(defun list->numeric-vector (tag list)
  "For the reader's #<tag>(...) and SRFI 4's list->s16vector etc."
  (let ((type (cdr (assoc tag numeric-vector-types :test #'string-equal))))
    (unless type (scheme-error "unknown homogeneous vector type ~A" tag))
    (make-array (length list) :element-type type
	        :initial-contents
		(mapcar (lambda (x)
			  (or (numeric-vector-element type x)
			      (scheme-error "#~A(...): not a valid element: ~S" tag x)))
			list))))

(defun numeric-vector-tag (x)
  "The SRFI 4 tag of X (\"u8\", \"f64\", ...) if it's a homogeneous
numeric vector, else NIL."
  (and (typep x '(simple-array * (*)))
       (not (stringp x))
       (not (simple-vector-p x))
       (car (find (array-element-type x) numeric-vector-types
		  :key (lambda (e) (upgraded-array-element-type (cdr e)))
		  :test #'equal))))

(defun numeric-vector-elements (x)
  "The elements of numeric vector X as Scheme numbers (inexact ones are
double floats)."
  (map 'list (lambda (e)
	       (typecase e
		 (single-float (coerce e 'double-float))
		 ((complex single-float) (coerce e '(complex double-float)))
		 (t e)))
       x))

(defvar *fold-case* nil
  "True when the Scheme reader folds symbols and character names to
lower case (R5RS behavior, or after #!fold-case).")

(defun intern-lisp-keyword (string)
  "The Lisp keyword that Scheme's #:STRING reads as (see
docs/interop.md): named by inverting case, like a Scheme symbol, so
#:test is :TEST."
  (values (intern (invert-case (if *fold-case* (string-downcase string) string))
		  "KEYWORD")))

; ----- Photons

; "A `photon' is an object that PRIN1's as if it had been PRINC'ed."
; 					  -- KMP

(defstruct (photon (:constructor make-photon (string-or-function))
		   (:copier nil)
		   (:print-function print-photon))
  string-or-function)

(defun print-photon (photon stream escape?)
  (declare (ignore escape?))
  (let ((z (photon-string-or-function photon)))
    (if (stringp z)
	(princ z stream)
	(funcall z stream))))

; Miscellaneous objects

(defvar unspecific (make-photon "#{Unspecific}"))
(defvar unassigned  (make-photon "#{Unassigned}"))

; A letrec variable's value where its init expressions refer to it,
; before it may have been assigned.
(declaim (inline letrec-value))
(defun letrec-value (value name)
  (if (eq value unassigned) (letrec-unassigned name) value))
(defun letrec-unassigned (name)
  (scheme-error "variable used before its initialization:" name))
(defvar eof-object  (make-photon "#{End-of-file}"))

; PROCEDURE?

(defparameter closures-might-be-conses-p
  (or (consp (eval '#'(lambda (x) x)))
      (consp (let ((g (gensym)))
	       (eval `(progn (defun ,g () 0) #',g))))
      (consp (compile nil '(lambda (x) x))) ;just for kicks
      (consp (funcall (compile nil '(lambda (x)
				      #'(lambda () (prog1 x (incf x)))))
		      0))))

(defun procedurep (obj)
  (and (functionp obj)
       (not (symbolp obj))
       (or (not (consp obj))
	   closures-might-be-conses-p)))

; Mumble

(declaim (inline booleanp char-whitespace-p char-numeric-p output-port-p))

(defun booleanp (obj)
  (or (eq obj true)
      (eq obj false)))

(defun char-whitespace-p (char)
  ;; Unicode's White_Space property.
  (let ((code (char-code char)))
    (or (<= 9 code 13) (= code 32) (= code #x85) (= code #xA0)
	(and (>= code #x1680)
	     (or (= code #x1680) (<= #x2000 code #x200A)
		 (= code #x2028) (= code #x2029) (= code #x202F)
		 (= code #x205F) (= code #x3000))))))

(defun char-numeric-p (char)
  ;; R7RS's char-numeric? is #t, not the digit's weight.  DIGIT-CHAR-P
  ;; recognizes the Unicode decimal digits (Nd) on SBCL.
  (and (digit-char-p char) t))

(defun input-port-p (obj)
  (and (streamp obj)
       (input-stream-p obj)
       t))

(defun output-port-p (obj)
  (and (streamp obj)
       (output-stream-p obj)
       t))

;; REALP is part of ANSI Common Lisp (CLtL II); only define it ourselves
;; on implementations that predate the standard.
#-ansi-cl
(defun realp (obj)
  (and (numberp obj)
       (not (complexp obj))))


; Auxiliary for SET!

(defun set!-aux (name value CL-sym)
  (case (get CL-sym 'defined)
    ((:assignable))
    ((:not-assignable)
     (cerror "Assign it anyhow"
	     "Variable ~S isn't supposed to be SET!"
	     (or name CL-sym)))
    ((nil)
     (warn "SET! of undefined variable ~S" (or name CL-sym))))
  (setf (symbol-value CL-sym) value)
  (if (procedurep value)
      (setf (symbol-function CL-sym) value)
      (fmakunbound CL-sym))
  unspecific)

; NEWLINE and WRITE-CHAR return no value worth printing, as in Scheme
(defun scheme-newline (&optional (port *standard-output*))
  (terpri port)
  unspecific)

(defun scheme-write-char (char &optional (port *standard-output*))
  (write-char char port)
  unspecific)

(defmacro at-top-level (&rest forms)
  `(progn ,@forms))

(defmacro maybe-fix-&rest-parameter (rest-var)
  (progn rest-var ;ignored
	 `nil))

(defvar *scheme-read*)
(defvar *scheme-write*)
(defvar *scheme-display*)
(defvar *scheme-write-shared*)
(defvar *scheme-write-simple*)

(defvar *define-syntax!*
  #'(lambda (name+exp) (declare (ignore name+exp)) 'define-syntax))

(defmacro %define-syntax! (name+exp)
  `(eval-when (load)
     (funcall *define-syntax!* ,name+exp)))



; These also appear in loadit.lisp
(defun filename-preferred-case (name)
  #+unix (string-downcase name)
  #-unix (string-upcase name)
  )
(defvar *translated-file-type* (filename-preferred-case "pso"))

; Prelude on all translated files

(defmacro begin-translated-file ()
  `(progn (eval-when (eval compile load)
	    (setq *readtable* cl-readtable))
	  (check-target-package)))

(defparameter cl-readtable (copy-readtable nil))

(defvar *target-package* nil)

(defun check-target-package ()
  (when (and *target-package*
	     (not (eq *target-package* *package*)))
    (warn "Translate-time package ~A differs from attempted load-time package ~A"
	  (package-name *package*)
	  (package-name *target-package*))))

; Auxiliaries for top-level DEFINE

(defun set-value-from-function (CL-sym &optional name) ;Follows a DEFUN
  (setf (symbol-value CL-sym) (symbol-function CL-sym))
  (after-define CL-sym name))

(defun really-set-function (CL-sym value)
  (cond ((procedurep value)
	 (setf (symbol-function CL-sym) value))
	(t
	 (fmakunbound CL-sym))))

(defun set-function-from-value (CL-sym &optional name) ;Follows a SETQ
  (let ((value (symbol-value CL-sym)))
    (really-set-function CL-sym value)
    (after-define CL-sym name)))

; Follows (SETQ *FOO* ...)

(defun set-forwarding-function (CL-sym &optional name)
  (setf (symbol-function CL-sym)
	#'(lambda (&rest args)
	    (apply (symbol-value CL-sym) args)))
  (after-define CL-sym name))

(defun after-define (CL-sym name)
  (setf (get CL-sym 'defined) t)
  (when name
    (make-photon #'(lambda (port)
		     (let ((*package* scheme-package))
		       (format port "~S defined." name))))))

; EQUAL?

; Differs from Common Lisp EQUAL in that it descends into vectors.
; This is here instead of in rts.lisp because it's an auxiliary for
; open-coding MEMBER and ASSOC, and the rule is that all auxiliaries
; are in the PS package (not REVISED^4-SCHEME).

;; EQUAL? must terminate even on circular structure (R6RS 11.5, R7RS
;; 6.1).  Plain recursion handles the usual case; once it has looked at
;; *EQUAL-BUDGET* nodes it starts again with union-find (Adams and
;; Dybvig, "Efficient nondestructive equality checking for trees and
;; graphs", ICFP 2008): two pairs or vectors already in the same class
;; are taken to be equal, and comparing two others first merges their
;; classes.  That is a bisimulation check, which is what EQUAL? means for
;; graphs, in time linear in their size, shared substructure included.

(defvar *equal-budget* 100000)

(defvar *bytevector-type* nil
  "NIL, or a function giving a bytevector's element type: bytevectors of
different types aren't EQUAL? (the Guile mode's SRFI 4 vectors are
bytevectors tagged with their type).")

(defvar *equal-extension* nil
  "NIL, or a function of two objects that EQUAL-STEP knows nothing of
and the function comparing their parts: true if they are EQUAL?.  The
Guile mode compares its structs with it.")

(defvar *record-equal-hook* nil
  "NIL, or a function of two structure objects and the function comparing
their parts: whether they are EQUAL?, or :PASS to leave them to
*EQUAL-EXTENSION*.  The Chez mode compares records with it, by the
procedures of record-type-equal-procedure.")

(defun structure-equal (obj1 obj2 recur)
  (let ((r (if *record-equal-hook* (funcall *record-equal-hook* obj1 obj2 recur) :pass)))
    (cond ((not (eq r :pass)) r)
	  (*equal-extension* (funcall *equal-extension* obj1 obj2 recur)))))

(defun scheme-equal-p (obj1 obj2)
  (let ((budget *equal-budget*))
    (declare (fixnum budget))
    (block bounded
      (labels ((walk (a b)
		 (when (minusp (decf budget)) (return-from bounded nil))
		 (cond ((eq a b) t)
		       ((consp a)
			(and (consp b) (walk (car a) (car b)) (walk (cdr a) (cdr b))))
		       (t (equal-step a b #'walk)))))
	(return-from scheme-equal-p (walk obj1 obj2))))
    (let ((parents (make-hash-table :test 'eq)))
      (labels ((class-root (x)
		 (let ((p (gethash x parents)))
		   (if (null p)
		       x
		       (let ((root (class-root p)))
			 (setf (gethash x parents) root)
			 root))))
	       (walk (a b)
		 (if (and (or (consp a) (simple-vector-p a) (typep a 'structure-object))
			  (or (consp b) (simple-vector-p b) (typep b 'structure-object)))
		     (let ((ra (class-root a)) (rb (class-root b)))
		       (or (eq ra rb)
			   (progn (setf (gethash ra parents) rb)
				  (equal-step a b #'walk))))
		     (equal-step a b #'walk))))
	(walk obj1 obj2)))))

(defun equal-step (obj1 obj2 recur)
  (cond ((eql obj1 obj2) t)
	;; boxes are equal? when their contents are, as in Chez and Racket
	((box-p obj1)
	 (and (box-p obj2) (funcall recur (box-contents obj1) (box-contents obj2))))
	;; the extension's objects are structure instances, and may be
	;; EQUAL? to vectors (a Guile array to the vector it views)
	((and (or *equal-extension* *record-equal-hook*)
	      (or (typep obj1 'structure-object) (typep obj2 'structure-object)))
	 (structure-equal obj1 obj2 recur))
        ((consp obj1)			;pair?
         (and (consp obj2)
	      (funcall recur (car obj1) (car obj2))
	      (funcall recur (cdr obj1) (cdr obj2))))
	((simple-string-p obj1)		;string?
	 (and (simple-string-p obj2)
	      (string= (the simple-string obj1)
		       (the simple-string obj2))))
	((simple-vector-p obj1)
	 (and (simple-vector-p obj2)
	      (let ((z (length (the simple-vector obj1))))
		(declare (fixnum z))
		(and (= z (length (the simple-vector obj2)))
		     (do ((i 0 (+ i 1)))
			 ((= i z) t)
		       (declare (fixnum i))
		       (when (not (funcall recur
					   (aref (the simple-vector obj1) i)
					   (aref (the simple-vector obj2) i)))
			 (return nil)))))))
	;; R7RS bytevectors
	((typep obj1 '(array (unsigned-byte 8) (*)))	; a slice is displaced
	 (and (typep obj2 '(array (unsigned-byte 8) (*)))
	      (equalp obj1 obj2)
	      (or (null *bytevector-type*)
		  (equal (funcall *bytevector-type* obj1) (funcall *bytevector-type* obj2)))))
	((bit-vector-p obj1)
	 (and (bit-vector-p obj2) (equal obj1 obj2)))
	;; SRFI 4 vectors: the same type, and elements EQV?
	((numeric-vector-tag obj1)
	 (and (typep obj2 '(simple-array * (*)))
	      (equal (array-element-type obj1) (array-element-type obj2))
	      (= (length obj1) (length obj2))
	      (every #'eql obj1 obj2)))
	(*equal-extension* (funcall *equal-extension* obj1 obj2 recur))
        (t nil)))


; Handy things.

; ERROR, WARN, SYNTAX-ERROR (nonstandard)

(defun scheme-error (message &rest irritants)
  (signal-scheme-condition #'error message irritants))

(defun scheme-warn (message &rest irritants)
  (signal-scheme-condition #'warn message irritants))

;;; The reader's errors are Lisp READER-ERRORs, which Scheme handlers see
;;; as &lexical conditions, so that R7RS's read-error? is true of them.

(define-condition scheme-reader-error (reader-error simple-condition) ()
  (:report (lambda (c stream)
	     (apply #'format stream (simple-condition-format-control c)
		    (simple-condition-format-arguments c)))))

(defun scheme-reading-error (port message &rest irritants)
  (signal-scheme-condition
   (lambda (control &rest arguments)
     (error 'scheme-reader-error :stream port
	    :format-control control :format-arguments arguments))
   message irritants))

;;; State the reader keeps across one datum, or for a port.

(defvar *port-fold-case*
  (trivial-garbage:make-weak-hash-table :weakness :key :test 'eq)
  "Ports from which a #!fold-case or #!no-fold-case directive was read,
and whether it was #!fold-case.  Other ports fold as *FOLD-CASE* says.")

(defvar *datum-labels* '()
  "The datum labels (R7RS 2.4) defined so far in the datum being read:
an alist from label numbers to placeholders.")

(defvar *chez-reader* nil
  "Does the reader read as Chez Scheme's does (pseudoscheme --chez)?
Then tokens that begin like numbers but aren't, such as 2.a, are
symbols, not errors, and #!r6rs restricts a port to R6RS syntax.")

(defun chez-reader-p () *chez-reader*)

; Chez Scheme's #!name data other than #!eof, as (name . object): the
; Chez mode's #!base-rtd (src/chez/chez.lisp)
(defvar *sharp-bang-data* '())

(defun sharp-bang-datum (name)
  "The object #!NAME reads as, or #f if it's a directive or a comment."
  (let ((entry (and *chez-reader* (assoc name *sharp-bang-data* :test #'string=))))
    (if entry (cdr entry) false)))


;;; Chez Scheme's record syntax, #[name field ...]: read by
;;; src/read.scm and written by src/write.scm in --chez mode, through
;;; these hooks, which src/chez/host.lisp sets (record-reader and
;;; record-writer).

(defvar *record-literal-hook* nil
  "NIL, or a function of #[...]'s elements, a list: the record.")

(defun read-record-literal (elements)
  (if *record-literal-hook*
      (funcall *record-literal-hook* elements)
      (scheme-error "#[...]: no record reader")))

(defvar *write-extension* nil
  "NIL, or a function of an object, a port and the writer of its parts:
true if it wrote the object.")

;;; A Lisp boolean: the writer (write.pso) tests it with OR, where an
;;; object it doesn't know is printed (PRIN1) if this is NIL.
(defun write-extension (obj port recur)
  (and *write-extension* (funcall *write-extension* obj port recur) t))

;;; Chez Scheme's #!r6rs: after it, a port's syntax must be R6RS's
;;; (src/read.scm), in --chez mode; #!chezscheme lifts that.

(defvar *port-r6rs* (trivial-garbage:make-weak-hash-table :weakness :key :test 'eq))

(defun set-port-r6rs (port r6rs)
  (when *chez-reader*
    (setf (gethash port *port-r6rs*) r6rs)))

(defun port-r6rs-p (port) (values (gethash port *port-r6rs*)))

(defvar *char-category-function* nil
  "A function from a character to its Unicode general category, a string
such as \"Lu\" (src/r6rs/enums.lisp sets it).")

(defun r6rs-identifier-p (string)
  "Is STRING an R6RS identifier (R6RS 4.2.4), escapes aside?"
  (flet ((category (c) (and *char-category-function* (funcall *char-category-function* c)))
	 (subsequent-ascii-p (c) (or (alphanumericp c) (find c "!$%&*/:<=>?^_~+-.@"))))
    (flet ((initial-p (c)
	     (if (< (char-code c) 128)
		 (or (alpha-char-p c) (find c "!$%&*/:<=>?^_~"))
		 (member (category c) '("Lu" "Ll" "Lt" "Lm" "Lo" "Mn" "Nl" "No" "Pd" "Pc"
					"Po" "Sc" "Sm" "Sk" "So" "Co")
			 :test #'equal)))
	   (subsequent-p (c)
	     (if (< (char-code c) 128)
		 (subsequent-ascii-p c)
		 (member (category c) '("Lu" "Ll" "Lt" "Lm" "Lo" "Mn" "Nl" "No" "Pd" "Pc" "Po"
					"Sc" "Sm" "Sk" "So" "Co" "Nd" "Mc" "Me")
			 :test #'equal))))
      (or (member string '("+" "-" "...") :test #'string=)
	  (and (> (length string) 0)
	       (or (initial-p (char string 0))
		   (and (> (length string) 1) (string= string "->" :end1 2)))
	       (every #'subsequent-p (subseq string 1)))))))

(defvar *forward-labels* nil
  "Was a datum label referred to before its definition, as Chez Scheme
allows anywhere in the datum being read?")

(defstruct (label-placeholder (:constructor make-label-placeholder ()))
  (datum nil) (defined nil))

(defvar *port-keywords*
  (trivial-garbage:make-weak-hash-table :weakness :key :test 'eq)
  "Ports from which a #!srfi-88 directive was read: foo: is a keyword.")

(defvar *keywords* nil "Is foo: a keyword (SRFI 88) here?")

(defvar *neoteric* nil
  "Are neoteric expressions read here (SRFI 105: within curly braces)?")

;;; Alternative readers (SRFIs 49, 110, 119): after a directive such as
;;; #!sweet, a port is read by a Scheme procedure of the port, which
;;; *READER-DIRECTIVES* gives for the directive's name.  It reads with
;;; the ordinary reader itself, which then reads as usual.

(defvar *port-readers*
  (trivial-garbage:make-weak-hash-table :weakness :key :test 'eq)
  "Ports read by an alternative reader, and the reader.")

(defvar *reader-directives* '()
  "(name . function): FUNCTION, of no arguments, returns the reader
directive #!name gives a port.  (src/r7rs/front.lisp)")

(defvar *in-alternative-reader* nil)

(defun reader-directive (port name)
  "For #!NAME read from PORT: the alternative reader it selects, made
PORT's; else #f."
  (let ((entry (assoc name *reader-directives* :test #'string-equal)))
    (if entry
	(setf (gethash port *port-readers*) (funcall (cdr entry)))
	false)))

(defun call-alternative-reader (reader port)
  (let ((*in-alternative-reader* t))
    (funcall reader port)))

(defun call-with-reader-state (port thunk)
  "Read one datum from PORT by calling THUNK: case folding and keywords as
PORT's directives left them, no datum labels yet, and outside curly
braces; or with PORT's alternative reader, if it has one."
  (let ((reader (and (not *in-alternative-reader*) (gethash port *port-readers*))))
    (if reader
	(call-alternative-reader reader port)
	(call-with-reader-state-1 port thunk))))

(defun call-with-reader-state-1 (port thunk)
  (let ((*fold-case* (multiple-value-bind (fold found) (gethash port *port-fold-case*)
		       (if found fold *fold-case*)))
	(*keywords* (values (gethash port *port-keywords*)))
	(*neoteric* nil)
	(*datum-labels* '())
	(*forward-labels* nil))
    (let ((datum (funcall thunk)))
      (if *forward-labels*
	  (resolve-forward-labels port datum)
	  datum))))

(defun set-port-fold-case (port fold)
  (setf (gethash port *port-fold-case*) fold
	*fold-case* fold))

(defun set-port-keywords (port keywords)
  (setf (gethash port *port-keywords*) keywords
	*keywords* keywords))

(defun keywords-p () (if *keywords* t false))

(defun neoteric-p () (if *neoteric* t false))

(defun call-neoterically (thunk)
  (let ((*neoteric* t)) (funcall thunk)))

;;; SRFI 88's keyword objects: interned by name, distinct from symbols
;;; (and from the Lisp keywords #:name reads as).  A compiled constant
;;; is the same object when loaded.

(defstruct (keyword-object (:constructor %make-keyword-object (name)))
  (name "" :type string :read-only t))

(defvar *keyword-objects* (trivial-garbage:make-weak-hash-table :weakness :value :test 'equal))

(defun intern-keyword-object (name)
  (or (gethash name *keyword-objects*)
      (setf (gethash name *keyword-objects*) (%make-keyword-object (copy-seq name)))))

(defmethod make-load-form ((k keyword-object) &optional environment)
  (declare (ignore environment))
  `(intern-keyword-object ,(keyword-object-name k)))

(defmethod print-object ((k keyword-object) stream)
  (if *print-escape*
      (format stream "~A:" (keyword-object-name k))
      (write-string (keyword-object-name k) stream)))

;;; Array literals (SRFI 163, src/read.scm): made by a procedure of the
;;; rank, the tag (a string), the bounds ((lower . length-or-nil) ...)
;;; and the datum, which *ARRAY-LITERAL-LOADER* returns (src/r7rs/front.lisp:
;;; (srfi 163)'s) the first time one is read.

(defvar *array-literal-constructor* nil)
(defvar *array-literal-loader* nil)

(defun make-array-literal (rank tag bounds datum)
  (unless *array-literal-constructor*
    (unless *array-literal-loader*
      (scheme-reading-error nil "array literals aren't available here"))
    (setq *array-literal-constructor* (funcall *array-literal-loader*)))
  (funcall *array-literal-constructor* rank tag bounds datum))

;;; SRFI 10's #,(tag datum ...): the constructors define-reader-ctor
;;; registers, by tag.  A #,(tag ...) whose tag has none is R6RS's
;;; unsyntax.

(defvar *reader-ctors* (make-hash-table :test 'eq))

(defun reader-ctor (tag) (gethash tag *reader-ctors* false))

(defun define-reader-ctor (tag procedure)
  (setf (gethash tag *reader-ctors*) procedure)
  unspecific)

(defun read-labelled-datum (port n thunk)
  "#N=<datum>: the datum THUNK reads, in which #N# refers to itself."
  (let ((placeholder (let ((old (cdr (assoc n *datum-labels*))))
		       (if (and old (not (label-placeholder-defined old)))
			   old
			   (make-label-placeholder)))))
    (push (cons n placeholder) *datum-labels*)
    (let ((datum (funcall thunk)))
      (when (eq datum placeholder)
	(scheme-reading-error port "datum label refers only to itself" n))
      (setf (label-placeholder-datum placeholder) datum
	    (label-placeholder-defined placeholder) t)
      (replace-placeholder datum placeholder)
      datum)))

(defun datum-label-reference (port n)
  "#N#: the datum labelled N, or its placeholder while it is being read."
  (let ((placeholder (cdr (assoc n *datum-labels*))))
    (cond ((null placeholder)
	   ;; Chez Scheme's forward reference: resolved once the datum is read
	   (let ((placeholder (make-label-placeholder)))
	     (setq *forward-labels* t)
	     (push (cons n placeholder) *datum-labels*)
	     placeholder))
	  ((label-placeholder-defined placeholder)
	   (label-placeholder-datum placeholder))
	  (t placeholder))))

(defun resolve-forward-labels (port datum)
  "DATUM, its labels' placeholders replaced by what they label."
  (dolist (entry *datum-labels*)
    (unless (label-placeholder-defined (cdr entry))
      (scheme-reading-error port "undefined datum label" (car entry))))
  (flet ((value (x) (if (label-placeholder-p x) (label-placeholder-datum x) x)))
    (let ((seen (make-hash-table :test 'eq)))
      (labels ((walk (x)
		 ;; a record's parts are its fields' own vector
		 (let ((x (or (datum-label-parts x) x)))
		 (when (and (or (consp x) (simple-vector-p x))
			    (not (gethash x seen)))
		   (setf (gethash x seen) t)
		   (if (consp x)
		       (progn (setf (car x) (value (car x))) (walk (car x))
			      (setf (cdr x) (value (cdr x))) (walk (cdr x)))
		       (dotimes (i (length x))
			 (setf (svref x i) (value (svref x i)))
			 (walk (svref x i))))))))
	(let ((datum (value datum)))
	  (walk datum)
	  datum)))))

(defun replace-placeholder (datum placeholder)
  "Replace PLACEHOLDER by the datum it stands for, in pairs and vectors
(and the Chez mode's records)."
  (let ((seen (make-hash-table :test 'eq))
	(value (label-placeholder-datum placeholder)))
    (labels ((walk (x)
	       ;; a record's parts are its fields' own vector
	       (let ((x (or (datum-label-parts x) x)))
	       (when (and (or (consp x) (simple-vector-p x))
			  (not (gethash x seen)))
		 (setf (gethash x seen) t)
		 (if (consp x)
		     (progn
		       (if (eq (car x) placeholder) (setf (car x) value) (walk (car x)))
		       (if (eq (cdr x) placeholder) (setf (cdr x) value) (walk (cdr x))))
		     (dotimes (i (length x))
		       (if (eq (svref x i) placeholder)
			   (setf (svref x i) value)
			   (walk (svref x i)))))))))
      (walk datum))))

(defun signal-scheme-condition (fun message irritants)
  (if (or (not (stringp message))
	  (find #\~ message))
      (apply fun message irritants)
      (apply fun
	     (apply #'concatenate
		    'string
		    (if (stringp message) "~a" "~s")
		    (mapcar #'(lambda (irritant)
				(declare (ignore irritant))
				"~%  ~s")
			    irritants))
	     message
	     irritants)))

;;; Datum labels for the writer (R7RS 6.13.3): write and display label
;;; the pairs and vectors on a cycle, write-shared every one reached more
;;; than once, write-simple none.

(defvar *write-labels* nil
  "While writing, a table from the objects that need labels to their
label numbers (NIL until written), or NIL if none do.")
(defvar *write-label-count* 0)

(defvar *datum-label-parts* nil
  "NIL, or a function giving the parts, a simple vector, of an object
other than a pair or vector that may be on a cycle (the Chez mode's
records), or NIL.")

(defun datum-label-parts (x)
  (and *datum-label-parts* (not (consp x)) (not (simple-vector-p x))
       (funcall *datum-label-parts* x)))

(defun datum-labels (object shared)
  "A table of the pairs and vectors in OBJECT that need labels: those
reached more than once if SHARED, else those on a cycle.  NIL if none."
  (let ((state (make-hash-table :test 'eq))
	(labels nil))
    (labels ((need (x)
	       (unless labels (setq labels (make-hash-table :test 'eq)))
	       (setf (gethash x labels) nil))
	     (visit (x)
	       ;; True if X is new and should be walked.
	       (case (gethash x state)
		 ((nil) (setf (gethash x state) :active) t)
		 (:active (need x) nil)
		 (t (when shared (need x)) nil)))
	     (walk (x)
	       ;; Down the cdrs by iteration, so long lists don't recurse.
	       (let ((spine '()))
		 (loop
		   (cond ((consp x)
			  (unless (visit x) (return))
			  (push x spine)
			  (walk (car x))
			  (setq x (cdr x)))
			 ((and (simple-vector-p x) (plusp (length x)))
			  (when (visit x)
			    (loop for e across x do (walk e))
			    (setf (gethash x state) :done))
			  (return))
			 (t (let ((parts (datum-label-parts x)))
			      (when (and parts (visit x))
				(loop for e across parts do (walk e))
				(setf (gethash x state) :done)))
			    (return))))
		 (dolist (p spine) (setf (gethash p state) :done)))))
      (walk object))
    labels))

(defun call-with-write-labels (object mode thunk)
  "Write OBJECT by calling THUNK, labelling as MODE says: 0, nothing; 1,
what is on a cycle; 2, whatever is shared."
  (let ((*write-labels* (and (or (consp object) (simple-vector-p object) (datum-label-parts object))
			     (/= mode 0)
			     (datum-labels object (= mode 2))))
	(*write-label-count* 0))
    (funcall thunk)))

(defun write-labelled-p (object)
  (and *write-labels* (nth-value 1 (gethash object *write-labels*))))

(defun write-label-reference (object)
  "OBJECT's label number if it has been written already, else #f."
  (or (and *write-labels* (gethash object *write-labels*)) false))

(defun write-label-definition (object)
  "A new label number for OBJECT if it needs one and has none, else #f."
  (if (and (write-labelled-p object) (null (gethash object *write-labels*)))
      (prog1 (setf (gethash object *write-labels*) *write-label-count*)
	(incf *write-label-count*))
      false))

;;; Symbols that wouldn't read back as themselves are written |like this|.

(defun symbol-needs-bars-p (name)
  (or (zerop (length name))
      (string= name ".")
      (find-if (lambda (c)
		 (or (member c '(#\( #\) #\[ #\] #\" #\; #\' #\` #\, #\| #\\))
		     (char-whitespace-p c)
		     (not (graphic-char-p c))))
	       name)
      (char= (char name 0) #\#)
      (digit-char-p (char name 0))
      ;; +5, -.5, +i, +inf.0 ...
      (and (member (char name 0) '(#\+ #\- #\.))
	   (> (length name) 1)
	   (let ((c (char name 1)))
	     (or (digit-char-p c)
		 (and (char= c #\.) (> (length name) 2) (digit-char-p (char name 2)))
		 (member (string-downcase name) '("+i" "-i" "+inf.0" "-inf.0" "+nan.0" "-nan.0")
			 :test #'string=))))))

; PP (nonstandard)

(defun pp (obj &optional (port *standard-output*))
  (let ((*print-pretty* t)
	(*print-length* nil)
	(*print-level* nil))
    (format port "~&")
    (print obj port)
    (values)))

; CALL-WITH-CURRENT-CONTINUATION: escaping only.  CATCH/THROW with a fresh
; tag rather than BLOCK/RETURN-FROM: calling the escape procedure once its
; extent has ended then signals a CONTROL-ERROR (CL requires THROW to),
; where RETURN-FROM to a dead block is undefined and can corrupt the image.

(defun call-with-escape (proc)
  (let ((tag (list 'continuation)))
    (catch tag
      (funcall proc (lambda (&rest vals)
		      (if (and vals (eq (car vals) :pseudoscheme-continuation-query))
			  (cons tag nil)	; its identity (src/continuations.lisp)
			  (throw tag (values-list vals))))))))
