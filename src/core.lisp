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

(defun list->bytevector (list)
  "For the reader's #u8(...) / #vu8(...)."
  (dolist (b list)
    (unless (typep b '(unsigned-byte 8))
      (scheme-error "bytevector literal: not a byte: ~S" b)))
  (make-array (length list) :element-type '(unsigned-byte 8) :initial-contents list))

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
		 (if (and (or (consp a) (simple-vector-p a)) (or (consp b) (simple-vector-p b)))
		     (let ((ra (class-root a)) (rb (class-root b)))
		       (or (eq ra rb)
			   (progn (setf (gethash ra parents) rb)
				  (equal-step a b #'walk))))
		     (equal-step a b #'walk))))
	(walk obj1 obj2)))))

(defun equal-step (obj1 obj2 recur)
  (cond ((eql obj1 obj2) t)
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
	((typep obj1 '(simple-array (unsigned-byte 8) (*)))
	 (and (typep obj2 '(simple-array (unsigned-byte 8) (*)))
	      (equalp obj1 obj2)))
	;; SRFI 4 vectors: the same type, and elements EQV?
	((numeric-vector-tag obj1)
	 (and (typep obj2 '(simple-array * (*)))
	      (equal (array-element-type obj1) (array-element-type obj2))
	      (= (length obj1) (length obj2))
	      (every #'eql obj1 obj2)))
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

(defstruct (label-placeholder (:constructor make-label-placeholder ()))
  (datum nil) (defined nil))

(defvar *port-keywords*
  (trivial-garbage:make-weak-hash-table :weakness :key :test 'eq)
  "Ports from which a #!srfi-88 directive was read: foo: is a keyword.")

(defvar *keywords* nil "Is foo: a keyword (SRFI 88) here?")

(defvar *neoteric* nil
  "Are neoteric expressions read here (SRFI 105: within curly braces)?")

(defun call-with-reader-state (port thunk)
  "Read one datum from PORT by calling THUNK: case folding and keywords as
PORT's directives left them, no datum labels yet, and outside curly
braces."
  (let ((*fold-case* (multiple-value-bind (fold found) (gethash port *port-fold-case*)
		       (if found fold *fold-case*)))
	(*keywords* (values (gethash port *port-keywords*)))
	(*neoteric* nil)
	(*datum-labels* '()))
    (funcall thunk)))

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
  (let ((placeholder (make-label-placeholder)))
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
	   (scheme-reading-error port "undefined datum label" n))
	  ((label-placeholder-defined placeholder)
	   (label-placeholder-datum placeholder))
	  (t placeholder))))

(defun replace-placeholder (datum placeholder)
  "Replace PLACEHOLDER by the datum it stands for, in pairs and vectors."
  (let ((seen (make-hash-table :test 'eq))
	(value (label-placeholder-datum placeholder)))
    (labels ((walk (x)
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
			   (walk (svref x i))))))))
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
			 (t (return))))
		 (dolist (p spine) (setf (gethash p state) :done)))))
      (walk object))
    labels))

(defun call-with-write-labels (object mode thunk)
  "Write OBJECT by calling THUNK, labelling as MODE says: 0, nothing; 1,
what is on a cycle; 2, whatever is shared."
  (let ((*write-labels* (and (or (consp object) (simple-vector-p object))
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
      (funcall proc (lambda (&rest vals) (throw tag (values-list vals)))))))
