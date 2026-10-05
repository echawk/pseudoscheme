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

(proclaim '(inline truep true? scheme-symbol-p))

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

(proclaim '(inline booleanp char-whitespace-p char-numeric-p output-port-p))

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
;; *EQUAL-BUDGET* nodes it starts again in a mode that remembers which
;; pairs of objects it is already comparing and assumes those equal -- a
;; bisimulation check, which is what EQUAL? means for graphs.

(defvar *equal-budget* 100000)

(defun scheme-equal-p (obj1 obj2)
  (let ((budget *equal-budget*))
    (block bounded
      (labels ((walk (a b)
		 (when (minusp (decf budget)) (return-from bounded nil))
		 (equal-step a b #'walk)))
	(return-from scheme-equal-p (walk obj1 obj2))))
    (let ((seen (make-hash-table :test 'eq)))
      (labels ((walk (a b)
		 (if (and (or (consp a) (simple-vector-p a))
			  (member b (gethash a seen) :test #'eq))
		     t
		     (progn
		       (when (or (consp a) (simple-vector-p a)) (push b (gethash a seen)))
		       (equal-step a b #'walk)))))
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
