; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-GUILE -*-

;;;; The C half of GOOPS (scm_init_goops_builtins), under Guile's own
;;;; oop/goops.scm.  GOOPS 3.0 is Scheme on structs: a class is a vtable,
;;;; an instance a struct.  What C supplies is class-of for every kind of
;;;; object, the root of the class hierarchy (%make-vtable-vtable), a
;;;; class's layout once its slots are known (%init-layout!), and the
;;;; in-place swap behind change-class (%modify-instance).
;;;;
;;;; Primitive generics: a primitive that a method is added to (+, or
;;;; write) dispatches to its generic, through its root variable, when it
;;;; is called on arguments it doesn't take.  See ENABLE-PRIMITIVE-GENERIC.

(in-package "PSEUDOSCHEME-GUILE")

(defconstant +vtable-flag-validated+ 1)
(defconstant +vtable-flag-goops-class+ 512)
(defconstant +vtable-flag-goops-slot+ 1024)
(defconstant +vtable-flag-goops-static-slot-allocation+ 2048)
(defconstant +vtable-flag-goops-indirect+ 4096)
(defconstant +vtable-flag-goops-needs-migration+ 8192)

(defvar *goops-module* nil "(oop goops), once its C half is loaded.")
(defvar *goops-classes* (make-hash-table :test 'equal)
  "The classes %goops-early-init looks up, by name: <integer>, <pair>, ...")

(defun goops-value (name)
  "The value of NAME in (oop goops)."
  (let ((v (module-variable* *goops-module* (ssym name))))
    (and v (not (eq (gvariable-value v) +unbound+)) (gvariable-value v))))

(defun goops-class (name)
  (or (gethash name *goops-classes*) (gethash "<unknown>" *goops-classes*)))

(defun goops-ready-p () (plusp (hash-table-count *goops-classes*)))

;;; ------------------------------------------------------------------
;;; class-of

(defun class-for-vtable (vtable)
  "The class of instances of a vtable that isn't a GOOPS class (a record
type, say): made the first time, as scm_i_define_class_for_vtable does."
  (or (gethash vtable *vtables*)
      (let* ((name (svref (gstruct-slots vtable) +vtable-index-name+))
	     (flags (vtable-flags vtable))
	     (class-name (ssym (if (and (symbolp name) name (not (eq name ps:false)))
				   (format nil "<~A>" (ps:scheme-symbol-name name))
				   "<>"))))
	(multiple-value-bind (meta super)
	    (cond ((logtest flags +vtable-flag-setter+)
		   (values "<applicable-struct-with-setter-class>" "<applicable-struct-with-setter>"))
		  ((logtest flags +vtable-flag-applicable+)
		   (values "<applicable-struct-class>" "<applicable-struct>"))
		  (t (values "<class>" "<top>")))
	  (setf (gethash vtable *vtables*)
		(funcall (goops-value "make-standard-class")
			 (goops-class meta) class-name (list (goops-class super)) '()))))))

(defun port-class (stream)
  (goops-class (cond ((and (input-stream-p stream) (output-stream-p stream)) "<file-input-output-port>")
		     ((input-stream-p stream) "<file-input-port>")
		     (t "<file-output-port>"))))

(defun struct-class (x)
  (let* ((vtable (struct-vtable-of x))
	 (flags (vtable-flags vtable))
	 (direct +vtable-flag-goops-class+)
	 (indirect (logior direct +vtable-flag-goops-indirect+)))
    (cond ((= (logand flags indirect) direct) vtable)
	  ((= (logand flags indirect) indirect)
	   ;; the instance's slots are in its last field
	   (let ((slots (svref (struct-slots-of x) (1- (vtable-nfields vtable)))))
	     (if (logtest (vtable-flags (struct-vtable-of slots)) +vtable-flag-goops-needs-migration+)
		 (funcall (goops-value "class-of-obsolete-indirect-instance") x)
		 vtable)))
	  (t (class-for-vtable vtable)))))

(defparameter *smob-classes*
  '(("<promise>" . gpromise-p) ("<thread>" . sb-thread::thread-p)
    ("<mutex>" . gmutex-p) ("<condition-variable>" . gcondvar-p)
    ("<regexp>" . regexp-p) ("<hook>" . hook-p) ("<random-state>" . random-state-p)
    ("<directory>" . directory-stream-p) ("<macro>" . gmacro-p) ("<character-set>" . nil)
    ("<guardian>" . nil))
  "libguile's smob types' classes, made at %goops-early-init (<guardian>
applicable), and what is an instance of each here.")

(defun smob-class (x)
  (loop for (name . predicate) in *smob-classes*
	when (and predicate (funcall predicate x)) return (goops-class name)))

(defun goops-class-of (x)
  (cond ((struct-p x) (struct-class x))
	((null x) (goops-class "<null>"))
	((consp x) (goops-class "<pair>"))
	((or (eq x ps:true) (eq x ps:false)) (goops-class "<boolean>"))
	((characterp x) (goops-class "<char>"))
	((integerp x) (goops-class "<integer>"))
	((typep x 'ratio) (goops-class "<fraction>"))
	((floatp x) (goops-class "<real>"))
	((complexp x) (goops-class "<complex>"))
	((stringp x) (goops-class "<string>"))
	((keywordp x) (goops-class "<keyword>"))
	((and (symbolp x) (not (ps::photon-p x))) (goops-class "<symbol>"))
	((simple-vector-p x) (goops-class "<vector>"))
	((typep x 'ps-r6rs::octets) (goops-class "<bytevector>"))
	((bit-vector-p x) (goops-class "<bitvector>"))
	((and (vectorp x) (ps::numeric-vector-tag x)) (goops-class "<uvec>"))
	((arrayp x) (goops-class "<array>"))
	((ghash-p x) (goops-class "<hashtable>"))
	((fluid-p x) (goops-class "<fluid>"))
	((gpointer-p x) (goops-class "<foreign>"))
	((syntax-object-p x) (goops-class "<syntax>"))
	((streamp x) (port-class x))
	((smob-class x))
	((functionp x)
	 (if (primitive-generic-of x)
	     (goops-class "<primitive-generic>")
	     (goops-class "<procedure>")))
	(t (goops-class "<unknown>"))))

;;; ------------------------------------------------------------------
;;; Classes as vtables

(defun make-vtable-vtable (layout)
  "A vtable of LAYOUT (a string) that is its own vtable: GOOPS's <class>."
  (let* ((layout (layout-symbol (layout-string layout)))
	 (string (ps:scheme-symbol-name layout)))
    (multiple-value-bind (n hidden) (parse-layout layout)
      (let* ((slots (make-array n))
	     (v (%make-vtable nil slots)))
	(dotimes (i n) (setf (svref slots i) (field-default string i)))
	(setf (gstruct-vtable v) v
	      (svref slots +vtable-index-layout+) layout
	      (svref slots +vtable-index-printer+) ps:false
	      (svref slots +vtable-index-name+) ps:false
	      (svref slots +vtable-index-size+) n
	      (vtable-nfields v) n
	      (vtable-hidden v) hidden)
	(set-vtable-flags v (logior +vtable-flag-vtable+ +vtable-flag-validated+))
	v))))

(defun set-vtable-flags (v flags)
  (setf (vtable-flags v) flags
	(svref (gstruct-slots v) +vtable-index-flags+) flags))

(defun init-layout (class layout)
  "Give CLASS, made before its slots were known, its LAYOUT."
  (unless (vtable-p class) (wrong-type "%init-layout!" 1 class))
  (let ((layout (layout-symbol (layout-string layout))))
    (multiple-value-bind (n hidden) (parse-layout layout)
      (setf (svref (gstruct-slots class) +vtable-index-layout+) layout
	    (svref (gstruct-slots class) +vtable-index-size+) n
	    (vtable-nfields class) n
	    (vtable-hidden class) hidden)
      (set-vtable-flags class (logior (vtable-flags class)
				      (inherited-vtable-flags (struct-vtable-of class) layout)
				      +vtable-flag-goops-class+))))
  *unspecified*)

(defun clear-fields (obj unbound)
  (unless (struct-p obj) (wrong-type "%clear-fields!" 1 obj))
  (let* ((slots (struct-slots-of obj))
	 (layout (svref (gstruct-slots (struct-vtable-of obj)) +vtable-index-layout+))
	 (string (and (symbolp layout) (ps:scheme-symbol-name layout))))
    (dotimes (i (length slots))
      (unless (and string (< (1+ (* 2 i)) (length string)) (char= (char string (* 2 i)) #\u))
	(setf (svref slots i) unbound))))
  *unspecified*)

(defun modify-instance (old new)
  "Swap OLD's and NEW's classes and fields: change-class's last step."
  (cond ((and (vtable-p old) (vtable-p new))
	 (rotatef (gstruct-vtable old) (gstruct-vtable new))
	 (rotatef (gstruct-slots old) (gstruct-slots new))
	 (rotatef (vtable-nfields old) (vtable-nfields new))
	 (rotatef (vtable-flags old) (vtable-flags new))
	 (rotatef (vtable-hidden old) (vtable-hidden new)))
	((and (gstruct-p old) (gstruct-p new) (not (vtable-p old)) (not (vtable-p new)))
	 (rotatef (gstruct-vtable old) (gstruct-vtable new))
	 (rotatef (gstruct-slots old) (gstruct-slots new)))
	((and (typep old 'applicable-struct) (typep new 'applicable-struct))
	 (rotatef (astruct-vtable old) (astruct-vtable new))
	 (rotatef (astruct-slots old) (astruct-slots new)))
	(t (wrong-type "%modify-instance" 2 new)))
  *unspecified*)

;;; ------------------------------------------------------------------
;;; Primitive generics

(defvar *primitive-generics* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "A primitive -> its generic, once a method has been added to it.")

(defparameter *primitive-generic-names*
  '("+" "-" "*" "/" "<" ">" "<=" ">=" "=" "max" "min" "abs" "quotient" "remainder"
    "modulo" "gcd" "lcm" "exact->inexact" "inexact->exact" "exp" "log" "sqrt" "sin" "cos"
    "tan" "asin" "acos" "atan" "sinh" "cosh" "tanh" "expt" "zero?" "positive?"
    "negative?" "odd?" "even?" "integer?" "rational?" "real?" "complex?" "number?"
    "floor" "ceiling" "round" "truncate" "numerator" "denominator" "magnitude" "angle"
    "real-part" "imag-part" "1+" "1-" "equal?" "write" "display" "length" "append"
    "list-tail" "vector-ref" "vector-set!" "vector-length")
  "The primitives that libguile defines with SCM_PRIMITIVE_GENERIC, roughly.")

(defun goops-generic-primitive-p (name)
  "Whether GOOPS is loaded and NAME's root variable is one that it makes
dispatch to a generic: a primcall of NAME is then a call through it."
  (and *goops-module* (member name *primitive-generic-names* :test #'string=) t))

(defvar *primitive-generic-functions* nil
  "The root values of *PRIMITIVE-GENERIC-NAMES*, and the names: (function . name).")

(defun primitive-generic-name (f)
  (unless *primitive-generic-functions*
    (setq *primitive-generic-functions*
	  (loop for name in *primitive-generic-names*
		for v = (root-value name)
		when (functionp v) collect (cons v name))))
  (cdr (assoc f *primitive-generic-functions* :test #'eq)))

(defun primitive-generic-of (f) (gethash f *primitive-generics*))

(defun enable-primitive-generic (f &optional generic)
  "Make F's root variable dispatch to GENERIC (a new one if none) when F
fails on its arguments; return the generic."
  (or (and (null generic) (primitive-generic-of f))
      (let* ((name (primitive-generic-name f))
	     (generic (or generic
			  (funcall (goops-value "make") (goops-class "<generic>")
				   :name (ssym name))))
	     (dispatching (lambda (&rest args)
			    (handler-case (let ((*dispatching-primitive* name)) (apply f args))
			      (wrong-type-dispatch () (apply generic args))
			      ((or type-error guile-throw) (c)
				(if (and (typep c 'guile-throw)
					 (not (string= (ps:scheme-symbol-name (guile-throw-key c)) "wrong-type-arg")))
				    (error c)
				    (apply generic args)))))))
	(setf (gethash f *primitive-generics*) generic
	      (gethash dispatching *primitive-generics*) generic)
	(let ((v (obarray-variable (ssym name))))
	  (when v (setf (gvariable-value v) dispatching)))
	(when (string= name "equal?") (setq *equal-generic* generic))
	(when (member name '("write" "display") :test #'string=)
	  (setf (gethash name *printing-generics*) generic))
	generic)))

(defvar *equal-generic* nil "equal?'s generic, which GOOPS sets: for instances.")
(defvar *printing-generics* (make-hash-table :test 'equal))

;;; ------------------------------------------------------------------

(defun make-vtable-classes (applicable)
  "Make the classes of the named vtables that have none yet: those of
applicable structs only if APPLICABLE.  Their classes inherit slots,
which GOOPS can make only once it is loaded."
  (let ((vtables '()))
    (maphash (lambda (v class)
	       (unless (or class
			   (logtest (vtable-flags v) +vtable-flag-goops-class+)
			   (and (not applicable) (logtest (vtable-flags v) +vtable-flag-applicable+)))
		 (push v vtables)))
	     *vtables*)
    (mapc #'class-for-vtable vtables)))

(defun goops-early-init ()
  (dolist (name '("<class>" "<top>" "<procedure-class>" "<applicable-struct-class>"
		  "<applicable-struct-with-setter-class>" "<method>" "<accessor-method>"
		  "<applicable>" "<applicable-struct>" "<applicable-struct-with-setter>"
		  "<generic>" "<extended-generic>" "<generic-with-setter>" "<accessor>"
		  "<extended-generic-with-setter>" "<extended-accessor>" "<boolean>" "<char>"
		  "<list>" "<pair>" "<null>" "<string>" "<symbol>" "<vector>" "<foreign>"
		  "<hashtable>" "<fluid>" "<dynamic-state>" "<frame>" "<keyword>" "<syntax>"
		  "<atomic-box>" "<vm-continuation>" "<bytevector>" "<uvec>" "<array>"
		  "<bitvector>" "<number>" "<complex>" "<real>" "<integer>" "<fraction>"
		  "<unknown>" "<procedure>" "<primitive-generic>" "<port>" "<input-port>"
		  "<output-port>" "<input-output-port>"))
    (let ((class (goops-value name)))
      (when class (setf (gethash name *goops-classes*) class))))
  ;; a class for each named vtable there is so far, records' among them
  (make-vtable-classes nil)
  ;; the smob classes
  (let ((make-class (goops-value "make-standard-class")))
    (loop for (name) in *smob-classes*
	  do (setf (gethash name *goops-classes*)
		   (funcall make-class (goops-class "<class>") (ssym name)
			    (list (goops-class (if (string= name "<guardian>") "<applicable>" "<top>")))
			    '()))))
  ;; the port classes, for the one port type here
  (let* ((make-class (goops-value "make-standard-class"))
	 (meta (goops-class "<class>"))
	 (port (funcall make-class meta (ssym "<file-port>") (list (goops-class "<port>")) '())))
    (setf (gethash "<file-port>" *goops-classes*) port)
    (loop for (name super) in '(("<file-input-port>" "<input-port>")
				("<file-output-port>" "<output-port>")
				("<file-input-output-port>" "<input-output-port>"))
	  do (setf (gethash name *goops-classes*)
		   (funcall make-class meta (ssym name) (list port (goops-class super)) '()))))
  *unspecified*)

(defextension "scm_init_goops_builtins"
  (setq *goops-module* *current-module*)
  (list
   (cons "%make-vtable-vtable" #'make-vtable-vtable)
   (cons "%init-layout!" #'init-layout)
   (cons "class-of" #'goops-class-of)
   (cons "instance?" (lambda (x)
		       (bool (and (struct-p x)
				  (logtest (vtable-flags (struct-vtable-of x)) +vtable-flag-goops-class+)))))
   (cons "generic-function-name" (lambda (g) (funcall (gethash "procedure-property" *guile-primitives*) g (ssym "name"))))
   (cons "%clear-fields!" #'clear-fields)
   (cons "%modify-instance" #'modify-instance)
   (cons "generic-capability?" (lambda (f) (bool (and (functionp f) (primitive-generic-name f)))))
   (cons "enable-primitive-generic!" (lambda (&rest fs) (mapc #'enable-primitive-generic fs) *unspecified*))
   (cons "set-primitive-generic!" (lambda (f generic) (enable-primitive-generic f generic) *unspecified*))
   (cons "primitive-generic-generic" (lambda (f)
				       (or (primitive-generic-of f)
					   (if (and (functionp f) (primitive-generic-name f))
					       (enable-primitive-generic f)
					       (wrong-type "primitive-generic-generic" 1 f)))))
   (cons "%goops-early-init" #'goops-early-init)
   (cons "%goops-loaded" (lambda ()
			   (make-vtable-classes t)
			   (setq *goops-write* (gethash "write" *printing-generics*))
			   *unspecified*))
   (cons "vtable-flag-vtable" +vtable-flag-vtable+)
   (cons "vtable-flag-applicable-vtable" +vtable-flag-applicable-vtable+)
   (cons "vtable-flag-setter-vtable" +vtable-flag-setter-vtable+)
   (cons "vtable-flag-validated" +vtable-flag-validated+)
   (cons "vtable-flag-goops-class" +vtable-flag-goops-class+)
   (cons "vtable-flag-goops-slot" +vtable-flag-goops-slot+)
   (cons "vtable-flag-goops-static-slot-allocation" +vtable-flag-goops-static-slot-allocation+)
   (cons "vtable-flag-goops-indirect" +vtable-flag-goops-indirect+)
   (cons "vtable-flag-goops-needs-migration" +vtable-flag-goops-needs-migration+)))

;;; equal? on structs, as libguile's: a GOOPS instance through equal?'s
;;; generic (which GOOPS sets), other structs of one vtable field by field.

(defun struct-equal (a b recur)
  (cond ((and (syntax-object-p a) (syntax-object-p b))
	 (and (funcall recur (syntax-object-expression a) (syntax-object-expression b))
	      (funcall recur (syntax-object-wrap a) (syntax-object-wrap b))
	      (funcall recur (syntax-object-module a) (syntax-object-module b))))
	(t (struct-equal-1 a b recur))))

(defun struct-equal-1 (a b recur)
  (if (or (garray-p a) (garray-p b))
      (and (guile-array-p a) (guile-array-p b) (arrays-equal a b))
      (struct-equal* a b recur)))

(defvar *structs-being-compared* '()
  "Pairs of structs being compared: met again (a cycle), they are taken
to be equal, as equal? does for pairs and vectors.")

(defun struct-equal* (a b recur)
  (and (struct-p a) (struct-p b)
       (eq (struct-vtable-of a) (struct-vtable-of b))
       (or (eq a b)
	   (if (logtest (vtable-flags (struct-vtable-of a)) +vtable-flag-goops-class+)
	       (and *equal-generic* (truthy (funcall *equal-generic* a b)))
	       (or (find-if (lambda (p) (and (eq (car p) a) (eq (cdr p) b))) *structs-being-compared*)
		   (let ((*structs-being-compared* (cons (cons a b) *structs-being-compared*))
			 (x (struct-slots-of a)) (y (struct-slots-of b)))
		     (and (= (length x) (length y))
			  (every recur x y))))))))

(setq ps::*equal-extension* 'struct-equal)
