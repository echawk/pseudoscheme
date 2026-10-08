; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-GUILE -*-

;;;; libguile's data and primitives, in Lisp (docs/guile.md, stage 1).
;;;;
;;;; Guile's Scheme (boot-9, psyntax, ice-9) runs on what libguile, in C,
;;;; provides.  The parts here are the ones that aren't Pseudoscheme's
;;;; already:
;;;;
;;;;   structs and vtables, which Guile's records, its Tree-IL and its
;;;;     modules are made of; applicable structs (parameters, procedures
;;;;     with setters) are funcallable instances, so they are procedures;
;;;;   variables, the boxes modules hold;
;;;;   Guile's hash tables (one table, any of hashq-/hashv-/hash- on it),
;;;;     whose handles are the pairs the table keeps;
;;;;   fluids, on the R7RS layer's parameter states, with the stack of
;;;;     outer values fluid-ref* reads;
;;;;   syntax objects and macros (make-syntax, make-syntax-transformer);
;;;;   the module system's C half: module-variable and friends, which
;;;;     read boot-9's module records;
;;;;   errors: scm-error, and Lisp errors raised as Guile exceptions.
;;;;
;;;; Every other primitive Guile and Pseudoscheme have in common (car,
;;;; string-append, ...) is Pseudoscheme's own procedure: see
;;;; INSTALL-ROOT-BINDINGS in src/guile/boot.lisp.

(in-package "PSEUDOSCHEME-GUILE")

(defvar *guile-primitives* (make-hash-table :test 'equal)
  "Guile's primitives defined here: name -> function.")

(defmacro defguile (name lambda-list &body body)
  "Define Guile primitive NAME (a string)."
  `(setf (gethash ,name *guile-primitives*)
	 (named-lambda-for ,name ,lambda-list ,@body)))

(defmacro named-lambda-for (name lambda-list &body body)
  (declare (ignore name))
  `(lambda ,lambda-list ,@body))

(declaim (inline bool truthy))
(defun bool (x) (if x ps:true ps:false))
(defun truthy (x) (not (eq x ps:false)))

(defvar *unspecified* ps:unspecific)

(defvar *program-arguments* '() "What Guile's (program-arguments) returns.")

(defun guile-error (key subr message args &optional (rest ps:false))
  "Raise Guile exception KEY as scm-error does."
  (funcall (gethash "scm-error" *guile-primitives*) key subr message args rest))

(defun wrong-type (subr pos obj)
  (guile-error (ssym "wrong-type-arg") subr "Wrong type argument in position ~A: ~S"
	       (list pos obj) (list obj)))

;;; ------------------------------------------------------------------
;;; Structs and vtables
;;;
;;; A struct is a vtable and a vector of fields.  A vtable is a struct
;;; too, whose first 8 fields are standard-vtable-fields: layout (a
;;; symbol such as pwpwuw), flags, finalizer, printer, name, size,
;;; unboxed fields, reserved; user fields follow.  As in Guile, a
;;; field whose layout letter is h ("hidden") gets no initializer from
;;; make-struct's arguments.

(defconstant +vtable-flag-vtable+ 2)
(defconstant +vtable-flag-applicable-vtable+ 4)
(defconstant +vtable-flag-applicable+ 8)
(defconstant +vtable-flag-setter-vtable+ 16)
(defconstant +vtable-flag-setter+ 32)

(defparameter *standard-vtable-fields* "pwuhuhpwphuhuhuh")
(defconstant +vtable-offset-user+ 8)
(defconstant +vtable-index-layout+ 0)
(defconstant +vtable-index-flags+ 1)
(defconstant +vtable-index-printer+ 3)
(defconstant +vtable-index-name+ 4)
(defconstant +vtable-index-size+ 5)

(defstruct (gstruct (:constructor %make-gstruct (vtable slots)) (:copier nil))
  vtable
  (slots #() :type simple-vector))

(defstruct (vtable (:include gstruct) (:constructor %make-vtable (vtable slots)) (:copier nil))
  (nfields 0 :type fixnum)
  (flags 0 :type fixnum)
  (hidden '() :type list))		; the hidden fields' indices

(defclass applicable-struct (sb-mop:funcallable-standard-object)
  ((vtable :initarg :vtable :accessor astruct-vtable)
   (slots :initarg :slots :accessor astruct-slots))
  (:metaclass sb-mop:funcallable-standard-class))

(defun make-applicable-struct (vtable slots)
  (let ((s (make-instance 'applicable-struct :vtable vtable :slots slots)))
    (sb-mop:set-funcallable-instance-function
     s (lambda (&rest args) (apply (the function (svref (astruct-slots s) 0)) args)))
    s))

(declaim (inline struct-p))
(defun struct-p (x) (or (gstruct-p x) (typep x 'applicable-struct)))

(defun struct-vtable-of (s)
  (if (gstruct-p s) (gstruct-vtable s) (astruct-vtable s)))

(defun struct-slots-of (s)
  (if (gstruct-p s) (gstruct-slots s) (astruct-slots s)))

(defun layout-string (layout)
  (cond ((symbolp layout) (symbol-name layout))	; made by make-struct-layout: kept as written
	((stringp layout) layout)
	(t (wrong-type "make-struct" 1 layout))))

(defun layout-symbol (string)
  ;; Layout symbols are Scheme symbols, with Guile's name
  (ssym string))

(defun parse-layout (layout)
  "Field count and hidden field indices of LAYOUT."
  (let* ((s (ps:scheme-symbol-name (if (stringp layout) (layout-symbol layout) layout)))
	 (n (floor (length s) 2)))
    (values n (loop for i below n
		    when (char= (char s (1+ (* 2 i))) #\h) collect i))))

(defun field-default (layout-string i)
  (if (char= (char layout-string (* 2 i)) #\u) 0 ps:false))

(defun init-slots (vtable inits &optional (who "make-struct"))
  (let* ((n (vtable-nfields vtable))
	 (hidden (vtable-hidden vtable))
	 (layout (ps:scheme-symbol-name (svref (gstruct-slots vtable) 0)))
	 (slots (make-array n)))
    (dotimes (i n)
      (setf (svref slots i)
	    (if (and inits (not (member i hidden)))
		(pop inits)
		(field-default layout i))))
    (when inits
      (guile-error (ssym "misc-error") who "too many initializers" '()))
    slots))

(defun inherited-vtable-flags (meta layout)
  "The flags a vtable of LAYOUT gets from its vtable META, as Guile's
scm_i_struct_inherit_vtable_magic computes them."
  (let ((meta-flags (vtable-flags meta))
	(string (ps:scheme-symbol-name layout)))
    (logior (if (and (>= (length string) (length *standard-vtable-fields*))
		     (string= *standard-vtable-fields* string
			      :end2 (length *standard-vtable-fields*)))
		+vtable-flag-vtable+ 0)
	    (if (logtest meta-flags +vtable-flag-applicable-vtable+)
		+vtable-flag-applicable+ 0)
	    (if (logtest meta-flags +vtable-flag-setter-vtable+)
		(logior +vtable-flag-applicable+ +vtable-flag-setter+) 0))))

(defvar *vtables* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "Each named vtable (set-struct-vtable-name!, as records are) -> its
GOOPS class, or NIL until GOOPS starts (src/guile/goops.lisp).")

(defun make-vtable-from-slots (meta slots)
  "SLOTS made into a vtable whose vtable is META: its layout parsed,
its flags computed as Guile's scm_i_struct_inherit_vtable_magic does.  A
GOOPS class is made before its layout is known (%init-layout! gives it
one): it has no fields until then."
  (let ((layout (svref slots 0)))
    (when (stringp layout) (setf (svref slots 0) (setq layout (layout-symbol layout))))
    (let ((v (%make-vtable meta slots)))
      (if (and layout (symbolp layout) (not (eq layout ps:false)) (not (keywordp layout)))
	  (multiple-value-bind (n hidden) (parse-layout layout)
	    (let ((flags (logior (let ((f (svref slots +vtable-index-flags+))) (if (integerp f) f 0))
				 (inherited-vtable-flags meta layout))))
	      (setf (vtable-nfields v) n (vtable-hidden v) hidden (vtable-flags v) flags
		    (svref slots +vtable-index-flags+) flags
		    (svref slots +vtable-index-size+) n)))
	  (let ((flags (logand (vtable-flags meta)
			       (logior +vtable-flag-applicable-vtable+ +vtable-flag-setter-vtable+))))
	    (setf (vtable-flags v) (logior (if (logtest flags +vtable-flag-applicable-vtable+) +vtable-flag-applicable+ 0)
					   (if (logtest flags +vtable-flag-setter-vtable+)
					       (logior +vtable-flag-applicable+ +vtable-flag-setter+) 0))
		  (svref slots +vtable-index-flags+) (vtable-flags v))))
      v)))

(defun make-struct* (vtable inits &optional (who "make-struct"))
  (unless (vtable-p vtable) (wrong-type who 1 vtable))
  (let ((slots (init-slots vtable inits who))
	(flags (vtable-flags vtable)))
    (cond ((logtest flags +vtable-flag-vtable+) (make-vtable-from-slots vtable slots))
	  ((logtest flags +vtable-flag-applicable+) (make-applicable-struct vtable slots))
	  (t (%make-gstruct vtable slots)))))

(defun make-root-vtable (flags)
  "A vtable-vtable of standard-vtable-fields, its own vtable when it's
<standard-vtable>."
  (let* ((n (/ (length *standard-vtable-fields*) 2))
	 (slots (make-array n :initial-element 0)))
    (setf (svref slots 0) (layout-symbol *standard-vtable-fields*)
	  (svref slots +vtable-index-printer+) ps:false
	  (svref slots +vtable-index-name+) ps:false)
    (let ((v (%make-vtable nil slots)))
      (multiple-value-bind (n hidden) (parse-layout (svref slots 0))
	(setf (vtable-nfields v) n (vtable-hidden v) hidden
	      (vtable-flags v) (logior +vtable-flag-vtable+ flags)
	      (svref slots +vtable-index-flags+) (vtable-flags v)
	      (svref slots +vtable-index-size+) n))
      v)))

(defvar *standard-vtable*
  (let ((v (make-root-vtable 0))) (setf (gstruct-vtable v) v) v))

(defvar *applicable-struct-vtable*
  (let ((v (make-root-vtable +vtable-flag-applicable-vtable+)))
    (setf (gstruct-vtable v) *standard-vtable*)
    v))

(defvar *applicable-struct-with-setter-vtable*
  (let ((v (make-root-vtable (logior +vtable-flag-applicable-vtable+ +vtable-flag-setter-vtable+))))
    (setf (gstruct-vtable v) *standard-vtable*)
    v))

(defvar *procedure-with-setter-vtable*
  (make-struct* *applicable-struct-with-setter-vtable* (list (layout-symbol "pwpw"))))

(defun check-index (who s i)
  (unless (and (typep i 'fixnum) (< -1 i (length (struct-slots-of s))))
    (guile-error (ssym "out-of-range") who "Value out of range: ~S" (list i) (list i))))

(defguile "struct?" (x) (bool (struct-p x)))
(defguile "struct-vtable?" (x) (bool (vtable-p x)))
(defguile "struct-vtable" (s)
  (unless (struct-p s) (wrong-type "struct-vtable" 1 s))
  (struct-vtable-of s))
(defguile "struct-ref" (s i)
  (unless (struct-p s) (wrong-type "struct-ref" 1 s))
  (check-index "struct-ref" s i)
  (svref (struct-slots-of s) i))
(defun check-unboxed (who s i v)
  "An unboxed field (u) holds a 64-bit word."
  (let ((layout (svref (gstruct-slots (struct-vtable-of s)) +vtable-index-layout+)))
    (when (and (symbolp layout) (not (typep v '(unsigned-byte 64))))
      (let ((string (ps:scheme-symbol-name layout)))
	(when (and (< (* 2 i) (length string)) (char= (char string (* 2 i)) #\u))
	  (guile-error (ssym "out-of-range") who "Value out of range: ~S" (list v) (list v)))))))

(defguile "struct-set!" (s i v)
  (unless (struct-p s) (wrong-type "struct-set!" 1 s))
  (check-index "struct-set!" s i)
  (check-unboxed "struct-set!" s i v)
  (setf (svref (struct-slots-of s) i) v)
  (when (and (vtable-p s) (= i +vtable-index-flags+)) (setf (vtable-flags s) v))
  v)					; as libguile's: (rnrs enums) relies on it
(setf (gethash "struct-ref/unboxed" *guile-primitives*) (gethash "struct-ref" *guile-primitives*)
      (gethash "struct-set!/unboxed" *guile-primitives*) (gethash "struct-set!" *guile-primitives*))
(defguile "make-struct/no-tail" (vtable &rest inits) (make-struct* vtable inits "make-struct/no-tail"))
(defguile "make-struct/simple" (vtable &rest inits) (make-struct* vtable inits "make-struct/simple"))
(defguile "allocate-struct" (vtable n)
  (declare (ignore n))
  (make-struct* vtable '() "allocate-struct"))
(defguile "make-struct-layout" (fields) (layout-symbol (layout-string fields)))
(defguile "make-vtable" (fields &optional (printer ps:false))
  (make-struct* *standard-vtable* (list (layout-symbol (layout-string fields)) printer) "make-vtable"))
(defguile "struct-layout" (s) (svref (gstruct-slots (struct-vtable-of s)) 0))
(defguile "struct-vtable-name" (v) (svref (gstruct-slots v) +vtable-index-name+))
(defguile "set-struct-vtable-name!" (v name)
  (setf (svref (gstruct-slots v) +vtable-index-name+) name)
  (if (goops-ready-p)
      (class-for-vtable v)
      (setf (gethash v *vtables*) nil))
  *unspecified*)

(defun struct-printer (s)
  (let ((printer (svref (gstruct-slots (struct-vtable-of s)) +vtable-index-printer+)))
    (and (functionp printer) printer)))

(defvar *goops-write* nil
  "GOOPS's write generic, once (oop goops) is loaded: GOOPS instances,
classes among them, are printed with it.")

(defun print-struct (s stream)
  (let ((printer (struct-printer s)))
    (cond (printer (funcall printer s stream))
	  ((and *goops-write*
		(logtest (vtable-flags (struct-vtable-of s)) 512)) ; vtable-flag-goops-class
	   (funcall *goops-write* s stream))
	  ((vtable-p s)
	   (let ((name (svref (gstruct-slots s) +vtable-index-name+)))
	     (format stream "#<vtable:~A ~A>"
		     (if (symbolp name) (ps:scheme-symbol-name name) "")
		     (ps:scheme-symbol-name (svref (gstruct-slots s) 0)))))
	  (t (let ((name (svref (gstruct-slots (struct-vtable-of s)) +vtable-index-name+)))
	       (format stream "#<~A ~X>"
		       (if (and name (symbolp name) (not (eq name ps:false)))
			   (ps:scheme-symbol-name name) "struct")
		       (logand (sb-kernel:get-lisp-obj-address s) #xffffffffff)))))))

(defmethod print-object ((s gstruct) stream) (print-struct s stream))
(defmethod print-object ((s applicable-struct) stream) (print-struct s stream))

;;; Procedures with setters

(defguile "make-procedure-with-setter" (procedure setter)
  (make-struct* *procedure-with-setter-vtable* (list procedure setter)))
(defguile "procedure-with-setter?" (x)
  (bool (and (typep x 'applicable-struct)
	     (logtest (vtable-flags (astruct-vtable x)) +vtable-flag-setter+))))
(defguile "setter" (x)
  (if (and (typep x 'applicable-struct)
	   (logtest (vtable-flags (astruct-vtable x)) +vtable-flag-setter+))
      (svref (astruct-slots x) 1)
      (wrong-type "setter" 1 x)))

;;; ------------------------------------------------------------------
;;; Variables

(defvar +unbound+ (ps::make-photon "#<unbound>"))

(defstruct (gvariable (:constructor make-gvariable (&optional (value +unbound+))) (:copier nil))
  value)

(defmethod print-object ((v gvariable) stream)
  (if (eq (gvariable-value v) +unbound+)
      (format stream "#<variable ~X unbound>" (logand (sb-kernel:get-lisp-obj-address v) #xffffffffff))
      (progn (format stream "#<variable ~X value: " (logand (sb-kernel:get-lisp-obj-address v) #xffffffffff))
	     (guile-write (gvariable-value v) stream)
	     (write-string ">" stream))))

(defguile "make-variable" (value) (make-gvariable value))
(defguile "make-undefined-variable" () (make-gvariable))
(defguile "variable?" (x) (bool (gvariable-p x)))
(defguile "variable-bound?" (v) (bool (not (eq (gvariable-value v) +unbound+))))
(defguile "variable-ref" (v)
  (let ((x (gvariable-value v)))
    (when (eq x +unbound+)
      (guile-error (ssym "unbound-variable") "variable-ref" "Unbound variable: ~S" (list v)))
    x))
(defguile "variable-set!" (v x) (setf (gvariable-value v) x) *unspecified*)
(defguile "variable-unset!" (v) (setf (gvariable-value v) +unbound+) *unspecified*)

;;; ------------------------------------------------------------------
;;; Hash tables
;;;
;;; A Guile table can be used with hashq-, hashv- and hash- procedures
;;; alike; each table here takes the equivalence of the first of them
;;; used on it (eq? and eqv? are both EQL).  Values are kept in handles,
;;; (key . value) pairs, which hash-get-handle returns and the caller may
;;; change.

(sb-ext:define-hash-table-test ps:scheme-equal-p ps-r6rs::sxhash-scheme)

(defstruct (ghash (:constructor %make-ghash (weakness &optional (initial-size 31))) (:copier nil))
  (table nil)
  (kind nil)				; :eql or :equal once used
  (weakness nil)
  (initial-size 31))

(defparameter *hash-table-sizes*
  '(31 61 113 223 443 883 1759 3517 7027 14051 28099 56197 112363 224717 449419 898823
    1797641 3595271 7190537 14381041 28762081 57524111 115048217 230096423)
  "Guile's bucket counts: a table has the first that holds its entries.")

(defun ghash-bucket-count (h)
  (let ((n (max (ghash-initial-size h) (if (ghash-table h) (hash-table-count (ghash-table h)) 0))))
    (or (find-if (lambda (size) (>= size n)) *hash-table-sizes*) n)))

(defmethod print-object ((h ghash) stream)
  (format stream "#<hash-table ~(~X~) ~D/~D>" (logand (sb-kernel:get-lisp-obj-address h) #xffffffffff)
	  (if (ghash-table h) (hash-table-count (ghash-table h)) 0)
	  (ghash-bucket-count h)))

(defun make-ghash (&optional weakness (size 31))
  (unless (integerp size) (wrong-type "make-hash-table" 1 size))
  (unless (>= size 0)
    (guile-error (ssym "out-of-range") "make-hash-table" "Value out of range: ~S" (list size) (list size)))
  (%make-ghash weakness (max 1 size)))

(defun ghash-table-for (h kind)
  "H's table, made for KIND (:eql or :equal) if it hasn't one yet."
  (let ((table (ghash-table h)))
    (cond ((null table)
	   (setf (ghash-kind h) kind
		 (ghash-table h) (apply #'make-hash-table
					:test (if (eq kind :eql) 'eql 'ps:scheme-equal-p)
					:synchronized t
					(and (ghash-weakness h) (list :weakness (ghash-weakness h))))))
	  ((and (eq kind :equal) (eq (ghash-kind h) :eql))
	   ;; used with hash- after hashq-: make it an equal? table
	   (let ((new (make-hash-table :test 'ps:scheme-equal-p :synchronized t)))
	     (maphash (lambda (k v) (setf (gethash k new) v)) table)
	     (setf (ghash-kind h) :equal (ghash-table h) new)))
	  (t table))))

(defun check-table (who h) (unless (ghash-p h) (wrong-type who 1 h)))

(defmacro define-hash-procedures (prefix kind)
  (flet ((name (s) (format nil "~A~A" prefix s)))
    `(progn
       (defguile ,(name "-ref") (h key &optional (default ps:false))
	 (check-table ,(name "-ref") h)
	 (let ((handle (gethash key (ghash-table-for h ,kind))))
	   (if handle (cdr handle) default)))
       (defguile ,(name "-set!") (h key value)
	 (check-table ,(name "-set!") h)
	 (let* ((table (ghash-table-for h ,kind)) (handle (gethash key table)))
	   (if handle (setf (cdr handle) value) (setf (gethash key table) (cons key value))))
	 *unspecified*)
       (defguile ,(name "-remove!") (h key)
	 (check-table ,(name "-remove!") h)
	 (let* ((table (ghash-table-for h ,kind)) (handle (gethash key table)))
	   (remhash key table)
	   (or handle ps:false)))
       (defguile ,(name "-get-handle") (h key)
	 (check-table ,(name "-get-handle") h)
	 (or (gethash key (ghash-table-for h ,kind)) ps:false))
       (defguile ,(name "-create-handle!") (h key init)
	 (check-table ,(name "-create-handle!") h)
	 (let ((table (ghash-table-for h ,kind)))
	   (or (gethash key table) (setf (gethash key table) (cons key init))))))))

(define-hash-procedures "hashq" :eql)
(define-hash-procedures "hashv" :eql)
(define-hash-procedures "hash" :equal)

(defguile "make-hash-table" (&optional (n 31)) (make-ghash nil n))
(defguile "make-weak-key-hash-table" (&optional (n 31)) (make-ghash :key n))
(defguile "make-weak-value-hash-table" (&optional (n 31)) (make-ghash :value n))
(defguile "make-doubly-weak-hash-table" (&optional (n 31)) (make-ghash :key-and-value n))
(defguile "hash-table?" (x) (bool (ghash-p x)))
(defguile "weak-key-hash-table?" (x) (bool (and (ghash-p x) (eq (ghash-weakness x) :key))))
(defguile "weak-value-hash-table?" (x) (bool (and (ghash-p x) (eq (ghash-weakness x) :value))))
(defguile "doubly-weak-hash-table?" (x) (bool (and (ghash-p x) (eq (ghash-weakness x) :key-and-value))))

(defun ghash-handles (h)
  (let ((table (ghash-table h)) (handles '()))
    (when table (maphash (lambda (k handle) (declare (ignore k)) (push handle handles)) table))
    handles))

(defguile "hash-clear!" (h) (when (ghash-table h) (clrhash (ghash-table h))) *unspecified*)
(defguile "hash-count" (pred h)
  (count-if (lambda (handle) (truthy (funcall pred (car handle) (cdr handle)))) (ghash-handles h)))
(defguile "hash-for-each" (proc h)
  (dolist (handle (ghash-handles h)) (funcall proc (car handle) (cdr handle)))
  *unspecified*)
(defguile "hash-for-each-handle" (proc h)
  (dolist (handle (ghash-handles h)) (funcall proc handle))
  *unspecified*)
(defguile "hash-map->list" (proc h)
  (mapcar (lambda (handle) (funcall proc (car handle) (cdr handle))) (ghash-handles h)))
(defguile "hash-fold" (proc init h)
  (let ((acc init))
    (dolist (handle (ghash-handles h) acc)
      (setq acc (funcall proc (car handle) (cdr handle) acc)))))

(defun check-hash-size (who size)
  (unless (and (integerp size) (plusp size))
    (guile-error (ssym "out-of-range") who "Value out of range: ~S" (list size) (list size))))
(defguile "hashq" (key size) (check-hash-size "hashq" size) (mod (sxhash key) size))
(defguile "hashv" (key size) (check-hash-size "hashv" size) (mod (sxhash key) size))
(defguile "hash" (key size) (check-hash-size "hash" size) (mod (ps-r6rs::sxhash-scheme key) size))

;;; The alist procedures Guile writes in C, which skip an entry that
;;; isn't a pair ("sloppy"), as the -ref, -set! and -remove! ones do
(defun alist-entry (alist key test)
  "The first pair in ALIST whose car is KEY by TEST: other entries, and
an improper tail, are skipped, as Guile's sloppy procedures do."
  (loop for tail = alist then (cdr tail)
	while (consp tail)
	when (and (consp (car tail)) (funcall test (caar tail) key)) return (car tail)))

(macrolet ((alist-procedures (prefix test)
	     (let ((ref (format nil "~A-ref" prefix)) (set (format nil "~A-set!" prefix))
		   (remove (format nil "~A-remove!" prefix)))
	       `(progn
		  (defguile ,ref (alist key)
		    (let ((e (alist-entry alist key #',test)))
		      (if e (cdr e) ps:false)))
		  (defguile ,set (alist key value)
		    (let ((e (alist-entry alist key #',test)))
		      (if e (progn (setf (cdr e) value) alist) (acons key value alist))))
		  (defguile ,remove (alist key)
		    (let ((e (alist-entry alist key #',test)))
		      (if e (delete e alist :count 1 :test #'eq) alist)))))))
  (alist-procedures "assq" eq)
  (alist-procedures "assv" eql)
  (alist-procedures "assoc" ps:scheme-equal-p))

;;; hashx-: the caller's hash and assoc procedures, over an alist per
;;; bucket kept in an equal? table by hash value

(defun hashx-bucket (h hash key)
  (let ((n (funcall hash key 1009)))
    (values (ghash-table-for h :eql) n)))
(defguile "hashx-ref" (hash assoc h key &optional (default ps:false))
  (multiple-value-bind (table n) (hashx-bucket h hash key)
    (let ((handle (funcall assoc key (gethash n table '()))))
      (if (consp handle) (cdr handle) default))))
(defguile "hashx-get-handle" (hash assoc h key)
  (multiple-value-bind (table n) (hashx-bucket h hash key)
    (funcall assoc key (gethash n table '()))))
(defguile "hashx-set!" (hash assoc h key value)
  (multiple-value-bind (table n) (hashx-bucket h hash key)
    (let ((handle (funcall assoc key (gethash n table '()))))
      (if (consp handle)
	  (setf (cdr handle) value)
	  (push (cons key value) (gethash n table '())))))
  *unspecified*)
(defguile "hashx-create-handle!" (hash assoc h key init)
  (multiple-value-bind (table n) (hashx-bucket h hash key)
    (let ((handle (funcall assoc key (gethash n table '()))))
      (if (consp handle)
	  handle
	  (car (push (cons key init) (gethash n table '())))))))
(defguile "hashx-remove!" (hash assoc h key)
  (multiple-value-bind (table n) (hashx-bucket h hash key)
    (let ((handle (funcall assoc key (gethash n table '()))))
      (when (consp handle) (setf (gethash n table) (remove handle (gethash n table) :test #'eq)))
      (if (consp handle) handle ps:false))))

;;; ------------------------------------------------------------------
;;; Fluids
;;;
;;; A fluid is a parameter state of the R7RS layer (src/r7rs/rts.lisp),
;;; so that it is per thread where parameters are, and with-fluid* is
;;; re-established when a continuation captured inside is re-entered.
;;; Its OUTER slot is the stack of values with-fluid* has shadowed,
;;; innermost first, which fluid-ref* reads.

(defstruct (fluid (:include ps-r7rs::parameter-state)
		  (:constructor %make-fluid (global converter))
		  (:copier nil))
  (outer '())
  (default ps:false)
  (special nil))			; a Lisp special variable that holds the value

(defmethod print-object ((f fluid) stream)
  (format stream "#<fluid ~(~X~)>" (logand (sb-kernel:get-lisp-obj-address f) #xffffffffff)))

(defun make-fluid* (&optional (default ps:false))
  (let ((f (%make-fluid default nil)))
    (setf (fluid-default f) default)
    f))

(declaim (inline fluid-value))
(defun fluid-value (f)
  (if (fluid-special f) (symbol-value (fluid-special f)) (ps-r7rs::parameter-state-value f)))
(defun (setf fluid-value) (v f)
  (if (fluid-special f)
      (setf (symbol-value (fluid-special f)) v)
      (setf (ps-r7rs::parameter-state-value f) v)))

(defun call-with-fluid (fluid value thunk)
  (when (fluid-special fluid)
    (return-from call-with-fluid
      (funcall ps-r7rs::*call-in-extent*
	       (lambda (inner) (progv (list (fluid-special fluid)) (list value) (funcall inner)))
	       thunk)))
  (funcall ps-r7rs::*call-in-extent*
	   (lambda (inner)
	     (let ((old (fluid-value fluid)))
	       (push old (fluid-outer fluid))
	       (unwind-protect
		    (progn (setf (fluid-value fluid) value)
			   (funcall inner))
		 (pop (fluid-outer fluid))
		 (setf (fluid-value fluid) old))))
	   thunk))

(defun check-fluid (who f) (unless (fluid-p f) (wrong-type who 1 f)))

(defguile "make-fluid" (&optional (default ps:false)) (make-fluid* default))
(defguile "make-unbound-fluid" () (make-fluid* +unbound+))
(defguile "make-thread-local-fluid" (&optional (default ps:false)) (make-fluid* default))
(defguile "fluid?" (x) (bool (fluid-p x)))
(defguile "fluid-thread-local?" (x) (declare (ignore x)) ps:false)
(defguile "fluid-ref" (f)
  (check-fluid "fluid-ref" f)
  (let ((v (fluid-value f)))
    (when (eq v +unbound+)
      (guile-error (ssym "unbound-variable") "fluid-ref" "Unbound fluid: ~S" (list f)))
    v))
(defguile "fluid-ref*" (f depth)
  (check-fluid "fluid-ref*" f)
  (if (zerop depth)
      (fluid-value f)
      (let ((tail (nthcdr (1- depth) (fluid-outer f))))
	(if tail (car tail) (fluid-default f)))))
(defguile "fluid-set!" (f v) (check-fluid "fluid-set!" f) (setf (fluid-value f) v) *unspecified*)
(defguile "fluid-unset!" (f) (setf (fluid-value f) +unbound+) *unspecified*)
(defguile "fluid-bound?" (f) (bool (not (eq (fluid-value f) +unbound+))))
(defguile "with-fluid*" (f v thunk) (check-fluid "with-fluid*" f) (call-with-fluid f v thunk))
(defguile "with-fluids*" (fluids values thunk)
  (if (null fluids)
      (funcall thunk)
      (call-with-fluid (car fluids) (car values)
		       (lambda () (funcall (gethash "with-fluids*" *guile-primitives*)
					   (cdr fluids) (cdr values) thunk)))))

;;; Dynamic states: a snapshot of every fluid's value.  Only the
;;; procedures; with-dynamic-state sets the values for its extent.

(defstruct (dynamic-state (:constructor make-dynamic-state (values)) (:copier nil))
  values)

(defguile "current-dynamic-state" () (make-dynamic-state '()))
(defguile "dynamic-state?" (x) (bool (dynamic-state-p x)))
(defguile "set-current-dynamic-state" (s) (declare (ignore s)) (make-dynamic-state '()))
(defguile "with-dynamic-state" (state thunk) (declare (ignore state)) (funcall thunk))

;;; ------------------------------------------------------------------
;;; Syntax objects and macros

(defstruct (syntax-object (:constructor make-syntax-object (expression wrap module sourcev))
			  (:copier nil))
  expression wrap module sourcev)

(defmethod print-object ((s syntax-object) stream)
  (write-string "#<syntax " stream)
  (guile-write (syntax-object-expression s) stream)
  (write-string ">" stream))

(defguile "make-syntax" (exp wrap module &optional (sourcev ps:false))
  (make-syntax-object exp wrap module sourcev))
(defguile "syntax?" (x) (bool (syntax-object-p x)))
(defguile "syntax-expression" (s) (syntax-object-expression s))
(defguile "syntax-wrap" (s) (syntax-object-wrap s))
(defguile "syntax-module" (s) (syntax-object-module s))
(defguile "syntax-sourcev" (s) (syntax-object-sourcev s))

(defstruct (gmacro (:constructor make-gmacro (name type binding)) (:copier nil))
  name type binding)

(defmethod print-object ((m gmacro) stream)
  (format stream "#<syntax-transformer ~A>" (if (symbolp (gmacro-name m)) (ps:scheme-symbol-name (gmacro-name m)) "")))

(defguile "make-syntax-transformer" (name type binding) (make-gmacro name type binding))
(defguile "macro?" (x) (bool (gmacro-p x)))
(defguile "macro-type" (m) (if (gmacro-p m) (gmacro-type m) ps:false))
(defguile "macro-name" (m) (if (gmacro-p m) (gmacro-name m) ps:false))
(defguile "macro-binding" (m) (if (gmacro-p m) (gmacro-binding m) ps:false))
(defguile "macro-transformer" (m)
  (if (and (gmacro-p m) (functionp (gmacro-binding m))) (gmacro-binding m) ps:false))

;;; ------------------------------------------------------------------
;;; Procedure properties, source properties, object properties

(defvar *procedure-properties* (trivial-garbage:make-weak-hash-table :weakness :key :test 'eq))

(defguile "procedure-properties" (p) (values (gethash p *procedure-properties* '())))
(defguile "set-procedure-properties!" (p alist) (setf (gethash p *procedure-properties*) alist) *unspecified*)
(defguile "procedure-property" (p key)
  (let ((entry (assoc key (gethash p *procedure-properties* '()))))
    (if entry (cdr entry) ps:false)))
(defguile "set-procedure-property!" (p key value)
  (let ((entry (assoc key (gethash p *procedure-properties* '()))))
    (if entry
	(setf (cdr entry) value)
	(push (cons key value) (gethash p *procedure-properties*))))
  *unspecified*)
(defvar *root-procedure-names* nil
  "A procedure bound in the root module -> its name there: the name of
a primitive, which (ice-9 documentation) finds its docstring by.")

(defun root-procedure-name (p)
  "P's name in the root module.  A Lisp function bound to several names
(car and first) takes the one that is its Lisp name, if one is."
  (unless *root-procedure-names*
    (let ((table (make-hash-table :test 'eq :weakness :key)))
      (dolist (handle (ghash-handles *obarray*))
	(let ((v (cdr handle)))
	  (when (and (gvariable-p v) (functionp (gvariable-value v)))
	    (let* ((f (gvariable-value v))
		   (old (gethash f table))
		   (lisp-name (ignore-errors (string-downcase (string (sb-kernel:%fun-name f))))))
	      (when (or (null old)
			(and lisp-name (string= lisp-name (ps:scheme-symbol-name (car handle)))))
		(setf (gethash f table) (car handle)))))))
      (setq *root-procedure-names* table)))
  (gethash p *root-procedure-names*))

(defguile "procedure-name" (p)
  (let ((entry (assoc (ssym "name") (gethash p *procedure-properties* '()))))
    (cond (entry (cdr entry))
	  ((root-procedure-name p))
	  (t ps:false))))
(defguile "procedure-documentation" (p)
  (let ((entry (assoc (ssym "documentation") (gethash p *procedure-properties* '()))))
    (if entry (cdr entry) ps:false)))
(defguile "procedure-source" (p) (declare (ignore p)) ps:false)

(defvar *source-properties* (trivial-garbage:make-weak-hash-table :weakness :key :test 'eq))
(defguile "source-properties" (x) (values (gethash x *source-properties* '())))
(defguile "set-source-properties!" (x alist) (setf (gethash x *source-properties*) alist) *unspecified*)
(defguile "source-property" (x key)
  (let ((entry (assoc key (gethash x *source-properties* '()))))
    (if entry (cdr entry) ps:false)))
(defguile "set-source-property!" (x key value)
  (push (cons key value) (gethash x *source-properties*))
  *unspecified*)
(defguile "supports-source-properties?" (x) (bool (consp x)))
(defguile "cons-source" (xorig x y) (declare (ignore xorig)) (cons x y))

(defvar *object-properties* (trivial-garbage:make-weak-hash-table :weakness :key :test 'eq))
(defguile "object-properties" (x) (values (gethash x *object-properties* '())))
(defguile "set-object-properties!" (x alist) (setf (gethash x *object-properties*) alist) *unspecified*)
(defguile "object-property" (x key)
  (let ((entry (assoc key (gethash x *object-properties* '()))))
    (if entry (cdr entry) ps:false)))
(defguile "set-object-property!" (x key value)
  (let ((entry (assoc key (gethash x *object-properties* '()))))
    (if entry (setf (cdr entry) value) (push (cons key value) (gethash x *object-properties*))))
  *unspecified*)

;;; ------------------------------------------------------------------
;;; Hooks

(defstruct (hook (:constructor make-hook* (arity)) (:copier nil))
  arity (procedures '()))

(defmethod print-object ((h hook) stream) (write-string "#<hook>" stream))

(defguile "make-hook" (&optional (arity 0)) (make-hook* arity))
(defguile "hook?" (x) (bool (hook-p x)))
(defguile "hook-empty?" (h) (bool (null (hook-procedures h))))
(defguile "add-hook!" (h proc &optional (append ps:false))
  (setf (hook-procedures h)
	(if (truthy append)
	    (append (remove proc (hook-procedures h)) (list proc))
	    (cons proc (remove proc (hook-procedures h)))))
  *unspecified*)
(defguile "remove-hook!" (h proc) (setf (hook-procedures h) (remove proc (hook-procedures h))) *unspecified*)
(defguile "reset-hook!" (h) (setf (hook-procedures h) '()) *unspecified*)
(defguile "hook->list" (h) (copy-list (hook-procedures h)))
(defguile "run-hook" (h &rest args)
  (dolist (p (hook-procedures h)) (apply p args))
  *unspecified*)

;;; ------------------------------------------------------------------
;;; Keywords: Lisp keywords, as #:kw reads

(defguile "keyword?" (x) (bool (and (keywordp x) x)))
(defguile "symbol->keyword" (s) (intern (symbol-name s) "KEYWORD"))
(defguile "keyword->symbol" (k) (intern (symbol-name k) ps:scheme-package))

;;; ------------------------------------------------------------------
;;; Prompts: Pseudoscheme's (src/continuations.lisp)

(defguile "make-prompt-tag" (&optional (stem "prompt")) (list stem))
(defguile "call-with-prompt" (tag thunk handler) (psx::call-with-prompt tag thunk handler))
(defguile "abort-to-prompt*" (tag args) (apply #'psx::abort-to-prompt tag args))
(defguile "abort-to-prompt" (tag &rest args) (apply #'psx::abort-to-prompt tag args))

;;; ------------------------------------------------------------------
;;; Errors
;;;
;;; scm-error calls boot-9's throw once boot-9 has defined it.  Before
;;; that, and for Lisp's own errors outside Guile code, a Lisp condition
;;; carries the throw's key and arguments.

(define-condition guile-throw (error)
  ((key :initarg :key :reader guile-throw-key)
   (args :initarg :args :reader guile-throw-args))
  (:report (lambda (c stream)
	     (format stream "Guile throw to ~A: " (guile-throw-key c))
	     (guile-write (guile-throw-args c) stream))))

(defvar *throw* nil
  "boot-9's throw, once it is defined (src/guile/boot.lisp).")

(defvar *throw-depth* 0)
(defvar *trace-throws* nil)

(defun call-throw (key args)
  "boot-9's throw, or a Lisp error before there is one, or when throwing
fails over and over (boot-9 half loaded)."
  (when *trace-throws*
    (format *trace-output* "~&;; throw ~A ~A~%" key (with-output-to-string (s) (guile-write args s))))
  (if (and *throw* (< *throw-depth* 3))
      (let ((*throw-depth* (1+ *throw-depth*)))
	(apply *throw* key args))
      (error 'guile-throw :key key :args args)))

(defguile "scm-error" (key subr message args rest)
  (call-throw key (list subr message args rest)))

(defguile "throw" (key &rest args) (call-throw key args))

;;; ------------------------------------------------------------------
;;; Odds and ends

(defvar *guile-gensym-counter* 0)
(defguile "gensym" (&optional (prefix " g"))
  (let ((prefix (if (stringp prefix) prefix (ps:scheme-symbol-name prefix))))
    (ssym (format nil "~A~D" prefix (incf *guile-gensym-counter*)))))

(defguile "noop" (&rest args) (if args (first args) ps:false))
(defguile "1+" (x) (ps:scheme+ x 1))
(defguile "1-" (x) (ps:scheme- x 1))
(defguile "self-evaluating?" (x)
  (bool (not (or (consp x) (null x) (and (symbolp x) (ps:scheme-symbol-p x) (not (keywordp x)))))))
(defguile "nil?" (x) (bool (or (null x) (eq x ps:false) (eq x *elisp-nil*))))
(defguile "acons" (k v alist) (acons k v alist))
(defguile "cons*" (x &rest more) (apply #'list* x more))
(defguile "logbit?" (i n) (bool (logbitp i n)))
(defguile "logtest" (a b) (bool (logtest a b)))
(defguile "logcount" (n) (logcount n))
(defguile "integer-length" (n) (integer-length n))
(defguile "ash" (n count)
  ;; a shift too large to represent is Guile's numerical-overflow
  (cond ((zerop n) 0)
	((< count (- (ash 1 32))) (if (minusp n) -1 0))
	((> count (ash 1 32))
	 (guile-error (ssym "numerical-overflow") "ash" "Numerical overflow" '()))
	(t (ash n count))))
(defguile "round-ash" (n count)
  (cond ((zerop n) 0)
	((< count (- (ash 1 32))) 0)
	((> count (ash 1 32))
	 (guile-error (ssym "numerical-overflow") "round-ash" "Numerical overflow" '()))
	((>= count 0) (ash n count))
	(t (round n (ash 1 (- count))))))
(defguile "logand" (&rest ns) (apply #'logand ns))
(defguile "logior" (&rest ns) (apply #'logior ns))
(defguile "logxor" (&rest ns) (apply #'logxor ns))
(defguile "lognot" (n) (lognot n))
(defguile "bit-extract" (n start end) (ldb (byte (- end start) start) n))
(defconstant +guile-fixnum-max+ (1- (ash 1 61)) "Guile's most-positive-fixnum, 62 bits.")

(defguile "object-address" (x)
  ;; Guile's bits for an immediate (a fixnum is n<<2 | 2, which (rnrs
  ;; arithmetic fixnums) tests), an address otherwise
  (cond ((and (integerp x) (<= (- -1 +guile-fixnum-max+) x +guile-fixnum-max+))
	 (ldb (byte 64 0) (logior (ash x 2) 2)))
	((characterp x) (logior (ash (char-code x) 8) #x0c))
	((eq x ps:false) #x4) ((eq x ps:true) #x404) ((null x) #x304)
	(t (logandc2 (sb-kernel:get-lisp-obj-address x) 7))))
(defguile "inf" () sb-ext:double-float-positive-infinity)
(defguile "nan" () (- sb-ext:double-float-positive-infinity sb-ext:double-float-positive-infinity))
(defun function-arity (f)
  "F's arity from its Lisp lambda list: required and optional counts,
and whether it takes more; NIL if that isn't known."
  (let ((lambda-list (if (functionp f) (sb-kernel:%fun-lambda-list f) :unknown)))
    (when (listp lambda-list)
      (let ((required 0) (optional 0) (rest nil) (state :required))
	(dolist (x lambda-list)
	  (case x
	    (&optional (setq state :optional))
	    ((&rest &body &key) (setq rest t state :done))
	    (&aux (setq state :done))
	    (t (case state
		 (:required (incf required))
		 (:optional (incf optional))))))
	(list required optional rest)))))

(defguile "thunk?" (x)
  (bool (and (functionp x)
	     (let ((arity (function-arity x)))
	       (or (null arity) (zerop (first arity)))))))
(defguile "primitive-exit" (&optional (code 0))
  (finish-output *standard-output*)
  (sb-ext:exit :code (cond ((eq code ps:false) 1) ((eq code ps:true) 0) (t code)) :abort t))
(setf (gethash "primitive-_exit" *guile-primitives*) (gethash "primitive-exit" *guile-primitives*))
(defguile "include-deprecated-features" () ps:false)
(defguile "issue-deprecation-warning" (&rest messages) (declare (ignore messages)) *unspecified*)
(defguile "%warn-auto-compilation-enabled" () *unspecified*)
(defguile "gc" () (sb-ext:gc :full t) *unspecified*)
(defguile "gc-stats" () '())
(defguile "major-version" () "3")
(defguile "minor-version" () "0")
(defguile "micro-version" () "11")
(defguile "effective-version" () "3.0")
(defguile "version" () "3.0.11")
(defguile "debug-options-interface" (&rest args) (declare (ignore args)) '())
;;; Option sets, as libguile's scm_options: (name boolean-p value doc).
;;; Called with no argument, the interface returns the options set (a
;;; boolean by its name alone, others as name and value); with a list, it
;;; sets them (booleans by being named); with #t, it describes them.

(defun make-options (specs)
  (mapcar (lambda (spec) (destructuring-bind (name boolean value doc) spec
			   (list (ssym name) boolean value doc)))
	  specs))

(defvar *print-options*
  (make-options
   '(("highlight-prefix" nil "{" "The string to print before highlighted values.")
     ("highlight-suffix" nil "}" "The string to print after highlighted values.")
     ("quote-keywordish-symbols" nil :reader "How to print symbols that have a colon as their first or last character.")
     ("escape-newlines" t t "Render newlines as \\n when printing using `write'.")
     ("r7rs-symbols" t nil "Escape symbols using R7RS |...| symbol notation.")
     ("bytestrings" t nil "Render bytevectors as bytestrings (SRFI 207)."))))

(defvar *read-options*
  (make-options
   '(("copy-source" t nil "Copy source code expressions.")
     ("positions" t t "Record positions of source code expressions.")
     ("case-insensitive" t nil "Convert symbols to lower case.")
     ("keywords" nil nil "Style of keyword recognition: #f, 'prefix or 'postfix.")
     ("r6rs-hex-escapes" t nil "Use R6RS variable-length character and string hex escapes.")
     ("square-brackets" t t "Treat `[' and `]' as parentheses, for R6RS compatibility.")
     ("hungry-eol-escapes" t nil "In strings, consume leading whitespace after an escaped end-of-line.")
     ("curly-infix" t nil "Support SRFI-105 curly infix expressions.")
     ("r7rs-symbols" t nil "Support R7RS |...| symbol notation.")
     ("bytestrings" t nil "Read bytestrings (SRFI 207)."))))

(defun option-scheme-value (v)
  (cond ((eq v :reader) (ssym "reader")) ((eq v t) ps:true) ((null v) ps:false) (t v)))

(defun option-value (options name)
  "NAME's value in OPTIONS, Lisp-style: booleans as T or NIL."
  (let ((o (find name options :key (lambda (o) (ps:scheme-symbol-name (first o))) :test #'string=)))
    (third o)))

(defun options-interface (options &optional (arg nil argp))
  (cond ((not argp)
	 (loop for (name boolean value) in options
	       if boolean when value collect name end
	       else append (list name (option-scheme-value value))))
	((eq arg ps:true)
	 (loop for (name boolean value doc) in options
	       collect (list name (if boolean (bool value) (option-scheme-value value)) doc)))
	(t (let ((old (options-interface options)))
	     (dolist (o options)
	       (when (second o) (setf (third o) nil)))
	     (loop for tail on arg
		   do (let ((o (find (car tail) options :key #'first)))
			(cond ((null o))
			      ((second o) (setf (third o) t))
			      (t (setq tail (cdr tail))
				 (setf (third o)
				       (let ((v (car tail)))
					 (cond ((eq v ps:false) nil)
					       ((eq v ps:true) t)
					       ((and (symbolp v) (string= (ps:scheme-symbol-name v) "reader")) :reader)
					       (t v))))))))
	     old))))

(defguile "read-options-interface" (&optional (arg nil argp))
  (if argp (options-interface *read-options* arg) (options-interface *read-options*)))
(defguile "print-options-interface" (&optional (arg nil argp))
  (if argp (options-interface *print-options* arg) (options-interface *print-options*)))
(defguile "call-with-blocked-asyncs" (thunk) (funcall thunk))
(defguile "call-with-unblocked-asyncs" (thunk) (funcall thunk))
(defguile "with-continuation-barrier" (thunk) (funcall thunk))
(defguile "%get-stack-size" () 0)
(defguile "get-internal-real-time" () (get-internal-real-time))
(defguile "get-internal-run-time" () (get-internal-run-time))
(defguile "current-time" () (- (get-universal-time) #.(encode-universal-time 0 0 0 1 1 1970 0)))
(defguile "program-arguments" () (copy-list *program-arguments*))
(defguile "set-program-arguments" (args) (setq *program-arguments* args) *unspecified*)

;; Keywords are Lisp symbols, but not Scheme symbols to Guile
(defguile "symbol?" (x) (bool (and (symbolp x) (ps:scheme-symbol-p x) (not (keywordp x)))))

(defvar *syntax-session-id* (list :session))
(defguile "syntax-session-id" () *syntax-session-id*)

;; Pseudoscheme's own procedures by name, for the replacement modules in
;; src/guile/modules/
(defguile "%host-ref" (name) (psx:host-ref (ps:scheme-symbol-name name)))

;;; #nil, Emacs Lisp's nil: false, and the empty list, to Guile's own
;;; predicates (src/guile/compile.lisp makes it false to `if').
(defguile "not" (x) (bool (or (eq x ps:false) (eq x *elisp-nil*))))
(defguile "null?" (x) (bool (or (null x) (eq x *elisp-nil*))))
(defguile "boolean?" (x) (bool (or (eq x ps:false) (eq x ps:true) (eq x *elisp-nil*))))
