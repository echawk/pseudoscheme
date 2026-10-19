; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-RACKET -*-

;;;; Procedures, natively: impersonators and chaperones, impersonator
;;;; properties, procedure-reduce-arity.
;;;;
;;;; A procedure's chaperone or impersonator is a new procedure, which
;;;; passes the arguments through the wrapper (and the results through the
;;;; result wrapper, when the wrapper returns one first), recorded in
;;;; *IMPERSONATORS* with what it wraps and its impersonator properties.
;;;; Racket's keyword procedures are structs racket/private/kw.rkt
;;;; chaperones itself, on top of these.
;;;; Chaperones of structs, boxes and hash tables are in chaperone.lisp.
;;;; Not yet, and approximated: a chaperone of a vector (and of the other
;;;; data below) is the value itself.

(in-package "QUASISCHEME-RACKET")

(defstruct (impersonation (:constructor make-impersonation (kind of props)))
  kind					; :chaperone or :impersonator
  of					; what it wraps
  props)				; alist: impersonator property -> value

(defvar *impersonators* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "An impersonator or chaperone -> its IMPERSONATION.")

(defun impersonation (x) (values (gethash x *impersonators*)))

(defstruct (impersonator-property (:constructor make-impersonator-property* (name))) name)

(defun plist->alist (props)
  (loop for (p v) on props by #'cddr collect (cons p v)))

(defun wrap-procedure (kind proc wrapper props)
  (let ((new (if (truthy wrapper)
		 (lambda (&rest args)
		   (let ((results (multiple-value-list (apply wrapper args))))
		     (if (and (= (length results) (1+ (length args))) (functionp (car results)))
			 ;; a result wrapper first, then the arguments
			 (multiple-value-call (car results) (apply proc (cdr results)))
			 (apply proc results))))
		 (lambda (&rest args) (apply proc args)))))
    (setf (gethash new *impersonators*) (make-impersonation kind proc (plist->alist props))
	  (gethash new *arity-masks*) (or (ignore-errors (arity-mask proc)) -1))
    new))

(defnative "chaperone-procedure" (proc wrapper &rest props) (:rumble "chaperone-procedure")
  (wrap-procedure :chaperone proc wrapper props))
(defnative "impersonate-procedure" (proc wrapper &rest props) (:rumble "impersonate-procedure")
  (wrap-procedure :impersonator proc wrapper props))
(defnative "chaperone-procedure*" (proc wrapper &rest props) ()
  (wrap-procedure :chaperone proc (and (truthy wrapper) (lambda (&rest args) (apply wrapper proc args))) props))
(defnative "impersonate-procedure*" (proc wrapper &rest props) ()
  (wrap-procedure :impersonator proc (and (truthy wrapper) (lambda (&rest args) (apply wrapper proc args))) props))
;;; The unsafe ones: WRAPPER is called instead of PROC, which it stands for
(defnative "unsafe-chaperone-procedure" (proc wrapper &rest props) (:instance "#%unsafe")
  (let ((new (lambda (&rest args) (apply wrapper args))))
    (setf (gethash new *impersonators*) (make-impersonation :chaperone proc (plist->alist props)))
    new))
(defnative "unsafe-impersonate-procedure" (proc wrapper &rest props) (:instance "#%unsafe")
  (let ((new (lambda (&rest args) (apply wrapper args))))
    (setf (gethash new *impersonators*) (make-impersonation :impersonator proc (plist->alist props)))
    new))

(defnative "impersonator?" (x) (:leaf t) (bool (impersonation x)))
(defnative "chaperone?" (x) (:leaf t)
  (let ((i (impersonation x))) (bool (and i (eq (impersonation-kind i) :chaperone)))))
(defun impersonates-p (a b)
  "Whether A is B, or wraps it (through any number of layers)."
  (loop for x = a then (impersonation-of i)
	for i = (impersonation x)
	thereis (or (eq x b) (equal* x b))
	while i))
(defnative "impersonator-of?" (a b) () (bool (impersonates-p a b)))
(defnative "chaperone-of?" (a b) () (bool (impersonates-p a b)))

;;; Impersonator properties: values a wrapper carries
(defnative "make-impersonator-property" (name &optional accessor-name contract realm) (:leaf t)
  (declare (ignore accessor-name contract realm))
  (let ((p (make-impersonator-property* name)))
    (flet ((lookup (x)
	     (loop for i = (impersonation x) then (impersonation (impersonation-of i))
		   while i
		   do (let ((e (assoc p (impersonation-props i))))
			(when e (return (values (cdr e) t)))))))
      (values p
	      (lambda (x) (bool (nth-value 1 (lookup x))))
	      (lambda (x &optional (fail nil fail-p))
		(multiple-value-bind (v found) (lookup x)
		  (cond (found v)
			((not fail-p) (racket-error (format nil "~A-accessor" (rname name))
						    "contract violation~%  given: ~A" (written x)))
			((functionp fail) (funcall fail))
			(t fail))))))))
(defnative "impersonator-property?" (x) (:leaf t) (bool (impersonator-property-p x)))

;;; Chaperones of other data: the value itself, for now (structs, boxes,
;;; hash tables: chaperone.lisp; vectors: see there too)
(macrolet ((same (&rest names)
	     `(progn ,@(loop for name in names
			     collect `(defnative ,name (v &rest args) (:leaf t)
					(declare (ignore args))
					v)))))
  (same "chaperone-struct-type" "chaperone-evt"
	"chaperone-channel" "impersonate-channel" "chaperone-continuation-mark-key"
	"impersonate-continuation-mark-key" "chaperone-prompt-tag" "impersonate-prompt-tag"))

(defun arity->mask (arity)
  "The arity mask of ARITY, procedure-arity's form: k, (arity-at-least
k), or a list of them."
  (cond ((integerp arity) (ash 1 arity))
	((listp arity) (reduce #'logior (mapcar #'arity->mask arity) :initial-value 0))
	((truthy (funcall (third *arity-at-least*) arity))
	 (- (ash 1 (funcall (fourth *arity-at-least*) arity 0))))
	(t (racket-error "procedure-reduce-arity" "contract violation~%  expected: procedure-arity?~%  given: ~A"
			 (written arity)))))

(defvar *reduced-arity-of* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "A procedure made by reduce-arity -> the procedure it wraps.")

(defun reduce-arity (who proc mask name)
  "PROC, taking only the argument counts in MASK.  (Racket checks that
they are some of PROC's; our arities of struct procedures and wrappers
are approximate, so that isn't checked.)"
  (declare (ignore who))
  (let ((new (lambda (&rest args)
	       (if (logbitp (length args) mask)
		   (apply proc args)
		   (racket-arity-error (if (truthy name) (rname name) "#<procedure>") (length args))))))
    (setf (gethash new *arity-masks*) mask)
    (setf (gethash new *reduced-arity-of*) proc)
    new))

(defnative "procedure-reduce-arity" (proc arity &optional (name qs:false) realm) (:rumble "procedure-reduce-arity")
  (declare (ignore realm))
  (reduce-arity "procedure-reduce-arity" proc (arity->mask arity) name))
(defnative "procedure-reduce-arity-mask" (proc mask &optional (name qs:false) realm) ()
  (declare (ignore realm))
  (reduce-arity "procedure-reduce-arity-mask" proc mask name))

;;; procedure-specialize: a hint to compile a closure for its values;
;;; the procedure itself here
(defnative "procedure-specialize" (f) (:leaf t) f)
(defnative "procedure-impersonator*?" (f) (:leaf t) (declare (ignore f)) qs:false)
(defnative "procedure-extract-target" (f) (:leaf t) (declare (ignore f)) qs:false)
(defvar *single-valued-primitives* nil
  "The procedures of *host-shared* that return one value, once found (EQ table).")

;;; Racket CS says 1 for what Chez infers returns one value; of the
;;; primitives, those of the host that do (not every one Racket does).
(defparameter *multiple-valued-primitives*
  '("apply" "values" "call-with-values" "dynamic-wind" "map" "for-each"))

(defun single-valued-primitive-p (f)
  (unless *single-valued-primitives*
    (let ((table (make-hash-table :test 'eq)))
      (dolist (name *host-shared*)
	(unless (member name *multiple-valued-primitives* :test #'string=)
	  (let ((v (primitive name)))
	    (when (functionp v) (setf (gethash v table) t)))))
      (setq *single-valued-primitives* table)))
  (gethash f *single-valued-primitives*))

(defnative "procedure-result-arity" (f) (:leaf t)
  (unless (functionp f) (arg-error "procedure-result-arity" "procedure?" f))
  ;; known: a struct's constructor, predicate, accessor or mutator, a
  ;; primitive that returns one value, or what wraps one
  (let ((g (loop for x = f then (or (gethash x *reduced-arity-of*) (impersonation-of i))
		 for i = (impersonation x)
		 while (or i (gethash x *reduced-arity-of*))
		 finally (return x))))
    (if (or (gethash g *struct-procedures*) (single-valued-primitive-p g)) 1 qs:false)))
(defnative "primitive-result-arity" (f) (:leaf t) (declare (ignore f)) qs:false)
