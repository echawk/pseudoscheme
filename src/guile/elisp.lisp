; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-GUILE -*-

;;;; Emacs Lisp's lists in Guile's list procedures.  Emacs Lisp's nil is
;;;; Guile's #nil (reader.lisp), so a list Emacs Lisp code conses up,
;;;; (cons 1 nil), ends in #nil rather than ().  Guile's list procedures
;;;; take #nil as a list's end; so do these.  The ones Scheme code
;;;; (boot-9's and SRFI-1's) defines get that from null? and pair?
;;;; (runtime.lisp).  Those implemented in Lisp are replaced here, both as
;;;; root bindings and where the compiler calls them directly
;;;; (*PRIMCALL-RENAMES*):
;;;;
;;;; - length, list?, reverse, the mem and ass procedures and last-pair are
;;;;   loops that stop at either end, as fast as the originals (and those
;;;;   that return a tail return the list's own);
;;;; - sort, merge and append! keep #nil as Guile does: (sort #nil <) is
;;;;   #nil, lists that end in #nil merge into one that does, and append!
;;;;   joins them so;
;;;; - the rest get a list ending in () in place of an argument that
;;;;   ends in #nil.  For the destructive ones (reverse!, delete!) that
;;;;   means a new list: their values are right, but such an argument
;;;;   isn't changed as Guile would change it.

(in-package "QUASISCHEME-GUILE")

(declaim (inline list-end-p))
(defun list-end-p (x) (or (null x) (eq x *elisp-nil*)))

(defun guile-list-length (x)
  "The length of the list X, ended by () or #nil; NIL if X is circular
or improper."
  (let ((slow x) (n 0))
    (declare (fixnum n))
    (loop
      (when (list-end-p x) (return n))
      (unless (consp x) (return nil))
      (setq x (cdr x))
      (when (list-end-p x) (return (+ n 1)))
      (unless (consp x) (return nil))
      (setq x (cdr x) slow (cdr slow))
      (incf n 2)
      (when (eq x slow) (return nil)))))

(defun nil-terminated-p (x)
  "Does X end in #nil (or is it #nil)?  Not if it's circular."
  (or (eq x *elisp-nil*)
      (let ((slow x))
	(loop
	  (unless (consp x) (return (eq x *elisp-nil*)))
	  (setq x (cdr x))
	  (unless (consp x) (return (eq x *elisp-nil*)))
	  (setq x (cdr x) slow (cdr slow))
	  (when (eq x slow) (return nil))))))

(defun de-nil (x)
  "X, if it ends in #nil, as a list ending in (); else X."
  (cond ((eq x *elisp-nil*) '())
	((nil-terminated-p x) (loop for tail = x then (cdr tail) while (consp tail) collect (car tail)))
	(t x)))

(defun list-type-error (who x)
  (guile-error (ssym "wrong-type-arg") who "Wrong type argument in position ~A: ~S"
	       (list 1 x) (list x)))

(defun guile-length (x)
  (or (guile-list-length x) (list-type-error "length" x)))

(defun guile-list-p (x) (bool (guile-list-length x)))

(defun guile-reverse (x)
  (let ((acc '()) (tail x))
    (loop while (consp tail) do (push (pop tail) acc))
    (unless (list-end-p tail) (list-type-error "reverse" x))
    acc))

(defun alist-type-error (who alist)
  (guile-error (ssym "wrong-type-arg") who
	       "Wrong type argument in position 2 (expecting association list): ~S"
	       (list alist) (list alist)))

(defmacro do-alist ((entry tail alist who) &body body)
  "Run BODY for each ENTRY of ALIST (ended by () or #nil), at TAIL; an
entry that isn't a pair is an error, as in Guile."
  `(loop for ,tail = ,alist then (cdr ,tail)
	 while (consp ,tail)
	 do (let ((,entry (car ,tail)))
	      (unless (consp ,entry) (alist-type-error ,who ,alist))
	      ,@body)))

(defun guile-memq (key list)
  (loop for tail = list then (cdr tail)
	while (consp tail)
	when (eq (car tail) key) return tail
	finally (return qs:false)))

(defun guile-assq (key alist)
  (do-alist (entry tail alist "assq")
    (when (eq (car entry) key) (return-from guile-assq entry)))
  qs:false)

(defun guile-memv (key list)
  (loop for tail = list then (cdr tail)
	while (consp tail)
	when (eql (car tail) key) return tail
	finally (return qs:false)))

(defun guile-assv (key alist)
  (do-alist (entry tail alist "assv")
    (when (eql (car entry) key) (return-from guile-assv entry)))
  qs:false)

(defun guile-member (key list &optional (same nil same-p))
  (loop for tail = list then (cdr tail)
	while (consp tail)
	when (if same-p
		 (truthy (funcall same key (car tail)))
		 (qs:scheme-equal-p key (car tail)))
	  return tail
	finally (return qs:false)))

(defun guile-assoc (key alist &optional (same nil same-p))
  (do-alist (entry tail alist "assoc")
    (when (if same-p
	      (truthy (funcall same key (car entry)))
	      (qs:scheme-equal-p key (car entry)))
      (return-from guile-assoc entry)))
  qs:false)

(defun guile-last-pair (list)
  ;; of a dotted list too, as Guile's: (last-pair '(e . f)) is (e . f)
  (if (consp list)
      (loop for x = list then (cdr x) while (consp (cdr x)) finally (return x))
      list))

(defun nil-tolerant (f)
  "F, given lists ending in () in place of any of its arguments that end
in #nil.  (Checking costs a walk of each list argument.)"
  (lambda (&rest args)
    (if (some #'nil-terminated-p args)
	(apply f (mapcar #'de-nil args))
	(apply f args))))

(defun guile-append! (&rest lists)
  "Guile's append! of lists some of which end in #nil: the lists joined,
each changed to point at the next; empty ones, () or #nil, skipped; the
last argument kept as it is.  (Without a #nil, Guile's own does it.)"
  (let ((result nil) (end nil))
    (loop for (x . more) on lists
	  do (cond ((null more)
		    (if end (setf (cdr end) x) (setq result x)))
		   ((list-end-p x))
		   (t (if end (setf (cdr end) x) (setq result x))
		      (setq end (loop for p = x then (cdr p)
				      while (consp (cdr p))
				      finally (return p))))))
    (if (or result end) result '())))

(defun nil-ended (result &rest args)
  "RESULT, a list, ending in #nil if one of ARGS did."
  (when (and (consp result) (some #'nil-terminated-p args))
    (setf (cdr (last result)) *elisp-nil*))
  result)

(defparameter *nil-adapted-procedures*
  `(("sort" . sort-adapter) ("stable-sort" . sort-adapter)
    ("sort!" . sort-adapter) ("stable-sort!" . sort-adapter)
    ("merge" . merge-adapter) ("merge!" . merge-adapter) ("append!" . append!-adapter))
  "Procedures adapted, given the original, to keep #nil as Guile does:
(sort #nil <) is #nil, and merging lists that end in #nil makes one.")

(defun sort-adapter (original)
  (lambda (seq less)
    (if (eq seq *elisp-nil*) seq (funcall original (de-nil seq) less))))

(defun append!-adapter (original)
  (lambda (&rest lists)
    (if (some #'nil-terminated-p (butlast lists))
	(apply #'guile-append! lists)
	(apply original lists))))

(defun merge-adapter (original)
  (lambda (a b less)
    (nil-ended (funcall original (de-nil a) (de-nil b) less) a b)))

(defparameter *nil-tolerant-procedures*
  '("append" "list->vector" "map" "for-each" "reverse!" "sorted?"
    "list->weak-vector" "with-fluids*" "list-copy" "list->string"
    "delete" "delete!" "delq" "delq!" "delv" "delv!" "length+" "list-index")
  "Procedures given NIL-TOLERANT versions.")

(defparameter *nil-list-procedures*
  `(("length" . guile-length) ("list?" . guile-list-p) ("reverse" . guile-reverse)
    ("memq" . guile-memq) ("assq" . guile-assq) ("memv" . guile-memv)
    ("assv" . guile-assv) ("member" . guile-member) ("assoc" . guile-assoc)
    ("last-pair" . guile-last-pair))
  "Procedures replaced by loops that take #nil as a list's end.")

;; the compiler calls these, not the host's procedures of the names
(setq *primcall-renames*
      (append (loop for (name) in *nil-list-procedures*
		    collect (cons name (format nil "%guile-nil:~A" name)))
	      (loop for name in *nil-tolerant-procedures*
		    when (member name *open-primitives* :test #'string=)
		      collect (cons name (format nil "%guile-nil:~A" name)))
	      (remove-if (lambda (r) (search "%guile-nil:" (cdr r))) *primcall-renames*)))

(defun install-elisp-list-procedures ()
  "Install the #nil-taking versions: as host procedures the compiler calls,
and as Guile's root bindings."
  (flet ((install (name f)
	   (qsx:host-set! (format nil "%guile-nil:~A" name) f)
	   (obarray-define (ssym name) f)
	   ;; and for the modules whose load-extension binds them
	   (when (gethash name *guile-primitives*)
	     (setf (gethash name *guile-primitives*) f))))
    (loop for (name . function) in *nil-list-procedures*
	  do (install name (symbol-function function)))
    (loop for (name . adapter) in *nil-adapted-procedures*
	  do (let ((original (gethash name *guile-primitives*)))
	       (when (functionp original)
		 (install name (funcall adapter original)))))
    (dolist (name *nil-tolerant-procedures*)
      (let ((original (or (let ((v (obarray-variable (ssym name))))
			    (and v (functionp (gvariable-value v)) (gvariable-value v)))
			  (gethash name *guile-primitives*)
			  (ignore-errors (qsx:host-ref name)))))
	(when (functionp original)
	  (install name (nil-tolerant original)))))))
