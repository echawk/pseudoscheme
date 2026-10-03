; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-R6RS -*-

;;;; (rnrs hashtables (6)), library report chapter 13.
;;;;
;;;; eq and eqv tables are CL hash tables (:test EQ / EQL).  A table with
;;;; a user-supplied hash function and equivalence predicate is a CL EQL
;;;; table from hash value to a bucket (alist), searched with the
;;;; predicate -- portable, unlike implementation-specific custom hash
;;;; table tests.

(in-package "PSEUDOSCHEME-R6RS")

(defstruct (hashtable (:constructor %make-hashtable (table kind hash equiv mutable)))
  table		; CL hash table
  kind		; :eq, :eqv or :custom
  hash		; Scheme procedure, for :custom
  equiv		; Scheme procedure, for :custom
  (mutable t))

(defmethod print-object ((h hashtable) stream)
  (print-unreadable-object (h stream)
    (format stream "Hashtable ~(~A~) ~D" (hashtable-kind h) (hashtable-count* h))))

(defun hashtable-count* (h)
  (if (eq (hashtable-kind h) :custom)
      (loop for bucket being the hash-values of (hashtable-table h) sum (length bucket))
      (hash-table-count (hashtable-table h))))

(defun check-hashtable (who h)
  (unless (hashtable-p h) (ps:scheme-error "~A: not a hashtable: ~S" who h)))

(defun check-mutable (who h)
  (check-hashtable who h)
  (unless (hashtable-mutable h) (ps:scheme-error "~A: hashtable is immutable" who)))

(defprim "make-eq-hashtable" (&optional k)
  (declare (ignore k))
  (%make-hashtable (make-hash-table :test 'eq) :eq nil nil t))

(defprim "make-eqv-hashtable" (&optional k)
  (declare (ignore k))
  (%make-hashtable (make-hash-table :test 'eql) :eqv nil nil t))

(defprim "make-hashtable" (hash equiv &optional k)
  (declare (ignore k))
  (check-procedure "make-hashtable" hash)
  (check-procedure "make-hashtable" equiv)
  (%make-hashtable (make-hash-table :test 'eql) :custom hash equiv t))

(defprim "hashtable?" (x) (ps:true? (hashtable-p x)))

(defun custom-hash (h key)
  (let ((v (funcall (hashtable-hash h) key)))
    (unless (and (integerp v) (>= v 0))
      (ps:scheme-error "hashtable: hash function returned ~S" v))
    v))

(defun custom-cell (h key)
  (let ((equiv (hashtable-equiv h)))
    (assoc-if (lambda (k) (truthy (funcall equiv k key)))
	      (gethash (custom-hash h key) (hashtable-table h)))))

(defun table-ref (h key default)
  (if (eq (hashtable-kind h) :custom)
      (let ((cell (custom-cell h key))) (if cell (cdr cell) default))
      (multiple-value-bind (v found) (gethash key (hashtable-table h))
	(if found v default))))

(defun table-set (h key value)
  (if (eq (hashtable-kind h) :custom)
      (let ((cell (custom-cell h key)))
	(if cell
	    (setf (cdr cell) value)
	    (push (cons key value) (gethash (custom-hash h key) (hashtable-table h)))))
      (setf (gethash key (hashtable-table h)) value)))

(defun table-delete (h key)
  (if (eq (hashtable-kind h) :custom)
      (let ((code (custom-hash h key)) (equiv (hashtable-equiv h)))
	(setf (gethash code (hashtable-table h))
	      (remove-if (lambda (cell) (truthy (funcall equiv (car cell) key)))
			 (gethash code (hashtable-table h)))))
      (remhash key (hashtable-table h))))

(defun table-entries (h)
  "List of (key . value)."
  (if (eq (hashtable-kind h) :custom)
      (loop for bucket being the hash-values of (hashtable-table h) append (copy-list bucket))
      (loop for k being the hash-keys of (hashtable-table h) using (hash-value v)
	    collect (cons k v))))

(defprim "hashtable-size" (h) (check-hashtable "hashtable-size" h) (hashtable-count* h))

(defprim "hashtable-ref" (h key default)
  (check-hashtable "hashtable-ref" h)
  (table-ref h key default))

(defprim "hashtable-set!" (h key value)
  (check-mutable "hashtable-set!" h)
  (table-set h key value)
  ps:unspecific)

(defprim "hashtable-delete!" (h key)
  (check-mutable "hashtable-delete!" h)
  (table-delete h key)
  ps:unspecific)

(defprim "hashtable-contains?" (h key)
  (check-hashtable "hashtable-contains?" h)
  (let ((missing (load-time-value (list 'missing))))
    (ps:true? (not (eq (table-ref h key missing) missing)))))

(defprim "hashtable-update!" (h key proc default)
  (check-mutable "hashtable-update!" h)
  (table-set h key (funcall proc (table-ref h key default)))
  ps:unspecific)

(defprim "hashtable-copy" (h &optional mutable)
  (check-hashtable "hashtable-copy" h)
  (let ((new (make-hash-table :test (hash-table-test (hashtable-table h)))))
    (maphash (lambda (k v) (setf (gethash k new) (if (listp v) (copy-alist v) v)))
	     (hashtable-table h))
    (%make-hashtable new (hashtable-kind h) (hashtable-hash h) (hashtable-equiv h)
		     (and mutable (truthy mutable)))))

(defprim "hashtable-clear!" (h &optional k)
  (declare (ignore k))
  (check-mutable "hashtable-clear!" h)
  (clrhash (hashtable-table h))
  ps:unspecific)

(defprim "hashtable-keys" (h)
  (check-hashtable "hashtable-keys" h)
  (coerce (mapcar #'car (table-entries h)) 'simple-vector))

(defprim "hashtable-entries" (h)
  (check-hashtable "hashtable-entries" h)
  (let ((entries (table-entries h)))
    (values (coerce (mapcar #'car entries) 'simple-vector)
	    (coerce (mapcar #'cdr entries) 'simple-vector))))

(defprim "hashtable-equivalence-function" (h)
  (check-hashtable "hashtable-equivalence-function" h)
  (case (hashtable-kind h)
    (:eq (r6rs-global "eq?"))
    (:eqv (r6rs-global "eqv?"))
    (t (hashtable-equiv h))))

(defprim "hashtable-hash-function" (h)
  (check-hashtable "hashtable-hash-function" h)
  (if (eq (hashtable-kind h) :custom) (hashtable-hash h) ps:false))

(defprim "hashtable-mutable?" (h)
  (check-hashtable "hashtable-mutable?" h)
  (ps:true? (hashtable-mutable h)))

;;; 13.3 Hash functions

(defprim "equal-hash" (obj) (sxhash-scheme obj))
(defprim "string-hash" (s) (sxhash s))
(defprim "string-ci-hash" (s) (sxhash (string-downcase s)))
(defprim "symbol-hash" (s) (sxhash s))

(defun sxhash-scheme (obj)
  ;; SXHASH is EQUAL-consistent; Scheme EQUAL? also descends vectors,
  ;; which CL EQUAL doesn't, so hash vectors by their contents.
  (typecase obj
    (simple-vector (let ((h 17))
		     (loop for x across obj
			   do (setq h (logand most-positive-fixnum (+ (* h 31) (sxhash-scheme x)))))
		     h))
    (cons (logand most-positive-fixnum
		  (+ (* 31 (sxhash-scheme (car obj))) (sxhash-scheme (cdr obj)))))
    (t (sxhash obj))))

(defvar *globals-hook* nil
  "Function from a name to a host global's value, set by src/psyntax.lisp,
for the few primitives that hand back other standard procedures.")

(defun r6rs-global (name) (funcall *globals-hook* name))
