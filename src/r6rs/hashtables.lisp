; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-R6RS -*-

;;;; (rnrs hashtables (6)), library report chapter 13.
;;;;
;;;; eq and eqv tables are CL hash tables (:test EQ / EQL).  A table with
;;;; a user-supplied hash function and equivalence predicate is a CL EQL
;;;; table from hash value to a bucket (alist), searched with the
;;;; predicate -- portable, unlike implementation-specific custom hash
;;;; table tests.

(in-package "PSEUDOSCHEME-R6RS")

(defstruct (hashtable (:constructor %make-hashtable (table kind hash equiv mutable &optional weakness side)))
  table		; CL hash table
  kind		; :eq, :eqv or :custom
  hash		; Scheme procedure, for :custom
  equiv		; Scheme procedure, for :custom
  (mutable t)
  weakness	; NIL, or Chez's :weak or :ephemeron
  side)		; for an :ephemeron :custom table, key -> value, weak on keys

;;; A :custom table's TABLE maps hash codes to buckets, lists of cells
;;; (key . value).  In a weak one, a cell's key is a weak pointer to the
;;; key, and a cell whose key is gone is dead; in an ephemeron one, the
;;; values are in SIDE, weak on its keys, so that a value doesn't keep
;;; its key alive.

(declaim (inline cell-key))
(defun cell-key (cell)
  "The key of CELL, and whether it is alive."
  (let ((k (car cell)))
    (if (sb-ext:weak-pointer-p k) (sb-ext:weak-pointer-value k) (values k t))))

(defun cell-value (h cell)
  (if (eq (hashtable-weakness h) :ephemeron)
      (values (gethash (cell-key cell) (hashtable-side h)))
      (cdr cell)))

(defun set-cell-value (h cell value)
  (if (eq (hashtable-weakness h) :ephemeron)
      (setf (gethash (cell-key cell) (hashtable-side h)) value)
      (setf (cdr cell) value)))

(defun make-cell (h key value)
  (if (hashtable-weakness h)
      (let ((cell (cons (sb-ext:make-weak-pointer key) value)))
	(when (eq (hashtable-weakness h) :ephemeron)
	  (setf (cdr cell) nil (gethash key (hashtable-side h)) value))
	cell)
      (cons key value)))

(defun live-cells (bucket)
  (remove-if-not (lambda (cell) (nth-value 1 (cell-key cell))) bucket))

(defmethod print-object ((h hashtable) stream)
  (print-unreadable-object (h stream)
    (format stream "Hashtable ~(~A~) ~D" (hashtable-kind h) (hashtable-count* h))))

(defun hashtable-count* (h)
  (if (eq (hashtable-kind h) :custom)
      (loop for bucket being the hash-values of (hashtable-table h)
	    sum (count-if (lambda (cell) (nth-value 1 (cell-key cell))) bucket))
      (hash-table-count (hashtable-table h))))

(defun check-hashtable (who h)
  (unless (hashtable-p h) (ps:scheme-error "~A: not a hashtable: ~S" who h)))

(defun check-mutable (who h)
  (check-hashtable who h)
  (unless (hashtable-mutable h) (ps:scheme-error "~A: hashtable is immutable" who)))

(defun check-hashtable-size (who k)
  "K, a hashtable's initial capacity if given, is a nonnegative exact integer."
  (unless (or (null k) (typep k '(integer 0)))
    (ps:scheme-error "~A: invalid size ~S" who k)))

(defprim "make-eq-hashtable" (&optional k)
  (check-hashtable-size "make-eq-hashtable" k)
  (%make-hashtable (make-hash-table :test 'eq) :eq nil nil t))

(defprim "make-eqv-hashtable" (&optional k)
  (check-hashtable-size "make-eqv-hashtable" k)
  (%make-hashtable (make-hash-table :test 'eql) :eqv nil nil t))

(defprim "make-hashtable" (hash equiv &optional k)
  (check-hashtable-size "make-hashtable" k)
  (check-procedure "make-hashtable" hash)
  (check-procedure "make-hashtable" equiv)
  (%make-hashtable (make-hash-table :test 'eql) :custom hash equiv t))

(defprim "hashtable?" (x) (bool (hashtable-p x)))

(defun custom-hash (h key)
  (let ((v (funcall (hashtable-hash h) key)))
    (unless (and (integerp v) (>= v 0))
      (ps:scheme-error "hashtable: hash function returned ~S" v))
    v))

(defun custom-cell (h key)
  (let ((equiv (hashtable-equiv h)))
    (find-if (lambda (cell)
	       (multiple-value-bind (k alive) (cell-key cell)
		 (and alive (truthy (funcall equiv k key)))))
	     (gethash (custom-hash h key) (hashtable-table h)))))

(defun table-ref (h key default)
  (if (eq (hashtable-kind h) :custom)
      (let ((cell (custom-cell h key))) (if cell (cell-value h cell) default))
      (multiple-value-bind (v found) (gethash key (hashtable-table h))
	(if found v default))))

(defun table-set (h key value)
  (if (eq (hashtable-kind h) :custom)
      (let ((cell (custom-cell h key)))
	(if cell
	    (set-cell-value h cell value)
	    (let ((code (custom-hash h key)) (table (hashtable-table h)))
	      (setf (gethash code table)
		    (cons (make-cell h key value)
			  (if (hashtable-weakness h) (live-cells (gethash code table)) (gethash code table)))))))
      (setf (gethash key (hashtable-table h)) value)))

(defun table-delete (h key)
  (if (eq (hashtable-kind h) :custom)
      (let ((code (custom-hash h key)) (equiv (hashtable-equiv h)))
	(setf (gethash code (hashtable-table h))
	      (remove-if (lambda (cell)
			   (multiple-value-bind (k alive) (cell-key cell)
			     (or (not alive) (truthy (funcall equiv k key)))))
			 (gethash code (hashtable-table h)))))
      (remhash key (hashtable-table h))))

(defun table-entries (h)
  "List of (key . value)."
  (if (eq (hashtable-kind h) :custom)
      (loop for bucket being the hash-values of (hashtable-table h)
	    append (loop for cell in bucket
			 for (k alive) = (multiple-value-list (cell-key cell))
			 when alive collect (cons k (cell-value h cell))))
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
    (bool (not (eq (table-ref h key missing) missing)))))

(defprim "hashtable-update!" (h key proc default)
  (check-mutable "hashtable-update!" h)
  (table-set h key (funcall proc (table-ref h key default)))
  ps:unspecific)

(defun make-table-like (h mutable)
  "An empty hashtable of H's kind and weakness."
  (let* ((weakness (hashtable-weakness h))
	 (test (hash-table-test (hashtable-table h)))
	 (custom (eq (hashtable-kind h) :custom)))
    (%make-hashtable (if (and weakness (not custom))
			 (trivial-garbage:make-weak-hash-table :test test :weakness :key)
			 (make-hash-table :test test))
		     (hashtable-kind h) (hashtable-hash h) (hashtable-equiv h) mutable weakness
		     (and custom (eq weakness :ephemeron)
			  (trivial-garbage:make-weak-hash-table :test 'eq :weakness :key)))))

(defprim "hashtable-copy" (h &optional mutable)
  (check-hashtable "hashtable-copy" h)
  (let ((new (make-table-like h t)))
    (loop for (k . v) in (table-entries h) do (table-set new k v))
    (setf (hashtable-mutable new) (and mutable (truthy mutable)))
    new))

(defprim "hashtable-clear!" (h &optional k)
  (declare (ignore k))
  (check-mutable "hashtable-clear!" h)
  (clrhash (hashtable-table h))
  (when (hashtable-side h) (clrhash (hashtable-side h)))
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
  (bool (hashtable-mutable h)))

;;; 13.3 Hash functions

(defprim "equal-hash" (obj) (sxhash-scheme obj))
(defprim "string-hash" (s) (sxhash s))
(defprim "string-ci-hash" (s) (sxhash (string-downcase s)))
(defprim "symbol-hash" (s) (sxhash s))

(defvar *record-hash-hook* nil
  "NIL, or a function of a structure object and the function hashing its
parts: its hash, or NIL to hash it as SXHASH does.  The Chez mode hashes
records with it, by the procedures of record-type-hash-procedure.")

(defun sxhash-scheme (obj)
  ;; SXHASH is EQUAL-consistent; Scheme EQUAL? also descends vectors,
  ;; which CL EQUAL doesn't, so hash vectors by their contents.  Only the
  ;; first nodes count, as with SXHASH: EQUAL? objects are walked alike,
  ;; so they still hash alike, and a circular one is finite work.
  (let ((budget 64))
    (labels ((walk (obj)
	       (if (<= (decf budget) 0)
		   0
		   (typecase obj
		     (simple-vector (let ((h 17))
				      (loop for x across obj
					    while (plusp budget)
					    do (setq h (logand most-positive-fixnum (+ (* h 31) (walk x)))))
				      h))
		     (cons (logand most-positive-fixnum
				   (+ (* 31 (walk (car obj))) (walk (cdr obj)))))
		     (structure-object (or (and *record-hash-hook* (funcall *record-hash-hook* obj #'walk))
					   (sxhash obj)))
		     (t (sxhash obj))))))
      (walk obj))))

(defvar *globals-hook* nil
  "Function from a name to a host global's value, set by src/psyntax.lisp,
for the few primitives that hand back other standard procedures.")

(defun r6rs-global (name) (funcall *globals-hook* name))

;;; SRFI 124's ephemerons (src/srfi/124.sld): a one-entry weak-key table,
;;; which SBCL makes ephemeral.

(defun make-ephemeron-table (key datum)
  (let ((table (make-hash-table :test 'eq :weakness :key)))
    (setf (gethash key table) datum)
    table))

(defun ephemeron-entry (table)
  "The key and datum of TABLE's entry, or #f and #f if it has none."
  (block nil
    (maphash (lambda (k v) (return (values k v))) table)
    (values ps:false ps:false)))
