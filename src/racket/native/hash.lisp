; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

;;;; Racket's hash tables, natively.
;;;;
;;;; Mutable ones (make-hash, make-hasheq, ...) are SBCL hash tables in an
;;;; RHASH, which says which equality it uses; equal?-based ones use a
;;;; table test of our own (RACKET-EQUAL, RACKET-EQUAL-HASH).
;;;; Immutable ones (hash, hasheq, hash-set, ...) are persistent: an
;;;; IHASH is a hash array mapped trie, five bits of the hash a level,
;;;; with collision nodes where two keys' hashes are the same; hash-set
;;;; copies one path.  Rumble uses Patricia tries and then "stencil
;;;; vectors"; this is the textbook layout, which can change without
;;;; anyone seeing it.
;;;;
;;;; Iteration positions (hash-iterate-first and the rest) are indexes
;;;; into a snapshot of the entries, made when iteration starts and
;;;; dropped when a mutable table changes.
;;;; Not yet: ephemeron tables, equal-always?, impersonators, chaperones,
;;;; and equal? on structs that are transparent or have prop:equal+hash.

(in-package "PSEUDOSCHEME-RACKET")

;;; ------------------------------------------------------------------
;;; Hashing

(defvar *identity-hashes* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "Object -> its eq hash: SBCL may move objects, so an address won't do.")
(defvar *identity-counter* 0)

(defun eq-hash (x)
  (if (or (typep x 'fixnum) (characterp x) (symbolp x))
      (logand (sxhash x) most-positive-fixnum)
      (or (gethash x *identity-hashes*)
	  (setf (gethash x *identity-hashes*)
		(logand (* (incf *identity-counter*) 2654435761) most-positive-fixnum)))))

(defun eqv-hash (x)
  (if (numberp x) (logand (sxhash x) most-positive-fixnum) (eq-hash x)))

(defun mix (h x)
  (logand (+ (* h 31) x) most-positive-fixnum))

(defun racket-equal-hash (x &optional (depth 0))
  (equal-hash* x depth))

(defun racket-equal (a b)
  (equal* a b))

(defun racket-eqv (a b)
  (or (eql a b)
      ;; eqv? on numbers: exactness and value (and NaN is eqv? to itself)
      (and (floatp a) (floatp b) (sb-ext:float-nan-p a) (sb-ext:float-nan-p b))))

(sb-ext:define-hash-table-test racket-equal racket-equal-hash)
(sb-ext:define-hash-table-test racket-eqv eqv-hash)

(defun kind-test (kind)
  (ecase kind (:eq #'eq) (:eqv #'racket-eqv) (:equal #'racket-equal)))

(defun kind-hash (kind)
  (ecase kind (:eq #'eq-hash) (:eqv #'eqv-hash) (:equal #'racket-equal-hash)))

;;; ------------------------------------------------------------------
;;; Mutable tables

(defstruct (rhash (:constructor %make-rhash (kind table weak)))
  kind table weak
  (snapshot nil))			; for iteration: #((key . value) ...)

(defun make-rhash (kind &optional weak)
  (%make-rhash kind
	       (make-hash-table :test (ecase kind (:eq 'eq) (:eqv 'racket-eqv) (:equal 'racket-equal))
				:weakness (and weak :key) :synchronized t)
	       weak))

(defmethod print-object ((x rhash) stream)
  (format stream "#<hash~A>" (ecase (rhash-kind x) (:eq "eq") (:eqv "eqv") (:equal ""))))

;;; ------------------------------------------------------------------
;;; Immutable tables: a hash array mapped trie

(defstruct (leaf (:constructor make-leaf (hash key value))) hash key value)
(defstruct (cnode (:constructor make-cnode (hash entries))) hash entries) ; entries: ((key . value) ...)
(defstruct (bnode (:constructor make-bnode (bitmap children)))
  (bitmap 0 :type (unsigned-byte 32)) (children #() :type simple-vector))

(defstruct (ihash (:constructor make-ihash (kind root count)))
  kind root (count 0)
  (snapshot nil))

(defmethod print-object ((x ihash) stream)
  (format stream "#hash~A(~{~A~^ ~})" (ecase (ihash-kind x) (:eq "eq") (:eqv "eqv") (:equal ""))
	  (map 'list (lambda (e) (format nil "(~S . ~S)" (car e) (cdr e))) (ihash-entries x))))

(defun chunk (hash shift) (ldb (byte 5 shift) hash))

(defun node-hash (n) (if (leaf-p n) (leaf-hash n) (cnode-hash n)))

(defun vector-insert (v i x)
  (let ((new (make-array (1+ (length v)))))
    (replace new v :end2 i)
    (setf (svref new i) x)
    (replace new v :start1 (1+ i) :start2 i)
    new))

(defun vector-remove (v i)
  (let ((new (make-array (1- (length v)))))
    (replace new v :end2 i)
    (replace new v :start1 i :start2 (1+ i))
    new))

(defun vector-replace (v i x)
  (let ((new (copy-seq v))) (setf (svref new i) x) new))

(defun merge-nodes (a b shift)
  "A trie node holding A and B, leaves or collision nodes whose hashes
differ, at SHIFT."
  (let ((ia (chunk (node-hash a) shift)) (ib (chunk (node-hash b) shift)))
    (cond ((= ia ib) (make-bnode (ash 1 ia) (vector (merge-nodes a b (+ shift 5)))))
	  ((< ia ib) (make-bnode (logior (ash 1 ia) (ash 1 ib)) (vector a b)))
	  (t (make-bnode (logior (ash 1 ia) (ash 1 ib)) (vector b a))))))

(defun trie-ref (node hash key test)
  "KEY's entry: its value and T, or NIL, NIL."
  (loop
    (cond ((null node) (return (values nil nil)))
	  ((leaf-p node)
	   (return (if (and (= (leaf-hash node) hash) (funcall test (leaf-key node) key))
		       (values (leaf-value node) t)
		       (values nil nil))))
	  ((cnode-p node)
	   (return (let ((e (and (= (cnode-hash node) hash)
				 (assoc key (cnode-entries node) :test test))))
		     (if e (values (cdr e) t) (values nil nil)))))
	  ;; a trie's root (TRIE-REF-BNODE walks the levels below)
	  (t (return (trie-ref-bnode node hash key test 0))))))

(defun trie-ref-bnode (node hash key test shift)
  (loop
    (let* ((bit (ash 1 (chunk hash shift)))
	   (bitmap (bnode-bitmap node)))
      (when (zerop (logand bitmap bit)) (return (values nil nil)))
      (let ((child (svref (bnode-children node) (logcount (logand bitmap (1- bit))))))
	(if (bnode-p child)
	    (setq node child shift (+ shift 5))
	    (return (trie-ref child hash key test)))))))

(defun trie-set (node hash key value test shift)
  "NODE with KEY set to VALUE; and whether that added an entry."
  (cond ((null node) (values (make-leaf hash key value) t))
	((leaf-p node)
	 (cond ((and (= (leaf-hash node) hash) (funcall test (leaf-key node) key))
		(values (if (eq (leaf-value node) value) node (make-leaf hash key value)) nil))
	       ((= (leaf-hash node) hash)
		(values (make-cnode hash (list (cons key value) (cons (leaf-key node) (leaf-value node)))) t))
	       (t (values (merge-nodes node (make-leaf hash key value) shift) t))))
	((cnode-p node)
	 (if (= (cnode-hash node) hash)
	     (let ((e (assoc key (cnode-entries node) :test test)))
	       (values (make-cnode hash (acons key value (if e (remove e (cnode-entries node)) (cnode-entries node))))
		       (not e)))
	     (values (merge-nodes node (make-leaf hash key value) shift) t)))
	(t
	 (let* ((bit (ash 1 (chunk hash shift)))
		(bitmap (bnode-bitmap node))
		(i (logcount (logand bitmap (1- bit)))))
	   (if (zerop (logand bitmap bit))
	       (values (make-bnode (logior bitmap bit)
				   (vector-insert (bnode-children node) i (make-leaf hash key value)))
		       t)
	       (multiple-value-bind (child added)
		   (trie-set (svref (bnode-children node) i) hash key value test (+ shift 5))
		 (values (if (eq child (svref (bnode-children node) i))
			     node
			     (make-bnode bitmap (vector-replace (bnode-children node) i child)))
			 added)))))))

(defun trie-remove (node hash key test shift)
  "NODE without KEY (NIL if that leaves it empty); and whether KEY was there."
  (cond ((null node) (values nil nil))
	((leaf-p node)
	 (if (and (= (leaf-hash node) hash) (funcall test (leaf-key node) key))
	     (values nil t)
	     (values node nil)))
	((cnode-p node)
	 (let ((e (and (= (cnode-hash node) hash) (assoc key (cnode-entries node) :test test))))
	   (cond ((null e) (values node nil))
		 (t (let ((rest (remove e (cnode-entries node))))
		      (values (if (cdr rest)
				  (make-cnode hash rest)
				  (make-leaf hash (car (first rest)) (cdr (first rest))))
			      t))))))
	(t
	 (let* ((bit (ash 1 (chunk hash shift)))
		(bitmap (bnode-bitmap node))
		(i (logcount (logand bitmap (1- bit)))))
	   (if (zerop (logand bitmap bit))
	       (values node nil)
	       (multiple-value-bind (child removed)
		   (trie-remove (svref (bnode-children node) i) hash key test (+ shift 5))
		 (cond ((not removed) (values node nil))
		       (child (values (make-bnode bitmap (vector-replace (bnode-children node) i child)) t))
		       ((= (length (bnode-children node)) 1) (values nil t))
		       (t (let ((rest (make-bnode (logxor bitmap bit) (vector-remove (bnode-children node) i))))
			    ;; one leaf left below the root: it can move up
			    (values (if (and (plusp shift) (= (length (bnode-children rest)) 1)
					     (not (bnode-p (svref (bnode-children rest) 0))))
					(svref (bnode-children rest) 0)
					rest)
				    t))))))))))

(defun trie-walk (node f)
  (cond ((null node))
	((leaf-p node) (funcall f (leaf-key node) (leaf-value node)))
	((cnode-p node) (loop for (k . v) in (cnode-entries node) do (funcall f k v)))
	(t (loop for child across (bnode-children node) do (trie-walk child f)))))

(defun ihash-ref (h key)
  (let ((hash (funcall (kind-hash (ihash-kind h)) key)) (test (kind-test (ihash-kind h))))
    (let ((root (ihash-root h)))
      (if (bnode-p root) (trie-ref-bnode root hash key test 0) (trie-ref root hash key test)))))

(defun ihash-set (h key value)
  (multiple-value-bind (root added)
      (trie-set (ihash-root h) (funcall (kind-hash (ihash-kind h)) key) key value (kind-test (ihash-kind h)) 0)
    (if (eq root (ihash-root h))
	h
	(make-ihash (ihash-kind h) root (if added (1+ (ihash-count h)) (ihash-count h))))))

(defun ihash-remove (h key)
  (multiple-value-bind (root removed)
      (trie-remove (ihash-root h) (funcall (kind-hash (ihash-kind h)) key) key (kind-test (ihash-kind h)) 0)
    (if removed (make-ihash (ihash-kind h) root (1- (ihash-count h))) h)))

(defun ihash-entries (h)
  (or (ihash-snapshot h)
      (setf (ihash-snapshot h)
	    (let ((entries '()))
	      (trie-walk (ihash-root h) (lambda (k v) (push (cons k v) entries)))
	      (coerce (nreverse entries) 'simple-vector)))))

;;; ------------------------------------------------------------------
;;; Generic operations

(defun hash-table-p* (x) (or (rhash-p x) (ihash-p x)))

(defun check-hash (who x)
  (unless (hash-table-p* x)
    (racket-error who "contract violation~%  expected: hash?~%  given: ~A"
		  (with-output-to-string (s) (funcall (psx:host-ref "write") x s)))))

(defun hash-kind (h) (if (rhash-p h) (rhash-kind h) (ihash-kind h)))

(defun entries (h)
  (if (ihash-p h)
      (ihash-entries h)
      (or (rhash-snapshot h)
	  (setf (rhash-snapshot h)
		(let ((l '()))
		  (maphash (lambda (k v) (push (cons k v) l)) (rhash-table h))
		  (coerce (nreverse l) 'simple-vector))))))

(defun lookup* (h key)
  (if (rhash-p h) (gethash key (rhash-table h)) (ihash-ref h key)))

(defun fail-value (who key fail fail-p)
  (cond ((not fail-p) (racket-error who "no value found for key~%  key: ~A"
				    (with-output-to-string (s) (funcall (psx:host-ref "write") key s))))
	((functionp fail) (funcall fail))
	(t fail)))

(defun empty-ihash (kind) (make-ihash kind nil 0))

(defun ihash-from-pairs (kind pairs)
  (let ((h (empty-ihash kind)))
    (loop for (k v) on pairs by #'cddr do (setq h (ihash-set h k v)))
    h))

(defun ihash-from-assocs (kind assocs)
  (let ((h (empty-ihash kind)))
    (dolist (a assocs h) (setq h (ihash-set h (car a) (cdr a))))))

(defun rhash-from-assocs (kind assocs &optional weak)
  (let ((h (make-rhash kind weak)))
    (dolist (a assocs h) (setf (gethash (car a) (rhash-table h)) (cdr a)))))

;;; ------------------------------------------------------------------
;;; The primitives

(defnative "make-hash" (&optional (assocs '())) (:leaf t) (rhash-from-assocs :equal assocs))
(defnative "make-hasheqv" (&optional (assocs '())) (:leaf t) (rhash-from-assocs :eqv assocs))
(defnative "make-hasheq" (&optional (assocs '())) (:leaf t) (rhash-from-assocs :eq assocs))
(defnative "make-weak-hash" (&optional (assocs '())) (:leaf t) (rhash-from-assocs :equal assocs t))
(defnative "make-weak-hasheqv" (&optional (assocs '())) (:leaf t) (rhash-from-assocs :eqv assocs t))
(defnative "make-weak-hasheq" (&optional (assocs '())) (:leaf t) (rhash-from-assocs :eq assocs t))
(defnative "make-immutable-hash" (&optional (assocs '())) (:leaf t) (ihash-from-assocs :equal assocs))
(defnative "make-immutable-hasheqv" (&optional (assocs '())) (:leaf t) (ihash-from-assocs :eqv assocs))
(defnative "make-immutable-hasheq" (&optional (assocs '())) (:leaf t) (ihash-from-assocs :eq assocs))
(defnative "hash" (&rest keys-and-values) (:rumble "hash" :leaf t) (ihash-from-pairs :equal keys-and-values))
(defnative "hasheqv" (&rest keys-and-values) (:rumble "hasheqv" :leaf t) (ihash-from-pairs :eqv keys-and-values))
(defnative "hasheq" (&rest keys-and-values) (:rumble "hasheq" :leaf t) (ihash-from-pairs :eq keys-and-values))

(defnative "hash?" (x) (:leaf t) (bool (hash-table-p* x)))
(defnative "hash-eq?" (h) (:leaf t) (check-hash "hash-eq?" h) (bool (eq (hash-kind h) :eq)))
(defnative "hash-eqv?" (h) (:leaf t) (check-hash "hash-eqv?" h) (bool (eq (hash-kind h) :eqv)))
(defnative "hash-equal?" (h) (:leaf t) (check-hash "hash-equal?" h) (bool (eq (hash-kind h) :equal)))
(defnative "hash-weak?" (h) (:leaf t) (check-hash "hash-weak?" h) (bool (and (rhash-p h) (rhash-weak h))))
(defnative "hash-strong?" (h) (:leaf t) (check-hash "hash-strong?" h) (bool (not (and (rhash-p h) (rhash-weak h)))))
(defnative "hash-count" (h) (:leaf t)
  (check-hash "hash-count" h)
  (if (rhash-p h) (hash-table-count (rhash-table h)) (ihash-count h)))

(defnative "hash-ref" (h key &optional (fail nil fail-p)) (:rumble "hash-ref")
  (check-hash "hash-ref" h)
  (multiple-value-bind (v found) (lookup* h key)
    (if found v (fail-value "hash-ref" key fail fail-p))))

(defnative "hash-ref-key" (h key &optional (fail nil fail-p)) ()
  (check-hash "hash-ref-key" h)
  (let ((e (find key (entries h) :key #'car :test (kind-test (hash-kind h)))))
    (if e (car e) (fail-value "hash-ref-key" key fail fail-p))))

(defnative "hash-set!" (h key value) (:leaf t)
  (unless (rhash-p h)
    (racket-error "hash-set!" "contract violation~%  expected: (and/c hash? (not/c immutable?))"))
  (setf (gethash key (rhash-table h)) value (rhash-snapshot h) nil)
  *void*)

(defnative "hash-remove!" (h key) (:leaf t)
  (unless (rhash-p h)
    (racket-error "hash-remove!" "contract violation~%  expected: (and/c hash? (not/c immutable?))"))
  (remhash key (rhash-table h))
  (setf (rhash-snapshot h) nil)
  *void*)

(defnative "hash-clear!" (h) (:leaf t)
  (check-hash "hash-clear!" h)
  (when (rhash-p h) (clrhash (rhash-table h)) (setf (rhash-snapshot h) nil))
  *void*)

(defnative "hash-set" (h key value) (:rumble "hash-set" :leaf t)
  (unless (ihash-p h)
    (racket-error "hash-set" "contract violation~%  expected: (and/c hash? immutable?)"))
  (ihash-set h key value))

(defnative "hash-remove" (h key) (:leaf t)
  (unless (ihash-p h)
    (racket-error "hash-remove" "contract violation~%  expected: (and/c hash? immutable?)"))
  (ihash-remove h key))

(defnative "hash-copy" (h) (:leaf t)
  (check-hash "hash-copy" h)
  (rhash-from-assocs (hash-kind h) (coerce (entries h) 'list) (and (rhash-p h) (rhash-weak h))))

(defnative "hash-for-each" (h proc &optional try-order?) ()
  (declare (ignore try-order?))
  (check-hash "hash-for-each" h)
  (loop for (k . v) across (entries h) do (funcall proc k v))
  *void*)

(defnative "hash-map" (h proc &optional try-order?) ()
  (declare (ignore try-order?))
  (check-hash "hash-map" h)
  (loop for (k . v) across (entries h) collect (funcall proc k v)))

;;; Iteration: positions index the snapshot
(defun iterate-check (h i who)
  (let ((e (entries h)))
    (unless (and (integerp i) (< -1 i (length e)))
      (racket-error who "no element at index~%  index: ~A" i))
    (svref e i)))

(defnative "hash-iterate-first" (h) (:rumble "hash-iterate-first" :leaf t)
  (check-hash "hash-iterate-first" h)
  (if (plusp (length (entries h))) 0 ps:false))
(defnative "hash-iterate-next" (h i) (:leaf t)
  (if (< (1+ i) (length (entries h))) (1+ i) ps:false))
(defnative "hash-iterate-key" (h i &optional bad) (:leaf t)
  (declare (ignore bad))
  (car (iterate-check h i "hash-iterate-key")))
(defnative "hash-iterate-value" (h i &optional bad) (:leaf t)
  (declare (ignore bad))
  (cdr (iterate-check h i "hash-iterate-value")))
(defnative "hash-iterate-pair" (h i &optional bad) (:leaf t)
  (declare (ignore bad))
  (let ((e (iterate-check h i "hash-iterate-pair"))) (cons (car e) (cdr e))))
(defnative "hash-iterate-key+value" (h i &optional bad) (:leaf t)
  (declare (ignore bad))
  (let ((e (iterate-check h i "hash-iterate-key+value"))) (values (car e) (cdr e))))

(defnative "equal-hash-code" (x) (:leaf t) (racket-equal-hash x))
(defnative "eqv-hash-code" (x) (:leaf t) (eqv-hash x))
(defnative "eq-hash-code" (x) (:leaf t) (eq-hash x))

;;; equal-always? tables are equal? ones here (equal-always? differs only
;;; for mutable values, which equal? on our values compares by identity
;;; already where it matters most); ephemeron tables are weak ones.
(defnative "make-hashalw" (&optional (assocs '())) (:leaf t) (rhash-from-assocs :equal assocs))
(defnative "make-weak-hashalw" (&optional (assocs '())) (:leaf t) (rhash-from-assocs :equal assocs t))
(defnative "make-immutable-hashalw" (&optional (assocs '())) (:leaf t) (ihash-from-assocs :equal assocs))
(defnative "hashalw" (&rest kvs) (:leaf t) (ihash-from-pairs :equal kvs))
(defnative "hash-equal-always?" (h) (:leaf t) (declare (ignore h)) ps:false)
(defnative "make-ephemeron-hash" (&optional (assocs '())) (:leaf t) (rhash-from-assocs :equal assocs t))
(defnative "make-ephemeron-hasheq" (&optional (assocs '())) (:leaf t) (rhash-from-assocs :eq assocs t))
(defnative "make-ephemeron-hasheqv" (&optional (assocs '())) (:leaf t) (rhash-from-assocs :eqv assocs t))
(defnative "make-ephemeron-hashalw" (&optional (assocs '())) (:leaf t) (rhash-from-assocs :equal assocs t))
(defnative "hash-ephemeron?" (h) (:leaf t) (bool (and (rhash-p h) (rhash-weak h))))
(defnative "hash-keys-subset?" (a b) (:leaf t)
  (bool (loop for (k) across (entries a) always (nth-value 1 (lookup* b k)))))

;;; Unsafe immutable-hash iteration: the same positions
(defnative "unsafe-immutable-hash-iterate-first" (h) (:instance "#%unsafe" :leaf t)
  (if (plusp (length (entries h))) 0 ps:false))
(defnative "unsafe-immutable-hash-iterate-next" (h i) (:instance "#%unsafe" :leaf t)
  (if (< (1+ i) (length (entries h))) (1+ i) ps:false))
(defnative "unsafe-immutable-hash-iterate-key" (h i) (:instance "#%unsafe" :leaf t)
  (car (svref (entries h) i)))
(defnative "unsafe-immutable-hash-iterate-value" (h i) (:instance "#%unsafe" :leaf t)
  (cdr (svref (entries h) i)))
(defnative "unsafe-immutable-hash-iterate-key+value" (h i) (:instance "#%unsafe" :leaf t)
  (let ((e (svref (entries h) i))) (values (car e) (cdr e))))
(defnative "unsafe-immutable-hash-iterate-pair" (h i) (:instance "#%unsafe" :leaf t)
  (let ((e (svref (entries h) i))) (cons (car e) (cdr e))))
