; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-R6RS -*-

;;;; Guardians, Chez Scheme's and Guile's: an object registered with a
;;;; guardian comes back from it (or a representative does) once nothing
;;;; but guardians references it.
;;;;
;;;; SBCL can't give back an object it has collected, so a guardian holds
;;;; its objects, and asks SBCL's own heap tracer (SB-EXT:SEARCH-ROOTS)
;;;; which of them are reachable other than through guardians.  That
;;;; walks the whole heap, so it is done at most once per full collection
;;;; that Scheme asked for (Chez's collect, Guile's gc: NOTE-FULL-COLLECTION),
;;;; the next time a guardian is asked for an object -- not after each of
;;;; SBCL's own collections.  An object a stale slot of the stack still
;;;; points to (SBCL scans the stack conservatively) stays reachable, as
;;;; under a conservative collector.
;;;;
;;;; Guardians here are unordered: an object reachable only from another
;;;; guarded object comes back in the same round.  Elsewhere than SBCL,
;;;; nothing comes back.

(in-package "PSEUDOSCHEME-R6RS")

(defstruct (guardian (:constructor %make-guardian (ordered)))
  ordered
  (entries '())		; ((object representative collections) ...), newest first:
			; COLLECTIONS, *FULL-COLLECTIONS* when it was registered
  (ready '()))		; representatives found inaccessible, to give back

(defvar *guardians* '() "Weak pointers to the guardians.")
(defvar *guardian-lock* (sb-thread:make-mutex :name "guardians"))
(defvar *full-collections* 0 "Full collections Scheme asked for.")
(defvar *guardians-checked-at* 0 "*FULL-COLLECTIONS* when guardians were last checked.")

(defun note-full-collection ()
  "After a full collection that Scheme asked for: guardians look for
inaccessible objects the next time they're asked."
  (sb-thread:with-mutex (*guardian-lock*)
    (incf *full-collections*)))

(defun new-guardian (&optional ordered)
  (let ((g (%make-guardian ordered)))
    (sb-thread:with-mutex (*guardian-lock*)
      (push (sb-ext:make-weak-pointer g) *guardians*))
    g))

(defun heap-object-p (x)
  "Can X be collected (it isn't a fixnum, character, or the like)?"
  (not (or (typep x 'fixnum) (characterp x) (typep x 'single-float))))

(defun guardian-register (g object &optional (representative object))
  (sb-thread:with-mutex (*guardian-lock*)
    (push (list object representative *full-collections*) (guardian-entries g)))
  ps:unspecific)

(defun live-guardians ()
  (setq *guardians* (delete-if-not #'sb-ext:weak-pointer-value *guardians*))
  (mapcar #'sb-ext:weak-pointer-value *guardians*))

(declaim (notinline guarded-weak-pointers))
(defun candidate-p (entry)
  "Could ENTRY's object have become inaccessible: is it a heap object,
registered before the last full collection?"
  (and (heap-object-p (first entry)) (< (third entry) *full-collections*)))

(defun guarded-weak-pointers (guardians)
  "A weak pointer to each candidate object the GUARDIANS hold."
  (let ((pointers '()))
    (dolist (g guardians pointers)
      (dolist (entry (guardian-entries g))
	(when (candidate-p entry)
	  (push (sb-ext:make-weak-pointer (first entry)) pointers))))))

(defun find-inaccessible-objects ()
  "Move the entries of objects that only guardians reach to their
guardians' ready lists."
  (let* ((guardians (live-guardians))
	 (pointers (guarded-weak-pointers guardians)))
    (when pointers
      ;; the stack below us may still hold an object we just went past
      (sb-sys:scrub-control-stack)
      (let* ((ignore (append guardians
			     ;; the lists and entries holding the objects
			     (loop for g in guardians
				   append (loop for tail on (guardian-entries g)
						collect tail
						append (loop for cell on (car tail) collect cell)))))
	     (paths (sb-ext:search-roots pointers :criterion :static :ignore ignore :print nil))
	     (reachable (make-hash-table :test 'eq)))
	(dolist (path paths)
	  (setf (gethash (car path) reachable) t))
	(dolist (g guardians)
	  (let ((kept '()) (dead '()))
	    (dolist (entry (guardian-entries g))
	      (if (or (not (candidate-p entry)) (gethash (first entry) reachable))
		  (push entry kept)
		  (push (second entry) dead)))
	    ;; entries stay newest first; the ready, oldest first
	    (setf (guardian-entries g) (nreverse kept)
		  (guardian-ready g) (append (guardian-ready g) dead))))))))

(defun guardian-next (g)
  "The representative of an object G guards that has become inaccessible,
or false."
  (sb-thread:with-mutex (*guardian-lock*)
    (when (/= *guardians-checked-at* *full-collections*)
      (setq *guardians-checked-at* *full-collections*)
      (find-inaccessible-objects))
    (if (guardian-ready g) (pop (guardian-ready g)) ps:false)))

(defun guardian-unregister (g)
  "Stop G guarding its objects: a list of them (not yet inaccessible)."
  (sb-thread:with-mutex (*guardian-lock*)
    (prog1 (mapcar #'first (guardian-entries g))
      (setf (guardian-entries g) '()))))
