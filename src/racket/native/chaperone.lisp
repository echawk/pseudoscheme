; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-RACKET -*-

;;;; Chaperones and impersonators of data, natively (Racket CS's
;;;; rumble/impersonator.ss, hash.ss and box.ss), and the primitives of
;;;; other files that must see them.  This file loads after all the
;;;; others: its definitions of a name replace theirs.
;;;;
;;;; Every chaperone and impersonator has an IMPERSONATION (procedures.lisp:
;;;; its kind, what it wraps, its impersonator properties) in
;;;; *IMPERSONATORS*, so chaperone?, impersonator-of? and impersonator
;;;; properties work alike for all of them.  What differs is how each
;;;; kind of data is wrapped:
;;;;   struct   a CSTRUCT (structs.lisp), sharing the fields of the instance
;;;;            under it; the accessors and mutators ask its layers
;;;;   box      a CBOX, which is a box (an RBOX) whose own value isn't used
;;;;   hash     a CHASH (hash.lisp)
;;;;   procedure  a new procedure (procedures.lisp)
;;;; Not yet: a chaperone of a vector is the vector itself: vector-ref and
;;;; the rest are the host's (*host-shared*), which open-codes them and
;;;; so can't see a wrapper.

(in-package "QUASISCHEME-RACKET")

;;; ------------------------------------------------------------------
;;; Boxes

(defstruct (cbox (:include rbox) (:constructor make-cbox (of kind unbox-proc set-proc)))
  of kind unbox-proc set-proc)

(defun cbox-ref (b orig)
  (if (cbox-p b)
      (let ((v (cbox-ref (cbox-of b) orig)))
	(checked-chaperone "unbox" (cbox-kind b) "result" v (funcall (cbox-unbox-proc b) orig v)))
      (rbox-value b)))

(defun cbox-set (b v orig)
  (if (cbox-p b)
      (cbox-set (cbox-of b)
		(checked-chaperone "set-box!" (cbox-kind b) "result" v (funcall (cbox-set-proc b) orig v))
		orig)
      (setf (rbox-value b) v)))

(defun box-ref* (b)
  "The value in B, through its chaperones."
  (if (cbox-p b) (cbox-ref b b) (rbox-value b)))

(defun box-base (b)
  (loop while (cbox-p b) do (setq b (cbox-of b)))
  b)

(defnative "unbox" (b) (:leaf t)
  (cond ((cbox-p b) (cbox-ref b b))
	((rbox-p b) (rbox-value b))
	(t (arg-error "unbox" "box?" b))))
(defnative "set-box!" (b x) (:leaf t)
  (cond ((and (rbox-p b) (immutable-p (box-base b)))
	 (arg-error "set-box!" "(and/c box? (not/c immutable?))" b))
	((cbox-p b) (cbox-set b x b))
	((rbox-p b) (setf (rbox-value b) x))
	(t (arg-error "set-box!" "(and/c box? (not/c immutable?))" b)))
  *void*)
(defnative "unsafe-unbox" (b) (:instance "#%unsafe" :leaf t)
  (if (cbox-p b) (cbox-ref b b) (rbox-value b)))
(defnative "unsafe-set-box!" (b x) (:instance "#%unsafe" :leaf t)
  (if (cbox-p b) (cbox-set b x b) (setf (rbox-value b) x))
  *void*)
;;; the starred ones ignore chaperones
(defnative "unbox*" (b) (:leaf t) (rbox-value (box-base b)))
(defnative "set-box*!" (b x) (:leaf t) (setf (rbox-value (box-base b)) x) *void*)
(defnative "unsafe-unbox*" (b) (:instance "#%unsafe" :leaf t) (rbox-value (box-base b)))
(defnative "unsafe-set-box*!" (b x) (:instance "#%unsafe" :leaf t) (setf (rbox-value (box-base b)) x) *void*)

(defun check-redirect (who proc arity what)
  (unless (and (functionp proc) (logbitp arity (arity-mask proc)))
    (arg-error who (format nil "(procedure-arity-includes/c ~D)" arity) proc))
  (values proc what))

(defun split-properties (who args)
  "ARGS, impersonator properties and their values: an alist."
  (loop for tail = args then (cddr tail)
	while tail
	do (unless (impersonator-property-p (car tail))
	     (arg-error who "impersonator-property?" (car tail)))
	   (unless (cdr tail)
	     (racket-error who "missing value argument after an imperonsonator-property argument~%  impersonator property: ~A"
			   (written (car tail))))
	collect (cons (car tail) (cadr tail))))

(defun make-box-chaperone (who kind b unbox-proc set-proc props)
  ;; a chaperone may wrap an immutable box; an impersonator, not
  (unless (and (rbox-p b) (or (eq kind :chaperone) (not (immutable-p (box-base b)))))
    (arg-error who (if (eq kind :chaperone) "box?" "(and/c box? (not/c immutable?))") b))
  (check-redirect who unbox-proc 2 "unbox-proc")
  (check-redirect who set-proc 2 "set-proc")
  (let ((new (make-cbox b kind unbox-proc set-proc)))
    (setf (gethash new *impersonators*) (make-impersonation kind b (split-properties who props)))
    new))

(defnative "chaperone-box" (b unbox-proc set-proc &rest props) ()
  (make-box-chaperone "chaperone-box" :chaperone b unbox-proc set-proc props))
(defnative "impersonate-box" (b unbox-proc set-proc &rest props) ()
  (make-box-chaperone "impersonate-box" :impersonator b unbox-proc set-proc props))

;;; ------------------------------------------------------------------
;;; Hash tables

(defun make-hash-chaperone (who kind h args)
  "chaperone-hash's and impersonate-hash's: H, then ref-proc, set-proc,
remove-proc and key-proc, an optional clear-proc and equal-key-proc, and
properties."
  (unless (hash-table-p* h) (arg-error who "hash?" h))
  (unless (>= (length args) 4)
    (racket-error who "arity mismatch;~% the expected number of arguments does not match the given number"))
  (destructuring-bind (ref-proc set-proc remove-proc key-proc &rest more) args
    (check-redirect who ref-proc 2 "ref-proc")
    (check-redirect who set-proc 3 "set-proc")
    (check-redirect who remove-proc 2 "remove-proc")
    (check-redirect who key-proc 2 "key-proc")
    (let ((clear-proc qs:false))
      (when (and more (not (impersonator-property-p (car more))))
	(setq clear-proc (pop more))
	(when (truthy clear-proc) (check-redirect who clear-proc 1 "clear-proc"))
	;; the equal-key-proc that may follow: keys already go through
	;; key-proc, so it only has its arity checked
	(when (and more (not (impersonator-property-p (car more))))
	  (let ((equal-key-proc (pop more)))
	    (when (truthy equal-key-proc) (check-redirect who equal-key-proc 2 "equal-key-proc")))))
      (let ((new (make-chash h kind ref-proc set-proc remove-proc key-proc clear-proc)))
	(setf (gethash new *impersonators*) (make-impersonation kind h (split-properties who more)))
	new))))

(defnative "chaperone-hash" (h &rest args) ()
  (make-hash-chaperone "chaperone-hash" :chaperone h args))
(defnative "impersonate-hash" (h &rest args) ()
  (make-hash-chaperone "impersonate-hash" :impersonator h args))

(defnative "immutable-hash?" (x) (:leaf t) (bool (and (hash-table-p* x) (hash-immutable-p x))))
(defnative "mutable-hash?" (x) (:leaf t) (bool (and (hash-table-p* x) (hash-mutable-p x))))
(defnative "hash-clear" (h) ()
  (unless (and (hash-table-p* h) (hash-immutable-p h))
    (arg-error "hash-clear" "(and/c hash? immutable?)" h))
  (hash-clear* h h))
(defnative "immutable?" (x) (:leaf t)
  (bool (or (ihash-p x) (and (chash-p x) (hash-immutable-p x))
	    (and (rbox-p x) (immutable-p (box-base x)))
	    (and (not (rbox-p x)) (immutable-p x)))))

;;; ------------------------------------------------------------------
;;; Structs

(defnative "chaperone-struct" (v &rest args) ()
  (make-struct-chaperone "chaperone-struct" t v args))
(defnative "impersonate-struct" (v &rest args) ()
  (make-struct-chaperone "impersonate-struct" nil v args))

;;; ------------------------------------------------------------------
;;; Vectors, and the other kinds of value there is nothing to wrap
;;; (the chaperone is the value; see above)

(macrolet ((same (&rest names)
	     `(progn ,@(loop for name in names
			     collect `(defnative ,name (v &rest args) (:leaf t)
					(declare (ignore args))
					v)))))
  (same "chaperone-vector" "impersonate-vector" "chaperone-vector*" "impersonate-vector*"))

(macrolet ((same (&rest names)
	     `(progn ,@(loop for name in names
			     collect `(defnative ,name (v &rest args) (:instance "#%unsafe" :leaf t)
					(declare (ignore args))
					v)))))
  (same "unsafe-chaperone-vector" "unsafe-impersonate-vector"))

(defnative "unsafe-impersonate-hash" (h &rest args) (:instance "#%unsafe")
  (apply (native-function (gethash "impersonate-hash" *natives*)) h args))
(defnative "unsafe-chaperone-hash" (h &rest args) (:instance "#%unsafe")
  (apply (native-function (gethash "chaperone-hash" *natives*)) h args))

;;; ------------------------------------------------------------------
;;; Arities: a struct's incomplete arity (a keyword procedure that has
;;; required keywords) doesn't count unless the caller says it may

(defvar *prop-incomplete-arity* nil)

(defun incomplete-arity-p (f)
  (and (struct-instance-p f)
       (let ((prop (or *prop-incomplete-arity*
		       (setq *prop-incomplete-arity* (primitive "prop:incomplete-arity")))))
	 (nth-value 1 (property-value f prop)))))

(defun procedure-mask (who f)
  (unless (functionp f) (arg-error who "procedure?" f))
  (or (ignore-errors (arity-mask f)) -1))

(defnative "procedure-arity-includes?" (f k &optional (incomplete-ok? qs:false)) (:leaf t)
  (let ((mask (procedure-mask "procedure-arity-includes?" f)))
    (unless (and (integerp k) (>= k 0)) (arg-error "procedure-arity-includes?" "exact-nonnegative-integer?" k))
    (bool (and (or (truthy incomplete-ok?) (not (incomplete-arity-p f)))
	       (logbitp k mask)))))
(defnative "procedure-arity-mask" (f) (:leaf t) (procedure-mask "procedure-arity-mask" f))
(defnative "procedure-arity" (f) (:leaf t) (mask->arity (procedure-mask "procedure-arity" f)))
