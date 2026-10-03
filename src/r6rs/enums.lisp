; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-R6RS -*-

;;;; (rnrs enums (6)), library report chapter 14, and the Unicode
;;;; procedures of (rnrs unicode (6)) missing from the R7RS layer
;;;; (chapter 1).
;;;;
;;;; An enum set is a universe -- a vector of distinct symbols, shared by
;;;; every set made from it -- and a bitmask of members.

(in-package "PSEUDOSCHEME-R6RS")

(defstruct (universe (:constructor make-universe (symbols)))
  symbols)			; simple-vector

(defstruct (enum-set (:constructor make-enum-set (universe mask)))
  universe mask)

(defmethod print-object ((s enum-set) stream)
  (print-unreadable-object (s stream)
    (format stream "Enum-set~{ ~A~}" (mapcar #'ps:scheme-symbol-name (enum-members s)))))

(defun enum-members (set)
  (let ((symbols (universe-symbols (enum-set-universe set))))
    (loop for i from 0 below (length symbols)
	  when (logbitp i (enum-set-mask set)) collect (svref symbols i))))

(defun check-enum (who x)
  (unless (enum-set-p x) (r6rs-assertion-violation who "not an enum set" x)))

(defun enum-index (universe symbol)
  (position symbol (universe-symbols universe)))

(defprim "make-enumeration" (symbols)
  (let* ((distinct (remove-duplicates symbols :from-end t))
	 (universe (make-universe (coerce distinct 'simple-vector))))
    (make-enum-set universe (1- (ash 1 (length distinct))))))

(defprim "enum-set-universe" (set)
  (check-enum "enum-set-universe" set)
  (let ((u (enum-set-universe set)))
    (make-enum-set u (1- (ash 1 (length (universe-symbols u)))))))

(defprim "enum-set-indexer" (set)
  (check-enum "enum-set-indexer" set)
  (let ((u (enum-set-universe set)))
    (lambda (symbol) (or (enum-index u symbol) ps:false))))

(defprim "enum-set-constructor" (set)
  (check-enum "enum-set-constructor" set)
  (let ((u (enum-set-universe set)))
    (lambda (symbols)
      (make-enum-set u (loop with mask = 0
			     for s in symbols
			     for i = (or (enum-index u s)
					 (r6rs-assertion-violation "enum-set-constructor" "not in the universe" s))
			     do (setf mask (logior mask (ash 1 i)))
			     finally (return mask))))))

(defprim "enum-set->list" (set) (check-enum "enum-set->list" set) (enum-members set))

(defprim "enum-set-member?" (symbol set)
  (check-enum "enum-set-member?" set)
  (let ((i (enum-index (enum-set-universe set) symbol)))
    (ps:true? (and i (logbitp i (enum-set-mask set))))))

(defun enum-subset-p (a b)
  (let ((ua (universe-symbols (enum-set-universe a)))
	(ub (enum-set-universe b)))
    (and (every (lambda (s) (enum-index ub s)) ua)
	 (every (lambda (s) (let ((i (enum-index ub s))) (and i (logbitp i (enum-set-mask b)))))
		(enum-members a)))))

(defprim "enum-set-subset?" (a b)
  (check-enum "enum-set-subset?" a) (check-enum "enum-set-subset?" b)
  (ps:true? (enum-subset-p a b)))

(defprim "enum-set=?" (a b)
  (check-enum "enum-set=?" a) (check-enum "enum-set=?" b)
  (ps:true? (and (enum-subset-p a b) (enum-subset-p b a))))

(defmacro def-enum-op (name op)
  `(defprim ,name (a b)
     (check-enum ,name a) (check-enum ,name b)
     (unless (eq (enum-set-universe a) (enum-set-universe b))
       (r6rs-assertion-violation ,name "enum sets have different universes" a b))
     (make-enum-set (enum-set-universe a) (,op (enum-set-mask a) (enum-set-mask b)))))

(def-enum-op "enum-set-union" logior)
(def-enum-op "enum-set-intersection" logand)
(def-enum-op "enum-set-difference" logandc2)

(defprim "enum-set-complement" (set)
  (check-enum "enum-set-complement" set)
  (let ((u (enum-set-universe set)))
    (make-enum-set u (logandc2 (1- (ash 1 (length (universe-symbols u)))) (enum-set-mask set)))))

(defprim "enum-set-projection" (a b)
  (check-enum "enum-set-projection" a) (check-enum "enum-set-projection" b)
  (let ((ub (enum-set-universe b)))
    (make-enum-set ub (loop with mask = 0
			    for s in (enum-members a)
			    for i = (enum-index ub s)
			    when i do (setf mask (logior mask (ash 1 i)))
			    finally (return mask)))))

;;; ------------------------------------------------------------------
;;; Unicode (1.1, 1.2)

(defprim "char-titlecase" (c)
  #+sbcl (let ((s (sb-unicode:titlecase (string c)))) (if (= (length s) 1) (char s 0) c))
  #-sbcl (char-upcase c))

(defprim "char-title-case?" (c)
  #+sbcl (ps:true? (eq (sb-unicode:general-category c) :lt))
  #-sbcl (progn c ps:false))

(defprim "char-general-category" (c)
  (ps:intern-scheme-symbol
   #+sbcl (let ((cat (symbol-name (sb-unicode:general-category c))))
	    ;; :LU -> Lu
	    (concatenate 'string (string (char cat 0)) (string-downcase (subseq cat 1))))
   #-sbcl (cond ((upper-case-p c) "Lu") ((lower-case-p c) "Ll") ((digit-char-p c) "Nd") (t "Cn"))))

(defprim "string-titlecase" (s)
  #+sbcl (coerce (sb-unicode:titlecase s) 'simple-string)
  #-sbcl (string-capitalize s))

(macrolet ((def-normalize (name form)
	     `(defprim ,name (s)
		#+sbcl (coerce (sb-unicode:normalize-string s ,form) 'simple-string)
		#-sbcl s)))
  (def-normalize "string-normalize-nfd" :nfd)
  (def-normalize "string-normalize-nfkd" :nfkd)
  (def-normalize "string-normalize-nfc" :nfc)
  (def-normalize "string-normalize-nfkc" :nfkc))

;;; Full Unicode case mapping (1.2): string-upcase etc. map whole
;;; strings, so (string-upcase "ßa") => "SSA"; the R7RS layer's are
;;; character by character.

(defprim "string-upcase" (s) #+sbcl (coerce (sb-unicode:uppercase s) 'simple-string) #-sbcl (string-upcase s))
(defprim "string-downcase" (s) #+sbcl (coerce (sb-unicode:lowercase s) 'simple-string) #-sbcl (string-downcase s))
(defprim "string-foldcase" (s) #+sbcl (coerce (sb-unicode:casefold s) 'simple-string) #-sbcl (string-downcase s))
(defprim "char-foldcase" (c)
  #+sbcl (let ((s (sb-unicode:casefold (string c)))) (if (= (length s) 1) (char s 0) c))
  #-sbcl (char-downcase c))
