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

;; Universes are shared by symbol list, so that two evaluations of
;; (file-options ...) or of one define-enumeration make compatible sets.
(defvar *universes* (trivial-garbage:make-weak-hash-table :test 'equal :weakness :value))

(defprim "make-enumeration" (symbols)
  (let* ((distinct (remove-duplicates symbols :from-end t))
	 (universe (or (gethash distinct *universes*)
		       (setf (gethash distinct *universes*)
			     (make-universe (coerce distinct 'simple-vector))))))
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
    (bool (and i (logbitp i (enum-set-mask set))))))

(defun enum-subset-p (a b)
  (let ((ua (universe-symbols (enum-set-universe a)))
	(ub (enum-set-universe b)))
    (and (every (lambda (s) (enum-index ub s)) ua)
	 (every (lambda (s) (let ((i (enum-index ub s))) (and i (logbitp i (enum-set-mask b)))))
		(enum-members a)))))

(defprim "enum-set-subset?" (a b)
  (check-enum "enum-set-subset?" a) (check-enum "enum-set-subset?" b)
  (bool (enum-subset-p a b)))

(defprim "enum-set=?" (a b)
  (check-enum "enum-set=?" a) (check-enum "enum-set=?" b)
  (bool (and (enum-subset-p a b) (enum-subset-p b a))))

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

;;; With cl-unicode; CL chars are taken to be code points.

(defun code-points->string (code-points)
  (map 'simple-string #'code-char code-points))

(defun unicode-category-name (c)
  "C's general category as a string: \"Lu\", \"Ll\", ..."
  (values (cl-unicode:general-category c)))

(defun cased-p (c)
  (member (unicode-category-name c) '("Lu" "Ll" "Lt") :test #'string=))

(defun case-ignorable-p (c)
  "Unicode's Case_Ignorable: marks, format characters, modifiers, and
the word-internal punctuation (MidLetter, MidNumLet, Single_Quote)."
  (or (member (unicode-category-name c) '("Mn" "Me" "Cf" "Lm" "Sk") :test #'string=)
      (find c "'.:·’‘﹒．：")))

(defun final-sigma-p (s i)
  "Is the capital sigma at I in S word-final (Unicode 3.13 Final_Sigma):
after a cased letter and not before one, skipping case-ignorables?
cl-unicode leaves conditional mappings to its caller."
  (flet ((cased-from (start step)
	   (loop for j = start then (+ j step)
		 while (< -1 j (length s))
		 unless (case-ignorable-p (char s j))
		   return (cased-p (char s j)))))
    (and (cased-from (1- i) -1) (not (cased-from (1+ i) 1)))))

(defun lowercase-at (s i)
  (if (and (char= (char s i) (code-char #x3A3)) (final-sigma-p s i))
      (list #x3C2)
      (cl-unicode:lowercase-mapping (char s i) :want-special-p t)))

(defun full-case-map (s position)
  "Map string S through the full (SpecialCasing) lowercase (POSITION 0)
or uppercase (1) mappings."
  (code-points->string
   (loop for c across s
	 for i from 0
	 append (if (= position 0)
		    (lowercase-at s i)
		    (cl-unicode:uppercase-mapping c :want-special-p t)))))

(defun word-constituent-p (s i)
  "Does the char at I continue a word: a letter, a digit, or an
apostrophe or the like between letters (Unicode word breaks, roughly)."
  (let ((c (char s i)))
    (or (alphanumericp c)
	(and (find c "'.:’·") (> i 0) (< (1+ i) (length s))
	     (alpha-char-p (char s (1- i))) (alpha-char-p (char s (1+ i)))))))

(defun full-titlecase (s)
  (code-points->string
   (loop with in-word = nil
	 for c across s
	 for i from 0
	 append (cond ((not (word-constituent-p s i))
		       (setf in-word nil)
		       (list (char-code c)))
		      (in-word (lowercase-at s i))
		      ((alpha-char-p c)
		       (setf in-word t)
		       (cl-unicode:titlecase-mapping c :want-special-p t))
		      (t (list (char-code c)))))))

(defun full-casefold (s)
  (code-points->string
   (cl-unicode:case-fold-mapping (map 'list #'char-code s) :want-code-point-p t)))

(defprim "char-titlecase" (c)
  (cl-unicode:titlecase-mapping c))

(defprim "char-title-case?" (c)
  (bool (string= (unicode-category-name c) "Lt")))

(defprim "char-general-category" (c)
  (ps:intern-scheme-symbol (unicode-category-name c)))

(defprim "string-titlecase" (s) (full-titlecase s))

(macrolet ((def-normalize (name fn)
	     `(defprim ,name (s) (code-points->string (,fn s)))))
  (def-normalize "string-normalize-nfd" cl-unicode:normalization-form-d)
  (def-normalize "string-normalize-nfkd" cl-unicode:normalization-form-k-d)
  (def-normalize "string-normalize-nfc" cl-unicode:normalization-form-c)
  (def-normalize "string-normalize-nfkc" cl-unicode:normalization-form-k-c))

;;; Full Unicode case mapping (1.2): string-upcase etc. map whole
;;; strings, so (string-upcase "ßa") => "SSA"; the R7RS layer's are
;;; character by character.

(defprim "string-upcase" (s) (full-case-map s 1))
(defprim "string-downcase" (s) (full-case-map s 0))
(defprim "string-foldcase" (s) (full-casefold s))
(defprim "char-foldcase" (c)
  (let ((s (full-casefold (string c))))
    (if (= (length s) 1) (char s 0) c)))
