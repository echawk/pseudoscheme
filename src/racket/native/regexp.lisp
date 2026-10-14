; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

;;;; Regular expressions, natively, on CL-PPCRE: strategy B's stand-in for
;;;; Racket's regexp layer, until that layer's linklet can be extracted
;;;; and compiled (docs/racket-linklet-compiler.md, milestone 2), which
;;;; will replace this.
;;;;
;;;; A regexp is an RREGEXP: its source (a string or bytes), whether it is
;;;; a pregexp, whether it is a byte regexp, and the CL-PPCRE scanner.
;;;; Byte strings are matched as Latin-1 strings, so a byte is a
;;;; character; a byte regexp's results are bytes, a string regexp's
;;;; strings.  Racket's syntax is Perl's in most things; what differs is
;;;; translated (TRANSLATE-REGEXP):
;;;;   rx: \ before a letter or digit is that character (no \d, \w,
;;;;       backreferences); px: Perl's classes and \N backreferences.
;;;;   both: (?i: ...) and the rest of Perl's groups; [[:alpha:]] in px.
;;;; Not yet: \p{..} properties, (?mode:...) beyond i and s, ports as
;;;; inputs, regexp-match with an output prefix, lookbehind limits.

(in-package "PSEUDOSCHEME-RACKET")

(defstruct (rregexp (:constructor %make-rregexp (source px bytes scanner)))
  source px bytes scanner)

(defmethod print-object ((r rregexp) stream)
  (format stream "#~A~A~S" (if (rregexp-px r) "px" "rx") (if (rregexp-bytes r) "#" "")
	  (if (rregexp-bytes r) (map 'string #'code-char (rregexp-source r)) (rregexp-source r))))

(defun latin-1 (bytes) (map 'string #'code-char bytes))

(defun translate-regexp (source px)
  "SOURCE (a string) in CL-PPCRE's syntax."
  (if px
      source
      (with-output-to-string (out)
	(loop with i = 0
	      while (< i (length source))
	      do (let ((c (char source i)))
		   (cond ((and (char= c #\\) (< (1+ i) (length source))
			       (alphanumericp (char source (1+ i))))
			  ;; rx: \a is a
			  (write-char (char source (1+ i)) out)
			  (incf i 2))
			 ((and (char= c #\\) (< (1+ i) (length source)))
			  (write-char c out) (write-char (char source (1+ i)) out)
			  (incf i 2))
			 (t (write-char c out) (incf i))))))))

(defun make-rregexp (source px)
  (let* ((bytes (typep source '(simple-array (unsigned-byte 8) (*))))
	 (text (if bytes (latin-1 source) source)))
    (%make-rregexp source px bytes
		   (handler-case (cl-ppcre:create-scanner (translate-regexp text px))
		     (cl-ppcre:ppcre-syntax-error (e)
		       (racket-error (if px "pregexp" "regexp") "~A" e))))))

(defvar *regexp-cache* (make-hash-table :test 'equalp :synchronized t)
  "A string or bytes used as a pattern -> its regexp.")

(defun as-regexp (pattern)
  (cond ((rregexp-p pattern) pattern)
	((regexp-literal-p pattern)
	 ;; a #rx or #px literal, compiled once
	 (or (gethash pattern *regexp-cache*)
	     (setf (gethash pattern *regexp-cache*)
		   (make-rregexp (regexp-literal-source pattern) (eq (regexp-literal-kind pattern) :px)))))
	((or (stringp pattern) (typep pattern '(simple-array (unsigned-byte 8) (*))))
	 (or (gethash pattern *regexp-cache*)
	     (setf (gethash pattern *regexp-cache*) (make-rregexp pattern nil))))
	(t (racket-error "regexp-match" "contract violation~%  expected: (or/c regexp? byte-regexp? string? bytes?)~%  given: ~A" (written pattern)))))

(defnative "regexp" (s &optional handler) (:leaf t) (declare (ignore handler)) (make-rregexp s nil))
(defnative "pregexp" (s &optional handler) (:leaf t) (declare (ignore handler)) (make-rregexp s t))
(defnative "byte-regexp" (b &optional handler) (:leaf t) (declare (ignore handler)) (make-rregexp b nil))
(defnative "byte-pregexp" (b &optional handler) (:leaf t) (declare (ignore handler)) (make-rregexp b t))
(defun regexp-of (x)
  "X as a regexp, if it is one (a #rx or #px literal is)."
  (cond ((rregexp-p x) x) ((regexp-literal-p x) (as-regexp x))))

(defnative "regexp?" (x) (:leaf t)
  (let ((r (regexp-of x))) (bool (and r (not (rregexp-bytes r))))))
(defnative "pregexp?" (x) (:leaf t)
  (let ((r (regexp-of x))) (bool (and r (rregexp-px r) (not (rregexp-bytes r))))))
(defnative "byte-regexp?" (x) (:leaf t)
  (let ((r (regexp-of x))) (bool (and r (rregexp-bytes r)))))
(defnative "byte-pregexp?" (x) (:leaf t)
  (let ((r (regexp-of x))) (bool (and r (rregexp-px r) (rregexp-bytes r)))))

(defun input-text (input)
  "INPUT (a string, bytes or path) as a string to match, and whether its
results are bytes."
  (cond ((stringp input) (values input nil))
	((typep input '(simple-array (unsigned-byte 8) (*))) (values (latin-1 input) t))
	((rpath-p input) (values (rpath-string input) t))
	(t (racket-error "regexp-match" "contract violation~%  expected: (or/c string? bytes? path?)~%  given: ~A"
			 (written input)))))

(defun match-positions (pattern input start end)
  "The match of PATTERN in INPUT from START to END: the regexp, the text,
whether results are bytes, and the start and end of the match and its
groups (vectors), or NIL."
  (let ((r (as-regexp pattern)))
    (multiple-value-bind (text input-bytes) (input-text input)
      (let ((end (if (and end (truthy end)) end (length text))))
	(multiple-value-bind (s e gs ge) (cl-ppcre:scan (rregexp-scanner r) text :start start :end end)
	  (values r text (or (rregexp-bytes r) input-bytes) s e gs ge))))))

(defun result-substring (text bytes s e)
  (if (and s e)
      (if bytes (map '(simple-array (unsigned-byte 8) (*)) #'char-code (subseq text s e)) (subseq text s e))
      ps:false))

(defnative "regexp-match" (pattern input &optional (start 0) (end nil) out prefix) ()
  (declare (ignore out prefix))
  (multiple-value-bind (r text bytes s e gs ge) (match-positions pattern input start end)
    (declare (ignore r))
    (if s
	(cons (result-substring text bytes s e)
	      (loop for g across gs for h across ge collect (result-substring text bytes g h)))
	ps:false)))

(defnative "regexp-match?" (pattern input &optional (start 0) (end nil) out prefix) ()
  (declare (ignore out prefix))
  (bool (nth-value 3 (match-positions pattern input start end))))

(defnative "regexp-match-positions" (pattern input &optional (start 0) (end nil) out prefix) ()
  (declare (ignore out prefix))
  (multiple-value-bind (r text bytes s e gs ge) (match-positions pattern input start end)
    (declare (ignore r text bytes))
    (if s
	(cons (cons s e) (loop for g across gs for h across ge collect (if g (cons g h) ps:false)))
	ps:false)))

(defun replacement (insert text bytes s e gs ge)
  "INSERT for one match: a procedure of the matched strings, or a string
with & and \\N for the match and its groups."
  (if (functionp insert)
      (let ((v (apply insert (result-substring text bytes s e)
		      (loop for g across gs for h across ge collect (result-substring text bytes g h)))))
	(if (stringp v) v (latin-1 v)))
      (let ((insert (if (stringp insert) insert (latin-1 insert))))
	(with-output-to-string (out)
	  (loop with i = 0
		while (< i (length insert))
		do (let ((c (char insert i)))
		     (cond ((char= c #\&) (write-string text out :start s :end e) (incf i))
			   ((and (char= c #\\) (< (1+ i) (length insert)))
			    (let ((d (char insert (1+ i))))
			      (cond ((digit-char-p d)
				     (let ((n (digit-char-p d)))
				       (if (zerop n)
					   (write-string text out :start s :end e)
					   (let ((g (and (<= n (length gs)) (aref gs (1- n)))))
					     (when g (write-string text out :start g :end (aref ge (1- n))))))))
				    (t (write-char d out)))
			      (incf i 2)))
			   (t (write-char c out) (incf i)))))))))

(defun regexp-replace* (pattern input insert all)
  (let ((r (as-regexp pattern)))
    (multiple-value-bind (text input-bytes) (input-text input)
      (let ((bytes (or (rregexp-bytes r) input-bytes))
	    (out (make-string-output-stream))
	    (pos 0))
	(loop
	  (multiple-value-bind (s e gs ge) (cl-ppcre:scan (rregexp-scanner r) text :start pos)
	    (when (null s) (return))
	    (write-string text out :start pos :end s)
	    (write-string (replacement insert text bytes s e gs ge) out)
	    (setq pos (if (= s e) (progn (when (< e (length text)) (write-char (char text e) out)) (1+ e)) e))
	    (when (or (not all) (> pos (length text))) (return))))
	(when (< pos (length text)) (write-string text out :start pos))
	(let ((result (get-output-stream-string out)))
	  (if bytes (map '(simple-array (unsigned-byte 8) (*)) #'char-code result) result))))))

(defnative "regexp-replace" (pattern input insert &optional prefix) ()
  (declare (ignore prefix))
  (regexp-replace* pattern input insert nil))
(defnative "regexp-replace*" (pattern input insert &optional start end prefix) ()
  (declare (ignore start end prefix))
  (regexp-replace* pattern input insert t))
