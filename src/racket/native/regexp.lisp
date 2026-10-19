; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-RACKET -*-

;;;; Regular expressions, natively, on CL-PPCRE: strategy B's stand-in for
;;;; Racket's regexp layer.  The layer's own linklet replaces these
;;;; natives when it is extracted and loaded (linklet/regexp-layer.lisp);
;;;; this is the fallback when it isn't.
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

(in-package "QUASISCHEME-RACKET")

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
		   (cond ((char= c #\[)
			  ;; a class: in rx a \ in it is itself, and a ] first
			  ;; (after any ^) is a member
			  (write-char c out)
			  (incf i)
			  (when (and (< i (length source)) (char= (char source i) #\^))
			    (write-char #\^ out) (incf i))
			  (when (and (< i (length source)) (char= (char source i) #\]))
			    (write-string "\\]" out) (incf i))
			  (loop while (and (< i (length source)) (char/= (char source i) #\]))
				do (let ((d (char source i)))
				     (cond ((char= d #\\) (write-string "\\\\" out))
					   ((and (char= d #\[) (< (1+ i) (length source))
						 (char= (char source (1+ i)) #\:))
					    ;; [:alpha:] and the like, through :]
					    (let ((close (search ":]" source :start2 i)))
					      (write-string source out :start i :end (+ close 2))
					      (setq i (+ close 1))))
					   (t (write-char d out))))
				   (incf i))
			  (when (< i (length source)) (write-char #\] out) (incf i)))
			 ((and (char= c #\\) (< (1+ i) (length source))
			       (alphanumericp (char source (1+ i))))
			  ;; rx: \a is a
			  (write-char (char source (1+ i)) out)
			  (incf i 2))
			 ((and (char= c #\\) (< (1+ i) (length source)))
			  (write-char c out) (write-char (char source (1+ i)) out)
			  (incf i 2))
			 (t (write-char c out) (incf i))))))))

(defun group-end (s start)
  "The index of the ) closing the group whose contents start at START in
S, skipping escapes, classes and nested groups."
  (let ((depth 0) (i start))
    (loop while (< i (length s))
	  do (let ((c (char s i)))
	       (cond ((char= c #\\) (incf i 2))
		     ((char= c #\[)
		      (incf i)
		      (when (and (< i (length s)) (char= (char s i) #\^)) (incf i))
		      (when (and (< i (length s)) (char= (char s i) #\])) (incf i))
		      (loop while (and (< i (length s)) (char/= (char s i) #\])) do (incf i))
		      (incf i))
		     ((char= c #\() (incf depth) (incf i))
		     ((char= c #\))
		      (when (zerop depth) (return-from group-end i))
		      (decf depth) (incf i))
		     (t (incf i)))))
    (length s)))

(defun top-level-alternatives (s)
  "S split at its |s outside groups and classes."
  (let ((parts '()) (start 0) (depth 0) (i 0))
    (loop while (< i (length s))
	  do (let ((c (char s i)))
	       (cond ((char= c #\\) (incf i 2))
		     ((char= c #\[) (setq i (1+ (or (position #\] s :start (+ i 2)) (1- (length s))))))
		     ((char= c #\() (incf depth) (incf i))
		     ((char= c #\)) (decf depth) (incf i))
		     ((and (char= c #\|) (zerop depth))
		      (push (subseq s start i) parts) (setq start (1+ i)) (incf i))
		     (t (incf i)))))
    (nreverse (cons (subseq s start) parts))))

(defun atom-end (s i)
  "Where the regexp atom of S that starts at I ends."
  (let ((c (char s i)))
    (cond ((char= c #\\) (min (length s) (+ i 2)))
	  ((char= c #\[)
	   (let ((j (1+ i)))
	     (when (and (< j (length s)) (char= (char s j) #\^)) (incf j))
	     (when (and (< j (length s)) (char= (char s j) #\])) (incf j))
	     (loop while (and (< j (length s)) (char/= (char s j) #\])) do (incf j))
	     (min (length s) (1+ j))))
	  ((char= c #\()
	   (min (length s) (1+ (group-end s (1+ i)))))
	  (t (1+ i)))))

(defun expand-bounded-repeats (s)
  "The alternatives S, a lookbehind's body, comes to without its x?
and x{n,m} (small ones): CL-PPCRE wants a fixed length, Racket only a
bounded one.  S itself (a list of it) if there are none, or too many,
or a group in S captures (copies would renumber it)."
  (let ((results (list ""))
	(i 0)
	(changed nil))
    (when (loop for j from 0 below (length s)
		thereis (and (char= (char s j) #\()
			     (or (= j 0) (char/= (char s (1- j)) #\\))
			     (or (= (1+ j) (length s)) (char/= (char s (1+ j)) #\?))))
      (return-from expand-bounded-repeats (list s)))
    (loop while (< i (length s))
	  do (let* ((end (atom-end s i))
		    (atom (subseq s i end))
		    (counts nil))
	       (cond ((and (< end (length s)) (char= (char s end) #\?)
			   (or (= (1+ end) (length s)) (not (find (char s (1+ end)) "?+"))))
		      (setq counts '(1 0) end (1+ end)))
		     ((and (< end (length s)) (char= (char s end) #\{))
		      (let* ((close (position #\} s :start end))
			     (body (and close (subseq s (1+ end) close)))
			     (comma (and body (position #\, body)))
			     (lo (and body (parse-integer body :end comma :junk-allowed t)))
			     (hi (and comma (parse-integer body :start (1+ comma) :junk-allowed t))))
			(when (and lo hi (<= lo hi 8) (not (and (< (1+ close) (length s)) (find (char s (1+ close)) "?+"))))
			  (setq counts (loop for k from hi downto lo collect k) end (1+ close))))))
	       (if counts
		   (let ((next (loop for prefix in results
				     append (loop for k in counts
						  collect (concatenate 'string prefix
								       (apply #'concatenate 'string
									      (loop repeat k collect atom)))))))
		     (when (> (length next) 64) (return-from expand-bounded-repeats (list s)))
		     (setq results next changed t))
		   (setq results (mapcar (lambda (prefix) (concatenate 'string prefix (subseq s i end))) results)))
	       (setq i end)))
    (if changed (remove-duplicates results :test #'string= :from-end t) (list s))))

(defun split-lookbehinds (s)
  "S with each lookbehind of several alternatives made one lookbehind per
alternative: CL-PPCRE wants each fixed-length, Racket only bounded."
  (let ((out (make-string-output-stream)) (i 0))
    (loop while (< i (length s))
	  do (let ((c (char s i)))
	       (cond ((char= c #\\)
		      (write-char c out) (when (< (1+ i) (length s)) (write-char (char s (1+ i)) out))
		      (incf i 2))
		     ((and (char= c #\() (< (+ i 3) (length s))
			   (string= "(?<" s :start2 i :end2 (+ i 3))
			   (member (char s (+ i 3)) '(#\= #\!)))
		      (let* ((negative (char= (char s (+ i 3)) #\!))
			     (end (group-end s (+ i 4)))
			     (alternatives (loop for a in (top-level-alternatives (subseq s (+ i 4) end))
						 append (expand-bounded-repeats (split-lookbehinds a)))))
			(if (null (cdr alternatives))
			    (format out "(?<~A~A)" (if negative "!" "=") (car alternatives))
			    (if negative
				(dolist (a alternatives) (format out "(?<!~A)" a))
				(format out "(?:~{(?<=~A)~^|~})" alternatives)))
			(setq i (1+ end))))
		     (t (write-char c out) (incf i)))))
    (get-output-stream-string out)))

(defun multi-mode-groups (text)
  "TEXT with Racket's (?m: (and (?im:, (?mi:) as cl-ppcre's, which also
turns its single-line mode off."
  (cl-ppcre:regex-replace-all "\\(\\?(i?)m(i?):" text "(?\\1m\\2-s:"))

(defun make-rregexp (source px)
  (let* ((bytes (typep source '(simple-array (unsigned-byte 8) (*))))
	 (text (if bytes (latin-1 source) source)))
    (%make-rregexp source px bytes
		   ;; Racket's . matches a newline except in (?m: mode, where ^ and
		   ;; $ match at line breaks too: cl-ppcre's single-line mode, and
		   ;; its (?m-s: for (?m:
		   (handler-case (cl-ppcre:create-scanner (multi-mode-groups (split-lookbehinds (translate-regexp text px)))
							  :single-line-mode t)
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
      qs:false))

(defnative "regexp-match" (pattern input &optional (start 0) (end nil) out prefix) ()
  (declare (ignore out prefix))
  (multiple-value-bind (r text bytes s e gs ge) (match-positions pattern input start end)
    (declare (ignore r))
    (if s
	(cons (result-substring text bytes s e)
	      (loop for g across gs for h across ge collect (result-substring text bytes g h)))
	qs:false)))

(defnative "regexp-match?" (pattern input &optional (start 0) (end nil) out prefix) ()
  (declare (ignore out prefix))
  (bool (nth-value 3 (match-positions pattern input start end))))

(defnative "regexp-match-positions" (pattern input &optional (start 0) (end nil) out prefix) ()
  (declare (ignore out prefix))
  (multiple-value-bind (r text bytes s e gs ge) (match-positions pattern input start end)
    (declare (ignore r text bytes))
    (if s
	(cons (cons s e) (loop for g across gs for h across ge collect (if g (cons g h) qs:false)))
	qs:false)))

(defun last-bytes (text bytes e count)
  "The COUNT (at most) bytes of TEXT before position E, as a byte string:
the /end variants' second value."
  (let ((piece (subseq text (max 0 (- e count)) e)))
    (if (or bytes (not (stringp piece)))
	(coerce (if (stringp piece) (map 'vector #'char-code piece) piece) '(simple-array (unsigned-byte 8) (*)))
	(let ((b (sb-ext:string-to-octets piece :external-format :utf-8)))
	  (subseq b (max 0 (- (length b) count)))))))

(defnative "regexp-match-positions/end" (pattern input &optional (start 0) (end nil) out prefix (count 1)) ()
  (declare (ignore out prefix))
  (multiple-value-bind (r text bytes s e gs ge) (match-positions pattern input start end)
    (declare (ignore r))
    (if s
	(values (cons (cons s e) (loop for g across gs for h across ge collect (if g (cons g h) qs:false)))
		(last-bytes text bytes e count))
	(values qs:false qs:false))))

(defnative "regexp-match/end" (pattern input &optional (start 0) (end nil) out prefix (count 1)) ()
  (declare (ignore out prefix))
  (multiple-value-bind (r text bytes s e gs ge) (match-positions pattern input start end)
    (declare (ignore r))
    (if s
	(values (cons (result-substring text bytes s e)
		      (loop for g across gs for h across ge collect (result-substring text bytes g h)))
		(last-bytes text bytes e count))
	(values qs:false qs:false))))

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

;;; How far back a match may look: (?<=...) and (?<!...) are bounded
;;; (as Racket's are), so the pattern's length bounds it
(defnative "regexp-max-lookbehind" (rx) (:leaf t)
  (let ((source (let ((s (if (rregexp-p rx) (rregexp-source rx) (funcall (native-function (gethash "object-name" *natives*)) rx))))
		  (if (stringp s) s (map 'string #'code-char s)))))
    (if (or (search "(?<=" source) (search "(?<!" source) (search "\\b" source) (search "\\B" source)
	    (find #\^ source))
	(max 1 (length source))
	0)))

;;; ------------------------------------------------------------------
;;; Peeking at a port: the match starts START bytes after its position;
;;; positions are byte positions from there, as Racket's are; a string
;;; regexp matches the bytes as UTF-8.

(defun peek-all-bytes (p skip limit blocking)
  "The bytes of input port P after SKIP, up to LIMIT of them (NIL: to
the end), as far as they are there (or, if not BLOCKING, there now)."
  (let ((out (make-array 256 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
	(buf (make-array 4096 :element-type '(unsigned-byte 8))))
    (loop
      (let* ((want (if limit (min 4096 (- limit (length out))) 4096))
	     (r (if (<= want 0) :eof (ip-peek-bytes p (+ skip (length out)) buf 0 want blocking))))
	(if (integerp r)
	    (loop for i below r do (vector-push-extend (aref buf i) out))
	    (return))))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

(defun utf-8-offsets (text)
  "A vector: the byte offset of each character index of TEXT (and its end)."
  (let ((offsets (make-array (1+ (length text)))) (n 0))
    (dotimes (i (length text))
      (setf (aref offsets i) n)
      (incf n (let ((c (char-code (char text i))))
		(cond ((< c #x80) 1) ((< c #x800) 2) ((< c #x10000) 3) (t 4)))))
    (setf (aref offsets (length text)) n)
    offsets))

(defun peek-match (who pattern in start end immediate)
  "Match PATTERN against the bytes of input port IN from START to END
without reading them: the bytes peeked, and the byte start and end of
the match and of its groups (vectors), or NIL."
  (let* ((r (as-regexp pattern))
	 (p (in-port who in))
	 (raw (peek-all-bytes p start (and end (truthy end) (- end start)) (not immediate))))
    (multiple-value-bind (s e gs ge)
	(if (rregexp-bytes r)
	    (cl-ppcre:scan (rregexp-scanner r) (latin-1 raw))
	    (let ((text (decode-utf-8 raw)))
	      (multiple-value-bind (s e gs ge) (cl-ppcre:scan (rregexp-scanner r) text)
		(if s
		    (let ((offsets (utf-8-offsets text)))
		      (values (aref offsets s) (aref offsets e)
			      (map 'vector (lambda (g) (and g (aref offsets g))) gs)
			      (map 'vector (lambda (g) (and g (aref offsets g))) ge)))
		    nil))))
      ;; -immediate only peeks what's available (PEEK-ALL-BYTES), rather
      ;; than waiting for more
      (if s
	  (values raw s e gs ge)
	  (values raw nil nil nil nil)))))

(defun peek-match-groups (raw s e gs ge)
  (cons (subseq raw s e)
	(loop for g across gs for h across ge collect (if g (subseq raw g h) qs:false))))

(defun peek-match-positions (start s e gs ge)
  (cons (cons (+ start s) (+ start e))
	(loop for g across gs for h across ge collect (if g (cons (+ start g) (+ start h)) qs:false))))

(defmacro define-peek-match (name immediate positions)
  `(progn
     (defnative ,name (pattern in &optional (start 0) (end nil) out prefix) ()
       (declare (ignore out prefix))
       (multiple-value-bind (raw s e gs ge) (peek-match ,name pattern in start end ,immediate)
	 (if s
	     ,(if positions
		  `(peek-match-positions start s e gs ge)
		  `(peek-match-groups raw s e gs ge))
	     qs:false)))
     (defnative ,(concatenate 'string name "/end") (pattern in &optional (start 0) (end nil) out prefix (count 1)) ()
       (declare (ignore out prefix))
       (multiple-value-bind (raw s e gs ge) (peek-match ,(concatenate 'string name "/end") pattern in start end ,immediate)
	 (if s
	     (values ,(if positions
			  `(peek-match-positions start s e gs ge)
			  `(peek-match-groups raw s e gs ge))
		     (subseq raw (max 0 (- e count)) e))
	     (values qs:false qs:false))))))

(define-peek-match "regexp-match-peek" nil nil)
(define-peek-match "regexp-match-peek-positions" nil t)
(define-peek-match "regexp-match-peek-immediate" t nil)
(define-peek-match "regexp-match-peek-positions-immediate" t t)

;;; The number of groups a regexp captures: the ( not followed by ?,
;;; outside classes and escapes
(defnative "regexp-capture-group-count" (rx) (:leaf t)
  (let* ((r (or (regexp-of rx)
		(racket-error "regexp-capture-group-count" "contract violation~%  expected: (or/c regexp? byte-regexp?)~%  given: ~A" (written rx))))
	 (source (let ((s (rregexp-source r))) (if (stringp s) s (latin-1 s))))
	 (n 0) (i 0) (len (length source)))
    (loop while (< i len)
	  do (let ((c (char source i)))
	       (cond ((char= c #\\) (incf i 2))
		     ((char= c #\[)
		      (incf i)
		      (when (and (< i len) (char= (char source i) #\^)) (incf i))
		      (when (and (< i len) (char= (char source i) #\])) (incf i))
		      (loop while (and (< i len) (char/= (char source i) #\]))
			    do (if (and (char= (char source i) #\[) (< (1+ i) len) (char= (char source (1+ i)) #\:))
				   (let ((close (search ":]" source :start2 i)))
				     (setq i (if close (+ close 2) len)))
				   (incf i)))
		      (incf i))
		     ((char= c #\()
		      (unless (and (< (1+ i) len) (char= (char source (1+ i)) #\?)) (incf n))
		      (incf i))
		     (t (incf i)))))
    n))

;;; The layer's procedures call Racket code (a handler, a custom port),
;;; so they aren't leaves, except the predicates and the counts.
(dolist (name '("regexp" "pregexp" "byte-regexp" "byte-pregexp"
		"regexp-match" "regexp-match/end" "regexp-match-positions"
		"regexp-match-positions/end" "regexp-match?"
		"regexp-match-peek" "regexp-match-peek-positions" "regexp-match-peek-positions/end"
		"regexp-match-peek-immediate" "regexp-match-peek-positions-immediate"
		"regexp-match-peek-positions-immediate/end" "regexp-replace" "regexp-replace*"))
  (let ((native (gethash name *natives*)))
    (when native (setf (native-leaf native) nil))))
