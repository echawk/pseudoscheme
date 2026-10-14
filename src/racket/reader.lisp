; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

;;;; Racket's datum syntax, for linklets and other S-expressions the
;;;; strategies read before the expander's own reader runs (BC's
;;;; startup.inc, tests/racket/linklets/).  Racket source modules are
;;;; read by the expander's reader, not this one.
;;;;
;;;; What differs from R7RS's: a token that isn't a number is a symbol
;;;; (1/byte-regexp), [ ] and { } are parentheses, #:keywords, #"byte
;;;; strings", #hash(...) #hasheq(...) #hasheqv(...) (immutable hash
;;;; tables), #&boxes, #rx"..." #px"..." (regexps), #s(prefab ...) and
;;;; #%symbols.  Symbols are case-sensitive.

(in-package "PSEUDOSCHEME-RACKET")

(defstruct (rkeyword (:constructor %make-rkeyword (name))) name)
(defvar *keywords* (make-hash-table :test 'equal :synchronized t))
(defun rkeyword (name)
  (or (gethash name *keywords*) (setf (gethash name *keywords*) (%make-rkeyword name))))
(defmethod print-object ((k rkeyword) stream) (format stream "#:~A" (rkeyword-name k)))

(defstruct (regexp-literal (:constructor make-regexp-literal (kind source)))
  kind					; :rx :px, and bytes or not by SOURCE's type
  source)

(defstruct (prefab-literal (:constructor make-prefab-literal (key fields))) key fields)

(defvar *eof* (make-symbol "EOF"))

(defun delimiter-p (c)
  (or (null c) (member c '(#\( #\) #\[ #\] #\{ #\} #\" #\; #\' #\` #\,))
      (sb-unicode:whitespace-p c)))

(defun skip-atmosphere (in)
  "Past whitespace and comments: ; #| |# #;datum."
  (loop
    (let ((c (peek-char nil in nil)))
      (cond ((null c) (return))
	    ((sb-unicode:whitespace-p c) (read-char in))
	    ((char= c #\;) (read-line in nil))
	    ((char= c #\#)
	     (read-char in)
	     (let ((d (peek-char nil in nil)))
	       (cond ((eql d #\|) (read-char in) (skip-block-comment in))
		     ((eql d #\;) (read-char in) (read-datum in))
		     (t (unread-char #\# in) (return)))))
	    (t (return))))))

(defun skip-block-comment (in)
  (let ((depth 1))
    (loop for p = nil then c
	  for c = (read-char in)
	  do (cond ((and (eql p #\|) (char= c #\#)) (when (zerop (decf depth)) (return)) (setq c nil))
		   ((and (eql p #\#) (char= c #\|)) (incf depth) (setq c nil))))))

(defun read-token (in &optional (prefix ""))
  "Characters up to a delimiter, with | | and \\ quoting; and whether any
were quoted (then it's a symbol whatever it looks like)."
  (let ((quoted nil))
    (values
     (with-output-to-string (out)
       (write-string prefix out)
       (loop for c = (peek-char nil in nil)
	     until (delimiter-p c)
	     do (read-char in)
		(cond ((char= c #\|)
		       (setq quoted t)
		       (loop for d = (read-char in) until (char= d #\|) do (write-char d out)))
		      ((char= c #\\) (setq quoted t) (write-char (read-char in) out))
		      (t (write-char c out)))))
     quoted)))

(defun parse-number (token)
  (and (plusp (length token))
       (or (digit-char-p (char token 0)) (find (char token 0) "+-.#"))
       (let ((n (ignore-errors (ps::parse-scheme-number token))))
	 (and n (numberp n) n))))

(defun read-datum (in)
  (skip-atmosphere in)
  (let ((c (read-char in nil)))
    (case c
      ((nil) *eof*)
      ((#\( #\[ #\{) (read-list-tail in (ecase c (#\( #\)) (#\[ #\]) (#\{ #\}))))
      ((#\) #\] #\}) (racket-error "read" "unexpected `~A'" c))
      (#\' (list (rsym "quote") (read-datum in)))
      (#\` (list (rsym "quasiquote") (read-datum in)))
      (#\, (if (eql (peek-char nil in nil) #\@)
	       (progn (read-char in) (list (rsym "unquote-splicing") (read-datum in)))
	       (list (rsym "unquote") (read-datum in))))
      (#\" (read-string-literal in))
      (#\# (read-hash in))
      (t (unread-char c in)
	 (multiple-value-bind (token quoted) (read-token in)
	   (or (and (not quoted) (parse-number token))
	       (if (and (not quoted) (string= token "."))
		   (racket-error "read" "illegal use of `.'")
		   (rsym token))))))))

(defun read-list-tail (in close)
  (let ((items '()) (tail '()))
    (loop
      (skip-atmosphere in)
      (let ((c (peek-char nil in nil)))
	(cond ((null c) (racket-error "read" "expected a `~A' to close the list" close))
	      ((member c '(#\) #\] #\})) (read-char in) (return (nreconc items tail)))
	      ((and (char= c #\.) (progn (read-char in) (delimiter-p (peek-char nil in nil))))
	       (setq tail (read-datum in))
	       (skip-atmosphere in)
	       (read-char in)
	       (return (nreconc items tail)))
	      ((char= c #\.)		; a token starting with . (read the rest)
	       (multiple-value-bind (token quoted) (read-token in ".")
		 (push (or (and (not quoted) (parse-number token)) (rsym token)) items)))
	      (t (push (read-datum in) items)))))))

(defun read-string-chars (in)
  "A string literal's characters, after its opening quote."
  (with-output-to-string (out)
    (loop for c = (read-char in)
	  until (char= c #\")
	  do (if (char/= c #\\)
		 (write-char c out)
		 (let ((e (read-char in)))
		   (case e
		     (#\n (write-char #\Newline out)) (#\t (write-char #\Tab out))
		     (#\r (write-char #\Return out)) (#\a (write-char (code-char 7) out))
		     (#\b (write-char (code-char 8) out)) (#\v (write-char (code-char 11) out))
		     (#\f (write-char (code-char 12) out)) (#\e (write-char (code-char 27) out))
		     ((#\x #\u #\U)
		      (let ((n 0) (digits 0) (max (ecase e (#\x 2) (#\u 4) (#\U 8))))
			(loop while (and (< digits max) (digit-char-p (peek-char nil in) 16))
			      do (setq n (+ (* n 16) (digit-char-p (read-char in) 16))) (incf digits))
			(write-char (code-char n) out)))
		     ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
		      (let ((n (digit-char-p e 8)))
			(loop repeat 2 while (digit-char-p (peek-char nil in) 8)
			      do (setq n (+ (* n 8) (digit-char-p (read-char in) 8))))
			(write-char (code-char n) out)))
		     (#\Newline)		; a line continuation
		     (t (write-char e out))))))))

(defun read-string-literal (in) (immutable! (read-string-chars in)))

(defun string-bytes (s)
  (map '(simple-array (unsigned-byte 8) (*)) #'char-code s))

(defparameter *character-names*
  '(("space" . #\Space) ("newline" . #\Newline) ("linefeed" . #\Newline) ("tab" . #\Tab)
    ("return" . #\Return) ("nul" . #.(code-char 0)) ("null" . #.(code-char 0))
    ("backspace" . #.(code-char 8)) ("rubout" . #.(code-char 127)) ("delete" . #.(code-char 127))
    ("page" . #.(code-char 12)) ("vtab" . #.(code-char 11)) ("alarm" . #.(code-char 7))
    ("escape" . #.(code-char 27))))

(defun read-character (in)
  (let* ((first (read-char in))
	 (rest (if (delimiter-p (peek-char nil in nil)) "" (read-token in)))
	 (token (concatenate 'string (string first) rest)))
    (cond ((= (length token) 1) first)
	  ((cdr (assoc token *character-names* :test #'string=)))
	  ((and (member first '(#\u #\U)) (every (lambda (c) (digit-char-p c 16)) rest))
	   (code-char (parse-integer rest :radix 16)))
	  ((every (lambda (c) (digit-char-p c 8)) token) (code-char (parse-integer token :radix 8)))
	  (t (racket-error "read" "bad character constant `#\\~A'" token)))))

(defun read-hash (in)
  "After a #."
  (let ((c (peek-char nil in)))
    (cond ((member c '(#\( #\[ #\{))
	   (read-char in) (immutable! (coerce (read-list-tail in (ecase c (#\( #\)) (#\[ #\]) (#\{ #\}))) 'simple-vector)))
	  ((char= c #\\) (read-char in) (read-character in))
	  ((char= c #\") (read-char in) (immutable! (string-bytes (read-string-chars in))))
	  ((char= c #\:) (read-char in) (rkeyword (read-token in)))
	  ((char= c #\&) (read-char in) (make-rbox :value (read-datum in)))
	  ((char= c #\') (read-char in) (list (rsym "syntax") (read-datum in)))
	  ((char= c #\`) (read-char in) (list (rsym "quasisyntax") (read-datum in)))
	  ((char= c #\,) (read-char in)
	   (if (eql (peek-char nil in nil) #\@)
	       (progn (read-char in) (list (rsym "unsyntax-splicing") (read-datum in)))
	       (list (rsym "unsyntax") (read-datum in))))
	  ((char= c #\%) (rsym (read-token in "#")))
	  (t
	   (let ((token (read-token in)))
	     (cond ((member token '("t" "true") :test #'string=) ps:true)
		   ((member token '("f" "false") :test #'string=) ps:false)
		   ((member token '("hash" "hasheq" "hasheqv") :test #'string=)
		    (let ((kind (cond ((string= token "hasheq") :eq) ((string= token "hasheqv") :eqv) (t :equal))))
		      (skip-atmosphere in)
		      (read-char in)
		      (ihash-from-assocs kind (read-list-tail in #\)))))
		   ((member token '("rx" "px" "rx#" "px#") :test #'string=)
		    (read-char in)	; the "
		    (let ((s (read-string-chars in)))
		      (make-regexp-literal (if (char= (char token 0) #\r) :rx :px)
					   (if (find #\# token) (string-bytes s) s))))
		   ((string= token "s")
		    (skip-atmosphere in) (read-char in)
		    (let ((items (read-list-tail in #\))))
		      (make-prefab-literal (car items) (cdr items))))
		   ((parse-number (concatenate 'string "#" token)))
		   (t (racket-error "read" "bad syntax `#~A'" token))))))))

(defun read-racket (in)
  "The next datum from stream IN, or *EOF*."
  (read-datum in))

(defun read-racket-file (path)
  (with-open-file (in path :external-format :utf-8)
    (loop for form = (read-racket in)
	  until (eq form *eof*)
	  collect form)))

;;; Load forms, for compiled linklets' literals (cache.lisp)
(defmethod make-load-form ((k rkeyword) &optional environment)
  (declare (ignore environment))
  `(rkeyword ,(rkeyword-name k)))
(defmethod make-load-form ((x ihash) &optional environment)
  (declare (ignore environment))
  `(ihash-from-assocs ,(ihash-kind x) ',(coerce (ihash-entries x) 'list)))
(defmethod make-load-form ((x rbox) &optional environment)
  (make-load-form-saving-slots x :environment environment))
(defmethod make-load-form ((x rpath) &optional environment)
  (make-load-form-saving-slots x :environment environment))
(defmethod make-load-form ((x regexp-literal) &optional environment)
  (make-load-form-saving-slots x :environment environment))
(defmethod make-load-form ((x prefab-literal) &optional environment)
  (make-load-form-saving-slots x :environment environment))
