; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-GUILE -*-

;;;; Guile's reader (docs/guile.md, stage 0).  Guile's own reader is in C
;;;; until boot-9 has loaded, and boot-9 is read with it, so it is written
;;;; here, after Guile 3.0's defaults:
;;;;
;;;;   #:kw keywords (Lisp keywords, as Pseudoscheme's #:kw already are),
;;;;   #{odd symbol}#, #!...!# block comments and the #!fold-case
;;;;   directives, #; #| |#, [ ] as parentheses, #' #` #, #,@ for syntax,
;;;;   #nil, #*101 bit vectors, #vu8( and SRFI 4's #u8( #s16( ..., #2(...)
;;;;   arrays as nested vectors, "\xHH" escapes of two digits, and the
;;;;   characters #\nul, #\101 (octal) and #\x41 (hex);
;;;;
;;;; and READ-HASH-EXTEND's procedures for the # syntaxes it doesn't know.
;;;; Unlike R7RS, | is an ordinary constituent of symbols.

(defpackage "PSEUDOSCHEME-GUILE"
  (:nicknames "PSG")
  (:use "COMMON-LISP")
  (:export "GUILE-READ" "BOOT" "EVAL-STRING" "LOAD-FILE" "REPL" "ERROR-TEXT"
	   "*PROGRAM-ARGUMENTS*"))

(in-package "PSEUDOSCHEME-GUILE")

(defvar *read-hash-procedures* '()
  "READ-HASH-EXTEND's table: (char . procedure of char and port).")

(defvar *guile-fold-case* nil)

(defvar *elisp-nil* (ps::make-photon "#nil")
  "#nil, Emacs Lisp's nil in Guile: its own object.")

(define-condition guile-read-error (ps::scheme-reader-error) ())

(defun read-error (port control &rest args)
  (error 'guile-read-error :stream port :format-control control :format-arguments args))

(defun delimiterp (c)
  (or (null c) (member c '(#\Space #\Tab #\Newline #\Return #\Page #\( #\) #\[ #\] #\" #\;))
      (char= c (code-char 11))))

(defun whitespacep (c)
  (member c '(#\Space #\Tab #\Newline #\Return #\Page #.(code-char 11) #.(code-char 160))))

(defun peekc (port) (peek-char nil port nil nil))
(defun readc (port) (read-char port nil nil))

(defun skip-whitespace-and-comments (port)
  "Skip to the next datum's first character; return it (not consumed),
or NIL at end of file."
  (loop
    (let ((c (peekc port)))
      (cond ((null c) (return nil))
	    ((whitespacep c) (readc port))
	    ((char= c #\;) (loop for d = (readc port) until (or (null d) (char= d #\Newline))))
	    ((char= c #\#)
	     ;; #; #| and #! comments are skipped here; anything else is a datum
	     (readc port)
	     (let ((d (peekc port)))
	       (case d
		 (#\; (readc port) (guile-read-1 port))
		 (#\| (readc port) (skip-block-comment port))
		 (#\! (readc port)
		  (unless (read-shebang port)
		    (unread-char #\# port)
		    (return #\#)))
		 (t (unread-char #\# port) (return #\#)))))
	    (t (return c))))))

(defun skip-block-comment (port)
  (let ((depth 1))
    (loop (let ((c (readc port)))
	    (cond ((null c) (read-error port "unterminated #| comment"))
		  ((and (char= c #\|) (eql (peekc port) #\#)) (readc port)
		   (when (zerop (decf depth)) (return)))
		  ((and (char= c #\#) (eql (peekc port) #\|)) (readc port) (incf depth)))))))

(defun read-shebang (port)
  "After #!: a directive (#!fold-case, #!r6rs, ...) or a block comment
ending at !#.  True if handled; it always is."
  (let ((name (make-string-output-stream)))
    (loop for c = (peekc port)
	  while (and c (or (alphanumericp c) (char= c #\-)))
	  do (write-char (readc port) name))
    (let ((name (get-output-stream-string name)))
      (cond ((and (delimiterp (peekc port))
		  (member name '("fold-case" "no-fold-case" "r6rs" "curly-infix"
				 "curly-infix-and-bracket-lists" "eof")
			  :test #'string=))
	     (cond ((string= name "fold-case") (setq *guile-fold-case* t))
		   ((string= name "no-fold-case") (setq *guile-fold-case* nil)))
	     t)
	    (t ;; a block comment: up to !#
	     (loop for c = (readc port)
		   do (cond ((null c) (read-error port "unterminated #! comment"))
			    ((and (char= c #\!) (eql (peekc port) #\#)) (readc port) (return t)))))))))

(defun guile-read (&optional (port *standard-input*))
  "Read one datum from PORT as Guile does; the EOF object at end of file."
  (let ((c (skip-whitespace-and-comments port)))
    (if (null c)
	ps:eof-object
	(guile-read-1 port))))

(defun guile-read-1 (port)
  (let ((c (skip-whitespace-and-comments port)))
    (when (null c) (read-error port "unexpected end of file"))
    (readc port)
    (case c
      ((#\( #\[) (read-list port (if (char= c #\() #\) #\])))
      ((#\) #\]) (read-error port "unexpected \"~A\"" c))
      (#\" (read-string-literal port))
      (#\' (list (ssym "quote") (guile-read-1 port)))
      (#\` (list (ssym "quasiquote") (guile-read-1 port)))
      (#\, (if (eql (peekc port) #\@)
	       (progn (readc port) (list (ssym "unquote-splicing") (guile-read-1 port)))
	       (list (ssym "unquote") (guile-read-1 port))))
      (#\# (read-hash port))
      (t (unread-char c port) (read-atom port)))))

(defun read-list (port close)
  (let ((items '()) (tail nil))
    (loop
      (let ((c (skip-whitespace-and-comments port)))
	(cond ((null c) (read-error port "unterminated list"))
	      ((member c '(#\) #\]))
	       (readc port)
	       (unless (char= c close) (read-error port "mismatched close paren ~A" c))
	       (let ((list (nreverse items)))
		 (when tail (setf (cdr (last list)) (car tail)))
		 (return list)))
	      (tail (read-error port "more than one datum after dot"))
	      ((and (char= c #\.) items)
	       (readc port)
	       (if (delimiterp (peekc port))
		   (setq tail (list (guile-read-1 port)))
		   (progn (unread-char #\. port) (push (guile-read-1 port) items))))
	      (t (push (guile-read-1 port) items)))))))

(defun read-token (port)
  (with-output-to-string (s)
    (loop for c = (peekc port)
	  until (delimiterp c)
	  do (write-char (readc port) s))))

(defun ssym (name) (ps:intern-scheme-symbol name))

(defun guile-symbol (name)
  (ssym (if *guile-fold-case* (string-downcase name) name)))

(defun guile-number (x)
  "Guile has no exact complex numbers: 1+3i is 1.0+3.0i."
  (if (and (complexp x) (rationalp (realpart x)))
      (coerce x '(complex double-float))
      x))

(defun read-atom (port)
  (let ((token (read-token port)))
    (or (and (plusp (length token)) (guile-number (ps:parse-scheme-number token 10)))
	(guile-symbol token))))

(defun read-hex (port digits terminator)
  "A character code from DIGITS hex digits (or up to TERMINATOR when
DIGITS is NIL)."
  (let ((s (make-string-output-stream)))
    (if digits
	(dotimes (i digits) (write-char (or (readc port) #\x) s))
	(loop for c = (readc port)
	      until (or (null c) (char= c terminator))
	      do (write-char c s)))
    (let ((n (parse-integer (get-output-stream-string s) :radix 16 :junk-allowed t)))
      (unless n (read-error port "bad hex escape"))
      (code-char n))))

(defun read-string-literal (port)
  (with-output-to-string (s)
    (loop
      (let ((c (readc port)))
	(cond ((null c) (read-error port "unterminated string"))
	      ((char= c #\") (return))
	      ((char= c #\\)
	       (let ((e (readc port)))
		 (case e
		   (#\a (write-char (code-char 7) s))
		   (#\b (write-char (code-char 8) s))
		   (#\t (write-char #\Tab s))
		   (#\n (write-char #\Newline s))
		   (#\v (write-char (code-char 11) s))
		   (#\f (write-char #\Page s))
		   (#\r (write-char #\Return s))
		   (#\e (write-char (code-char 27) s))
		   (#\0 (write-char (code-char 0) s))
		   (#\x (write-char (read-hex port 2 nil) s))
		   (#\u (write-char (read-hex port 4 nil) s))
		   (#\U (write-char (read-hex port 6 nil) s))
		   (#\Newline)		; a line continuation
		   ((nil) (read-error port "unterminated string"))
		   (t (write-char e s)))))
	      (t (write-char c s)))))))

(defparameter *char-names*
  `(("nul" . 0) ("null" . 0) ("soh" . 1) ("stx" . 2) ("etx" . 3) ("eot" . 4) ("enq" . 5)
    ("ack" . 6) ("bel" . 7) ("alarm" . 7) ("bs" . 8) ("backspace" . 8) ("ht" . 9) ("tab" . 9)
    ("lf" . 10) ("nl" . 10) ("newline" . 10) ("linefeed" . 10) ("vt" . 11) ("vtab" . 11)
    ("ff" . 12) ("np" . 12) ("page" . 12) ("cr" . 13) ("return" . 13) ("so" . 14) ("si" . 15)
    ("dle" . 16) ("dc1" . 17) ("dc2" . 18) ("dc3" . 19) ("dc4" . 20) ("nak" . 21)
    ("syn" . 22) ("etb" . 23) ("can" . 24) ("em" . 25) ("sub" . 26) ("esc" . 27)
    ("escape" . 27) ("altmode" . 27) ("fs" . 28) ("gs" . 29) ("rs" . 30) ("us" . 31)
    ("sp" . 32) ("space" . 32) ("del" . 127) ("delete" . 127) ("rubout" . 127)))

(defun read-character (port)
  (let* ((first (readc port))
	 (rest (if (delimiterp (peekc port)) "" (read-token port)))
	 (name (concatenate 'string (string first) rest)))
    (cond ((null first) (read-error port "end of file in character"))
	  ((zerop (length rest)) first)
	  ((assoc name *char-names* :test #'string-equal)
	   (code-char (cdr (assoc name *char-names* :test #'string-equal))))
	  ((and (char-equal first #\x) (every (lambda (c) (digit-char-p c 16)) rest))
	   (code-char (parse-integer rest :radix 16)))
	  ((every (lambda (c) (digit-char-p c 8)) name)
	   (code-char (parse-integer name :radix 8)))
	  ((and (char-equal first #\U) (> (length rest) 1) (char= (char rest 0) #\+))
	   (code-char (parse-integer rest :start 1 :radix 16)))
	  (t (read-error port "unknown character name ~A" name)))))

(defun read-extended-symbol (port)
  ;; #{...}#, with \x escapes as in Guile's writer
  (ssym (with-output-to-string (s)
	  (loop
	    (let ((c (readc port)))
	      (cond ((null c) (read-error port "unterminated #{"))
		    ((and (char= c #\}) (eql (peekc port) #\#)) (readc port) (return))
		    ((char= c #\\)
		     (let ((e (readc port)))
		       (if (eql e #\x)
			   (write-char (read-hex port nil #\;) s)
			   (write-char e s))))
		    (t (write-char c s))))))))

(defun read-hash (port)
  (let ((c (readc port)))
    (case c
      ((nil) (read-error port "end of file after #"))
      (#\( (coerce (read-list port #\)) 'simple-vector))
      (#\\ (read-character port))
      (#\: (let ((name (read-token port)))
	     (intern (ps::invert-case name) "KEYWORD")))
      (#\{ (read-extended-symbol port))
      (#\' (list (ssym "syntax") (guile-read-1 port)))
      (#\` (list (ssym "quasisyntax") (guile-read-1 port)))
      (#\, (if (eql (peekc port) #\@)
	       (progn (readc port) (list (ssym "unsyntax-splicing") (guile-read-1 port)))
	       (list (ssym "unsyntax") (guile-read-1 port))))
      (#\* (let ((bits (read-token port)))
	     (make-array (length bits) :element-type 'bit
			 :initial-contents (map 'list (lambda (c) (if (char= c #\1) 1 0)) bits))))
      (t
       (let ((proc (cdr (assoc c *read-hash-procedures*))))
	 (cond (proc (funcall proc c port))
	       ((alpha-char-p c) (read-hash-alpha c port))
	       ((digit-char-p c) (read-array-literal c port))
	       (t (read-error port "unknown # syntax #~A" c))))))))

(defun read-hash-alpha (c port)
  (let* ((token (concatenate 'string (string c) (read-token port))))
    (cond ((member token '("t" "true") :test #'string=) ps:true)
	  ((member token '("f" "false") :test #'string=) ps:false)
	  ((string= token "nil") *elisp-nil*)
	  ((string= token "eof") ps:eof-object)
	  ((and (string= token "vu8") (eql (peekc port) #\())
	   (readc port) (ps::list->bytevector (read-list port #\))))
	  ((and (eql (peekc port) #\()
		(member token '("u8" "s8" "u16" "s16" "u32" "s32" "u64" "s64" "f32" "f64" "c32" "c64")
			:test #'string-equal))
	   ;; a SRFI 4 vector: a bytevector tagged with its type (arrays.lisp)
	   (readc port)
	   (funcall 'list->typed-array* (ssym (string-downcase token)) 1 (read-list port #\))))
	  ;; #f64:3(...), #s8@1(...): a rank-1 typed array with bounds
	  ((let ((at (position-if (lambda (ch) (member ch '(#\@ #\:))) token)))
	     (and at (plusp at) (eql (peekc port) #\()
		  (member (subseq token 0 at)
			  '("u8" "s8" "u16" "s16" "u32" "s32" "u64" "s64" "f32" "f64" "c32" "c64" "a" "b")
			  :test #'string-equal)))
	   (let* ((at (position-if (lambda (ch) (member ch '(#\@ #\:))) token))
		  (type (string-downcase (subseq token 0 at)))
		  (spec (subseq token at))
		  (lo (let ((p (position #\@ spec)))
			(if p (parse-integer spec :start (1+ p) :junk-allowed t) 0)))
		  (list (progn (readc port) (read-list port #\))))
		  (len (let ((p (position #\: spec)))
			 (if p (parse-integer spec :start (1+ p) :junk-allowed t) (length list)))))
	     (funcall 'list->typed-array* (ssym type) (list (list lo (+ lo len -1))) list)))
	  ((member (char-downcase c) '(#\e #\i #\x #\o #\b #\d))
	   (or (guile-number (ps:parse-scheme-number (concatenate 'string "#" token) 10))
	       (read-error port "bad number #~A" token)))
	  (t (read-error port "unknown # syntax #~A" token)))))

(defun read-array-literal (c port)
  ;; #2((1 2) (3 4)), #1@1(a b), #2f64:2:2(...), #0(x): an array
  ;; (arrays.lisp), its rank, type, lower bounds and lengths as written
  (let ((rank (digit-char-p c)) (type "") (bounds '()))
    (loop for d = (peekc port) while (and d (digit-char-p d))
	  do (setq rank (+ (* rank 10) (digit-char-p (readc port)))))
    (loop for d = (peekc port) while (and d (alphanumericp d))
	  do (setq type (concatenate 'string type (string (readc port)))))
    (flet ((read-integer ()
	     (let ((sign (if (eql (peekc port) #\-) (progn (readc port) -1) 1)) (n 0))
	       (loop for d = (peekc port) while (and d (digit-char-p d))
		     do (setq n (+ (* n 10) (digit-char-p (readc port)))))
	       (* sign n))))
      (loop for d = (peekc port)
	    while (member d '(#\@ #\:))
	    do (readc port)
	       (if (char= d #\@)
		   (push (list (read-integer) nil) bounds)
		   (let ((len (read-integer)))
		     (if (and bounds (null (second (first bounds))))
			 (setf (second (first bounds)) len)
			 (push (list 0 len) bounds))))))
    (setq bounds (nreverse bounds))
    (unless (eql (readc port) #\() (read-error port "bad array literal"))
    (let* ((data (read-list port #\)))
	   (lens (labels ((lens (x r) (if (zerop r) '() (cons (if (listp x) (length x) 0) (lens (if (consp x) (car x) nil) (1- r))))))
		   (lens data rank)))
	   (shape (loop for k below rank
			for b = (nth k bounds)
			for lo = (if b (first b) 0)
			for len = (or (and b (second b)) (nth k lens))
			collect (list lo (+ lo len -1)))))
      (funcall 'list->typed-array* (if (string= type "") ps:true (ssym type))
	       (if (zerop rank) 0 shape)
	       (if (zerop rank) (car data) data)))))

(defun install-read-hash-extend ()
  "Guile's READ-HASH-EXTEND, for the runtime."
  (lambda (char proc)
    (setq *read-hash-procedures*
	  (if (eq proc ps:false)
	      (remove char *read-hash-procedures* :key #'car)
	      (acons char proc (remove char *read-hash-procedures* :key #'car))))
    ps:unspecific))
