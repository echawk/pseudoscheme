;;; boot/reader.scm -- a reader for the translator's sources.
;;;
;;; Under Common Lisp the translator's sources are read by the CL reader
;;; (ps:scheme-read-using-commonlisp-reader): symbols are folded to
;;; upper case and interned in SCHEME, PKG:NAME names a symbol in
;;; package PKG, #'X is (FUNCTION X), and #F / #T / characters / strings
;;; follow CL's rules.  The host Scheme's own READ can't be relied on to
;;; do any of that (nor to agree with other hosts), so this reads the
;;; same syntax, producing host data in which CL symbols are represented
;;; as described in base.scm.

(define boot:eof (list 'eof))
(define (boot:eof? x) (eq? x boot:eof))

(define (boot:whitespace? c)
  (or (char=? c #\space)
      (char=? c #\newline)
      (char=? c #\tab)
      (memv (char->integer c) '(10 12 13))))

(define (boot:terminator? c)
  (or (boot:whitespace? c)
      (memv c '(#\( #\) #\' #\" #\; #\` #\,))))

(define (boot:reader-error port msg . irritants)
  (apply boot:die (string-append "read: " msg) irritants))

(define boot:dot (list 'dot))
(define boot:close (list 'close))

(define (boot:read port)
  (let ((x (boot:read-1 port)))
    (cond ((eq? x boot:dot) (boot:reader-error port "unexpected \".\""))
	  ((eq? x boot:close) (boot:read port))  ;CL ignores stray ) at top level, with a warning
	  (else x))))

(define (boot:read-datum port)
  (let ((x (boot:read-1 port)))
    (cond ((boot:eof? x) (boot:reader-error port "unexpected end of file"))
	  ((or (eq? x boot:dot) (eq? x boot:close))
	   (boot:reader-error port "unexpected delimiter"))
	  (else x))))

(define (boot:skip-line port)
  (let ((c (read-char port)))
    (if (not (or (eof-object? c) (char=? c #\newline)))
	(boot:skip-line port))))

(define (boot:skip-block-comment port)
  ;; After #|; nests.
  (let loop ((depth 1))
    (let ((c (read-char port)))
      (cond ((eof-object? c) (boot:reader-error port "unterminated #|"))
	    ((and (char=? c #\|) (eqv? (peek-char port) #\#))
	     (read-char port)
	     (if (> depth 1) (loop (- depth 1))))
	    ((and (char=? c #\#) (eqv? (peek-char port) #\|))
	     (read-char port)
	     (loop (+ depth 1)))
	    (else (loop depth))))))

(define (boot:read-1 port)
  (let ((c (read-char port)))
    (cond ((eof-object? c) boot:eof)
	  ((boot:whitespace? c) (boot:read-1 port))
	  ((char=? c #\;) (boot:skip-line port) (boot:read-1 port))
	  ((char=? c #\() (boot:read-list port))
	  ((char=? c #\)) boot:close)
	  ((char=? c #\') (list 'quote (boot:read-datum port)))
	  ((char=? c #\`) (list 'quasiquote (boot:read-datum port)))
	  ((char=? c #\,)
	   (if (eqv? (peek-char port) #\@)
	       (begin (read-char port)
		      (list 'unquote-splicing (boot:read-datum port)))
	       (list 'unquote (boot:read-datum port))))
	  ((char=? c #\") (boot:read-string port))
	  ((char=? c #\#) (boot:read-sharp port))
	  (else (boot:read-token c port)))))

(define (boot:read-list port)
  (let loop ((acc '()))
    (let ((x (boot:read-1 port)))
      (cond ((boot:eof? x) (boot:reader-error port "unterminated list"))
	    ((eq? x boot:close) (reverse acc))
	    ((eq? x boot:dot)
	     (if (null? acc) (boot:reader-error port "bad dotted list"))
	     (let* ((tail (boot:read-datum port))
		    (end (boot:read-1 port)))
	       (if (not (eq? end boot:close))
		   (boot:reader-error port "bad dotted list"))
	       (append (reverse acc) tail)))
	    (else (loop (cons x acc)))))))

(define (boot:read-string port)
  ;; CL syntax: backslash quotes the next character, whatever it is.
  (let loop ((acc '()))
    (let ((c (read-char port)))
      (cond ((eof-object? c) (boot:reader-error port "unterminated string"))
	    ((char=? c #\") (list->string (reverse acc)))
	    ((char=? c #\\) (loop (cons (read-char port) acc)))
	    (else (loop (cons c acc)))))))

(define boot:char-names
  '(("SPACE" . 32) ("NEWLINE" . 10) ("LINEFEED" . 10) ("TAB" . 9)
    ("RETURN" . 13) ("PAGE" . 12) ("BACKSPACE" . 8) ("RUBOUT" . 127)
    ("DELETE" . 127) ("NUL" . 0) ("NULL" . 0) ("ESCAPE" . 27)
    ("ESC" . 27) ("ALTMODE" . 27) ("BELL" . 7) ("BEL" . 7)))

(define (boot:read-sharp port)
  (let ((c (read-char port)))
    (cond ((eof-object? c) (boot:reader-error port "end of file after #"))
	  ((char=? c #\() (list->vector (boot:read-list port)))
	  ((char=? c #\') (list (boot:ps-symbol "FUNCTION") (boot:read-datum port)))
	  ((char=? c #\\) (boot:read-char-literal port))
	  ((char=? c #\|) (boot:skip-block-comment port) (boot:read-1 port))
	  ((char=? c #\;) (boot:read-datum port) (boot:read-1 port))
	  ((memv c '(#\t #\T #\f #\F))
	   (if (and (not (eof-object? (peek-char port)))
		    (not (boot:terminator? (peek-char port))))
	       (boot:reader-error port "bad # syntax" c))
	   (if (memv c '(#\t #\T)) #t #f))
	  ((memv c '(#\x #\X #\b #\B #\o #\O))
	   (let* ((tok (boot:read-raw-token port))
		  (n (string->number tok (case (char-downcase c)
					   ((#\x) 16) ((#\b) 2) (else 8)))))
	     (or n (boot:reader-error port "bad number" tok))))
	  (else (boot:reader-error port "unsupported # syntax" c)))))

(define (boot:read-raw-token port)
  (let loop ((acc '()))
    (let ((c (peek-char port)))
      (if (or (eof-object? c) (boot:terminator? c))
	  (list->string (reverse acc))
	  (loop (cons (read-char port) acc))))))

(define (boot:read-char-literal port)
  (let* ((c (read-char port))
	 (rest (boot:read-raw-token port)))
    (if (string=? rest "")
	c
	(let* ((name (boot:string-upcase (string-append (string c) rest)))
	       (probe (assoc name boot:char-names)))
	  (if probe
	      (integer->char (cdr probe))
	      (boot:reader-error port "unknown character name" name))))))

;; Tokens: symbols and numbers.  A token is read as a list of
;; characters, each paired with whether it was escaped (\x or |...|).

(define (boot:read-token c port)
  ;; C, the token's first character, has been consumed.
  (let loop ((c c) (acc '()) (in-bars? #f))
    (let* ((step
	    (cond ((char=? c #\\)
		   (cons (cons (cons (read-char port) #t) acc) in-bars?))
		  ((char=? c #\|) (cons acc (not in-bars?)))
		  (else (cons (cons (cons c in-bars?) acc) in-bars?))))
	   (acc (car step))
	   (in-bars? (cdr step))
	   (next (peek-char port)))
      (cond ((eof-object? next)
	     (if in-bars?
		 (boot:reader-error port "unterminated |")
		 (boot:parse-token (reverse acc) port)))
	    ((and (not in-bars?) (boot:terminator? next))
	     (boot:parse-token (reverse acc) port))
	    (else (loop (read-char port) acc in-bars?))))))

(define (boot:parse-number s)
  ;; Only ask the host about tokens that start like numbers: hosts vary
  ;; in what else STRING->NUMBER accepts (e.g. polar complex syntax), and
  ;; Scheme 48 returns an unspecified value, not #f, for some.
  (let ((n (string-length s)))
    (and (> n 0)
	 (or (char-numeric? (string-ref s 0))
	     (and (> n 1)
		  (memv (string-ref s 0) '(#\+ #\- #\.))
		  (or (char-numeric? (string-ref s 1))
		      (and (> n 2)
			   (char=? (string-ref s 1) #\.)
			   (char-numeric? (string-ref s 2))))))
	 (boot:string-every? (lambda (c)
			       (or (char-numeric? c)
				   (memv c '(#\+ #\- #\. #\/ #\e #\E))))
			     s)
	 (let ((x (string->number s)))
	   (and (number? x) x)))))

(define (boot:token->string chars)
  ;; CL reader case folding: unescaped characters are upcased.
  (list->string (map (lambda (p) (if (cdr p) (car p) (char-upcase (car p))))
		     chars)))

(define (boot:parse-token chars port)
  (let ((escaped? (let loop ((l chars))
		    (and (pair? l) (or (cdar l) (loop (cdr l))))))
	(colon (let loop ((l chars) (i 0))
		 (cond ((null? l) #f)
		       ((and (char=? (caar l) #\:) (not (cdar l))) i)
		       (else (loop (cdr l) (+ i 1)))))))
    (cond ((and (not escaped?)
		(= (length chars) 1)
		(char=? (caar chars) #\.))
	   boot:dot)
	  ((and (not escaped?)
		(not colon)
		(boot:parse-number (list->string (map car chars))))
	   => (lambda (n) n))
	  ((not colon)
	   (boot:intern (boot:token->string chars) boot:scheme-package))
	  (else
	   (let* ((after (list-tail chars (+ colon 1)))
		  (after (if (and (pair? after)
				  (char=? (caar after) #\:)
				  (not (cdar after)))
			     (cdr after)
			     after))
		  (name (boot:token->string after)))
	     (if (= colon 0)
		 (boot:keyword name)
		 (let* ((pname (boot:token->string
				(let loop ((l chars) (i 0))
				  (if (= i colon) '()
				      (cons (car l) (loop (cdr l) (+ i 1)))))))
			(package (boot:find-package pname)))
		   (if (not package)
		       (boot:reader-error port "no such package" pname))
		   (boot:cl-constant (boot:intern name package)))))))))

;; Under Pseudoscheme, CL's NIL is (), T is #t and PS:FALSE is #f
;; (core.lisp), so that's how the translator sees them in its sources.
(define (boot:cl-constant sym)
  (cond ((eq? sym (boot:ps-symbol "NIL")) '())
	((eq? sym (boot:ps-symbol "T")) #t)
	((eq? sym (boot:ps-symbol "FALSE")) #f)
	(else sym)))

(define (boot:read-file filename)
  (call-with-input-file filename
    (lambda (port)
      (let loop ((acc '()))
	(let ((form (boot:read port)))
	  (if (boot:eof? form)
	      (reverse acc)
	      (loop (cons form acc))))))))

;; PS's exports: the CL symbols (and runtime entry points) translated
;; code may refer to without a package prefix.  Read from pack.lisp, the
;; same file that defines the package under Common Lisp.

(define (boot:load-ps-exports! pack-file)
  (for-each
   (lambda (form)
     (if (and (pair? form)
	      (eq? (car form) 'defpackage)
	      (equal? (cadr form) "PS"))
	 (for-each
	  (lambda (clause)
	    (if (and (pair? clause) (eq? (car clause) (boot:keyword "EXPORT")))
		(for-each (lambda (name)
			    (boot:export (boot:intern name boot:ps-package)
					 boot:ps-package))
			  (cdr clause))))
	  (cddr form))))
   (boot:read-file pack-file))
  ;; pack.lisp exports INTERN separately (an old ABCL workaround).
  (boot:export (boot:intern "INTERN" boot:ps-package) boot:ps-package))
