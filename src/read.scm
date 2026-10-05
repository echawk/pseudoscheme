
; Scheme48's READ module, adapted for use in Pseudoscheme.
; (compile-file "~/pseudo/read" scheme-translator::revised^4-scheme-env)
; (load "~/pseudo/read" scheme-translator::revised^4-scheme-env)
; (define read revised^4-scheme:.read)

(define char->ascii #'ps-lisp:char-code)
(define ascii->char #'ps-lisp:code-char)
(define ascii-whitespaces '(32 10 9 12 13)) ;space linefeed tab page return
(define ascii-limit 256)

(define (reverse-list->string l n)
  ;; Significantly faster than (list->string (reverse l))
  (let ((s (make-string n #\x)))
    (let loop ((i (- n 1)) (l l))
      (if (< i 0)
	  s
	  (begin (string-set! s i (car l))
		 (loop (- i 1) (cdr l)))))))


(define (input-port-option port-option)
  (cond ((null? port-option) (current-input-port))
	((null? (cdr port-option)) (car port-option))
	(else (error "read-mumble: too many arguments" port-option))))

(define error #'ps:scheme-error)
(define warn #'ps:scheme-warn)


; -*- Mode: Scheme; Syntax: Scheme; Package: Scheme; -*-
; Copyright (c) 1993 by Richard Kelsey and Jonathan Rees.  See file COPYING.


; A little Scheme reader.

; Nonstandard things used:
;  Ascii stuff: char->ascii, ascii->char, ascii-whitespaces, ascii-limit
;    (for dispatch table; portable definitions in alt/ascii.scm)
;  reverse-list->string  -- ok to define as follows:
;    (define (reverse-list->string l n)
;      (list->string (reverse l)))
;  really-string->symbol -- ok to define as follows:
;    ;  signal (only for use by reading-error; easily excised)


(define (scheme-read . port-option)
  (let ((port (input-port-option port-option)))
    (let loop ()
      (let ((form (sub-read port)))
        (cond ((not (reader-token? form)) form)
              ((eq? form close-paren)
               ;; Too many right parens.
	       (warn "discarding extraneous right parenthesis")
               (loop))
	      (else
	       (reading-error port (cdr form))))))))

(define (sub-read-carefully port)
  (let ((form (sub-read port)))
    (cond ((eof-object? form)
           (reading-error port "unexpected end of file"))
	  ((reader-token? form) (reading-error port (cdr form)))
	  (else form))))

(define reader-token-marker (list 'reader-token))
(define (make-reader-token message) (cons reader-token-marker message))
(define (reader-token? form)
  (and (pair? form) (eq? (car form) reader-token-marker)))

(define close-paren (make-reader-token "unexpected right parenthesis"))
(define dot         (make-reader-token "unexpected \" . \""))


; Main dispatch

; Characters beyond the dispatch tables (anything non-ASCII) are
; symbol constituents, as R6RS and R7RS allow for most of Unicode.

(define (sub-read port)
  (let ((c (read-char port)))
    (cond ((eof-object? c) c)
	  ((>= (char->ascii c) ascii-limit)
	   (parse-token (sub-read-token c port) port))
	  (else
	   ((vector-ref read-dispatch-vector (char->ascii c))
	    c port)))))

(define (terminating? c)
  (and (< (char->ascii c) ascii-limit)
       (vector-ref read-terminating?-vector (char->ascii c))))

(define read-dispatch-vector
  (make-vector ascii-limit
               (lambda (c port)
                 (reading-error port "illegal character read" c))))

(define read-terminating?-vector
  (make-vector ascii-limit #t))

(define (set-standard-syntax! char terminating? reader)
  (vector-set! read-dispatch-vector     (char->ascii char) reader)
  (vector-set! read-terminating?-vector (char->ascii char) terminating?))

(let ((sub-read-whitespace
       (lambda (c port)
         c                              ;ignored
         (sub-read port))))
  (for-each (lambda (c)
              (vector-set! read-dispatch-vector c sub-read-whitespace))
            ascii-whitespaces))

(let ((sub-read-constituent
       (lambda (c port)
	 (parse-token (sub-read-token c port) port))))
  (for-each (lambda (c)
              (set-standard-syntax! c #f sub-read-constituent))
            (string->list
             (string-append "!$%&*+-./0123456789:<=>?@^_~ABCDEFGHIJKLM"
                            "NOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))))

; Usual read macros

(define (set-standard-read-macro! c terminating? proc)
  (set-standard-syntax! c terminating? proc))

(define (sub-read-list c port)
  (let ((form (sub-read port)))
    (cond ((eof-object? form)
           (reading-error port
			  "end of file inside list -- unbalanced parentheses"))
          ((eq? form close-paren) '())
          ((eq? form dot)
           (let* ((last-form (sub-read-carefully port))
                  (another-form (sub-read port)))
             (cond ((eq? another-form close-paren) last-form)
                   (else
                    (reading-error port
				   "randomness after form after dot"
				   another-form)))))
          (else (cons form (sub-read-list c port))))))

(set-standard-read-macro! #\( #t sub-read-list)

; R6RS 4.2.1: brackets are parentheses.  (Not checked for matching.)

(set-standard-read-macro! #\[ #t sub-read-list)

(set-standard-read-macro! #\] #t
  (lambda (c port)
    c port
    close-paren))

; |symbol| with any characters (R7RS 2.1): \| \\ and \x<hex>; escapes.
; The name is taken exactly as written, with no case folding.

(set-standard-read-macro! #\| #t
  (lambda (c port)
    c
    (let loop ((l '()) (i 0))
      (let ((c (read-char port)))
	(cond ((eof-object? c)
	       (reading-error port "end of file within |symbol|"))
	      ((char=? c #\|)
	       (ps:intern-scheme-symbol (reverse-list->string l i)))
	      ((char=? c #\\)
	       (loop (cons (read-escape port) l) (+ i 1)))
	      (else (loop (cons c l) (+ i 1))))))))

(set-standard-read-macro! #\) #t
  (lambda (c port)
    c port
    close-paren))

(set-standard-read-macro! #\' #t
  (lambda (c port)
    c
    (list 'quote (sub-read-carefully port))))

(set-standard-read-macro! #\` #t
  (lambda (c port)
    c
    (list 'quasiquote (sub-read-carefully port))))

(set-standard-read-macro! #\, #t
  (lambda (c port)
    c
    (let* ((next (peek-char port))
	   ;; DO NOT beta-reduce!
	   (keyword (cond ((eof-object? next)
			   (reading-error port "end of file after ,"))
			  ((char=? next #\@)
			   (read-char port)
			   'unquote-splicing)
			  (else 'unquote))))
      (list keyword
            (sub-read-carefully port)))))

; String escapes: \\ and \" are the only ones R5RS requires, but \n \t
; \r \a \b \0 are standard as of R6RS/R7RS and are such common Scheme
; (and C) idiom that most R5RS implementations already accept them too
; -- and chibi's own R5RS test suite relies on \n. A backslash before
; any other character is read as that character literally, same as
; before.

; READ-ESCAPE reads what follows a backslash in a string or |symbol|:
; the one-character escapes above, or \x<hex>; (R6RS/R7RS).

(define (read-escape port)
  (let ((c (read-char port)))
    (cond ((eof-object? c)
	   (reading-error port "end of file after \\"))
	  ((char=? c #\x)
	   (let loop ((digits '()))
	     (let ((d (read-char port)))
	       (cond ((eof-object? d)
		      (reading-error port "end of file in \\x escape"))
		     ((char=? d #\;)
		      (let ((n (string->number (list->string (reverse digits)) 16)))
			(if n
			    (ascii->char n)
			    (reading-error port "bad \\x escape"))))
		     (else (loop (cons d digits)))))))
	  (else (string-escape-char c)))))

(define (string-escape-char c)
  (cond ((char=? c #\n) #\newline)
	((char=? c #\t) #\tab)
	((char=? c #\r) #\return)
	((char=? c #\a) (ascii->char 7))
	((char=? c #\b) (ascii->char 8))
	((char=? c #\0) (ascii->char 0))
	(else c)))

(define (skip-line-continuation port)
  (let skip-before ()
    (let ((c (read-char port)))
      (cond ((eof-object? c) c)
	    ((char=? c #\newline)
	     (let skip-after ()
	       (let ((d (peek-char port)))
		 (if (and (char? d) (or (char=? d #\space) (char=? d #\tab)))
		     (begin (read-char port) (skip-after))))))
	    (else (skip-before))))))

(set-standard-read-macro! #\" #t
  (lambda (c port)
    c ;ignored
    (let loop ((l '()) (i 0))
      (let ((c (read-char port)))
        (cond ((eof-object? c)
               (reading-error port "end of file within a string"))
              ((char=? c #\\)
	       (let ((next (peek-char port)))
		 (cond ((eof-object? next)
			(reading-error port "end of file within a string"))
		       ;; \<intraline whitespace><newline><intraline
		       ;; whitespace>: a line continuation, read as nothing.
		       ((or (char=? next #\newline) (char=? next #\space)
			    (char=? next #\tab))
			(skip-line-continuation port)
			(loop l i))
		       (else
			(loop (cons (read-escape port) l) (+ i 1))))))
              ((char=? c #\")
	       (reverse-list->string l i))
              (else
	       (loop (cons c l) (+ i 1))))))))

(set-standard-read-macro! #\; #t
  (lambda (c port)
    c ;ignored
    (gobble-line port)
    (sub-read port)))

(define (gobble-line port)
  (let loop ()
    (let ((c (read-char port)))
      (cond ((eof-object? c) c)
	    ((char=? c #\newline) #f)
	    (else (loop))))))

(define *sharp-macros* '())

(define (define-sharp-macro c proc)
  (set! *sharp-macros* (cons (cons c proc) *sharp-macros*)))

(set-standard-read-macro! #\# #f
  (lambda (c port)
    c ;ignored
    (let* ((c (peek-char port))
	   (c (if (eof-object? c)
		  (reading-error port "end of file after #")
		  (char-downcase c)))
	   (probe (assq c *sharp-macros*)))
      (if probe
	  ((cdr probe) c port)
	  (reading-error port "unknown # syntax" c)))))

; #t #f and R7RS's #true #false.

(define (sharp-boolean value long-name)
  (lambda (c port)
    c
    (let ((name (car (sub-read-token (read-char port) port))))
      (if (or (= (string-length name) 1)
	      (string=? (common-lisp:string-downcase name) long-name))
	  value
	  (reading-error port "unknown # syntax" name)))))

(define-sharp-macro #\f
  (let ((boolean (sharp-boolean #f "false")))
    (lambda (c port)
      ;; #f32( and #f64( are SRFI 4 vectors
      (read-char port)                  ;consume f
      (let ((next (peek-char port)))
	(if (and (char? next) (char-numeric? next))
	    (sharp-numeric-vector-after "f" c port)
	    (let ((name (car (sub-read-token #\f port))))
	      (if (or (= (string-length name) 1)
		      (string=? (common-lisp:string-downcase name) "false"))
		  #f
		  (reading-error port "unknown # syntax" name))))))))
(define-sharp-macro #\t (sharp-boolean #t "true"))

; Directives: #!fold-case and #!no-fold-case (R7RS 2.1), #!r6rs (R6RS
; 4.2.4, a flag with no effect here), and any other #!<identifier>,
; which is ignored the same way.  Each is a comment, so read on.

(define-sharp-macro #\!
  (lambda (c port)
    (read-char port)                   ;consume the !
    (let ((name (common-lisp:string-downcase (car (sub-read-token (read-char port) port)))))
      (cond ((string=? name "fold-case")
	     (ps-lisp:setq ps:*fold-case* ps-lisp:t))
	    ((string=? name "no-fold-case")
	     (ps-lisp:setq ps:*fold-case* ps-lisp:nil)))
      (sub-read port))))

(define named-characters
  `((space     . ,(ascii->char 32))
    (newline   . ,(ascii->char 10))
    (tab       . ,(ascii->char 9))
    (nul       . ,(ascii->char 0))
    (null      . ,(ascii->char 0))
    (backspace . ,(ascii->char 8))
    (delete    . ,(ascii->char 127))
    (rubout    . ,(ascii->char 127))
    (escape    . ,(ascii->char 27))
    (altmode   . ,(ascii->char 27))
    (return    . ,(ascii->char 13))
    (linefeed  . ,(ascii->char 10))
    (page      . ,(ascii->char 12))
    (alarm     . ,(ascii->char 7))
    (vtab      . ,(ascii->char 11))    ;R6RS
    (esc       . ,(ascii->char 27))))  ;R6RS

(define-sharp-macro #\\
  (lambda (c port)
    c
    (read-char port)
    (let ((c (peek-char port)))
      (cond ((eof-object? c)
	     (reading-error port "end of file after #\\"))
	    ((char-alphabetic? c)
	     (let ((name (sub-read-carefully port)))
	       (cond ((= (string-length (symbol->string name)) 1)
		      c)
		     ;; #\x<hex> (R6RS/R7RS)
		     ((and (char=? (string-ref (symbol->string name) 0) #\x)
			   (string->number (substring (symbol->string name) 1
						      (string-length (symbol->string name)))
					   16))
		      => ascii->char)
		     ;; Character names match case-insensitively (#\Space,
		     ;; #\newline, ...); the keys of NAMED-CHARACTERS are
		     ;; lower-case names.
		     ((assq (string->symbol
			     (common-lisp:string-downcase (symbol->string name)))
			    named-characters)
		      => cdr)
		     (else
		      (reading-error port "unknown #\\ name" name)))))
	    (else
	     (read-char port))))))

(define-sharp-macro #\(
  (lambda (c port)
    (read-char port)
    (list->vector (sub-read-list c port))))

; #| ... |# block comments (nestable) and #;datum datum comments.

(define (skip-block-comment port depth)
  (if (= depth 0)
      #f
      (let ((c (read-char port)))
	(cond ((eof-object? c)
	       (reading-error port "end of file within a block comment"))
	      ((and (char=? c #\#) (eqv? (peek-char port) #\|))
	       (read-char port)
	       (skip-block-comment port (+ depth 1)))
	      ((and (char=? c #\|) (eqv? (peek-char port) #\#))
	       (read-char port)
	       (skip-block-comment port (- depth 1)))
	      (else
	       (skip-block-comment port depth))))))

(define-sharp-macro #\|
  (lambda (c port)
    (read-char port)                   ;consume the |
    (skip-block-comment port 1)
    (sub-read port)))

; #:name -- a Lisp keyword, for calling Lisp functions with keyword
; arguments (docs/interop.md).  Neither R6RS nor R7RS gives #: a meaning.
(define-sharp-macro #\:
  (lambda (c port)
    (read-char port)                   ;consume the :
    (let ((p (peek-char port)))
      (if (or (eof-object? p) (terminating? p))
          (reading-error port "missing keyword name after #:")
          (ps:intern-lisp-keyword (car (sub-read-token (read-char port) port)))))))

(define-sharp-macro #\;
  (lambda (c port)
    (read-char port)                   ;consume the ;
    (sub-read-carefully port)          ;discard the next datum
    (sub-read port)))

(let ((number-sharp-macro
       (lambda (c port)
	 c
	 (let ((string (car (sub-read-token #\# port))))
	   (or (string->number string)
	       (reading-error port "unsupported number syntax" string))))))
  (for-each (lambda (c)
	      (define-sharp-macro c number-sharp-macro))
	    '(#\b #\o #\d #\x #\i #\e)))


; Tokens
;
; SUB-READ-TOKEN returns a list whose car is the token as written.
; (It used to return a case-folded spelling and the raw one; symbols
; are now case-sensitive -- see INVERT-CASE in core.lisp -- and folding,
; when wanted, happens in INTERN-TOKEN.)

(define (sub-read-token c port)
  (let loop ((l (list c)) (n 1))
    (let ((p (peek-char port)))
      (cond ((or (eof-object? p) (terminating? p))
             (list (reverse-list->string l n)))
            (else
             (let ((c (read-char port)))
               (loop (cons c l) (+ n 1))))))))

(define (parse-token token port)
  (let ((string (car token)))
    (if (let ((c (string-ref string 0)))
	  (or (char-numeric? c) (char=? c #\+) (char=? c #\-) (char=? c #\.)))
	(cond ((string->number string))
	      ((member string strange-symbol-names)
	       (intern-token string))
	      ((string=? string ".")
	       dot)
	      ;; Anything else that doesn't parse as a number: a symbol
	      ;; if it can be one (R6RS/R7RS "peculiar identifiers" such
	      ;; as ->x and .foo), else an error.
	      ((char-numeric? (string-ref string 0))
	       (reading-error port "unsupported number syntax" string))
	      (else (intern-token string)))
	(intern-token string))))

(define strange-symbol-names
  '("+" "-" "..." "1+" "-1+"))  ;The latter two only for S&ICP support

; NB: no special handling of ":" here. It's tempting to read FOO:BAR
; as a Common-Lisp-style package-qualified symbol (the way the CL
; reader bridge, and the #'-escape below, do) -- but unlike in CL, ":"
; is just an ordinary constituent character in R5RS/R7RS symbol syntax
; with no special meaning, and real Scheme code uses it that way (e.g.
; ":::" as a custom syntax-rules ellipsis identifier, in R7RS). Giving
; it CL semantics here breaks those. This reader is for Scheme source;
; code that specifically wants to name a CL symbol from Scheme should
; keep using the #'-escape (below), same as read.scm's own definitions
; at the top of this file do.

(define (intern-token string)
  (ps:intern-scheme-symbol
   (ps-lisp:if ps:*fold-case* (common-lisp:string-downcase string) string)))

; Reader errors

(define (reading-error port message . irritants)
  port ;unused
  (apply error message irritants))


; Initialize for Pseudoscheme...

(ps-lisp:setq ps:*scheme-read* scheme-read)


;; Bytevectors: R7RS #u8(...) and R6RS #vu8(...); and SRFI 4's (and
;; SRFI 160's) homogeneous vectors, #s16(...), #f64(...), #c128(...).

(define (sharp-numeric-vector c port)
  (sharp-numeric-vector-after (string (char-downcase (read-char port))) c port))

;; After # and the tag's letter LETTER: digits, then the list.
(define (sharp-numeric-vector-after letter c port)
  (let loop ((digits '()))
    (let ((d (peek-char port)))
      (if (and (char? d) (char-numeric? d))
	  (loop (cons (read-char port) digits))
	  (let ((tag (string-append letter (list->string (reverse digits)))))
	    (if (not (eqv? (read-char port) #\())
		(reading-error port "bad homogeneous vector syntax" tag))
	    (if (string=? tag "u8")
		(ps:list->bytevector (sub-read-list c port))
		(ps:list->numeric-vector tag (sub-read-list c port))))))))

(define-sharp-macro #\u sharp-numeric-vector)
(define-sharp-macro #\s sharp-numeric-vector)
(define-sharp-macro #\c sharp-numeric-vector)

(define-sharp-macro #\v
  (lambda (c port)
    (read-char port)                   ;consume v
    (if (not (and (eqv? (read-char port) #\u)
		  (eqv? (read-char port) #\8)
		  (eqv? (read-char port) #\()))
	(reading-error port "bad #vu8 syntax"))
    (ps:list->bytevector (sub-read-list c port))))

;; R6RS 4.3.5 abbreviations: #'x #`x #,x #,@x.  (These replace an older
;; Pseudoscheme-specific #'cl-function escape; see README.)

(define-sharp-macro #\'
  (lambda (c port)
    c
    (read-char port)
    (list 'syntax (sub-read-carefully port))))

(define-sharp-macro #\`
  (lambda (c port)
    c
    (read-char port)
    (list 'quasisyntax (sub-read-carefully port))))

(define-sharp-macro #\,
  (lambda (c port)
    c
    (read-char port)
    (if (eqv? (peek-char port) #\@)
	(begin (read-char port)
	       (list 'unsyntax-splicing (sub-read-carefully port)))
	(list 'unsyntax (sub-read-carefully port)))))
