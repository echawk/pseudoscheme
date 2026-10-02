
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

(define really-string->symbol string->symbol)

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
;    (define really-string->symbol string->symbol)
;  signal (only for use by reading-error; easily excised)


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

(define (sub-read port)
  (let ((c (read-char port)))
    (if (eof-object? c)
        c
        ((vector-ref read-dispatch-vector (char->ascii c))
         c port))))

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

(define (string-escape-char c)
  (cond ((char=? c #\n) #\newline)
	((char=? c #\t) #\tab)
	((char=? c #\r) #\return)
	((char=? c #\a) (ascii->char 7))
	((char=? c #\b) (ascii->char 8))
	((char=? c #\0) (ascii->char 0))
	(else c)))

(set-standard-read-macro! #\" #t
  (lambda (c port)
    c ;ignored
    (let loop ((l '()) (i 0))
      (let ((c (read-char port)))
        (cond ((eof-object? c)
               (reading-error port "end of file within a string"))
              ((char=? c #\\)
               (let ((c (read-char port)))
		 (cond ((eof-object? c)
			(reading-error port "end of file within a string"))
		       (else
			(loop (cons (string-escape-char c) l) (+ i 1))))))
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

(define-sharp-macro #\f
  (lambda (c port) c (read-char port) #f))

(define-sharp-macro #\t
  (lambda (c port) c (read-char port) #t))

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
    (alarm     . ,(ascii->char 7))))

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
		     ;; Character names match case-insensitively (#\Space,
		     ;; #\newline, ...): fold to whichever case
		     ;; NAMED-CHARACTERS' own keys were folded to by
		     ;; PREFERRED-CASE when this file was translated.
		     ((assq (string->symbol
			     (list->string
			      (map preferred-case
				   (string->list (symbol->string name)))))
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
; SUB-READ-TOKEN collects both the case-folded spelling (used for
; symbol identity, same as always -- see PREFERRED-CASE above) and the
; raw, as-typed spelling (used only to remember how to display the
; resulting symbol -- see RECORD-ORIGINAL-SPELLING! below).

(define (sub-read-token c port)
  (let loop ((l (list (preferred-case c))) (raw (list c)) (n 1))
    (let ((p (peek-char port)))
      (cond ((or (eof-object? p)
                 (vector-ref read-terminating?-vector (char->ascii p)))
             (cons (reverse-list->string l n) (reverse-list->string raw n)))
            (else
             (let ((c (read-char port)))
               (loop (cons (preferred-case c) l) (cons c raw) (+ n 1))))))))

(define (parse-token token port)
  (let ((string (car token)))
    (if (let ((c (string-ref string 0)))
	  (or (char-numeric? c) (char=? c #\+) (char=? c #\-) (char=? c #\.)))
	(cond ((string->number string))
	      ((member string strange-symbol-names)
	       (intern-token string))
	      ((string=? string ".")
	       dot)
	      (else
	       (reading-error port "unsupported number syntax" string)))
	(record-original-spelling! (intern-token string) (cdr token)))))

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
  (really-string->symbol string))

; Reading folds case to match the rest of the system (see
; PREFERRED-CASE), so the only way to show a symbol in the case the
; program actually wrote it in -- what SYMBOL->STRING and WRITE users
; expect -- is to remember that original spelling out of band. First
; spelling seen for a given symbol wins, same as R5RS's case-folding
; model implies: once folded, two different original spellings denote
; the same symbol, so there's only one case to remember.

(define (record-original-spelling! sym raw)
  (ps-lisp:if (ps-lisp:get sym 'scheme::%original-spelling)
	      sym
	      (ps-lisp:progn
	       (ps-lisp:setf (ps-lisp:get sym 'scheme::%original-spelling) raw)
	       sym)))

; Tokens are case-folded to match the host Common Lisp's own default
; readtable case, same as the rest of Pseudoscheme's bootstrap (every
; already-translated .pso file, and every .scm file in this system,
; was read -- and its internal `(eq? ... 'foo)' comparisons baked in --
; under this same folding; switching it would require re-bootstrapping
; everything in lockstep, not just this file). What IS fixed here is
; that the *original*, as-typed spelling of each symbol is preserved
; separately (see RECORD-ORIGINAL-SPELLING!/SYMBOL->DISPLAY-STRING
; below) so WRITE and SYMBOL->STRING can still show a symbol in the
; case the program actually wrote it in, the way R5RS/R7RS users
; expect, without disturbing EQ?-identity or any internal keyword
; matching.

(define preferred-case
  (if (char=? (string-ref (symbol->string 't) 0) #\T)
      char-upcase
      char-downcase))


; Reader errors

(define (reading-error port message . irritants)
  port ;unused
  (apply error message irritants))


; Initialize for Pseudoscheme...

(ps-lisp:setq ps:*scheme-read* scheme-read)


(define-sharp-macro #\'
  (lambda (c port)
    c ;unused
    (read-char port)
    `(ps-lisp:function ,(ps-lisp:read-preserving-whitespace port))))
