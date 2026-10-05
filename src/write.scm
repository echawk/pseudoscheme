
; Scheme48's WRITE module, adapted for use in Pseudoscheme.
; (compile-file "~/pseudo/write" scheme-translator::revised^4-scheme-env)
; (load "~/pseudo/write" scheme-translator::revised^4-scheme-env)
; (define write revised^4-scheme:.write)
; (define display revised^4-scheme:display)

; Problem: symbols come out in upper case.


(define (output-port-option port-option)
  (cond ((null? port-option) (current-output-port))
	((null? (cdr port-option)) (car port-option))
	(else (ps:scheme-error "write-mumble: too many arguments"
				 port-option))))

(define (disclose obj)
  obj ;ignored
  #f)

(define (write-string string port)
  (ps-lisp:princ string port))


; -*- Mode: Scheme; Syntax: Scheme; Package: Scheme; -*-
; Copyright (c) 1993 by Richard Kelsey and Jonathan Rees.  See file COPYING.


; This is file write.scm.

;;;; WRITE

; To use this with some Scheme other than Scheme48, do the following:
;  1. Copy the definition of output-port-option from port.scm
;  2. Define write-string as appropriate (as a write-char loop)
;  3. (define (disclose x) #f)

(define (scheme-write obj . port-option)
  (write-labelled obj (output-port-option port-option) 1))

(define (scheme-write-shared obj . port-option)
  (write-labelled obj (output-port-option port-option) 2))

(define (scheme-write-simple obj . port-option)
  (write-labelled obj (output-port-option port-option) 0))

; Datum labels, #n= and #n# (R7RS 6.13.3): which objects get them is
; worked out beforehand (ps:call-with-write-labels).  MODE is 0 for
; none, 1 for those on a cycle, 2 for every shared one.

(define (write-labelled obj port mode)
  (ps:call-with-write-labels obj mode
    (lambda ()
      (let recur ((obj obj))
	(write-with-label obj port recur
	  (lambda () (recurring-write obj port recur)))))))

(define (write-with-label obj port recur write-it)
  (cond ((ps:write-label-reference obj)
	 => (lambda (n)
	      (write-char #\# port)
	      (write-number n port)
	      (write-char #\# port)))
	((ps:write-label-definition obj)
	 => (lambda (n)
	      (write-char #\# port)
	      (write-number n port)
	      (write-char #\= port)
	      (write-it)))
	(else (write-it))))

(define (recurring-write obj port recur)
  (cond ((null? obj) (write-string "()" port))
        ((pair? obj) (write-list obj port recur))
        ((eq? obj #t) (write-boolean 't port))
        ((eq? obj #f) (write-boolean 'f port))
        ((symbol? obj) (write-symbol obj port))
        ((number? obj) (write-number obj port))
        ((string? obj) (write-string-literal obj port))
        ((char? obj) (write-char-literal obj port))
	(else (write-other obj port recur))))

(define (write-symbol obj port)
  (cond ((ps:true? (ps-lisp:keywordp obj))     ;#:name, see read.scm
         (write-string "#:" port)
         (write-string (ps:scheme-symbol-name obj) port))
        ((ps:true? (ps:symbol-needs-bars-p (symbol->string obj)))
	 (write-barred-symbol (symbol->string obj) port))
        (else (write-string (symbol->string obj) port))))

; |symbol| (R7RS 2.1), with \| \\ and \x<hex>; escapes.

(define (write-barred-symbol name port)
  (write-char #\| port)
  (let ((len (string-length name)))
    (do ((i 0 (+ i 1)))
	((= i len) (write-char #\| port))
      (let ((c (string-ref name i)))
	(cond ((or (char=? c #\|) (char=? c #\\))
	       (write-char #\\ port)
	       (write-char c port))
	      ((or (< (char->integer c) 32) (= (char->integer c) 127))
	       (write-string "\\x" port)
	       (write-string (number->string (char->integer c) 16) port)
	       (write-char #\; port))
	      (else (write-char c port)))))))

(define (write-boolean mumble port)
  (write-char #\# port)
  (write-symbol mumble port))

(define (write-number n port)
  (write-string (number->string n 10) port))

(define (write-char-literal obj port)
  (let ((probe (character-name obj)))
    (write-string "#\\" port)
    (if probe
	(write-symbol probe port)
	(write-char obj port))))

(define (character-name char)
  (cond ((char=? char #\space) 'space)
        ((char=? char #\newline) 'newline)
	(else #f)))

(define (write-string-literal obj port)
  (write-char #\" port)
  (let ((len (string-length obj)))
    (do ((i 0 (+ i 1)))
	((= i len) (write-char #\" port))
      (let ((c (string-ref obj i)))
	(if (or (char=? c #\\) (char=? c #\"))
	    (write-char #\\ port))
	(write-char c port)))))

(define (write-list obj port recur)
  (cond ((and (quotation? obj)
	      (not (ps:true? (ps:write-labelled-p (cdr obj)))))
         (write-char #\' port)
         (recur (cadr obj)))
        (else
         (write-char #\( port)
         (recur (car obj))
         (let loop ((l (cdr obj))
                    (n 1))
              (cond ((or (not (pair? l))
			 ;; a labelled tail is written as a datum
			 (ps:true? (ps:write-labelled-p l)))
                     (cond ((not (null? l))
                            (write-string " . " port)
                            (recur l))))
                    (else
                      (write-char #\space port)
                      (recur (car l))
                      (loop (cdr l) (+ n 1)))))
         (write-char #\) port))))

(define (quotation? obj)
  (and (pair? obj)
       (eq? (car obj) 'quote)
       (pair? (cdr obj))
       (null? (cddr obj))))

(define (write-vector obj port recur)
   (write-string "#(" port)
   (let ((z (vector-length obj)))
     (cond ((> z 0)
            (recur (vector-ref obj 0))
            (let loop ((i 1))
              (cond ((>= i z))
                    (else
                     (write-char #\space port)
                     (recur (vector-ref obj i))
                     (loop (+ i 1))))))))
   (write-char #\) port))

; The vector case goes last just so that this version of WRITE can be
; used in Scheme implementations in which records, ports, or
; procedures are represented as vectors.  (Scheme48 doesn't have this
; property.)

(define (write-other obj port recur)
  (cond ((disclose obj)
	 => (lambda (l)
	      (write-string "#{" port)
	      (display-type-name (car l) port)
	      (for-each (lambda (x)
			  (write-char #\space port)
			  (recur x))
			(cdr l))
	      (write-string "}" port)))
	((procedure? obj) (write-string "#{Procedure}" port))
	((input-port? obj)  (write-string "#{Input-port}" port))
	((output-port? obj) (write-string "#{Output-port}" port))
	((eof-object? obj) (write-string "#{End-of-file}" port))
	((vector? obj) (write-vector obj port recur))
	((ps:true? (ps:numeric-vector-tag obj))  ;bytevectors, SRFI 4 vectors
	 => (lambda (tag)
	      (write-string "#" port)
	      (write-string tag port)
	      (let ((elements (ps:numeric-vector-elements obj)))
		(if (null? elements)
		    (write-string "()" port)
		    (write-list elements port recur)))))
	((eq? obj (if #f #f)) (write-string "#{Unspecific}" port))
	(else
	 ;; (write-string "#{Random object}" port)
	 (ps-lisp:prin1 obj port)
	 )))

; Display the symbol WHO-CARES as Who-cares.

(define (display-type-name name port)
  (if (symbol? name)
      (let* ((s (symbol->string name))
	     (len (string-length s)))
	(if (and (> len 0)
		 (char-alphabetic? (string-ref s 0)))
	    (begin (write-char (char-upcase (string-ref s 0)) port)
		   (do ((i 1 (+ i 1)))
		       ((>= i len))
		     (write-char (char-downcase (string-ref s i)) port)))
	    (display name port)))
      (display name port)))

;(define (write-string s port)
;  (do ((i 0 (+ i 1)))
;      ((= i (string-length s)))
;    (write-char (string-ref s i) port)))



; DISPLAY

(define (scheme-display obj . port-option)
  (let ((port (output-port-option port-option)))
    (ps:call-with-write-labels obj 1
      (lambda ()
	(let recur ((obj obj))
	  (cond ((string? obj) (write-string obj port))
		((char? obj) (write-char obj port))
		((and (symbol? obj) (not (ps:true? (ps-lisp:keywordp obj))))
		 (write-string (symbol->string obj) port))
		(else
		 (write-with-label obj port recur
		   (lambda () (recurring-write obj port recur))))))))))

(ps-lisp:setq ps:*scheme-write* scheme-write)
(ps-lisp:setq ps:*scheme-write-shared* scheme-write-shared)
(ps-lisp:setq ps:*scheme-write-simple* scheme-write-simple)
(ps-lisp:setq ps:*scheme-display* scheme-display)
