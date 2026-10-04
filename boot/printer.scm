;;; boot/printer.scm -- printing translated code as Common Lisp source.
;;;
;;; Replaces the translator's WRITE-PRETTY (p-utils.scm), which binds
;;; *PACKAGE* to the target package and calls the CL pretty printer
;;; with *PRINT-CASE* :UPCASE.  Symbols accessible in the target package
;;; print without a prefix.  The layout is our own, not SBCL's: what
;;; matters is that the CL reader reads back the same forms (see
;;; boot/check.lisp), and that every host prints identical text.

(define boot:line-width 80)

;;; Atoms

(define (boot:potential-number? name)
  ;; Close enough to CLHS 2.3.1.1 for our purposes: if the CL reader
  ;; could take it for a number, escape it.
  (and (> (string-length name) 0)
       (or (char-numeric? (string-ref name 0))
	   (and (memv (string-ref name 0) '(#\+ #\- #\. #\^ #\_))
		(> (string-length name) 1)
		(not (boot:string-every? (lambda (c) (char=? c #\.)) name))))
       (not (boot:string-every? (lambda (c) (not (char-numeric? c))) name))
       (boot:parse-number name)))

(define (boot:symbol-needs-bars? name)
  (or (string=? name "")
      (boot:string-every? (lambda (c) (char=? c #\.)) name)
      (boot:potential-number? name)
      (char=? (string-ref name 0) #\#)
      (not (boot:string-every?
	    (lambda (c)
	      (and (not (char-lower-case? c))
		   (not (boot:whitespace? c))
		   (not (memv c '(#\( #\) #\' #\" #\; #\` #\, #\| #\\ #\:)))))
	    name))))

(define (boot:symbol-token name)
  (if (boot:symbol-needs-bars? name)
      (boot:string-join
       (append (list "|")
	       (map (lambda (c)
		      (if (memv c '(#\| #\\)) (string #\\ c) (string c)))
		    (string->list name))
	       (list "|")))
      name))

(define (boot:symbol->cl-string sym package)
  (let* ((home (boot:symbol-package sym))
	 (name (boot:symbol-name sym))
	 (found (boot:find-symbol name package)))
    (cond ((eq? home boot:keyword-package)
	   (string-append ":" (boot:symbol-token name)))
	  ((and found (eq? (car found) sym))
	   (boot:symbol-token name))
	  (else
	   (string-append (boot:package-name home)
			  (if (boot:table-ref (boot:package-externals home) name #f)
			      ":"
			      "::")
			  (boot:symbol-token name))))))

(define boot:cl-char-names
  '((0 . "Nul") (7 . "Bel") (8 . "Backspace") (9 . "Tab") (10 . "Newline")
    (11 . "Vt") (12 . "Page") (13 . "Return") (27 . "Esc") (32 . "Space")
    (127 . "Rubout")))

(define (boot:char->cl-string c)
  (let ((probe (assv (char->integer c) boot:cl-char-names)))
    (string-append "#\\" (if probe (cdr probe) (string c)))))

(define (boot:string->cl-string s)
  (boot:string-join
   (append (list "\"")
	   (map (lambda (c)
		  (if (memv c '(#\" #\\)) (string #\\ c) (string c)))
		(string->list s))
	   (list "\""))))

(define (boot:number->cl-string n)
  (let ((s (number->string n)))
    (cond ((exact? n) s)
	  ((not (boot:string-every?
		 (lambda (c) (or (char-numeric? c) (memv c '(#\- #\. #\e))))
		 s))
	   (boot:die "can't print this number in CL syntax" n))
	  (else
	   ;; Hosts disagree about "2." / ".5" / "2.0"; CL wants digits on
	   ;; both sides of the point.
	   (let* ((s (if (boot:string-index s #\.) s
			 (let ((e (boot:string-index s #\e)))
			   (if e
			       (string-append (substring s 0 e) ".0"
					      (substring s e (string-length s)))
			       (string-append s ".0")))))
		  (dot (boot:string-index s #\.))
		  (s (if (or (= dot 0) (char=? (string-ref s (- dot 1)) #\-))
			 (string-append (substring s 0 dot) "0"
					(substring s dot (string-length s)))
			 s))
		  (dot (boot:string-index s #\.)))
	     (if (or (= (+ dot 1) (string-length s))
		     (not (char-numeric? (string-ref s (+ dot 1)))))
		 (string-append (substring s 0 (+ dot 1)) "0"
				(substring s (+ dot 1) (string-length s)))
		 s))))))

(define (boot:atom->cl-string x package)
  (cond ((null? x) (boot:symbol->cl-string (boot:ps-symbol "NIL") package))
	((eq? x #t) (boot:symbol->cl-string (boot:ps-symbol "T") package))
	((eq? x #f) (boot:symbol->cl-string (boot:ps-symbol "FALSE") package))
	((symbol? x) (boot:symbol->cl-string x package))
	((number? x) (boot:number->cl-string x))
	((string? x) (boot:string->cl-string x))
	((char? x) (boot:char->cl-string x))
	(else (boot:die "can't print this object in CL syntax" x))))

;;; Flat printing

(define (boot:quote-prefix x)
  ;; 'X and #'X, as the CL pretty printer prints (QUOTE X), (FUNCTION X)
  (and (pair? x) (pair? (cdr x)) (null? (cddr x))
       (cond ((eq? (car x) (boot:ps-symbol "QUOTE")) "'")
	     ((eq? (car x) (boot:ps-symbol "FUNCTION")) "#'")
	     (else #f))))

(define (boot:flat x package)
  (boot:string-join (reverse (boot:flat-pieces x package '()))))

(define (boot:flat-pieces x package acc)
  (cond ((vector? x)
	 (cons ")" (boot:flat-list-pieces (vector->list x) package
					  (cons "#(" acc))))
	((not (pair? x)) (cons (boot:atom->cl-string x package) acc))
	((boot:quote-prefix x)
	 => (lambda (prefix)
	      (boot:flat-pieces (cadr x) package (cons prefix acc))))
	(else (cons ")" (boot:flat-list-pieces x package (cons "(" acc))))))

(define (boot:flat-list-pieces l package acc)
  (let loop ((l l) (acc acc) (first? #t))
    (cond ((null? l) acc)
	  ((pair? l)
	   (loop (cdr l)
		 (boot:flat-pieces (car l) package (if first? acc (cons " " acc)))
		 #f))
	  (else
	   (boot:flat-pieces l package (cons " . " acc))))))

;;; Pretty printing.  Each procedure takes the current column and
;;; returns the column after what it printed.

;; Operators whose first N arguments stay on the first line, with the
;; rest (the body) indented 2 under the open parenthesis.
(define boot:body-operators
  '(("DEFUN" . 2) ("DEFMACRO" . 2) ("DEFTYPE" . 2) ("LAMBDA" . 1)
    ("LET" . 1) ("LET*" . 1) ("FLET" . 1) ("LABELS" . 1) ("MACROLET" . 1)
    ("PROGN" . 0) ("LOCALLY" . 0) ("TAGBODY" . 0) ("AT-TOP-LEVEL" . 0)
    ("BLOCK" . 1) ("PROG" . 1) ("WHEN" . 1) ("UNLESS" . 1) ("CASE" . 1)
    ("DEFSTRUCT" . 1) ("DEFPACKAGE" . 1) ("DOLIST" . 1) ("DOTIMES" . 1)
    ("WITH-OPEN-FILE" . 1) ("UNWIND-PROTECT" . 1)
    ("MULTIPLE-VALUE-BIND" . 2) ("DESTRUCTURING-BIND" . 2)
    ("PROGV" . 2) ("EVAL-WHEN" . 1)))

(define (boot:spaces port n)
  (do ((i 0 (+ i 1))) ((= i n)) (write-char #\space port)))

(define (boot:newline-indent port col)
  (newline port)
  (boot:spaces port col)
  col)

(define (boot:display-flat s port col)
  (display s port)
  (let ((nl (let loop ((i (- (string-length s) 1)))
	      (cond ((< i 0) #f)
		    ((char=? (string-ref s i) #\newline) i)
		    (else (loop (- i 1)))))))
    (if nl
	(- (- (string-length s) nl) 1)
	(+ col (string-length s)))))

(define (boot:proper-list? x)
  (cond ((null? x) #t)
	((pair? x) (boot:proper-list? (cdr x)))
	(else #f)))

(define (boot:pp x package port col)
  (let ((flat (boot:flat x package)))
    (cond ((or (<= (+ col (string-length flat)) boot:line-width)
	       (not (pair? x))
	       (not (boot:proper-list? x)))
	   (boot:display-flat flat port col))
	  ((boot:quote-prefix x)
	   => (lambda (prefix)
		(boot:pp (cadr x) package port
			 (boot:display-flat prefix port col))))
	  ((symbol? (car x)) (boot:pp-call x package port col))
	  (else (boot:pp-column x package port col)))))

;; (a
;;  b
;;  c)
(define (boot:pp-column l package port col)
  (display "(" port)
  (let loop ((l l) (c (+ col 1)) (first? #t))
    (if (null? l)
	(begin (display ")" port) (+ c 1))
	(loop (cdr l)
	      (boot:pp (car l) package port
		       (if first? c (boot:newline-indent port (+ col 1))))
	      #f))))

(define (boot:pp-call x package port col)
  (let* ((op (boot:flat (car x) package))
	 (op-name (boot:symbol-name (car x)))
	 (body (assoc op-name boot:body-operators))
	 (args (cdr x)))
    (display "(" port)
    (display op port)
    (cond
     (body
      ;; (defun name args
      ;;   body...)
      (let loop ((args args) (n (cdr body)) (c (+ col 1 (string-length op))))
	(cond ((null? args) (display ")" port) (+ c 1))
	      ((> n 0)
	       (display " " port)
	       (loop (cdr args) (- n 1) (boot:pp (car args) package port (+ c 1))))
	      (else
	       (loop (cdr args) 0
		     (boot:pp (car args) package port
			      (boot:newline-indent port (+ col 2))))))))
     ((null? args) (display ")" port) (+ col 2 (string-length op)))
     (else
      ;; (f a        or, if F is long,  (f
      ;;    b                             a
      ;;    c)                            b)
      (let ((arg-col (if (<= (string-length op) 12)
			 (+ col 2 (string-length op))
			 (+ col 1))))
	(if (= arg-col (+ col 1))
	    (boot:newline-indent port arg-col)
	    (display " " port))
	(let loop ((args (cdr args))
		   (c (boot:pp (car args) package port arg-col)))
	  (if (null? args)
	      (begin (display ")" port) (+ c 1))
	      (loop (cdr args)
		    (boot:pp (car args) package port
			     (boot:newline-indent port arg-col))))))))))

;;; The translator's WRITE-PRETTY: (fresh-line) then the form.
;;; Translated files are written by the translator with plain DISPLAY
;;; and NEWLINE, so we track whether the port is at the start of a line
;;; through BOOT:WRITE-PRETTY only; every form starts on a new line.

(define (boot:write-pretty form port package)
  (boot:pp form package port 0)
  (newline port))
