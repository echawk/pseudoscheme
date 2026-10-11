;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-REPL -*-

;;;; A lexer for Scheme text good enough to color it: rainbow
;;;; parentheses, strings, comments, characters, numbers, syntactic
;;;; keywords; and to tell whether an input is complete (the REPL's
;;;; Enter submits it only then) and which parenthesis matches which.
;;;; It knows every mode's syntax only roughly; the mode's own reader
;;;; decides what the text means.

(in-package "PSEUDOSCHEME-REPL")

(defstruct (token (:constructor make-token (start end kind &optional depth)))
  start end kind depth)

(defparameter *special-forms*
  '("define" "define-syntax" "define-record-type" "define-library" "define-values"
    "define-module" "define-public" "define*" "define-macro" "define-syntax-rule"
    "lambda" "lambda*" "case-lambda" "let" "let*" "letrec" "letrec*" "let-values"
    "let*-values" "let-syntax" "letrec-syntax" "if" "cond" "case" "and" "or" "when"
    "unless" "do" "begin" "set!" "quote" "quasiquote" "unquote" "unquote-splicing"
    "syntax-rules" "syntax-case" "with-syntax" "syntax" "quasisyntax" "import" "export"
    "library" "include" "guard" "parameterize" "delay" "delay-force" "make-promise"
    "else" "=>" "assert" "fluid-let" "module" "use-modules" "eval-when" "match"
    "receive" "named-lambda" "case-lambda*" "with-exception-handler" "dynamic-wind"
    "call/cc" "call-with-current-continuation" "values" "call-with-values")
  "Names colored as syntax.")

(defvar *special-form-table* nil)

(defun special-form-p (name)
  (unless *special-form-table*
    (setq *special-form-table* (make-hash-table :test 'equal))
    (dolist (n *special-forms*) (setf (gethash n *special-form-table*) t)))
  (gethash name *special-form-table*))

(defun delimiter-p (c)
  (or (member c '(#\( #\) #\[ #\] #\{ #\} #\" #\; #\' #\` #\,))
      (member c '(#\Space #\Tab #\Newline #\Return #\Page))))

(defun lex (text)
  "TEXT's tokens, and the depth of parentheses open at its end, and
whether it ends inside a string or block comment (:string, :comment)."
  (let ((tokens '()) (depth 0) (i 0) (n (length text)) (state nil))
    (flet ((add (start end kind &optional d) (push (make-token start end kind d) tokens))
	   (at (j) (and (< j n) (char text j))))
      (loop
	(when (>= i n) (return))
	(let ((c (char text i)) (start i))
	  (cond
	    ((member c '(#\Space #\Tab #\Newline #\Return #\Page)) (incf i))
	    ((char= c #\;)
	     (loop while (and (< i n) (char/= (char text i) #\Newline)) do (incf i))
	     (add start i :comment))
	    ((and (char= c #\#) (eql (at (1+ i)) #\|))
	     (let ((level 1))
	       (incf i 2)
	       (loop while (and (< i n) (> level 0))
		     do (cond ((and (char= (char text i) #\|) (eql (at (1+ i)) #\#))
			       (decf level) (incf i 2))
			      ((and (char= (char text i) #\#) (eql (at (1+ i)) #\|))
			       (incf level) (incf i 2))
			      (t (incf i))))
	       (when (> level 0) (setq state :comment))
	       (add start i :comment)))
	    ((and (char= c #\#) (eql (at (1+ i)) #\;))
	     (incf i 2) (add start i :comment))
	    ((char= c #\")
	     (incf i)
	     (loop (cond ((>= i n) (setq state :string) (return))
			 ((char= (char text i) #\\) (incf i 2))
			 ((char= (char text i) #\") (incf i) (return))
			 (t (incf i))))
	     (setq i (min i n))
	     (add start i :string))
	    ((member c '(#\( #\[ #\{))
	     (incf i) (add start i :open depth) (incf depth))
	    ((member c '(#\) #\] #\}))
	     (incf i) (decf depth) (add start i :close depth))
	    ((member c '(#\' #\`))
	     (incf i) (add start i :quote))
	    ((char= c #\,)
	     (incf i) (when (eql (at i) #\@) (incf i)) (add start i :quote))
	    ((char= c #\#)
	     (let ((d (at (1+ i))))
	       (cond
		 ((eql d #\\)		; a character: #\x, #\space, #\(
		  (incf i 3)
		  (loop while (and (< i n) (not (delimiter-p (char text i)))) do (incf i))
		  (setq i (min i n))
		  (add start i :char))
		 ((member d '(#\' #\` #\,))
		  (incf i 2) (when (eql (at i) #\@) (incf i)) (add start i :quote))
		 ((eql d #\()
		  (incf i 2) (add start i :open depth) (incf depth))
		 (t
		  (incf i)
		  (loop while (and (< i n) (not (delimiter-p (char text i)))) do (incf i))
		  (cond ((eql (at i) #\()	; #u8( #vu8( #s16(
			 (incf i) (add start i :open depth) (incf depth))
			(t
			 (let ((word (string-downcase (subseq text start i))))
			   (add start i
				(cond ((member word '("#t" "#f" "#true" "#false" "#nil") :test #'string=)
				       :boolean)
				      ((or (search "#x" word) (search "#b" word) (search "#o" word)
					   (search "#d" word) (search "#e" word) (search "#i" word))
				       :number)
				      ((and (> (length word) 1) (char= (char word 1) #\:)) :keyword)
				      (t :hash))))))))))
	    ((char= c #\|)
	     (incf i)
	     (loop while (and (< i n) (char/= (char text i) #\|)) do (incf i))
	     (setq i (min (1+ i) n))
	     (add start i :symbol))
	    (t
	     (loop while (and (< i n) (not (delimiter-p (char text i))))
		   do (if (char= (char text i) #\|)
			  (progn (incf i)
				 (loop while (and (< i n) (char/= (char text i) #\|)) do (incf i))
				 (setq i (min (1+ i) n)))
			  (incf i)))
	     (let ((word (subseq text start i)))
	       (add start i
		    (cond ((number-like-p word) :number)
			  ((or (and (> (length word) 1) (char= (char word (1- (length word))) #\:))
			       (and (> (length word) 1) (char= (char word 0) #\:)))
			   :keyword)
			  ((special-form-p word) :special)
			  (t :symbol))))))))
      (values (nreverse tokens) depth state))))

(defun number-like-p (word)
  (let ((c (char word 0)))
    (or (digit-char-p c)
	(and (member c '(#\+ #\- #\.)) (> (length word) 1)
	     (or (digit-char-p (char word 1))
		 (and (char= (char word 1) #\.) (> (length word) 2) (digit-char-p (char word 2)))))
	(member word '("+inf.0" "-inf.0" "+nan.0" "-nan.0") :test #'string-equal))))

(defun input-complete-p (text)
  "Is TEXT ready to evaluate: its parentheses closed (or overclosed, for
the reader to complain), and not inside a string or block comment, nor
ending in a quote that waits for its datum?"
  (multiple-value-bind (tokens depth state) (lex text)
    (and (<= depth 0)
	 (null state)
	 (let ((last (car (last tokens))))
	   (not (and last (eq (token-kind last) :quote)))))))

(defun blank-input-p (text)
  "Is TEXT only whitespace and comments?"
  (every (lambda (tok) (eq (token-kind tok) :comment)) (lex text)))

(defun matching-paren (text point)
  "The index of the parenthesis matching the one just before POINT (a
closer) or at POINT (an opener), and that one's index; or NIL."
  (let ((tokens (lex text)) (stack '()) (pairs '()))
    (dolist (tok tokens)
      (case (token-kind tok)
	(:open (push tok stack))
	(:close (let ((open (pop stack)))
		  (when open
		    (push (cons (1- (token-end open)) (token-start tok)) pairs))))))
    (or (loop for (open . close) in pairs
	      when (= (1+ close) point) return (values open close))
	(loop for (open . close) in pairs
	      when (= open point) return (values close open)))))

(defparameter *paren-colors*
  '(:bright-blue :bright-magenta :bright-cyan :bright-yellow :bright-green :bright-red))

(defun token-style (tok)
  (case (token-kind tok)
    ((:open :close)
     (if (< (token-depth tok) 0)
	 (list :bright-red :inverse)
	 (list (nth (mod (token-depth tok) (length *paren-colors*)) *paren-colors*))))
    (:string '(:green))
    (:comment '(:gray :italic))
    (:char '(:yellow))
    (:number '(:cyan))
    (:boolean '(:magenta))
    (:quote '(:yellow))
    (:keyword '(:cyan))
    (:special '(:bold))
    (:hash '(:yellow))
    (t nil)))

(defun highlight (text &key mark)
  "TEXT with escape sequences coloring it; the characters at the indices
in MARK (matching parentheses) shown bold and underlined."
  (if (not (option :color))
      text
      (with-output-to-string (out)
	(let ((tokens (lex text)) (i 0))
	  (flet ((plain (end)
		   (when (< i end) (write-string text out :start i :end end))
		   (setq i (max i end))))
	    (dolist (tok tokens)
	      (plain (token-start tok))
	      (let ((style (if (option :rainbow)
			       (token-style tok)
			       (and (not (member (token-kind tok) '(:open :close))) (token-style tok)))))
		(loop for j from (token-start tok) below (token-end tok)
		      do (let ((marked (member j mark)))
			   (when (or style marked)
			     (write-string (apply #'sgr (append style (and marked '(:bold :underline)))) out))
			   (write-char (char text j) out)
			   (when (or style marked) (write-string (sgr :reset) out))))
		(setq i (token-end tok))))
	    (plain (length text)))))))
