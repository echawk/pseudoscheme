;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-REPL -*-

;;;; The REPL every mode shares.  A language (DEFINE-LANGUAGE, in
;;;; languages.lisp) says how to read, evaluate and print, and what's
;;;; bound for completion and ,describe; the REPL adds the line editor
;;;; (editor.lisp) at a terminal, meta-commands after a comma (,help
;;;; lists them, with the names Guile's REPL uses), backtraces of the
;;;; last error, and ,language to move between modes.  Without a
;;;; terminal it reads lines, prompting as before.

(in-package "PSEUDOSCHEME-REPL")

(defstruct (language (:constructor %make-language))
  name					; a keyword: :r7rs, :chez, ...
  (title "")				; for the banner and ,language
  prompt				; a string or a function of no arguments
  eval					; a function of one form, returning its values
  read-forms				; a function of a string, returning its forms
  (write #'default-write)		; a function of a value and a stream
  names					; a function of no arguments: the names bound at top level
  describe				; a function of a name (string) and a stream
  expand				; a function of a form, returning its expansion
  error-text				; a function of a condition, returning its message
  load					; a function of a path
  cell					; a function of a name: the Lisp symbol holding its value, or NIL
  run					; if a function, the language has a REPL of its own
  (unspecified-p (constantly nil)))	; a function: is a value not worth printing?

(defvar *languages* '() "The languages ,language knows, in order.")

(defmacro define-language (name &rest keys)
  "Define (or redefine) language NAME, a keyword, by MAKE-LANGUAGE's keys."
  `(register-language (%make-language :name ,name ,@keys)))

(defun register-language (language)
  (setf *languages*
	(append (remove (language-name language) *languages* :key #'language-name)
		(list language)))
  language)

(defun make-language (&rest keys) (apply #'%make-language keys))

(defun find-language (name)
  (find (intern (string-upcase (string name)) :keyword) *languages* :key #'language-name))

(defun default-write (value stream)
  (funcall ps:*scheme-write* value stream))

;;; ------------------------------------------------------------------
;;; State

(defvar *language* nil "The language the REPL is running.")
(defvar *histories* (make-hash-table) "Each language's input history.")
(defvar *values* '() "The values printed, newest first: ($n . value).")
(defvar *value-count* 0)
(defvar *last-backtrace* nil "The backtrace of the last error, as text.")
(defvar *last-error* nil)
(defvar *interactive* nil "Is the line editor in use?")
(defvar *quiet* nil "No prompts when the line editor isn't in use (Chez's --quiet).")
(defvar *quit* nil "Set by ,quit: the tag to throw to.")

(defun history-for (language)
  (or (gethash (language-name language) *histories*)
      (setf (gethash (language-name language) *histories*)
	    (if *interactive* (load-history (language-name language)) (make-instance 'history)))))

(defun prompt-string (language)
  (let ((p (language-prompt language)))
    (if (functionp p) (funcall p) p)))

;;; ------------------------------------------------------------------
;;; Output

(defun out () *standard-output*)

(defun say (control &rest args)
  (fresh-line (out))
  (apply #'format (out) control args)
  (fresh-line (out))
  (finish-output (out)))

(defun colored (text &rest style)
  (if (and *interactive* (option :color))
      (concatenate 'string (apply #'sgr style) text (sgr :reset))
      text))

(defun value-text (value)
  (with-output-to-string (s) (funcall (language-write *language*) value s)))

(defun show-value (value)
  (let ((text (value-text value)))
    (push (cons (incf *value-count*) value) *values*)
    (when (> (length *values*) 100) (setf *values* (subseq *values* 0 100)))
    (when (and *interactive* (option :color))
      (setq text (highlight text)))
    (if (option :print-numbered)
	(format (out) "~A ~A~%" (colored (format nil "$~D =" *value-count*) :gray) text)
	(format (out) "~A~%" text))))

(defun show-values (values)
  (dolist (v values)
    (unless (funcall (language-unspecified-p *language*) v)
      (show-value v)))
  (finish-output (out)))

(defun show-error (e)
  (let ((text (handler-case (if (language-error-text *language*)
				(funcall (language-error-text *language*) e)
				(princ-to-string e))
		(error () (format nil "~A" (type-of e))))))
    (fresh-line (out))
    (format (out) "~A ~A~%" (colored ";; Error:" :bright-red :bold) (colored text :red))
    (when *interactive*
      (format (out) "~A~%" (colored ";; ,bt shows the backtrace" :gray)))
    (finish-output (out))))

;;; ------------------------------------------------------------------
;;; psyntax's names

(defun source-name (symbol)
  (and (symbolp symbol) (get symbol :source-name)))

(defun readable-datum (x)
  "X, an expansion, with psyntax's generated names replaced by the names
in the source they stand for (made distinct with .1, .2 ...)."
  (let ((names (make-hash-table :test 'eq)) (used (make-hash-table :test 'equal)))
    (labels ((rename (sym)
	       (or (gethash sym names)
		   (setf (gethash sym names)
			 (let* ((base (source-name sym))
				(n (incf (gethash base used 0))))
			   (ps:intern-scheme-symbol (if (= n 1) base (format nil "~A.~D" base (1- n))))))))
	     (walk (x seen)
	       (cond ((and (symbolp x) (source-name x)) (rename x))
		     ((and (consp x) (not (gethash x seen)))
		      (setf (gethash x seen) t)
		      (cons (walk (car x) seen) (walk (cdr x) seen)))
		     ((and (simple-vector-p x) (not (stringp x)))
		      (map 'vector (lambda (e) (walk e seen)) x))
		     (t x))))
      (walk x (make-hash-table :test 'eq)))))

(defun readable-text (text)
  "TEXT (a backtrace) with psyntax's generated names, |g$...$N|, as
their source names, and the Scheme package prefixes left out."
  (let ((out (make-string-output-stream)) (i 0) (n (length text)))
    (loop while (< i n)
	  do (let ((start (search "g$" text :start2 i)))
	       (if (null start)
		   (progn (write-string text out :start i) (setq i n))
		   (let* ((end (or (position-if-not (lambda (c) (or (alphanumericp c) (char= c #\$)))
						    text :start (+ start 2))
				   n))
			  (sym (find-symbol (subseq text start end) "SCHEME"))
			  (name (source-name sym)))
		     (write-string text out :start i :end start)
		     (write-string (or name (subseq text start end)) out)
		     (setq i end)))))
    (let ((s (get-output-stream-string out)))
      (dolist (prefix '("|LIBRARY psyntax host|::" "SCHEME::") s)
	(loop for p = (search prefix s) while p
	      do (setq s (concatenate 'string (subseq s 0 p) (subseq s (+ p (length prefix))))))))))

;;; ------------------------------------------------------------------
;;; Evaluating

(defun capture-backtrace ()
  (with-output-to-string (s)
    #+sbcl (ignore-errors (sb-debug:print-backtrace :count 60 :stream s))
    #-sbcl (format s "(no backtrace on this Lisp)")))

(defmacro with-repl-errors ((&key) &body body)
  "Run BODY; an error or an interrupt is reported, its backtrace kept
for ,bt, and NIL returned."
  `(call-with-repl-errors (lambda () ,@body)))

(defun call-with-repl-errors (thunk)
  (block nil
    (handler-bind
	((serious-condition
	   (lambda (e)
	     (when #+sbcl (typep e 'sb-sys:interactive-interrupt) #-sbcl nil
	       (fresh-line (out))
	       (format (out) "~A~%" (colored ";; Interrupted" :yellow))
	       (return nil))
	     (unless (typep e 'storage-condition)
	       (setq *last-backtrace* (capture-backtrace)))
	     (setq *last-error* e)
	     (show-error e)
	     (return nil))))
      (funcall thunk))))

(defun eval-text (text)
  "Read TEXT's forms in the current language and evaluate each,
printing the values; stop at the first error."
  (let ((forms (with-repl-errors () (list (funcall (language-read-forms *language*) text)))))
    (when forms
      (dolist (form (car forms))
	(let ((values (with-repl-errors ()
			(list (multiple-value-list (funcall (language-eval *language*) form))))))
	  (if values
	      (show-values (car values))
	      (return)))))))

;;; ------------------------------------------------------------------
;;; Meta-commands

(defstruct command names args doc function)

(defvar *commands* '())

(defmacro define-command ((name &rest aliases) lambda-list doc &body body)
  "A meta-command ,NAME (or ,ALIAS).  LAMBDA-LIST is () for none, (FORM)
for one form read from the rest of the line in the current language, or
(TEXT) for the rest of the line as a string."
  `(register-command
    (make-command :names ',(mapcar (lambda (n) (string-downcase (string n))) (cons name aliases))
		  :args ',(cond ((null lambda-list) nil)
				((string= (string (car lambda-list)) "FORM") 'form)
				(t 'text))
		  :doc ,doc
		  :function (lambda ,lambda-list ,@body))))

(defun register-command (command)
  (setf *commands*
	(append (remove (car (command-names command)) *commands*
			:key (lambda (c) (car (command-names c))) :test #'string-equal)
		(list command))))

(defun find-command (name)
  (find-if (lambda (c) (member name (command-names c) :test #'string-equal)) *commands*))

(defun split-command (text)
  "\",name rest...\" as the name and the rest."
  (let* ((s (string-left-trim " " (subseq text 1)))
	 (end (or (position-if (lambda (c) (member c '(#\Space #\Tab #\Newline))) s) (length s))))
    (values (subseq s 0 end) (string-trim '(#\Space #\Tab #\Newline) (subseq s end)))))

(defun run-command (text)
  (multiple-value-bind (name rest) (split-command text)
    (let ((command (find-command name)))
      (cond ((null command)
	     (say "Unknown command ,~A; ,help lists them." name))
	    (t
	     (with-repl-errors ()
	       (let ((arg (case (command-args command)
			    ((nil) nil)
			    (form (let ((forms (funcall (language-read-forms *language*) rest)))
				    (if forms (car forms) (error "~A needs an expression" name))))
			    (t rest))))
		 (if (command-args command)
		     (funcall (command-function command) arg)
		     (funcall (command-function command))))))))))

(defun command-p (text)
  "Is TEXT a meta-command: a comma, then a letter?"
  (let ((s (string-left-trim '(#\Space #\Tab #\Newline) text)))
    (and (> (length s) 1) (char= (char s 0) #\,) (alpha-char-p (char s 1))
	 (find-command (split-command s)))))

;;; ------------------------------------------------------------------
;;; The loop

(defun completions (prefix)
  "The names bound in the current language, and the commands after a
comma, beginning with PREFIX."
  (let ((names (if (and (> (length prefix) 0) (char= (char prefix 0) #\,))
		   (loop for c in *commands*
			 append (mapcar (lambda (n) (format nil ",~A" n)) (command-names c)))
		   (and (language-names *language*)
			(ignore-errors (funcall (language-names *language*)))))))
    (remove-if-not (lambda (n) (and (>= (length n) (length prefix))
				    (string= prefix n :end2 (length prefix))))
		   names)))

(defun read-text (language)
  "The next input: a string, or :EOF."
  (let ((prompt (prompt-string language)))
    (if *interactive*
	(loop
	  (let ((text (read-input (colored prompt :bold :green)
				  :history (history-for language)
				  :completer #'completions
				  :complete-p (lambda (s) (or (command-p s) (input-complete-p s))))))
	    (unless (eq text :interrupt) (return text))))
	(progn
	  (unless *quiet* (format (out) "~A" prompt))
	  (finish-output (out))
	  ;; lines until the input is complete
	  (let ((text nil))
	    (loop
	      (let ((line (read-line *standard-input* nil nil)))
		(cond ((null line) (return (or text :eof)))
		      (t (setq text (if text (format nil "~A~%~A" text line) line))
			 (when (or (command-p text) (input-complete-p text))
			   (return text)))))))))))

(defun run-language (language)
  "Run LANGUAGE's REPL until ,quit, end of file, or a switch to another
language (which is then returned)."
  (let ((*language* language))
    (catch 'switch
      (loop
	(let ((text (read-text language)))
	  (cond ((eq text :eof) (return nil))
		((blank-input-p text))
		((command-p text) (run-command text))
		(t (eval-text text))))))))

(defun run-repl (language &key banner)
  "Run the REPL in LANGUAGE (a language or its name), moving to others
as ,language asks, until ,quit or end of file."
  (let ((*interactive* (interactive-p))
	(language (if (language-p language) language (find-language language))))
    (when banner (say "~A" banner))
    (when *interactive*
      (say "~A" (colored "Enter ,help for help." :gray)))
    (catch 'quit
      (loop
	(let ((next (if (language-run language)
			(funcall (language-run language))
			(run-language language))))
	  (if (language-p next)
	      (progn (setq language next)
		     (say "~A" (colored (format nil ";; ~A" (language-title next)) :gray)))
	      (return)))))
    (values)))
