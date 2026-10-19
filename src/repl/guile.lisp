;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-REPL -*-

;;;; Guile's language for the REPL: Guile's own REPL, (system repl repl),
;;;; with its meta-commands (,describe ,apropos ,time ,bt ,language
;;;; ...), run on Quasischeme's Guile.  At a terminal it reads through
;;;; the line editor, as Guile's (ice-9 readline) would: its current
;;;; input port is filled by READ-INPUT, and its repl-reader tells that
;;;; the prompt.  Guile's ,mode moves to another of Quasischeme's
;;;; languages, as ,language does in theirs.

(in-package "QUASISCHEME-REPL")

(defvar *guile-prompt* "scheme@(guile-user)> "
  "The prompt Guile's REPL last asked for.")

(defvar *guile-completer* nil
  "A Guile procedure of a prefix: the names bound in the current module
and the modules it uses that begin with it, and the meta-commands.")

(defvar *guile-hooks-installed* nil)

(defparameter *guile-setup*
  "(use-modules (system repl repl) (system repl command) (system repl common))
     ((@ (system repl repl) %inhibit-welcome-message) #t)
     (module-set! the-root-module 'repl-reader
       (lambda* (prompt #:optional (reader (fluid-ref current-reader)))
         (%quasischeme-set-prompt! (if (string? prompt) prompt (prompt)))
         ((or reader read) (current-input-port))))
     (define-meta-command ((mode quasischeme) repl #:optional name)
       \"mode [NAME]
Show Quasischeme's languages, or switch to one (r7rs, r6rs, chez, ...).\"
       (%quasischeme-mode (if name (symbol->string name) \"\")))
     ;; compiling each input natively takes SBCL a good part of a second
     (repl-default-option-set! 'interp #t)
     (define-meta-command ((disassemble compile) repl (form))
       \"disassemble EXP
Disassemble a compiled procedure: the VM's bytecode, or the native code
of a procedure compiled to Lisp.\"
       (let ((obj (repl-eval repl (repl-parse repl form))))
         (if (false-if-exception (and ((@ (system vm program) program?) obj) ((@ (system vm program) program-code) obj)))
             ((@ (system vm disassembler) disassemble-program) obj)
             (%quasischeme-lisp-disassemble obj))))
     (define-meta-command ((x compile) repl (form))
       \"x EXP
Disassemble a compiled procedure (,disassemble).\"
       (let ((obj (repl-eval repl (repl-parse repl form))))
         (if (false-if-exception (and ((@ (system vm program) program?) obj) ((@ (system vm program) program-code) obj)))
             ((@ (system vm disassembler) disassemble-program) obj)
             (%quasischeme-lisp-disassemble obj))))
     (define-meta-command ((describe help) repl (form))
       \"describe OBJ
Show OBJ's documentation, or what it is: a procedure's arguments, a value's type.\"
       (let* ((obj (repl-eval repl (repl-parse repl form)))
              (form (or (false-if-exception (syntax->datum form)) form))
              (doc (false-if-exception
                    ((module-ref (resolve-interface '(ice-9 documentation)) 'object-documentation)
                     obj))))
         (cond
          (doc (display doc) (newline))
          ((procedure? obj)
           (let ((a (procedure-property obj 'arglist))
                 (name (if (symbol? form) form (or (procedure-name obj) form))))
             (if (and (list? a) (= (length a) 5))
                 (format #t \"~a is a procedure: (~a~a~a~a~a)~%\" name name
                         (apply string-append (map (lambda (i) \" _\") (iota (car a))))
                         (if (> (cadr a) 0)
                             (apply string-append \" #:optional\"
                                    (map (lambda (i) \" _\") (iota (cadr a))))
                             \"\")
                         (if (pair? (caddr a))
                             (format #f \" #:key~{ ~a~}\" (map (lambda (k) (car k)) (caddr a)))
                             \"\")
                         (if (list-ref a 4) (format #f \" . ~a\" (list-ref a 4)) \"\"))
                 (format #t \"~a is a procedure~%\" name))))
          (else
           (format #t \"~s is ~a~%\" form
                   (cond ((number? obj) \"a number\") ((string? obj) \"a string\")
                         ((symbol? obj) \"a symbol\") ((pair? obj) \"a pair\")
                         ((vector? obj) \"a vector\") ((boolean? obj) \"a boolean\")
                         ((char? obj) \"a character\") ((module? obj) \"a module\")
                         ((struct? obj) \"a struct\") (else \"an object\")))))))
     (lambda (prefix)
       (let ((acc '()))
         (define (add! s)
           (if (and (>= (string-length s) (string-length prefix))
                    (string=? prefix (substring s 0 (string-length prefix))))
               (set! acc (cons s acc))))
         (define (add-module! m)
           (module-for-each (lambda (k v) (add! (symbol->string k))) m))
         (if (and (> (string-length prefix) 0) (char=? (string-ref prefix 0) #\\,))
             (for-each (lambda (group)
                         (for-each (lambda (names)
                                     (for-each (lambda (n) (add! (string-append \",\" (symbol->string n))))
                                               names))
                                   (cdr group)))
                       (@@ (system repl command) *command-table*))
             (begin
               (add-module! (current-module))
               (for-each add-module! (module-uses (current-module)))))
         acc))"
  "Evaluated in Guile once, a form at a time (so the macros the first
form imports are there for the rest): the reader hook, ,mode, and the completer.")

(defun install-guile-hooks ()
  (unless *guile-hooks-installed*
    (qsg::obarray-define (qsg::ssym "%quasischeme-set-prompt!")
			 (lambda (prompt)
			   ;; "" is a meta-command's reading its arguments: keep
			   ;; the prompt, should the editor need to show one
			   (unless (equal prompt "") (setq *guile-prompt* prompt))
			   ;; without the editor, Guile's prompt as Guile shows it
			   (unless (or *interactive* (equal prompt ""))
			     (write-string prompt) (finish-output))
			   qsg::*unspecified*))
    (qsg::obarray-define (qsg::ssym "%quasischeme-lisp-disassemble")
			 (lambda (f)
			   (if (functionp f)
			       (disassemble f)
			       (format t "Not a procedure: ~A~%" f))
			   qsg::*unspecified*))
    (qsg::obarray-define (qsg::ssym "%quasischeme-mode")
			 (lambda (name)
			   (if (string= name "")
			       (dolist (l *languages*)
				 (format t "~:[ ~;*~] ~(~8A~) ~A~%" (eq (language-name l) :guile)
					 (language-name l) (language-title l)))
			       (let ((l (find-language name)))
				 (if l
				     (throw 'switch l)
				     (format t "No language ~A; ,mode lists them.~%" name))))
			   qsg::*unspecified*))
    (setq *guile-completer* (qsg:eval-string *guile-setup*)
	  *guile-hooks-installed* t)))

(defun guile-completions (prefix)
  (let ((names (ignore-errors (funcall *guile-completer* prefix))))
    (loop for x = names then (cdr x)
	  while (consp x)
	  when (stringp (car x)) collect (car x))))

(defun guile-input ()
  "The next input for Guile's REPL, from the line editor: a string
ending in a newline, or NIL at end of file."
  (loop
    (let ((text (read-input (colored *guile-prompt* :bold :green)
			    :history (history-for (find-language :guile))
			    :completer #'guile-completions
			    :complete-p (lambda (s)
					  (let ((s (string-left-trim '(#\Space #\Tab #\Newline) s)))
					    (or (and (> (length s) 0) (char= (char s 0) #\,))
						(input-complete-p s)))))))
      (cond ((eq text :eof) (return nil))
	    ((eq text :interrupt))
	    (t (return (format nil "~A~%" text)))))))

(defun editor-port ()
  "A Guile input port whose characters come from the line editor."
  (let ((pending (make-array 0 :element-type '(unsigned-byte 8))))
    (qsg::make-port
     :kind "quasischeme-repl" :input t :output nil :encoding "UTF-8"
     :buffering :none
     :read (lambda (buf start count)
	     (when (zerop (length pending))
	       (let ((text (guile-input)))
		 (setq pending (if text
				   (coerce (sb-ext:string-to-octets text :external-format :utf-8)
					   '(simple-array (unsigned-byte 8) (*)))
				   (make-array 0 :element-type '(unsigned-byte 8))))))
	     (let ((n (min count (length pending))))
	       (replace buf pending :start1 start :end2 n)
	       (setq pending (subseq pending n))
	       n)))))

(defun run-guile-repl ()
  "Guile's REPL, until it quits (NIL) or ,mode switches language (which
is returned)."
  (qsg:boot)
  (install-guile-hooks)
  (catch 'switch
    (handler-case
	(let ((*standard-input* (if *interactive* (editor-port) *standard-input*)))
	  (qsg:eval-string "((@ (system repl repl) start-repl))"))
      (error (e)
	(say ";; Guile's REPL stopped: ~A" (qsg:error-text e))))
    nil))

;;; Evaluating Guile without its REPL: for the editor interface
;;; (ide.lisp), which sends text to evaluate rather than running a REPL.

(defun guile-ready ()
  (qsg:boot)
  (install-guile-hooks))

(defun guile-read-forms (text)
  (guile-ready)
  (with-input-from-string (in text)
    (loop for form = (qsg:guile-read in)
	  until (eq form qs:eof-object)
	  collect form)))

(defun guile-eval (form)
  (guile-ready)
  (qsg::call-with-guile-catch (lambda () (qsg::primitive-eval form))))

(defvar *guile-expander* nil)

(defun guile-expand (form)
  (guile-ready)
  (funcall (or *guile-expander*
	       (setq *guile-expander*
		     (qsg:eval-string
		      "(lambda (x) ((@ (language tree-il) tree-il->scheme) (macroexpand x)))")))
	   form))

(define-language :guile
  :title "Guile 3.0 (Guile's own REPL; ,mode switches back)"
  :prompt "scheme@(guile-user)> "
  :run #'run-guile-repl
  :read-forms #'guile-read-forms
  :eval #'guile-eval
  :write (lambda (value stream) (qsg::guile-write value stream))
  :names (lambda () (guile-ready) (guile-completions ""))
  :expand #'guile-expand
  :error-text #'qsg:error-text
  :load (lambda (path) (guile-ready) (qsg:load-file path))
  :unspecified-p (lambda (v) (eq v qsg::*unspecified*)))
