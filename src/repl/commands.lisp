;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-REPL -*-

;;;; The REPL's meta-commands, named as Guile's are where they overlap.

(in-package "PSEUDOSCHEME-REPL")

(define-command (help h) (text)
  "Show the commands, or one command's help: ,help [COMMAND]."
  (if (and text (> (length text) 0))
      (let ((c (find-command (string-left-trim "," text))))
	(if c
	    (say ",~{~A~^, ,~}~%  ~A" (command-names c) (command-doc c))
	    (say "No command ,~A." text)))
      (progn
	(say "Commands (abbreviations in brackets):")
	(dolist (c *commands*)
	  (format (out) "  ~A~22T~A~%"
		  (colored (format nil ",~A~@[ [~{,~A~^ ~}]~]" (car (command-names c))
				   (cdr (command-names c)))
			   :bold)
		  (let ((doc (command-doc c)))
		    (subseq doc 0 (or (position #\: doc) (position #\. doc) (length doc))))))
	(say "~A" (colored "At a terminal: Tab completes, Up/Down and Ctrl-R search the history,
Enter evaluates once the parentheses close (Alt-Enter inserts a newline),
Ctrl-C cancels, Ctrl-D on an empty line quits." :gray)))))

(define-command (quit q exit) ()
  "Leave the REPL."
  (throw 'quit nil))

(define-command (language L mode) (text)
  "Show the languages, or change to one: ,language [NAME]."
  (if (or (null text) (= (length text) 0))
      (dolist (l *languages*)
	(format (out) "~:[ ~;*~] ~(~8A~) ~A~%" (eq l *language*) (language-name l) (language-title l)))
      (let ((l (find-language text)))
	(if l
	    (throw 'switch l)
	    (say "No language ~A; ,language lists them." text)))))

(define-command (load l) (text)
  "Load a file in the current language: ,load FILE."
  (let ((path (string-trim "\"" text)))
    (if (language-load *language*)
	(funcall (language-load *language*) path)
	(error "this language can't load files"))
    (say "~A" (colored (format nil ";; loaded ~A" path) :gray))))

(define-command (describe d) (text)
  "Describe what a name is bound to: ,describe NAME."
  (if (language-describe *language*)
      (funcall (language-describe *language*) text (out))
      (say "This language can't describe bindings.")))

(define-command (apropos a) (text)
  "List the bound names containing a string: ,apropos STRING."
  (let ((names (sort (remove-if-not (lambda (n) (search text n :test #'char-equal))
				    (and (language-names *language*) (funcall (language-names *language*))))
		     #'string<)))
    (if names
	(format (out) "~A~%" (columns-text names))
	(say "Nothing bound matches ~S." text))))

(define-command (expand exp) (form)
  "Expand an expression's macros: ,expand EXPR."
  (if (language-expand *language*)
      (show-value (readable-datum (funcall (language-expand *language*) form)))
      (say "This language can't expand macros.")))

(define-command (time t) (form)
  "Evaluate an expression and show the time it took: ,time EXPR."
  (let* ((real (get-internal-real-time))
	 (run (get-internal-run-time))
	 #+sbcl (bytes (sb-ext:get-bytes-consed))
	 (values (multiple-value-list (funcall (language-eval *language*) form))))
    (show-values values)
    (say "~A"
	 (colored (format nil ";; ~,6Fs real time, ~,6Fs run time~@[, ~:D bytes allocated~]"
			  (/ (- (get-internal-real-time) real) internal-time-units-per-second)
			  (/ (- (get-internal-run-time) run) internal-time-units-per-second)
			  #+sbcl (- (sb-ext:get-bytes-consed) bytes) #-sbcl nil)
		  :gray))))

(define-command (pp) (form)
  "Evaluate an expression and pretty-print its value: ,pp EXPR."
  (dolist (v (multiple-value-list (funcall (language-eval *language*) form)))
    (let ((text (value-text v)))
      (format (out) "~A~%" (if (and *interactive* (option :color)) (highlight (pretty text)) (pretty text))))))

(define-command (backtrace bt) (text)
  "Show the backtrace of the last error: ,bt [all] (all: the REPL's own
frames and the error system's too)."
  (if *last-backtrace*
      (format (out) "~A" (if (string-equal text "all")
			     *last-backtrace*
			     (readable-text (trim-backtrace *last-backtrace*))))
      (say "No error yet.")))

(define-command (error-info e) ()
  "Show the last error again, with its condition type."
  (if *last-error*
      (progn (show-error *last-error*)
	     (say "~A" (colored (format nil ";; a ~(~A~)" (type-of *last-error*)) :gray)))
      (say "No error yet.")))

(define-command (values v) ()
  "Show the values printed so far, numbered."
  (dolist (entry (reverse (subseq *values* 0 (min 20 (length *values*)))))
    (format (out) "~A ~A~%" (colored (format nil "$~D =" (car entry)) :gray) (value-text (cdr entry)))))

(define-command (option o) (text)
  "Show the options, or set one: ,option [NAME [VALUE]]."
  (if (or (null text) (= (length text) 0))
      (loop for (k v) on *options* by #'cddr
	    do (format (out) "  ~(~A~)~20T~A~%" k (cond ((eq v t) "#t") ((null v) "#f") (t v))))
      (let* ((parts (uiop:split-string text :separator '(#\Space)))
	     (key (intern (string-upcase (first parts)) :keyword)))
	(unless (member key *options*)
	  (error "no option ~A" (first parts)))
	(if (second parts)
	    (setf (option key)
		  (let ((v (second parts)))
		    (cond ((member v '("#t" "t" "on" "yes" "true") :test #'string-equal) t)
			  ((member v '("#f" "nil" "off" "no" "false") :test #'string-equal) nil)
			  ((every #'digit-char-p v) (parse-integer v))
			  (t v))))
	    (format (out) "~(~A~) = ~A~%" key (option key))))))

(define-command (cd) (text)
  "Change the current directory: ,cd DIRECTORY."
  (let ((dir (uiop:ensure-directory-pathname
	      (merge-pathnames (if (> (length text) 0) text (user-homedir-pathname))))))
    (unless (uiop:directory-exists-p dir) (error "no directory ~A" dir))
    (setf *default-pathname-defaults* (truename dir))
    #+sbcl (sb-posix:chdir (namestring (truename dir)))
    (say "~A" (namestring *default-pathname-defaults*))))

(define-command (pwd) ()
  "Show the current directory."
  (say "~A" (namestring *default-pathname-defaults*)))

(define-command (shell !) (text)
  "Run a shell command: ,shell COMMAND."
  (uiop:run-program text :force-shell t :output :interactive :error-output :interactive
			 :input :interactive :ignore-error-status t))

(define-command (history) (text)
  "Show the last inputs: ,history [N]."
  (let* ((entries (history-entries (history-for *language*)))
	 (n (min (length entries) (if (and text (every #'digit-char-p text) (> (length text) 0))
				      (parse-integer text) 20))))
    (loop for i from (- (length entries) n) below (length entries)
	  do (format (out) "~4D  ~A~%" (1+ i)
		     (substitute #\Space #\Newline (aref entries i))))))

(define-command (clear) ()
  "Clear the screen."
  (format (out) "~A~A" (csi "H") (csi "2J")) (finish-output (out)))

;;; ------------------------------------------------------------------
;;; Tracing and disassembling

(defvar *traced* '() "(cell original . name): the procedures ,trace wraps.")
(defvar *trace-depth* 0)

(defun binding-cell (name)
  (let ((cell (and (language-cell *language*) (funcall (language-cell *language*) name))))
    (unless (and cell (boundp cell))
      (error "~A isn't a variable defined here" name))
    cell))

(define-command (trace tr) (text)
  "Trace calls of top-level procedures: ,trace NAME ...  (,trace alone lists them)."
  (if (= (length text) 0)
      (say "~:[Nothing is traced.~;Traced: ~:*~{~A~^ ~}~]" (mapcar #'cddr *traced*))
      (dolist (name (uiop:split-string text :separator '(#\Space)))
	(unless (= (length name) 0)
	  (let* ((cell (binding-cell name)) (f (symbol-value cell)))
	    (cond ((not (functionp f)) (say "~A isn't a procedure." name))
		  ((assoc cell *traced*) (say "~A is traced already." name))
		  (t
		   (push (list* cell f name) *traced*)
		   (set-cell cell (tracer name f))
		   (say "~A" (colored (format nil ";; tracing ~A" name) :gray)))))))))

(defun set-cell (cell f)
  "Set a top-level procedure's location, value and function both, as
psyntax's host-set! does: calls go through the function."
  (setf (symbol-value cell) f)
  (ps:set-function-from-value cell))

(defun tracer (name f)
  (lambda (&rest args)
    (let ((indent (make-string (* 2 (min *trace-depth* 20)) :initial-element #\Space)))
      (format (out) "~A~A~%" indent
	      (colored (format nil "(~A~{ ~A~})" name (mapcar #'value-text args)) :yellow))
      (let ((values (let ((*trace-depth* (1+ *trace-depth*))) (multiple-value-list (apply f args)))))
	(format (out) "~A~A~%" indent
		(colored (format nil "=> ~{~A~^ ~}" (mapcar #'value-text values)) :gray))
	(values-list values)))))

(define-command (untrace) (text)
  "Stop tracing: ,untrace [NAME ...] (all, without names)."
  (let ((names (remove "" (uiop:split-string text :separator '(#\Space)) :test #'string=)))
    (setf *traced*
	  (remove-if (lambda (entry)
		       (destructuring-bind (cell f . name) entry
			 (when (or (null names) (member name names :test #'string=))
			   (set-cell cell f)
			   (say "~A" (colored (format nil ";; untraced ~A" name) :gray))
			   t)))
		     *traced*))))

(define-command (disassemble x) (form)
  "Disassemble a procedure's native code: ,disassemble EXPR."
  (let ((f (funcall (language-eval *language*) form)))
    (if (functionp f)
	(disassemble f :stream (out))
	(say "Not a procedure: ~A" (value-text f)))))

;;; ------------------------------------------------------------------
;;; Helpers

(defun trim-backtrace (text)
  "TEXT's frames from the one that signalled the error down to the REPL's
own, renumbered."
  (let* ((lines (remove-if (lambda (l) (or (= (length l) 0) (search "Backtrace for" l)))
			   (uiop:split-string text :separator '(#\Newline))))
	 (signal (position-if (lambda (l) (some (lambda (s) (search s l))
						'("%SIGNAL" "(ERROR " "(CERROR " "INTERNAL-ERROR")))
			      lines :from-end t
				    :end (or (position-if (lambda (l) (search "PSEUDOSCHEME-REPL::EVAL-TEXT" l))
							  lines)
					     (length lines))))
	 (start (if signal (1+ signal) 0))
	 (end (or (position-if (lambda (l) (search "PSEUDOSCHEME-REPL::" l)) lines :start start)
		  (length lines))))
    (with-output-to-string (out)
      (loop for line in (remove-if #'plumbing-frame-p (subseq lines start end))
	    for i from 0
	    do (format out "~3D: ~A~%" i
		       (let ((colon (position #\: line)))
			 (if (and colon (every #'digit-char-p (string-trim " " (subseq line 0 colon))))
			     (string-left-trim " " (subseq line (1+ colon)))
			     line)))))))

(defparameter *plumbing-frames*
  '("SB-INT:SIMPLE-EVAL" "(EVAL " "SB-IMPL::" "SB-C::" "SB-C:" "(FLET \"WITHOUT"
    "PSEUDOSCHEME-PSYNTAX::EVAL-COMPILED" "PSEUDOSCHEME-PSYNTAX::CALL-WITH-"
    "PSEUDOSCHEME-PSYNTAX::HOST-EVAL" ":IN PSEUDOSCHEME-PSYNTAX::HOST-EVAL")
  "Frames ,bt leaves out: the evaluator's own (,bt all shows them).")

(defun plumbing-frame-p (line)
  (some (lambda (s) (search s line)) *plumbing-frames*))

(defun pretty (text)
  "TEXT, written data, broken into lines and indented when long."
  (if (< (length text) 70)
      text
      (handler-case
	  (with-output-to-string (out)
	    (let ((depth 0) (col 0) (i 0) (n (length text)))
	      (loop while (< i n)
		    do (let ((c (char text i)))
			 (cond ((char= c #\")
				(let ((end (loop for j from (1+ i) below n
						 when (char= (char text j) #\\) do (incf j)
						 else when (char= (char text j) #\") return (1+ j)
						 finally (return n))))
				  (write-string text out :start i :end end)
				  (incf col (- end i)) (setq i end)))
			       ((member c '(#\( #\[))
				(when (and (> depth 0) (> col (+ 40 (* 2 depth))))
				  (format out "~%~VA" (* 2 depth) "") (setq col (* 2 depth)))
				(write-char c out) (incf depth) (incf col) (incf i))
			       ((member c '(#\) #\]))
				(write-char c out) (decf depth) (incf col) (incf i))
			       (t (write-char c out) (incf col) (incf i)))))))
	(error () text))))
