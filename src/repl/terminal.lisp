;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-REPL -*-

;;;; The terminal under the REPL's line editor (editor.lisp): raw mode,
;;;; keys, the window's width and ANSI output.  Raw mode needs SBCL's
;;;; sb-posix; elsewhere, or when the standard input or output isn't a
;;;; terminal, the REPL reads lines as before (repl.lisp).

(defpackage "PSEUDOSCHEME-REPL"
  (:use "COMMON-LISP")
  (:export "RUN-REPL" "MAKE-LANGUAGE" "LANGUAGE" "*LANGUAGES*" "FIND-LANGUAGE"
	   "READ-INPUT" "INTERACTIVE-P" "*OPTIONS*" "OPTION"
	   "DEFINE-LANGUAGE" "DEFINE-COMMAND" "HIGHLIGHT"))

(in-package "PSEUDOSCHEME-REPL")

(defun interactive-p ()
  "Are the standard input and output both terminals, and does $TERM
allow a line editor?"
  #+sbcl (and (= 1 (sb-unix:unix-isatty 0))
	      (= 1 (sb-unix:unix-isatty 1))
	      (not (equal (sb-posix:getenv "TERM") "dumb")))
  #-sbcl nil)

;;; ------------------------------------------------------------------
;;; Raw mode

(defmacro with-raw-terminal (() &body body)
  "Run BODY with the terminal unbuffered and not echoing: each key comes
at once, Ctrl-C included, and nothing is printed but what the editor
prints.  Bracketed paste is on, so a paste arrives as one insertion."
  `(call-with-raw-terminal (lambda () ,@body)))

#+sbcl
(defun call-with-raw-terminal (thunk)
  (let* ((old (sb-posix:tcgetattr 0))
	 (raw (sb-posix:tcgetattr 0)))
    (setf (sb-posix:termios-lflag raw)
	  (logandc2 (sb-posix:termios-lflag raw)
		    (logior sb-posix:icanon sb-posix:echo sb-posix:isig sb-posix:iexten))
	  (sb-posix:termios-iflag raw)
	  (logandc2 (sb-posix:termios-iflag raw)
		    (logior sb-posix:icrnl sb-posix:ixon)))
    (let ((cc (sb-posix:termios-cc raw)))
      (setf (aref cc sb-posix:vmin) 1
	    (aref cc sb-posix:vtime) 0)
      (setf (sb-posix:termios-cc raw) cc))
    (unwind-protect
	 (progn (sb-posix:tcsetattr 0 sb-posix:tcsadrain raw)
		(emit (csi "?2004h"))
		(funcall thunk))
      (emit (csi "?2004l"))
      (sb-posix:tcsetattr 0 sb-posix:tcsadrain old))))

#-sbcl
(defun call-with-raw-terminal (thunk) (funcall thunk))

(defun terminal-width ()
  "The terminal's width in columns: TIOCGWINSZ's, else $COLUMNS, else 80."
  (or #+sbcl
      (ignore-errors
       (sb-alien:with-alien ((ws (sb-alien:array (sb-alien:unsigned 16) 4)))
	 (let ((request #+darwin #x40087468 #+linux #x5413 #-(or darwin linux) nil))
	   (when (and request
		      (sb-unix:unix-ioctl 1 request (sb-alien:alien-sap ws)))
	     (let ((cols (sb-alien:deref ws 1)))
	       (and (> cols 0) cols))))))
      (ignore-errors (parse-integer (uiop:getenv "COLUMNS")))
      80))

;;; ------------------------------------------------------------------
;;; Output

(defvar *out* nil
  "Where the editor draws: the terminal; NIL for *STANDARD-OUTPUT*.")

(defun csi (&rest parts)
  (format nil "~C[~{~A~}" #\Esc parts))

(defun emit (&rest strings)
  (let ((out (or *out* *standard-output*)))
    (dolist (s strings) (write-string s out))
    (finish-output out)))

(defparameter *colors*
  '(:black 30 :red 31 :green 32 :yellow 33 :blue 34 :magenta 35 :cyan 36 :white 37
    :gray 90 :bright-red 91 :bright-green 92 :bright-yellow 93 :bright-blue 94
    :bright-magenta 95 :bright-cyan 96))

(defun sgr (&rest attributes)
  "The escape sequence selecting ATTRIBUTES: colors from *COLORS*, :bold,
:dim, :underline, :inverse, or :reset."
  (format nil "~C[~{~D~^;~}m" #\Esc
	  (mapcar (lambda (a)
		    (case a
		      (:reset 0) (:bold 1) (:dim 2) (:italic 3) (:underline 4) (:inverse 7)
		      (t (or (getf *colors* a) 0))))
		  attributes)))

(defun char-columns (c)
  "How many columns C takes: 2 for East Asian wide characters, 0 for
combining marks, else 1."
  (let ((code (char-code c)))
    (cond ((< code #x300) 1)
	  ((<= #x300 code #x36f) 0)
	  ((or (<= #x1100 code #x115f) (<= #x2e80 code #xa4cf) (<= #xac00 code #xd7a3)
	       (<= #xf900 code #xfaff) (<= #xfe30 code #xfe4f) (<= #xff00 code #xff60)
	       (<= #xffe0 code #xffe6) (<= #x1f300 code #x1f64f) (<= #x1f900 code #x1f9ff)
	       (<= #x20000 code #x3fffd))
	   2)
	  (t 1))))

(defun string-columns (s &key (start 0) (end (length s)))
  (loop for i from start below end sum (char-columns (char s i))))

;;; ------------------------------------------------------------------
;;; Keys

#+sbcl
(defun read-byte-raw (&optional timeout)
  "The next byte from the terminal, or NIL if TIMEOUT seconds pass
first, or :EOF."
  (when (or (null timeout) (sb-sys:wait-until-fd-usable 0 :input timeout))
    (sb-alien:with-alien ((buf (sb-alien:array (sb-alien:unsigned 8) 1)))
      (loop
	(multiple-value-bind (n errno) (sb-unix:unix-read 0 (sb-alien:alien-sap buf) 1)
	  (cond ((and n (= n 1)) (return (sb-alien:deref buf 0)))
		((and n (= n 0)) (return :eof))
		((eql errno sb-unix:eintr))
		(t (return :eof))))))))

(defun read-char-raw (&optional timeout)
  "The next character from the terminal (decoding UTF-8), NIL on
timeout, or :EOF."
  (let ((b (read-byte-raw timeout)))
    (cond ((not (integerp b)) b)
	  ((< b #x80) (code-char b))
	  (t (let* ((n (cond ((>= b #xf0) 3) ((>= b #xe0) 2) (t 1)))
		    (code (logand b (case n (3 #x07) (2 #x0f) (t #x1f)))))
	       (loop repeat n
		     for c = (read-byte-raw 0.05)
		     while (integerp c)
		     do (setq code (logior (ash code 6) (logand c #x3f))))
	       (or (ignore-errors (code-char code)) #\?))))))

(defun read-key ()
  "The next key: a character, or a keyword (:up, :left, :home, :delete,
:ctrl-left, :alt-b, :paste ...), or (:paste . text) for a bracketed
paste, or :EOF."
  (let ((c (read-char-raw)))
    (cond ((not (characterp c)) :eof)
	  ((char= c #\Esc) (read-escape))
	  (t c))))

(defun read-escape ()
  (let ((c (read-char-raw 0.05)))
    (cond ((null c) :escape)
	  ((not (characterp c)) :eof)
	  ((char= c #\[) (read-csi))
	  ((char= c #\O)
	   (case (read-char-raw 0.05)
	     (#\A :up) (#\B :down) (#\C :right) (#\D :left) (#\H :home) (#\F :end)
	     (t :unknown)))
	  ((member c '(#\Return #\Newline)) :alt-return)
	  ((member (char-code c) '(127 8)) :alt-backspace)
	  ((alpha-char-p c)
	   (intern (format nil "ALT-~A" (char-upcase c)) :keyword))
	  (t :unknown))))

(defun read-csi ()
  "After ESC [: the parameters and final character of a control sequence."
  (let ((params (make-string-output-stream)))
    (loop for c = (read-char-raw 0.05)
	  while (characterp c)
	  do (if (or (digit-char-p c) (char= c #\;))
		 (write-char c params)
		 (let ((p (get-output-stream-string params)))
		   (return
		     (cond ((and (char= c #\~) (string= p "200")) (read-paste))
			   ((char= c #\~)
			    (case (parse-integer p :junk-allowed t)
			      ((1 7) :home) ((4 8) :end) (3 :delete) (5 :page-up) (6 :page-down)
			      (t :unknown)))
			   (t
			    (let ((modifier (let ((semi (position #\; p)))
					      (and semi (parse-integer p :start (1+ semi) :junk-allowed t))))
				  (key (case c
					 (#\A :up) (#\B :down) (#\C :right) (#\D :left)
					 (#\H :home) (#\F :end) (#\Z :backtab) (t :unknown))))
			      (cond ((member modifier '(5)) ; ctrl
				     (case key (:right :ctrl-right) (:left :ctrl-left)
				       (:up :ctrl-up) (:down :ctrl-down) (t key)))
				    ((member modifier '(3 9)) ; alt
				     (case key (:right :alt-f) (:left :alt-b) (t key)))
				    (t key))))))))
	  finally (return :unknown))))

(defun read-paste ()
  "A bracketed paste's text, up to ESC [ 201 ~."
  (let ((text (make-string-output-stream))
	(end (format nil "~C[201~~" #\Esc)))
    (loop with tail = ""
	  for c = (read-char-raw 1)
	  while (characterp c)
	  do (write-char c text)
	     (setq tail (let ((s (concatenate 'string tail (string c))))
			  (subseq s (max 0 (- (length s) (length end))))))
	  until (string= tail end))
    (let ((s (get-output-stream-string text)))
      (cons :paste
	    (substitute #\Newline #\Return
			(if (and (>= (length s) (length end))
				 (string= end s :start2 (- (length s) (length end))))
			    (subseq s 0 (- (length s) (length end)))
			    s))))))
