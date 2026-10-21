; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-RACKET -*-

;;;; read's fast path: a datum of the plain syntax (lists, symbols,
;;;; numbers, strings, characters, booleans, vectors, keywords, the
;;;; quote forms and comments), ASCII, from a port whose bytes are all
;;;; in its buffer (a file's, a string's), with the reader's parameters
;;;; at their defaults: parsed here, in Lisp, rather than by the
;;;; expander's reader, compiled Racket, which is ten times slower.
;;;; Anything else (a byte past ASCII, #hash, #rx, #0=, |sym|, an
;;;; infix dot, a malformed datum, ...) and nothing is consumed: the
;;;; expander's reader reads it, as it would have, errors included.
;;;; Like that reader's in read mode (not read-syntax), the strings and
;;;; vectors are mutable.

(in-package "QUASISCHEME-RACKET")

(defvar *fast-read-parameters* nil
  "((parameter . default) ...): the reader's parameters this path needs
at their defaults, from the expander, once it's up (INSTALL-FAST-READ).")

(defun install-fast-read (export)
  "Find the reader's parameters through EXPORT (the expander's exports)."
  (setq *fast-read-parameters*
	(loop for (name default) in `(("read-case-sensitive" ,qs:true)
				      ("read-square-bracket-as-paren" ,qs:true)
				      ("read-curly-brace-as-paren" ,qs:true)
				      ("read-square-bracket-with-tag" ,qs:false)
				      ("read-curly-brace-with-tag" ,qs:false)
				      ("read-decimal-as-inexact" ,qs:true)
				      ("read-single-flonum" ,qs:false)
				      ("read-cdot" ,qs:false)
				      ("read-accept-dot" ,qs:true)
				      ("read-accept-quasiquote" ,qs:true)
				      ("current-readtable" ,qs:false))
	      for p = (ignore-errors (funcall export name))
	      unless p do (return nil)
	      collect (cons p default))))

(defun fast-read-p (p)
  (and *fast-read-parameters*
       (rinport-p p) (not (rinport-closed p)) (null (rinport-fill p)) (null (rinport-custom p))
       (not (rinport-count-lines p))
       (loop for (param . default) in *fast-read-parameters*
	     always (eq (funcall param) default))))

(define-condition fast-read-unsupported (error) ())

(defun fast-read (p)
  "The next datum of rinport P, consumed, and T; or NIL, NIL, nothing
consumed, if the expander's reader must read it."
  (if (not (fast-read-p p))
      (values nil nil)
      (let* ((bytes (rinport-bytes p))
	     (end (ip-end p))
	     (pos (rinport-pos p)))
	(declare (type (simple-array (unsigned-byte 8) (*)) bytes) (fixnum end pos))
	(handler-case
	    (multiple-value-bind (datum next) (fast-read-datum bytes pos end)
	      (ip-advance p (- next pos))
	      (values datum t))
	  (fast-read-unsupported () (values nil nil))))))

(defmacro give-up () '(error 'fast-read-unsupported))

(declaim (inline fast-delimiter-p))
(defun fast-delimiter-p (b)
  (or (<= b 32) (member b '(40 41 91 93 123 125 34 44 39 96 59))))

(defun fast-skip-atmosphere (bytes pos end)
  "Past whitespace and ; and #| |# comments (not #;, which reads a datum)."
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes) (fixnum pos end))
  (loop
    (when (>= pos end) (return pos))
    (let ((b (aref bytes pos)))
      (cond ((>= b #x80) (give-up))
	    ((<= b 32) (incf pos))
	    ((= b 59)			; ;
	     (let ((nl (position 10 bytes :start pos :end end)))
	       (setq pos (if nl (1+ nl) end))))
	    ((and (= b 35) (< (1+ pos) end) (= (aref bytes (1+ pos)) 124)) ; #|
	     (let ((depth 1) (i (+ pos 2)))
	       (declare (fixnum depth i))
	       (loop
		 (when (>= (1+ i) end) (give-up))
		 (let ((c (aref bytes i)) (d (aref bytes (1+ i))))
		   (cond ((and (= c 124) (= d 35)) (incf i 2) (when (zerop (decf depth)) (return)))
			 ((and (= c 35) (= d 124)) (incf i 2) (incf depth))
			 (t (incf i)))))
	       (setq pos i)))
	    (t (return pos))))))

(defun fast-read-datum (bytes pos end)
  "The datum at POS, and the position after it; eof at the end."
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes) (fixnum pos end))
  (setq pos (fast-skip-atmosphere bytes pos end))
  (when (>= pos end) (return-from fast-read-datum (values qs:eof-object pos)))
  (multiple-value-bind (datum next) (fast-read-object bytes pos end)
    (when (eq datum qs:eof-object) (give-up))
    (values datum next)))

(defun fast-read-required (bytes pos end)
  "A datum (not eof) at POS, after atmosphere and #; comments."
  (declare (fixnum pos end))
  (loop
    (setq pos (fast-skip-atmosphere bytes pos end))
    (when (>= pos end) (give-up))
    (if (and (= (aref bytes pos) 35) (< (1+ pos) end) (= (aref bytes (1+ pos)) 59)) ; #;
	(setq pos (nth-value 1 (fast-read-required bytes (+ pos 2) end)))
	(return (fast-read-object bytes pos end)))))

(defun fast-read-object (bytes pos end)
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes) (fixnum pos end))
  (let ((b (aref bytes pos)))
    (case b
      ((40 91 123) (fast-read-list bytes (1+ pos) end (case b (40 41) (91 93) (t 125))))
      ((41 93 125) (give-up))
      (34 (fast-read-string bytes (1+ pos) end))
      (39 (fast-read-quoted "quote" bytes (1+ pos) end))
      (96 (fast-read-quoted "quasiquote" bytes (1+ pos) end))
      (44 (if (and (< (1+ pos) end) (= (aref bytes (1+ pos)) 64))
	      (fast-read-quoted "unquote-splicing" bytes (+ pos 2) end)
	      (fast-read-quoted "unquote" bytes (1+ pos) end)))
      (35 (fast-read-hash bytes pos end))
      (t (fast-read-atom bytes pos end)))))

(defun fast-read-quoted (name bytes pos end)
  (multiple-value-bind (datum next) (fast-read-required bytes pos end)
    (values (list (rsym name) datum) next)))

(defun fast-read-list (bytes pos end close)
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes) (fixnum pos end close))
  (let* ((head (list nil)) (tail head))
    (loop
      (setq pos (fast-skip-atmosphere bytes pos end))
      (when (>= pos end) (give-up))
      (let ((b (aref bytes pos)))
	(cond ((= b close) (return (values (cdr head) (1+ pos))))
	      ((member b '(41 93 125)) (give-up))
	      ((and (= b 35) (< (1+ pos) end) (= (aref bytes (1+ pos)) 59)) ; #;
	       (setq pos (nth-value 1 (fast-read-required bytes (+ pos 2) end))))
	      ;; a dot: (a . b), with the closing paren right after b
	      ((and (= b 46) (< (1+ pos) end) (fast-delimiter-p (aref bytes (1+ pos))))
	       (when (eq tail head) (give-up))
	       (multiple-value-bind (datum next) (fast-read-required bytes (1+ pos) end)
		 (setq next (fast-skip-atmosphere bytes next end))
		 (unless (and (< next end) (= (aref bytes next) close)) (give-up))
		 (setf (cdr tail) datum)
		 (return (values (cdr head) (1+ next)))))
	      (t (multiple-value-bind (datum next) (fast-read-object bytes pos end)
		   (setf (cdr tail) (list datum) tail (cdr tail) pos next))))))))

(defun fast-read-string (bytes pos end)
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes) (fixnum pos end))
  (let ((out (make-string-output-stream)))
    (loop
      (when (>= pos end) (give-up))
      (let ((b (aref bytes pos)))
	(cond ((>= b #x80) (give-up))
	      ((= b 34) (return (values (coerce (get-output-stream-string out) '(simple-array character (*)))
					(1+ pos))))
	      ((= b 92)
	       (when (>= (1+ pos) end) (give-up))
	       (let ((e (code-char (aref bytes (1+ pos)))))
		 (write-char (case e
			       (#\a (code-char 7)) (#\b (code-char 8)) (#\t (code-char 9))
			       (#\n (code-char 10)) (#\v (code-char 11)) (#\f (code-char 12))
			       (#\r (code-char 13)) (#\e (code-char 27))
			       ((#\" #\' #\\) e)
			       (t (give-up)))
			     out)
		 (incf pos 2)))
	      (t (write-char (code-char b) out) (incf pos)))))))

(defun fast-token-end (bytes pos end)
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes) (fixnum pos end))
  (loop while (and (< pos end) (not (fast-delimiter-p (aref bytes pos))))
	do (let ((b (aref bytes pos)))
	     ;; |...| and \ in symbols, and anything past ASCII
	     (when (or (>= b #x80) (= b 124) (= b 92)) (give-up))
	     (incf pos)))
  pos)

(defun fast-token (bytes start stop)
  (let ((s (make-string (- stop start))))
    (loop for i from start below stop for j from 0 do (setf (schar s j) (code-char (aref bytes i))))
    s))

(defun fast-read-atom (bytes pos end)
  "A symbol or a number."
  (let* ((stop (fast-token-end bytes pos end))
	 (token (fast-token bytes pos stop))
	 (c (char token 0)))
    (cond ((string= token ".") (give-up))
	  ((or (digit-char-p c) (member c '(#\+ #\- #\.)))
	   (values (or (fast-number token) (if (fast-symbol-like-number-p token) (give-up) (rsym token)))
		   stop))
	  (t (values (rsym token) stop)))))

(defun fast-number (token)
  "TOKEN as a number, or NIL: a fixnum here, anything else by Racket's
string->number (the reader's)."
  (if (and (every #'digit-char-p token) (< (length token) 18))
      (parse-integer token)
      (let ((n (funcall (native-function (gethash "string->number" *natives*)) token)))
	(cond ((eq n qs:false) nil)
	      ((numberp n) n)
	      (t (give-up))))))		; an extflonum

(defun fast-symbol-like-number-p (token)
  "Whether the expander's reader should judge TOKEN, which starts like a
number and isn't one: a digit in it (1/0 is an error, 1+ a symbol)."
  (some #'digit-char-p token))

(defparameter *fast-character-names*
  '(("nul" . 0) ("null" . 0) ("backspace" . 8) ("tab" . 9) ("newline" . 10) ("linefeed" . 10)
    ("vtab" . 11) ("page" . 12) ("return" . 13) ("space" . 32) ("rubout" . 127) ("delete" . 127)))

(defun fast-read-hash (bytes pos end)
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes) (fixnum pos end))
  (when (>= (1+ pos) end) (give-up))
  (let ((b (aref bytes (1+ pos))))
    (case b
      (40 (multiple-value-bind (items next) (fast-read-list bytes (+ pos 2) end 41)
	    (unless (and (listp items) (null (cdr (last items)))) (give-up))
	    (values (coerce items 'simple-vector) next)))
      (39 (fast-read-quoted "syntax" bytes (+ pos 2) end))
      (96 (fast-read-quoted "quasisyntax" bytes (+ pos 2) end))
      (44 (if (and (< (+ pos 2) end) (= (aref bytes (+ pos 2)) 64))
	      (fast-read-quoted "unsyntax-splicing" bytes (+ pos 3) end)
	      (fast-read-quoted "unsyntax" bytes (+ pos 2) end)))
      (92 (fast-read-character bytes (+ pos 2) end))
      (58 (let ((stop (fast-token-end bytes (+ pos 2) end)))
	    (when (= stop (+ pos 2)) (give-up))
	    (values (rkeyword (fast-token bytes (+ pos 2) stop)) stop)))
      (t (let* ((stop (fast-token-end bytes (1+ pos) end))
		(token (fast-token bytes (1+ pos) stop)))
	   (values (cond ((member token '("t" "true") :test #'string=) qs:true)
			 ((member token '("f" "false") :test #'string=) qs:false)
			 ;; #x1F, #e1.5 and the like
			 ((and (plusp (length token)) (find (char-downcase (char token 0)) "xbodei"))
			  (let ((n (funcall (native-function (gethash "string->number" *natives*))
					    (concatenate 'string "#" token))))
			    (if (numberp n) n (give-up))))
			 (t (give-up)))
		   stop))))))

(defun fast-read-character (bytes pos end)
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes) (fixnum pos end))
  (when (>= pos end) (give-up))
  (let ((b (aref bytes pos)))
    (when (>= b #x80) (give-up))
    (if (or (= (1+ pos) end) (not (alpha-char-p (code-char b)))
	    (fast-delimiter-p (aref bytes (1+ pos))))
	;; one character: #\a, #\(, #\space-less #\ followed by a delimiter
	(if (and (< (1+ pos) end) (digit-char-p (code-char b) 8)
		 (not (fast-delimiter-p (aref bytes (1+ pos)))))
	    (give-up)			; #\101, octal
	    (values (code-char b) (1+ pos)))
	(let* ((stop (fast-token-end bytes pos end))
	       (token (fast-token bytes pos stop))
	       (named (assoc token *fast-character-names* :test #'string=)))
	  (cond (named (values (code-char (cdr named)) stop))
		(t (give-up)))))))

(defun fast-read-wrapper (slow)
  "read, as #%kernel (#%read) exports it: this fast path, else SLOW, the
expander's read (the expander linklet's read-primitives table is made
with this around its read: INSTANTIATE-EXPANDER)."
  (let ((fast (lambda (&optional (in nil in-p))
		(multiple-value-bind (datum ok)
		    (fast-read (unwrap-in (if in-p in (current-port :input))))
		  (cond (ok datum)
			(in-p (funcall slow in))
			(t (funcall slow)))))))
    (setf (gethash fast *renamed-procedures*) (rsym "read"))
    fast))

(define-primitive "#%quasischeme" "quasischeme-fast-read" #'fast-read-wrapper)
