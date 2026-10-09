; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-GUILE -*-

;;;; write and display as Guile's: #{odd symbol}#, #:keywords, #nil,
;;;; Guile's character names and string escapes, #<eof> and
;;;; #<unspecified>, #<procedure name (args)>, records and GOOPS instances
;;;; through their printers, and the rest as #<type address>.  Written for
;;;; Pseudoscheme after libguile's print.c's behaviour.

(in-package "PSEUDOSCHEME-GUILE")

(defun object-address-string (x)
  (format nil "~(~X~)" (logand (sb-kernel:get-lisp-obj-address x) #xffffffffff)))

;;; Symbols

(defun identifier-category-p (c initial)
  "Whether C's Unicode category may appear in a symbol (first, if
INITIAL): letters, marks, numbers, punctuation and symbols."
  (member (sb-unicode:general-category c)
	  (if initial
	      '(:lu :ll :lt :lm :lo :mn :nl :no :pd :pc :po :sc :sm :sk :so :co)
	      '(:lu :ll :lt :lm :lo :mn :nl :no :pd :pc :po :sc :sm :sk :so :co :nd :mc :me))))

(defun identifier-char-p (c initial)
  "Whether C may appear in a bare symbol: less Scheme's delimiters."
  (and (not (find c "()[]{}\"';`,")) (identifier-category-p c initial)))

(defun quote-keywordish-p ()
  (let ((option (option-value *print-options* "quote-keywordish-symbols")))
    (if (eq option :reader)
	(and (option-value *read-options* "keywords") t)
	option)))

(defun symbol-needs-braces-p (name)
  (let ((len (length name))
	(r7rs (option-value *print-options* "r7rs-symbols")))
    (or (zerop len)
	(and r7rs (or (find #\| name) (find #\\ name)))
	(and (or (char= (char name 0) #\:) (char= (char name (1- len)) #\:))
	     (quote-keywordish-p))
	(let ((c (char name 0)))
	  (or (find c "'`,\";#")
	      (and (char= c #\.) (= len 1))
	      (and (find c "+-.0123456789") (ps:parse-scheme-number name 10))
	      (not (identifier-char-p c t))))
	(loop for i from 1 below len
	      for c = (char name i)
	      thereis (or (not (identifier-char-p c nil)) (find c "\";#"))))))

(defun write-r7rs-symbol (name stream)
  (write-char #\| stream)
  (loop for c across name
	do (case c
	     (#\Bel (write-string "\\a" stream))
	     (#\Backspace (write-string "\\b" stream))
	     (#\Tab (write-string "\\t" stream))
	     (#\Newline (write-string "\\n" stream))
	     (#\Return (write-string "\\r" stream))
	     (#\| (write-string "\\|" stream))
	     (#\\ (write-string "\\x5c;" stream))
	     (t (if (or (char= c #\Space) (graphic-char-p* c) (find (sb-unicode:general-category c) '(:ps :pe :pi :pf)))
		    (write-char c stream)
		    (format stream "\\x~(~X~);" (char-code c))))))
  (write-char #\| stream))

(defun write-symbol-name (name stream)
  (cond
    ((not (symbol-needs-braces-p name)) (write-string name stream))
    ((option-value *print-options* "r7rs-symbols") (write-r7rs-symbol name stream))
    (t
	(write-string "#{" stream)
	(loop for c across name
	      do (if (or (identifier-category-p c nil) (eq (sb-unicode:general-category c) :zs))
		     (write-char c stream)
		     (format stream "\\x~(~X~);" (char-code c))))
	(write-string "}#" stream))))

;;; Characters and strings

(defparameter *char-write-names*
  '((#x20 . "space") (#x0a . "newline") (#x00 . "nul") (#x07 . "alarm") (#x08 . "backspace")
    (#x09 . "tab") (#x0b . "vtab") (#x0c . "page") (#x0d . "return") (#x1b . "esc")
    (#x7f . "delete")
    (#x01 . "soh") (#x02 . "stx") (#x03 . "etx") (#x04 . "eot") (#x05 . "enq") (#x06 . "ack")
    (#x0e . "so") (#x0f . "si") (#x10 . "dle") (#x11 . "dc1") (#x12 . "dc2") (#x13 . "dc3")
    (#x14 . "dc4") (#x15 . "nak") (#x16 . "syn") (#x17 . "etb") (#x18 . "can") (#x19 . "em")
    (#x1a . "sub") (#x1c . "fs") (#x1d . "gs") (#x1e . "rs") (#x1f . "us"))
  "Guile's names for characters, in the order it looks them up.")

(defun graphic-char-p* (c)
  (member (sb-unicode:general-category c)
	  '(:lu :ll :lt :lm :lo :mn :mc :me :nd :nl :no :pc :pd :ps :pe :pi :pf :po :sm :sc :sk :so)))

(defun write-guile-char (c stream)
  (write-string "#\\" stream)
  (let ((name (cdr (assoc (char-code c) *char-write-names*))))
    (cond ((and (member (sb-unicode:general-category c) '(:mn :mc :me))
		(plusp (sb-unicode:combining-class c)))
	   ;; a combining character, over a dotted circle so it shows
	   (write-char (code-char #x25cc) stream) (write-char c stream))
	  ((and (graphic-char-p* c) (char/= c #\Space)) (write-char c stream))
	  (name (write-string name stream))
	  ((option-value *read-options* "r6rs-hex-escapes") (format stream "x~(~X~)" (char-code c)))
	  (t (format stream "~O" (char-code c))))))

(defun write-guile-string (s stream)
  (write-char #\" stream)
  (loop for c across s
	do (case c
	     (#\" (write-string "\\\"" stream))
	     (#\\ (write-string "\\\\" stream))
	     (t (cond ((or (char= c #\Space) (graphic-char-p* c)
		       (and (char= c #\Newline) (not (option-value *print-options* "escape-newlines"))))
		       (write-char c stream))
		      (t (let ((code (char-code c)))
			   (case code
			     (7 (write-string "\\a" stream))
			     (8 (write-string "\\b" stream))
			     (9 (write-string "\\t" stream))
			     (10 (if (option-value *print-options* "escape-newlines")
				     (write-string "\\n" stream)
				     (write-char c stream)))
			     (11 (write-string "\\v" stream))
			     (12 (write-string "\\f" stream))
			     (13 (write-string "\\r" stream))
			     (t (cond ((option-value *read-options* "r6rs-hex-escapes")
				       (format stream "\\x~(~X~);" code))
				      ((< code #x100) (format stream "\\x~(~2,'0X~)" code))
				      ((< code #x10000) (format stream "\\u~(~4,'0X~)" code))
				      (t (format stream "\\U~(~6,'0X~)" code)))))))))))
  (write-char #\" stream))

;;; Procedures

(defun procedure-arguments-string (f)
  ;; a primitive's arguments, and Scheme's (renamed ~N), are written _
  (let ((lambda-list (ignore-errors (sb-kernel:%fun-lambda-list f)))
	(anonymous (root-procedure-name f)))
    (if (listp lambda-list)
	(let ((state :required) (parts '()))
	  (dolist (x lambda-list)
	    (case x
	      (&optional (setq state :optional) (push "#:optional" parts))
	      ((&rest &body) (setq state :rest))
	      (&key (setq state :key) (push "#:key" parts))
	      (&aux (return))
	      (t (let ((name (let ((n (string-downcase (princ-to-string (if (consp x) (car x) x)))))
			       (if (or anonymous (char= (char n 0) #\~)) "_" n))))
		   (if (eq state :rest)
		       (push (format nil ". ~A" name) parts)
		       (push name parts))))))
	  (format nil "(~{~A~^ ~})" (nreverse parts)))
	"_")))

(defvar *write-program-hook* nil
  "A function writing a compiled program (src/guile/vm.lisp) and returning
true, or NIL.")

(defun write-procedure (f stream)
  (when (and *write-program-hook* (funcall *write-program-hook* f stream))
    (return-from write-procedure))
  (when (psx::continuation-procedure-p f)
    (return-from write-procedure (format stream "#<continuation ~A>" (object-address-string f))))
  (let ((name (funcall (gethash "procedure-name" *guile-primitives*) f)))
    (if (and name (not (eq name ps:false)))
	(format stream "#<procedure ~A ~A>"
		(with-output-to-string (s) (guile-write name s))
		(procedure-arguments-string f))
	(format stream "#<procedure ~A ~A>" (object-address-string f) (procedure-arguments-string f)))))

;;; Lists and vectors

(defvar *print-stack* '() "The pairs and vectors being written, innermost first.")

(defun write-cycle (x stream)
  "If X is being written already, write #N# for it and return true."
  (let ((i (position x *print-stack* :test #'eq)))
    (when i
      (format stream "#~D#" i)
      t)))

(defun write-guile-list (x stream display)
  ;; (quote x) is written so, not abbreviated, as in Guile
  (let ((*print-stack* (cons x *print-stack*)))
    (write-char #\( stream)
    (guile-print (car x) stream display)
    (loop for tail = (cdr x) then (cdr tail)
	  do (cond ((null tail) (return))
		   ((and (consp tail) (not (member tail *print-stack* :test #'eq)))
		    (write-char #\Space stream)
		    (guile-print (car tail) stream display))
		   (t (write-string " . " stream)
		      (guile-print tail stream display)
		      (return))))
    (write-char #\) stream)))

(defun write-elements (prefix elements stream display)
  (write-string prefix stream)
  (write-char #\( stream)
  (loop for (x . more) on elements
	do (guile-print x stream display)
	   (when more (write-char #\Space stream)))
  (write-char #\) stream))

;;; Everything

(defun guile-print (x stream display)
  (cond ((eq x ps:false) (write-string "#f" stream))
	((eq x ps:true) (write-string "#t" stream))
	((null x) (write-string "()" stream))
	((eq x *elisp-nil*) (write-string "#nil" stream))
	((eq x ps:eof-object) (write-string "#<eof>" stream))
	((eq x *unspecified*) (write-string "#<unspecified>" stream))
	((eq x +unbound+) (write-string "#<unbound>" stream))
	((characterp x) (if display (write-char x stream) (write-guile-char x stream)))
	((stringp x) (if display (write-string x stream) (write-guile-string x stream)))
	((keywordp x) (write-string "#:" stream)
	 (let ((name (ps:scheme-symbol-name (guile-keyword->symbol x))))
	   (if display (write-string name stream) (write-symbol-name name stream))))
	((symbolp x)
	 (let ((name (ps:scheme-symbol-name x)))
	   (if (or display (ps::photon-p x)) (write-string name stream) (write-symbol-name name stream))))
	((numberp x) (write-string (funcall (gethash "number->string" *guile-primitives*) x) stream))
	((consp x) (or (write-cycle x stream) (write-guile-list x stream display)))
	((simple-vector-p x)
	 (or (write-cycle x stream)
	     (let ((*print-stack* (cons x *print-stack*)))
	       (write-elements "#" (coerce x 'list) stream display))))
	((garray-p x) (write-array x stream display))
	((and (typep x 'ps-r6rs::octets) (gethash x *bytevector-types*))
	 (write-tagged-bytevector x (gethash x *bytevector-types*) stream display))
	((typep x 'ps-r6rs::octets) (write-elements "#vu8" (coerce x 'list) stream display))
	((bit-vector-p x) (write-string "#*" stream) (loop for b across x do (write-char (if (zerop b) #\0 #\1) stream)))
	((and (vectorp x) (ps::numeric-vector-tag x))
	 (write-elements (format nil "#~A" (guile-uniform-tag x)) (coerce x 'list) stream display))
	((struct-p x) (print-struct x stream))
	((functionp x) (write-procedure x stream))
	((ghash-p x) (print-object x stream))
	((streamp x) (if (gport-p x) (print-object x stream) (print-object (stream-port x) stream)))
	(t (let ((*print-pretty* nil)) (princ x stream)))))

(defun guile-uniform-tag (v)
  (let ((tag (ps::numeric-vector-tag v)))
    (cond ((string= tag "c64") "c32") ((string= tag "c128") "c64") (t tag))))

(defun guile-keyword->symbol (k)
  (funcall (gethash "keyword->symbol" *guile-primitives*) k))

(defun guile-write (x &optional (stream *standard-output*))
  (let ((*print-stack* '())) (guile-print x stream nil))
  *unspecified*)

(defun guile-display (x &optional (stream *standard-output*))
  (let ((*print-stack* '())) (guile-print x stream t))
  *unspecified*)
