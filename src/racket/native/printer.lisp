; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

;;;; Racket's printer, natively: write, display and print for Racket's
;;;; values (in Racket CS, the io layer's printer).  Numbers, strings,
;;;; characters and symbols are written by the host's writer, which
;;;; agrees with Racket's for them; the rest is here:
;;;;   #<procedure:name>  #<void>  #:keyword  #&box  #hash((k . v) ...)
;;;;   #<path:...>  mcons as {a b}  a struct as #<name>, or, transparent
;;;;   or prefab, #(struct:name field ...) and #s(key field ...)
;;;; print is write with print-as-expression's quoting: '(1 2), 'a.
;;;; The print parameters that change what is written: print-as-expression,
;;;; print-boolean-long-form, print-box, print-hash-table, print-struct,
;;;; print-vector-length, print-pair-curly-braces, print-mpair-curly-braces,
;;;; print-reader-abbreviations and print-unreadable.
;;;; Not yet: cycles and sharing (print-graph), print-syntax-width.

(in-package "PSEUDOSCHEME-RACKET")

;;; ------------------------------------------------------------------
;;; The print parameters (Racket's io/print/parameter.rkt)

(defun boolean-guard (v) (bool (truthy v)))

(defmacro define-print-parameter (variable name value &optional (guard '#'boolean-guard))
  `(progn (defvar ,variable (make-parameter* ,value ,guard (rsym ,name)))
	  (define-primitive "#%kernel" ,name ,variable)))

(define-print-parameter *print-as-expression* "print-as-expression" ps:true)
(define-print-parameter *print-boolean-long-form* "print-boolean-long-form" ps:false)
(define-print-parameter *print-box* "print-box" ps:true)
(define-print-parameter *print-graph* "print-graph" ps:false)
(define-print-parameter *print-hash-table* "print-hash-table" ps:true)
(define-print-parameter *print-mpair-curly-braces* "print-mpair-curly-braces" ps:true)
(define-print-parameter *print-pair-curly-braces* "print-pair-curly-braces" ps:false)
(define-print-parameter *print-reader-abbreviations* "print-reader-abbreviations" ps:false)
(define-print-parameter *print-struct* "print-struct" ps:true)
(define-print-parameter *print-unreadable* "print-unreadable" ps:true)
(define-print-parameter *print-vector-length* "print-vector-length" ps:false)
(define-print-parameter *print-syntax-width* "print-syntax-width" 256
  (lambda (v)
    (unless (or (eql v 0) (and (floatp v) (sb-ext:float-infinity-p v) (plusp v))
		(and (integerp v) (>= v 3)))
      (arg-error "print-syntax-width" "(or/c +inf.0 0 (and/c exact-integer? (>=/c 3)))" v))
    v))

(defstruct (print-options (:conc-name po-))
  expr long box hash pair-curly mpair-curly abbrev struct vlen unreadable)

(defvar *print-options* nil
  "While something is printed: the print parameters' values, read once.")
(defvar *print-who* "write" "The procedure an unprintable value's error names.")
(defvar *print-abbreviate* nil "Whether print is writing: (quote x) as 'x.")

(defun read-print-options ()
  (flet ((on (parameter) (truthy (funcall parameter))))
    (make-print-options :expr (on *print-as-expression*) :long (on *print-boolean-long-form*)
			:box (on *print-box*) :hash (on *print-hash-table*)
			:pair-curly (on *print-pair-curly-braces*) :mpair-curly (on *print-mpair-curly-braces*)
			:abbrev (on *print-reader-abbreviations*) :struct (on *print-struct*)
			:vlen (on *print-vector-length*) :unreadable (on *print-unreadable*))))

;;; unquoted-printing-string: a string that prints as itself
(defstruct (unquoted-ps (:constructor make-unquoted-ps (value))) value)

(defvar *abbreviations*
  (mapcar (lambda (e) (cons (rsym (car e)) (cdr e)))
	  '(("quote" . "'") ("quasiquote" . "`") ("unquote" . ",") ("unquote-splicing" . ",@")
	    ("syntax" . "#'") ("quasisyntax" . "#`") ("unsyntax" . "#,") ("unsyntax-splicing" . "#,@")))
  "The symbols whose two-element lists print as a prefix and the datum.")

(defun abbreviation-prefix (x)
  "The prefix X, a pair, prints with if it's (quote d) and the like."
  (and (consp (cdr x)) (null (cddr x))
       (cdr (assoc (car x) *abbreviations* :test #'eq))))

(defun named-escape (code)
  "The letter of the escape \\a, \\n and the like for CODE, if it has one."
  (case code (7 #\a) (8 #\b) (9 #\t) (10 #\n) (11 #\v) (12 #\f) (13 #\r) (27 #\e)))

(defun write-racket-string (x stream)
  "X written as Racket writes a string: control characters escaped."
  (write-char #\" stream)
  (loop for c across x
	for code = (char-code c)
	do (cond ((member c '(#\" #\\)) (write-char #\\ stream) (write-char c stream))
		 ((named-escape code) (write-char #\\ stream) (write-char (named-escape code) stream))
		 ((or (< code 32) (<= 127 code 159) (and (> code 159) (not (graphic-char-p c))))
		  (if (> code #xFFFF)
		      (format stream "\\U~8,'0X" code)
		      (format stream "\\u~4,'0X" code)))
		 (t (write-char c stream))))
  (write-char #\" stream))

(defun host-write (x stream) (funcall (psx:host-ref "write") x stream))
(defun host-display (x stream) (funcall (psx:host-ref "display") x stream))

(defun procedure-name-string (f)
  (let ((name (funcall (native-function (gethash "object-name" *natives*)) f)))
    (and (symbolp name) name (not (eq name ps:false)) (rname name))))

(defun print-value (x stream mode)
  "X on STREAM, MODE :write or :display."
  (if *print-options*
      (print-value* x stream mode)
      (let ((*print-options* (read-print-options)))
	(print-value* x stream mode))))

(defun unreadable-text (x stream mode text)
  "TEXT, the printed form of X, which can't be read back: an error when
print-unreadable is #f and X is written."
  (when (and (eq mode :write) (not (po-unreadable *print-options*)))
    (error 'racket-error :who *print-who* :type "exn:fail"
			 :message (format nil "printing disabled for unreadable value~%  value: ~A" text)))
  (write-string text stream)
  x)

(defun vector-elements-printed (x)
  "How many of vector X's elements print under print-vector-length: not
the trailing ones equal to the one before."
  (let ((n (length x)))
    (loop while (and (> n 1) (equal* (svref x (- n 1)) (svref x (- n 2)))) do (decf n))
    n))

(defun print-value* (x stream mode)
  (let ((o *print-options*))
    (labels ((p (x) (print-value* x stream mode))
	     (unreadable (text) (unreadable-text x stream mode text)))
      (cond ((eq x *void*) (unreadable "#<void>"))
	    ((eq x *eof*) (unreadable "#<eof>"))
	    ((eq x ps:eof-object) (unreadable "#<eof>"))
	    ((and (po-long o) (eq x ps:true)) (write-string "#true" stream))
	    ((and (po-long o) (eq x ps:false)) (write-string "#false" stream))
	    ((unquoted-ps-p x) (write-string (unquoted-ps-value x) stream))
	    ((consp x)
	     (let ((prefix (and (or (po-abbrev o) *print-abbreviate*) (abbreviation-prefix x))))
	       (if prefix
		   (progn (write-string prefix stream) (p (cadr x)))
		   (progn
		     (write-char (if (po-pair-curly o) #\{ #\() stream)
		     (loop for tail = x then (cdr tail)
			   for first = t then nil
			   while (and (consp tail)
				      (or first
					  (not (and (or (po-abbrev o) *print-abbreviate*) (abbreviation-prefix tail)))))
			   do (unless first (write-char #\Space stream))
			      (p (car tail))
			   finally (when tail (write-string " . " stream) (p tail)))
		     (write-char (if (po-pair-curly o) #\} #\)) stream)))))
	    ((simple-vector-p x)
	     (let ((n (if (po-vlen o) (vector-elements-printed x) (length x))))
	       (if (po-vlen o) (format stream "#~D(" (length x)) (write-string "#(" stream))
	       (loop for i below n
		     do (unless (zerop i) (write-char #\Space stream)) (p (svref x i)))
	       (write-char #\) stream)))
	    ((typep x '(simple-array (unsigned-byte 8) (*)))
	     (write-string "#\"" stream)
	     (loop for i from 0 below (length x)
		   for b = (aref x i)
		   do (cond ((member b '(34 92)) (write-char #\\ stream) (write-char (code-char b) stream))
			    ((<= 32 b 126) (write-char (code-char b) stream))
			    ((named-escape b) (write-char #\\ stream) (write-char (named-escape b) stream))
			    ;; octal, all three digits if a digit follows
			    ((and (< (1+ i) (length x)) (<= 48 (aref x (1+ i)) 55))
			     (format stream "\\~3,'0O" b))
			    (t (format stream "\\~O" b))))
	     (write-char #\" stream))
	    ((and (stringp x) (not (eq mode :display)))
	     (write-racket-string x stream))
	    ((rkeyword-p x) (format stream "#:~A" (rkeyword-name x)))
	    ((or (regexp-literal-p x) (rregexp-p x))
	     (let ((source (if (rregexp-p x) (rregexp-source x) (regexp-literal-source x)))
		   (px (if (rregexp-p x) (rregexp-px x) (eq (regexp-literal-kind x) :px))))
	       (write-string (if px "#px" "#rx") stream)
	       (p source)))
	    ((rbox-p x)
	     (if (po-box o)
		 (progn (write-string "#&" stream) (p (rbox-value x)))
		 (write-string "#<box>" stream)))
	    ((mpair-p x)
	     (write-char (if (po-mpair-curly o) #\{ #\() stream)
	     (loop for tail = x then (mpair-cdr tail)
		   for first = t then nil
		   while (mpair-p tail)
		   do (unless first (write-char #\Space stream))
		      (p (mpair-car tail))
		   finally (when tail (write-string " . " stream) (p tail)))
	     (write-char (if (po-mpair-curly o) #\} #\)) stream))
	    ((rpath-p x) (unreadable (format nil "#<path:~A>" (rpath-string x))))
	    ((or (rinport-p x) (routport-p x)) (unreadable (port-desc x)))
	    ((or (ihash-p x) (rhash-p x))
	     (if (po-hash o)
		 (progn
		   (format stream "#hash~A(" (ecase (hash-kind x) (:eq "eq") (:eqv "eqv") (:equal "")))
		   (loop for (k . v) across (entries x) for first = t then nil
			 do (unless first (write-char #\Space stream))
			    (write-char #\( stream) (p k) (write-string " . " stream) (p v) (write-char #\) stream))
		   (write-char #\) stream))
		 (write-string "#<hash>" stream)))
	    ((custom-write-procedure x)
	     (write-string (custom-written x (custom-write-procedure x) (if (eq mode :display) ps:false ps:true))
			   stream))
	    ((struct-instance-p x)
	     (let ((type (struct-of-type x)))
	       (cond ((and (struct-type-prefab-key type) (po-struct o))
		      (write-string "#s(" stream)
		      (p (struct-type-prefab-key type))
		      (loop for f across (struct-fields x) do (write-char #\Space stream) (p f))
		      (write-char #\) stream))
		     ((and (transparent-p type) (po-struct o))
		      (format stream "#(struct:~A" (rname (struct-type-name type)))
		      (loop for f across (struct-fields x) do (write-char #\Space stream) (p f))
		      (write-char #\) stream))
		     ((or (struct-type-prefab-key type) (transparent-p type))
		      (format stream "#<~A>" (rname (struct-type-name type))))
		     ((typep x 'applicable-rstruct)
		      (unreadable (format nil "#<procedure:~A>" (rname (struct-type-name type)))))
		     (t (unreadable (format nil "#<~A>" (rname (struct-type-name type))))))))
	    ((struct-type-p x) (unreadable (format nil "#<struct-type:~A>" (rname (struct-type-name x)))))
	    ((functionp x)
	     (let ((name (procedure-name-string x)))
	       (unreadable (format nil "#<procedure~@[:~A~]>" name))))
	    ((numberp x) (write-string (racket-number-string x) stream))
	    ((eq mode :display) (host-display x stream))
	    (t (host-write x stream))))))

(defun print-quoted (x stream)
  "print: as write, with ' before what isn't self-quoting; a transparent
struct as its constructor's application, (point 1 2); a mutable pair as
(mcons a d)."
  (let ((*print-options* (or *print-options* (read-print-options)))
	(*print-who* "print")
	(*print-abbreviate* t))
    (cond ((not (po-expr *print-options*)) (print-value x stream :write))
	  ((and (struct-instance-p x) (transparent-p (struct-of-type x))
		(not (struct-type-prefab-key (struct-of-type x)))
		(po-struct *print-options*))
	   (format stream "(~A" (rname (struct-type-name (struct-of-type x))))
	   (loop for f across (struct-fields x) do (write-char #\Space stream) (print-quoted f stream))
	   (write-char #\) stream))
	  ((mpair-p x)
	   (write-string "(mcons " stream)
	   (print-quoted (mpair-car x) stream)
	   (write-char #\Space stream)
	   (print-quoted (mpair-cdr x) stream)
	   (write-char #\) stream))
	  (t
	   (when (or (and (symbolp x) x (not (eq x ps:true)) (not (eq x ps:false)))
		     (consp x) (null x) (simple-vector-p x) (rbox-p x) (rkeyword-p x)
		     (ihash-p x))
	     (write-char #\' stream))
	   (print-value x stream :write)))))

(defun port-stream (port)
  "PORT as something the host's writers take: the current output port if
not given."
  (or port (current-port :output)))

(defun custom-write-procedure (x)
  "X's prop:custom-write procedure, if X is a struct with one."
  (and (struct-instance-p x)
       (multiple-value-bind (v found) (property-value x (primitive "prop:custom-write"))
	 (and found (functionp v) v))))

(defun custom-written (x f mode)
  "What F, X's prop:custom-write procedure, writes in MODE, as a string."
  (let ((port (make-routport (rsym "string"))))
    (funcall f x port mode)
    (sb-ext:octets-to-string (coerce (routport-buffer port) '(simple-array (unsigned-byte 8) (*)))
			     :external-format :latin-1)))

(defun write-with-mode (x port mode lisp-mode)
  "write, display or print of X to PORT: its prop:custom-write procedure
writes straight to the port (compiled code writes its bytes so)."
  (let ((f (custom-write-procedure x)))
    (if f
	(progn (funcall f x (output-port port) mode) *void*)
	(to-port port (lambda (s) (if (eq lisp-mode :print) (print-quoted x s) (print-value x s lisp-mode)))))))

(defun to-port (port f)
  (port-write-string (with-output-to-string (s) (funcall f s)) port)
  *void*)

(defnative "write" (x &optional port) () (write-with-mode x port ps:true :write))
(defnative "display" (x &optional port) () (write-with-mode x port ps:false :display))
(defnative "print" (x &optional port quote-depth) ()
  (declare (ignore quote-depth))
  (write-with-mode x port 0 :print))
(defnative "writeln" (x &optional port) ()
  (to-port port (lambda (s) (print-value x s :write) (terpri s))))
(defnative "displayln" (x &optional port) ()
  (to-port port (lambda (s) (print-value x s :display) (terpri s))))
(defnative "println" (x &optional port) ()
  (to-port port (lambda (s) (print-quoted x s) (terpri s))))

;;; member and assoc with Racket's equal?
(defnative "member" (x list &optional (test nil test-p)) ()
  (loop for tail on list
	when (if test-p (truthy (funcall test x (car tail))) (equal* x (car tail)))
	  return tail
	finally (return ps:false)))
(defnative "assoc" (x alist &optional (test nil test-p)) ()
  (loop for e in alist
	when (and (consp e) (if test-p (truthy (funcall test x (car e))) (equal* x (car e))))
	  return e
	finally (return ps:false)))

;;; current-print: print, then a newline, for what isn't #<void>
(define-primitive "#%kernel" "current-print"
  (make-parameter* (lambda (v)
		     (unless (eq v *void*)
		       (to-port nil (lambda (s) (print-quoted v s) (terpri s))))
		     *void*)))

;;; Port handlers: display, write and print through a port go through
;;; these, by default the printer's own
(defvar *port-handlers* (make-hash-table :test 'equal :weakness :key-or-value :synchronized t))

(macrolet ((handler (name mode default)
	     `(defnative ,name (port &optional (handler nil handler-p)) (:leaf t)
		(if handler-p
		    (progn (setf (gethash (cons ,mode port) *port-handlers*) handler) *void*)
		    (or (gethash (cons ,mode port) *port-handlers*)
			(lambda (v p &optional depth) (declare (ignore depth)) ,default))))))
  (handler "port-display-handler" :display (write-with-mode v p ps:false :display))
  (handler "port-write-handler" :write (write-with-mode v p ps:true :write))
  (handler "port-print-handler" :print (write-with-mode v p 0 :print)))

(defvar *global-port-print-handler*
  (make-parameter* (lambda (v p &optional depth) (declare (ignore depth)) (write-with-mode v p 0 :print))))
(define-primitive "#%kernel" "global-port-print-handler" *global-port-print-handler*)

;;; ------------------------------------------------------------------
;;; Numbers as Racket prints them: a flonum in the fewest digits that
;;; read back as it, positionally when its decimal exponent E is from -4
;;; to 13, or above that if it has fewer than 3 digits fewer than E
;;; (racket/src/cs/rumble/number.ss, init-flonum-printing!), else with a
;;; signed exponent (1e+15, 1.5e-5).

(defun racket-flonum-string (x)
  (cond ((sb-ext:float-nan-p x) "+nan.0")
	((sb-ext:float-infinity-p x) (if (plusp x) "+inf.0" "-inf.0"))
	((zerop x) (if (minusp (float-sign x)) "-0.0" "0.0"))
	(t
	 (multiple-value-bind (k digits) (sb-impl::flonum-to-digits (abs x))
	   ;; x = 0.DIGITS * 10^K
	   (let ((e (1- k)) (n (length digits)) (sign (if (minusp x) "-" "")))
	     (if (or (< e -4) (and (>= e 14) (>= (- e n) 3)))
		 (format nil "~A~A~:[.~A~;~*~]e~A~D" sign (char digits 0) (= n 1) (subseq digits 1)
			 (if (minusp e) "-" "+") (abs e))
		 (format nil "~A~A" sign
			 (cond ((<= k 0) (format nil "0.~A~A" (make-string (- k) :initial-element #\0) digits))
			       ((>= k n) (format nil "~A~A.0" digits (make-string (- k n) :initial-element #\0)))
			       (t (format nil "~A.~A" (subseq digits 0 k) (subseq digits k)))))))))))

(defun racket-number-string (z &optional (radix 10))
  (cond ((floatp z) (racket-flonum-string (coerce z 'double-float)))
	((and (complexp z) (or (floatp (realpart z)) (floatp (imagpart z))))
	 (let ((re (racket-number-string (realpart z))) (im (racket-number-string (imagpart z))))
	   (format nil "~A~:[+~;~]~Ai" re (or (char= (char im 0) #\-) (char= (char im 0) #\+)) im)))
	((complexp z)
	 (format nil "~A~:[+~;~]~Ai" (racket-number-string (realpart z) radix)
		 (minusp (imagpart z)) (racket-number-string (imagpart z) radix)))
	((rationalp z) (string-downcase (write-to-string z :base radix :radix nil)))
	(t (princ-to-string z))))

(defnative "number->string" (z &optional (radix 10)) (:leaf t) (racket-number-string z radix))
