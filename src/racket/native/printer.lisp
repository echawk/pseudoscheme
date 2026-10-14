; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

;;;; Racket's printer, natively: write, display and print for Racket's
;;;; values (in Racket CS, the io layer's printer).  Numbers, strings,
;;;; characters and symbols are written by the host's writer, which
;;;; agrees with Racket's for them; the rest is here:
;;;;   #<procedure:name>  #<void>  #:keyword  #&box  #hash((k . v) ...)
;;;;   #<path:...>  mcons as {a b}  a struct as #<name>, or, transparent
;;;;   or prefab, #(struct:name field ...) and #s(key field ...)
;;;; print is write with print-as-expression's quoting: '(1 2), 'a.
;;;; Not yet: cycles (print-graph), prop:custom-write, print parameters.

(in-package "PSEUDOSCHEME-RACKET")

(defun host-write (x stream) (funcall (psx:host-ref "write") x stream))
(defun host-display (x stream) (funcall (psx:host-ref "display") x stream))

(defun procedure-name-string (f)
  (let ((name (funcall (native-function (gethash "object-name" *natives*)) f)))
    (and (symbolp name) name (not (eq name ps:false)) (rname name))))

(defun print-value (x stream mode)
  "X on STREAM, MODE :write or :display."
  (labels ((p (x) (print-value x stream mode)))
    (cond ((eq x *void*) (write-string "#<void>" stream))
	  ((eq x *eof*) (write-string "#<eof>" stream))
	  ((eq x ps:eof-object) (write-string "#<eof>" stream))
	  ((consp x)
	   (write-char #\( stream)
	   (loop for tail = x then (cdr tail)
		 for first = t then nil
		 while (consp tail)
		 do (unless first (write-char #\Space stream))
		    (p (car tail))
		 finally (when tail (write-string " . " stream) (p tail)))
	   (write-char #\) stream))
	  ((simple-vector-p x)
	   (write-string "#(" stream)
	   (loop for e across x for first = t then nil
		 do (unless first (write-char #\Space stream)) (p e))
	   (write-char #\) stream))
	  ((typep x '(simple-array (unsigned-byte 8) (*)))
	   (write-string "#\"" stream)
	   (loop for b across x
		 do (cond ((member b '(34 92)) (write-char #\\ stream) (write-char (code-char b) stream))
			  ((<= 32 b 126) (write-char (code-char b) stream))
			  (t (format stream "\\~O" b))))
	   (write-char #\" stream))
	  ((rkeyword-p x) (format stream "#:~A" (rkeyword-name x)))
	  ((or (regexp-literal-p x) (rregexp-p x))
	   (let ((source (if (rregexp-p x) (rregexp-source x) (regexp-literal-source x)))
		 (px (if (rregexp-p x) (rregexp-px x) (eq (regexp-literal-kind x) :px))))
	     (write-string (if px "#px" "#rx") stream)
	     (p source)))
	  ((rbox-p x) (write-string "#&" stream) (p (rbox-value x)))
	  ((mpair-p x)
	   (write-char #\{ stream)
	   (loop for tail = x then (mpair-cdr tail)
		 for first = t then nil
		 while (mpair-p tail)
		 do (unless first (write-char #\Space stream))
		    (p (mpair-car tail))
		 finally (when tail (write-string " . " stream) (p tail)))
	   (write-char #\} stream))
	  ((rpath-p x) (format stream "#<path:~A>" (rpath-string x)))
	  ((rinport-p x) (write-string "#<input-port>" stream))
	  ((routport-p x) (write-string "#<output-port>" stream))
	  ((or (ihash-p x) (rhash-p x))
	   (format stream "#hash~A(" (ecase (hash-kind x) (:eq "eq") (:eqv "eqv") (:equal "")))
	   (loop for (k . v) across (entries x) for first = t then nil
		 do (unless first (write-char #\Space stream))
		    (write-char #\( stream) (p k) (write-string " . " stream) (p v) (write-char #\) stream))
	   (write-char #\) stream))
	  ((custom-write-procedure x)
	   (write-string (custom-written x (custom-write-procedure x) (if (eq mode :display) ps:false ps:true))
			 stream))
	  ((struct-instance-p x)
	   (let ((type (struct-of-type x)))
	     (cond ((struct-type-prefab-key type)
		    (write-string "#s(" stream)
		    (p (struct-type-prefab-key type))
		    (loop for f across (struct-fields x) do (write-char #\Space stream) (p f))
		    (write-char #\) stream))
		   ((transparent-p type)
		    (format stream "#(struct:~A" (rname (struct-type-name type)))
		    (loop for f across (struct-fields x) do (write-char #\Space stream) (p f))
		    (write-char #\) stream))
		   ((typep x 'applicable-rstruct)
		    (format stream "#<procedure:~A>" (rname (struct-type-name type))))
		   (t (format stream "#<~A>" (rname (struct-type-name type)))))))
	  ((struct-type-p x) (format stream "#<struct-type:~A>" (rname (struct-type-name x))))
	  ((functionp x)
	   (let ((name (procedure-name-string x)))
	     (format stream "#<procedure~@[:~A~]>" name)))
	  ((numberp x) (write-string (racket-number-string x) stream))
	  ((eq mode :display) (host-display x stream))
	  (t (host-write x stream)))))

(defun print-quoted (x stream)
  "print: as write, with ' before what isn't self-quoting; a transparent
struct as its constructor's application, (point 1 2)."
  (when (and (struct-instance-p x) (transparent-p (struct-of-type x))
	     (not (struct-type-prefab-key (struct-of-type x))))
    (format stream "(~A" (rname (struct-type-name (struct-of-type x))))
    (loop for f across (struct-fields x) do (write-char #\Space stream) (print-quoted f stream))
    (write-char #\) stream)
    (return-from print-quoted))
  (when (or (and (symbolp x) x (not (eq x ps:true)) (not (eq x ps:false)))
	    (consp x) (null x) (simple-vector-p x) (rbox-p x) (rkeyword-p x)
	    (ihash-p x))
    (write-char #\' stream))
  (print-value x stream :write))

(defun port-stream (port)
  "PORT as something the host's writers take: the current output port if
not given."
  (or port (funcall (psx:host-ref "current-output-port"))))

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
;;; read back as it, positionally when its decimal exponent is from -4
;;; to 20 (1.5, 0.0001, 100000000000000000000.0), else with an exponent
;;; that has a sign and two digits at least (1e+21, 1.5e-07), as Racket
;;; BC's printf did and Racket CS keeps

(defun racket-flonum-string (x)
  (cond ((sb-ext:float-nan-p x) "+nan.0")
	((sb-ext:float-infinity-p x) (if (plusp x) "+inf.0" "-inf.0"))
	((zerop x) (if (minusp (float-sign x)) "-0.0" "0.0"))
	(t
	 (multiple-value-bind (k digits) (sb-impl::flonum-to-digits (abs x))
	   ;; x = 0.DIGITS * 10^K
	   (let ((e (1- k)) (n (length digits)) (sign (if (minusp x) "-" "")))
	     (if (or (< e -4) (>= e 21))
		 (format nil "~A~A~:[.~A~;~*~]e~A~2,'0D" sign (char digits 0) (= n 1) (subseq digits 1)
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
	 (format nil "~A~:[+~;~]~Ai" (if (zerop (realpart z)) "" (racket-number-string (realpart z) radix))
		 (minusp (imagpart z)) (racket-number-string (imagpart z) radix)))
	((rationalp z) (string-downcase (write-to-string z :base radix :radix nil)))
	(t (princ-to-string z))))

(defnative "number->string" (z &optional (radix 10)) (:leaf t) (racket-number-string z radix))
