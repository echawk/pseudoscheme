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
	  ((eq mode :display) (host-display x stream))
	  (t (host-write x stream)))))

(defun print-quoted (x stream)
  "print: as write, with ' before what isn't self-quoting."
  (when (or (and (symbolp x) x (not (eq x ps:true)) (not (eq x ps:false)))
	    (consp x) (null x) (simple-vector-p x) (rbox-p x) (rkeyword-p x)
	    (ihash-p x))
    (write-char #\' stream))
  (print-value x stream :write))

(defun port-stream (port)
  "PORT as something the host's writers take: the current output port if
not given."
  (or port (funcall (psx:host-ref "current-output-port"))))

(defun to-port (port f)
  (port-write-string (with-output-to-string (s) (funcall f s)) port)
  *void*)

(defnative "write" (x &optional port) () (to-port port (lambda (s) (print-value x s :write))))
(defnative "display" (x &optional port) () (to-port port (lambda (s) (print-value x s :display))))
(defnative "print" (x &optional port quote-depth) ()
  (declare (ignore quote-depth))
  (to-port port (lambda (s) (print-quoted x s))))
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
