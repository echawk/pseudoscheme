; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-GUILE -*-

;;;; Guile's regular expressions are POSIX's, and so are these: libc's
;;;; regcomp and regexec, through CFFI, which (ice-9 regex) builds on.
;;;; Matching is done on the UTF-8 bytes of the string; the match's byte
;;;; offsets are turned back into character indices.

(in-package "PSEUDOSCHEME-GUILE")

;;; The flag values and regoff_t's size are the platform's.
(defconstant +reg-extended+ 1)
(defconstant +reg-icase+ 2)
(defconstant +reg-newline+ #+darwin 8 #-darwin 4)
(defconstant +reg-notbol+ 1)
(defconstant +reg-noteol+ 2)
(defconstant +regoff-size+ #+darwin 8 #-darwin 4)
(defconstant +regex-t-size+ 256 "Bytes for a regex_t: more than any libc's.")

(defstruct (regexp (:constructor %make-regexp (pattern flags pointer)) (:copier nil))
  pattern flags pointer)

(defmethod print-object ((r regexp) stream)
  (format stream "#<regexp ~S>" (regexp-pattern r)))

(defun regexp-error (pointer code subr)
  (let ((message (cffi:with-foreign-object (buf :char 256)
		   (cffi:foreign-funcall "regerror" :int code :pointer pointer :pointer buf
						    :size 256 :size)
		   (cffi:foreign-string-to-lisp buf))))
    (guile-error (ssym "regular-expression-syntax") subr message '())))

;;; macOS's regcomp (unlike glibc's, which Guile is usually built on)
;;; rejects empty alternatives, as in (|.*;): such a group becomes an
;;; optional group of its other alternatives, (.*;)?, numbered the same.

(defun regex-groups (pattern)
  "Each group of PATTERN, an extended regular expression: (open close
alternative-starts), indices into PATTERN."
  (let ((stack '()) (groups '()) (i 0) (n (length pattern)))
    (loop while (< i n)
	  do (let ((c (char pattern i)))
	       (cond ((char= c #\\) (incf i))
		     ((char= c #\[)
		      ;; a bracket expression: ] first is literal
		      (incf i)
		      (when (and (< i n) (char= (char pattern i) #\^)) (incf i))
		      (when (and (< i n) (char= (char pattern i) #\])) (incf i))
		      (loop while (and (< i n) (char/= (char pattern i) #\])) do (incf i)))
		     ((char= c #\() (push (list i '()) stack))
		     ((and (char= c #\|) stack) (push i (second (car stack))))
		     ((and (char= c #\)) stack)
		      (destructuring-bind (open bars) (pop stack)
			(push (list open i (reverse bars)) groups)))))
	     (incf i))
    groups))

(defun drop-empty-alternatives (pattern)
  (let ((group (find-if (lambda (g)
			  (destructuring-bind (open close bars) g
			    (let ((edges (append (list open) bars (list close))))
			      (and bars
				   (loop for (a b) on edges while b thereis (= (1+ a) b))
				   (not (and (< (1+ close) (length pattern))
					     (find (char pattern (1+ close)) "*+?{")))))))
			(regex-groups pattern))))
    (if (null group)
	pattern
	(destructuring-bind (open close bars) group
	  (let* ((edges (append (list open) bars (list close)))
		 (alternatives (loop for (a b) on edges while b
				     collect (subseq pattern (1+ a) b)))
		 (kept (remove "" alternatives :test #'string=)))
	    (drop-empty-alternatives
	     (if kept
		 (format nil "~A(~{~A~^|~})?~A" (subseq pattern 0 open) kept (subseq pattern (1+ close)))
		 pattern)))))))

(defguile "make-regexp" (pattern &rest flags)
  #+darwin (setq pattern (drop-empty-alternatives pattern))
  (let* ((cflags (reduce #'logior flags :initial-value 0))
	 (cflags (if (logtest cflags 256) (logandc2 cflags 256) (logior cflags +reg-extended+)))
	 (pointer (cffi:foreign-alloc :char :count +regex-t-size+))
	 (code (cffi:with-foreign-string (p pattern :encoding :utf-8)
		 (cffi:foreign-funcall "regcomp" :pointer pointer :pointer p :int cflags :int))))
    (unless (zerop code)
      (regexp-error pointer code "make-regexp"))
    (let ((r (%make-regexp pattern cflags pointer)))
      (trivial-garbage:finalize r (lambda ()
				    (cffi:foreign-funcall "regfree" :pointer pointer :void)
				    (cffi:foreign-free pointer)))
      r)))

(defguile "regexp?" (x) (bool (regexp-p x)))

(defun byte->char-index (octets start byte)
  "The character index, in the string OCTETS encode, of byte offset BYTE
counted from START."
  (+ start (length (sb-ext:octets-to-string octets :external-format :utf-8 :end byte))))

(defguile "regexp-exec" (rx string &optional (start 0) (eflags 0))
  (let* ((rx (if (stringp rx) (funcall (gethash "make-regexp" *guile-primitives*) rx) rx))
	 (octets (sb-ext:string-to-octets string :external-format :utf-8 :start start))
	 (n 10)
	 (size (* 2 +regoff-size+)))
    (cffi:with-foreign-objects ((buf :uint8 (1+ (length octets)))
				(matches :uint8 (* n size)))
      (loop for b across octets for i from 0 do (setf (cffi:mem-aref buf :uint8 i) b))
      (setf (cffi:mem-aref buf :uint8 (length octets)) 0)
      (let ((code (cffi:foreign-funcall "regexec" :pointer (regexp-pointer rx) :pointer buf
						  :size n :pointer matches :int eflags :int)))
	(if (/= code 0)
	    ps:false
	    (let ((v (make-array (1+ n) :initial-element (cons -1 -1)))
		  (count 0))
	      (setf (svref v 0) string)
	      (dotimes (i n)
		(let ((so (cffi:mem-ref matches (if (= +regoff-size+ 8) :int64 :int32) (* i size)))
		      (eo (cffi:mem-ref matches (if (= +regoff-size+ 8) :int64 :int32) (+ (* i size) +regoff-size+))))
		  (if (minusp so)
		      (setf (svref v (1+ i)) (cons -1 -1))
		      (setf (svref v (1+ i)) (cons (byte->char-index octets start so)
						   (byte->char-index octets start eo))
			    count (1+ i)))))
	      (subseq v 0 (1+ (max count (min n (1+ (count #\( (regexp-pattern rx)))))))))))))

(defun install-regex-root-bindings ()
  (flet ((def (name value) (obarray-define (ssym name) value)))
    (def "regexp/basic" 256)
    (def "regexp/extended" +reg-extended+)
    (def "regexp/icase" +reg-icase+)
    (def "regexp/newline" +reg-newline+)
    (def "regexp/notbol" +reg-notbol+)
    (def "regexp/noteol" +reg-noteol+)))
