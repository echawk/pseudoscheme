; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

;;;; Extflonums, as Racket CS has them (rumble/extfl.ss): unsupported.
;;;; The reader still makes pseudo-extflonums of literals like 1.0t0
;;;; (the host's string->number in 'read mode does, which the
;;;; expander's reader calls: EXTFLONUM-FROM-STRING); they print as
;;;; written and satisfy extflonum?, but every operation on them raises
;;;; exn:fail:unsupported, and extflonum-available? is #f.
;;;;
;;;; This file loads after basics.lisp, whose extflonum? and
;;;; extflonum-available? it replaces.

(in-package "PSEUDOSCHEME-RACKET")

(defstruct (extflonum (:constructor make-extflonum (str)) (:predicate extflonum-p))
  str)

;; a literal in compiled code (the reader's 3.14t0) is dumped into a fasl
(defmethod make-load-form ((x extflonum) &optional environment)
  (declare (ignore environment))
  `(make-extflonum ,(extflonum-str x)))

(defun extflonum-string-p (s)
  "Whether S reads as an extflonum: a number's digits, then t or T and
an exponent (1.0t0, +inf.t, #e1t2 isn't one: the reader said it was)."
  (and (stringp s)
       (let ((tpos (position-if (lambda (c) (char-equal c #\t)) s :from-end t)))
	 (and tpos (plusp tpos)
	      (let ((before (subseq s 0 tpos)))
		(or (some #'digit-char-p before)
		    (member before '("+inf." "-inf." "+nan." "-nan.") :test #'string-equal)))
	      (every (lambda (c) (or (digit-char-p c) (find c "+-.#/exEdDfFsSlLinaINA"))) (subseq s 0 tpos))
	      (let ((exponent (string-left-trim "+-" (subseq s (1+ tpos)))))
		(or (zerop (length exponent)) (every #'digit-char-p exponent)))))))

(defun extflonum-from-string (s more)
  "The pseudo-extflonum the host's string->number makes of S when asked
in 'read mode (MORE: its remaining arguments, radix then mode), or NIL."
  (and (extflonum-string-p s)
       (second more)
       (eq (second more) (rsym "read"))
       (make-extflonum s)))

(defnative "extflonum?" (x) (:instance "#%extfl" :leaf t) (bool (extflonum-p x)))
(defnative "extflonum-available?" () (:instance "#%extfl" :leaf t) ps:false)

(defun extfl-unsupported (who)
  (raise-exn "exn:fail:unsupported" (format nil "~A: unsupported" who)))

(defun check-extfl (who &rest args)
  (dolist (a args)
    (unless (extflonum-p a) (arg-error who "extflonum?" a))))

(defmacro define-extfl-operations (instance &rest definitions)
  "Each (name arg ...): a procedure of INSTANCE that checks its
arguments are extflonums and says it's unsupported."
  `(progn
     ,@(loop for (name . args) in definitions
	     collect `(defnative ,name ,args (:instance ,instance :leaf t)
			(check-extfl ,name ,@args)
			(extfl-unsupported ,name)))))

(define-extfl-operations "#%extfl"
  ("extfl*" a b) ("extfl+" a b) ("extfl-" a b) ("extfl/" a b)
  ("extfl<" a b) ("extfl<=" a b) ("extfl=" a b) ("extfl>" a b) ("extfl>=" a b)
  ("extfl->exact" a) ("extfl->exact-integer" a) ("extfl->fx" a) ("extfl->inexact" a)
  ("extflabs" a) ("extflacos" a) ("extflasin" a) ("extflatan" a) ("extflceiling" a)
  ("extflcos" a) ("extflexp" a) ("extflexpt" a b) ("extflfloor" a) ("extfllog" a)
  ("extflmax" a b) ("extflmin" a b) ("extflround" a) ("extflsin" a) ("extflsqrt" a)
  ("extfltan" a) ("extfltruncate" a))

(define-extfl-operations "#%unsafe"
  ("unsafe-extfl*" a b) ("unsafe-extfl+" a b) ("unsafe-extfl-" a b) ("unsafe-extfl/" a b)
  ("unsafe-extfl<" a b) ("unsafe-extfl<=" a b) ("unsafe-extfl=" a b)
  ("unsafe-extfl>" a b) ("unsafe-extfl>=" a b)
  ("unsafe-extflabs" a) ("unsafe-extflmax" a b) ("unsafe-extflmin" a b)
  ("unsafe-extflsqrt" a) ("unsafe-extfl->fx" a))

(defnative "extfl->floating-point-bytes" (a &rest more) (:instance "#%extfl" :leaf t)
  (declare (ignore more))
  (check-extfl "extfl->floating-point-bytes" a)
  (extfl-unsupported "extfl->floating-point-bytes"))

(defnative "->extfl" (a) (:instance "#%extfl" :leaf t)
  (unless (integerp a) (arg-error "->extfl" "exact-integer?" a))
  (extfl-unsupported "->extfl"))

(defnative "fx->extfl" (a) (:instance "#%extfl" :leaf t)
  (unless (typep a '(signed-byte 61)) (arg-error "fx->extfl" "fixnum?" a))
  (extfl-unsupported "fx->extfl"))

(defnative "unsafe-fx->extfl" (a) (:instance "#%unsafe" :leaf t)
  (unless (typep a '(signed-byte 61)) (arg-error "unsafe-fx->extfl" "fixnum?" a))
  (extfl-unsupported "unsafe-fx->extfl"))

(defnative "real->extfl" (a) (:instance "#%extfl" :leaf t)
  (unless (realp a) (arg-error "real->extfl" "real?" a))
  (extfl-unsupported "real->extfl"))

(defnative "floating-point-bytes->extfl" (bstr &optional big-endian? start end) (:instance "#%extfl" :leaf t)
  (declare (ignore big-endian?))
  (let ((who "floating-point-bytes->extfl"))
    (unless (typep bstr '(simple-array (unsigned-byte 8) (*))) (arg-error who "bytes?" bstr))
    (let ((start (or start 0))
	  (end (or end (length bstr))))
      (unless (exact-nonneg-p start) (arg-error who "exact-nonnegative-integer?" start))
      (unless (exact-nonneg-p end) (arg-error who "exact-nonnegative-integer?" end))
      (if (= (- end start) 10)
	  (extfl-unsupported who)
	  (racket-error who "length is not 10 bytes~%  length: ~A" (- end start))))))

;;; extflvectors can't be made, so nothing is one

(defnative "extflvector?" (x) (:instance "#%extfl" :leaf t) (declare (ignore x)) ps:false)

(macrolet ((define-makers (&rest names)
	     `(progn
		,@(loop for name in names
			collect `(defnative ,name (len &optional (v nil v-p)) (:instance "#%extfl" :leaf t)
				   (unless (exact-nonneg-p len) (arg-error ,name "exact-nonnegative-integer?" len))
				   (when v-p (check-extfl ,name v))
				   (extfl-unsupported ,name)))))
	   (define-listers (&rest names)
	     `(progn
		,@(loop for name in names
			collect `(defnative ,name (&rest args) (:instance "#%extfl" :leaf t)
				   (apply #'check-extfl ,name args)
				   (extfl-unsupported ,name))))))
  (define-makers "make-extflvector" "make-shared-extflvector")
  (define-listers "extflvector" "shared-extflvector"))

(macrolet ((define-accessors (&rest specs)
	     `(progn
		,@(loop for (instance name . args) in specs
			collect `(defnative ,name ,args (:instance ,instance :leaf t)
				   (declare (ignore ,@(cdr args)))
				   (arg-error ,name "extflvector?" ,(car args)))))))
  (define-accessors
    ("#%extfl" "extflvector-length" a)
    ("#%extfl" "extflvector-ref" a i)
    ("#%extfl" "extflvector-set!" a i v)
    ("#%unsafe" "unsafe-extflvector-length" a)
    ("#%unsafe" "unsafe-extflvector-ref" a i)
    ("#%unsafe" "unsafe-extflvector-set!" a i v)))
