; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-R6RS -*-

;;;; Chez Scheme's foreign-function interface, on CFFI: load-shared-object,
;;;; foreign-procedure, and foreign memory as integer addresses
;;;; (foreign-alloc, foreign-free, foreign-ref, foreign-set!,
;;;; foreign-sizeof), on the layer the Schemes share (src/ffi.lisp).
;;;;
;;;; A foreign procedure is compiled when it is made: a Lisp function
;;;; calling the entry point through CFFI with the argument and result
;;;; types translated from Chez's.  Not here: ftypes, and the types
;;;; scheme-object, wchar_t and the u16*/u32* buffers.

(in-package "QUASISCHEME-R6RS")

(defun chez-ffi-type (type)
  "The CFFI type for Chez's foreign type TYPE (a Scheme symbol), and how
values cross: :int, :pointer for u8* (a bytevector's data), :string, :address
(an integer, as Chez gives pointers), :boolean, :void."
  (let ((name (qs:scheme-symbol-name type)))
    (cond ((member name '("int" "integer-32") :test #'string=) (values :int :number))
	  ((member name '("unsigned" "unsigned-int" "unsigned-32") :test #'string=) (values :unsigned-int :number))
	  ((member name '("long") :test #'string=) (values :long :number))
	  ((member name '("unsigned-long") :test #'string=) (values :unsigned-long :number))
	  ((member name '("short" "integer-16") :test #'string=) (values :short :number))
	  ((member name '("unsigned-short" "unsigned-16") :test #'string=) (values :unsigned-short :number))
	  ((member name '("integer-8") :test #'string=) (values :int8 :number))
	  ((member name '("unsigned-8") :test #'string=) (values :uint8 :number))
	  ((member name '("integer-64" "long-long") :test #'string=) (values :int64 :number))
	  ((member name '("unsigned-64" "unsigned-long-long") :test #'string=) (values :uint64 :number))
	  ((member name '("iptr" "ssize_t" "fixnum" "ptrdiff_t") :test #'string=) (values :intptr :number))
	  ((member name '("uptr" "size_t") :test #'string=) (values :uintptr :number))
	  ((member name '("void*") :test #'string=) (values :uintptr :number))
	  ((member name '("double" "double-float") :test #'string=) (values :double :float))
	  ((member name '("float" "single-float") :test #'string=) (values :float :float))
	  ((member name '("boolean") :test #'string=) (values :int :boolean))
	  ((member name '("char") :test #'string=) (values :uint8 :char))
	  ;; wchar_t: 32 bits on Linux and macOS
	  ((member name '("wchar" "wchar_t") :test #'string=) (values :uint32 :char))
	  ((member name '("string" "utf-8") :test #'string=) (values :pointer :string))
	  ((member name '("u8*") :test #'string=) (values :pointer :bytes))
	  ((member name '("void") :test #'string=) (values :void :void))
	  (t (qs:scheme-error "foreign-procedure: unsupported type ~A" name)))))

(defun chez-type (type)
  "Chez's foreign TYPE as a type of the shared layer (src/ffi.lisp)."
  (multiple-value-list (chez-ffi-type type)))

(defprim "chez:load-shared-object" (path)
  (qsffi:load-library path)
  qs:unspecific)

(defun varargs-after (conventions)
  "Where CONVENTIONS, a foreign-procedure's, say the variable arguments
start: N for (__varargs_after N), 1 for __varargs; else NIL."
  (loop for c in conventions
	thereis (cond ((and (symbolp c) (string= (qs:scheme-symbol-name c) "__varargs")) 1)
		      ((and (consp c) (symbolp (car c))
			    (string= (qs:scheme-symbol-name (car c)) "__varargs_after"))
		       (cadr c)))))

;;; Signal masks and sigwait.  SBCL owns the signals it defers (it
;;; blocks them all or none, and dies on a mask that blocks some), and a
;;; thread inside sigwait never answers its signal to stop for a
;;; collection on macOS.  So pthread_sigmask, sigprocmask and sigwait, as
;;; foreign procedures, are emulated for the signals a program blocks to
;;; wait for (*EMULATED-SIGNALS*), as e's base does: a blocked one is
;;; caught by a Lisp handler and kept pending until sigwait takes it, or,
;;; unblocked, it is raised again for the handler it had.  The mask is
;;; the process's, not each thread's.  Other signals' masking is left
;;; alone (a request to block them does nothing).

(defparameter *emulated-signals*
  (loop for name in '("SIGHUP" "SIGINT" "SIGQUIT" "SIGTERM" "SIGALRM" "SIGTSTP" "SIGCHLD" "SIGWINCH" "SIGIO")
	for symbol = (find-symbol name "SB-UNIX")
	when (and symbol (boundp symbol)) collect (symbol-value symbol)))

(defvar *pending-signals* (make-array 64 :initial-element nil)
  "Signal -> whether it arrived while blocked (set by a handler).")
(defvar *blocked-signals* '() "(signal . the handler it had) of each blocked signal.")
(defvar *signal-mask-lock* (sb-thread:make-mutex :name "signal mask"))

(defun signal-set-members (address)
  (loop for signal from 1 below 32
	when (= 1 (cffi:foreign-funcall "sigismember" :pointer (cffi:make-pointer address)
					:int signal :int))
	  collect signal))

(defun block-emulated-signal (signal)
  (unless (assoc signal *blocked-signals*)
    (push (cons signal (sb-sys:enable-interrupt
			signal (lambda (sig info context)
				 (declare (ignore info context))
				 (setf (svref *pending-signals* sig) t))))
	  *blocked-signals*)))

(defun unblock-emulated-signal (signal)
  (let ((entry (assoc signal *blocked-signals*)))
    (when entry
      (setq *blocked-signals* (remove entry *blocked-signals*))
      (sb-sys:enable-interrupt signal (cdr entry))
      (when (svref *pending-signals* signal)
	(setf (svref *pending-signals* signal) nil)
	(sb-posix:kill (sb-posix:getpid) signal)))))

(defun emulated-sigmask (how set old)
  "pthread_sigmask(HOW, SET, OLD), and sigprocmask's, for *EMULATED-SIGNALS*."
  (sb-thread:with-recursive-lock (*signal-mask-lock*)
    (unless (zerop old)
      (cffi:foreign-funcall "sigemptyset" :pointer (cffi:make-pointer old) :int)
      (dolist (entry *blocked-signals*)
	(cffi:foreign-funcall "sigaddset" :pointer (cffi:make-pointer old) :int (car entry) :int)))
    (unless (zerop set)
      (let ((members (intersection (signal-set-members set) *emulated-signals*)))
	(case how
	  (#+darwin 1 #-darwin 0 (mapc #'block-emulated-signal members))
	  (#+darwin 2 #-darwin 1 (mapc #'unblock-emulated-signal members))
	  (t (dolist (signal *emulated-signals*)
	       (if (member signal members)
		   (block-emulated-signal signal)
		   (unblock-emulated-signal signal))))))))
  0)

(defun emulated-sigwait (set sig)
  "sigwait(SET, SIG): the next pending signal of SET, waited for in Lisp
(a sleep between looks), so that collections go on meanwhile."
  (let ((members (signal-set-members set)))
    (loop
      (dolist (signal members)
	(when (svref *pending-signals* signal)
	  (setf (svref *pending-signals* signal) nil)
	  (unless (zerop sig) (setf (cffi:mem-ref (cffi:make-pointer sig) :int) signal))
	  (return-from emulated-sigwait 0)))
      (sleep 0.01))))

(defprim "chez:foreign-procedure" (entry params result &optional conventions)
  (let ((address (if (integerp entry) entry (qsffi:symbol-address entry))))
    (unless address
      (qs:scheme-error "foreign-procedure: no entry for ~S" entry))
    (let ((f (qsffi:foreign-function address (mapcar #'chez-type params) (chez-type result)
				     :varargs-after (varargs-after conventions))))
      (cond ((and (equal entry "sigwait") (= (length params) 2)) #'emulated-sigwait)
	    ((and (member entry '("pthread_sigmask" "sigprocmask") :test #'equal) (= (length params) 3))
	     #'emulated-sigmask)
	    (t f)))))

(defprim "chez:foreign-callable" (procedure params result)
  (qsffi:foreign-callback procedure (mapcar #'chez-type params) (chez-type result)))

(defprim "chez:foreign-entry?" (name)
  (bool (qsffi:symbol-address name)))

(defprim "chez:foreign-entry" (name)
  (or (qsffi:symbol-address name)
      (qs:scheme-error "foreign-entry: no entry for ~S" name)))

;;; Memory, addressed by integers

(defprim "chez:foreign-alloc" (n) (qsffi:allocate n))

(defprim "chez:foreign-free" (address)
  (qsffi:free address)
  qs:unspecific)

(defprim "chez:foreign-sizeof" (type)
  (qsffi:type-size (chez-ffi-type type)))

(defun memory-ctype (type)
  (multiple-value-bind (ctype kind) (chez-ffi-type type)
    (values (if (member kind '(:string :bytes)) :uintptr ctype) kind ctype)))

(defprim "chez:foreign-ref" (type address offset)
  (multiple-value-bind (ctype kind) (memory-ctype type)
    (let ((v (qsffi:mem-ref ctype address offset)))
      (case kind
	(:char (code-char v))
	(:boolean (if (zerop v) qs:false t))
	(:float (coerce v 'double-float))
	(t v)))))

(defprim "chez:foreign-set!" (type address offset value)
  (multiple-value-bind (ctype kind) (memory-ctype type)
    (qsffi:mem-set ctype address offset
		   (case kind
		     (:char (char-code value))
		     (:boolean (if (eq value qs:false) 0 1))
		     (:float (coerce value (if (eq ctype :double) 'double-float 'single-float)))
		     (t value))))
  qs:unspecific)
