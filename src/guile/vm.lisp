; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-GUILE -*-

;;;; Guile's VM: the machine
;;;;
;;;; Written from Guile's manual (9.3, "A Virtual Machine for Guile"),
;;;; not from libguile's sources; see docs/guile-vm.md.
;;;;
;;;; The stack is a Lisp vector indexed by height: index 0 is its
;;;; bottom, and a frame's locals are above its frame pointer.  The
;;;; manual's stack grows down, so its addresses are our heights
;;;; reversed: with FP the height of the frame's machine return address,
;;;;
;;;;    local I                      FP + 1 + I
;;;;    machine return address       FP
;;;;    virtual return address       FP - 1
;;;;    dynamic link (FP - caller's FP)   FP - 2
;;;;    the caller's locals          below
;;;;
;;;; and SP is the height of the frame's last local, so the frame has
;;;; SP - FP locals and the sp-relative slot J is at SP - J.

(in-package "PSEUDOSCHEME-GUILE")

;;; ------------------------------------------------------------------
;;; Images: an ELF image's bytes, and its memory as 8-byte cells

(defstruct (vm-image (:constructor %make-vm-image))
  bytes					; the image, (unsigned-byte 8)
  cells					; one per 8 bytes: raw bits, or a Lisp object stored there
  (objects (make-hash-table))		; byte address -> the object decoded there
  elf)

(defun make-vm-image (bytes)
  (let* ((n (ceiling (length bytes) 8))
	 (cells (make-array n)))
    (dotimes (i n)
      (let ((v 0))
	(loop for k from 7 downto 0
	      for at = (+ (* 8 i) k)
	      do (setq v (logior (ash v 8) (if (< at (length bytes)) (aref bytes at) 0))))
	(setf (svref cells i) v)))
    (%make-vm-image :bytes bytes :cells cells)))

(defstruct (code-pointer (:constructor make-code-pointer (image word)))
  "A bytecode address: an image and a 32-bit word index in it."
  image word)

;;; ------------------------------------------------------------------
;;; SCM bits: immediates as Guile encodes them

(defconstant +scm-false+ 4)
(defconstant +scm-nil+ 260)
(defconstant +scm-null+ 772)
(defconstant +scm-true+ 1028)
(defconstant +scm-unspecified+ 2052)
(defconstant +scm-undefined+ 2308)
(defconstant +scm-eof+ 2564)
(defconstant +fixnum-bits+ 62)

(defun scm-from-bits (bits &optional image)
  "The Scheme value whose SCM bits are BITS, an immediate, or a pointer
into IMAGE."
  (cond ((= (logand bits 3) 2) (sign-extend (ash bits -2) +fixnum-bits+))
	((= (logand bits #xff) 12) (code-char (ash bits -8)))
	((= bits +scm-false+) ps:false)
	((= bits +scm-null+) '())
	((= bits +scm-true+) ps:true)
	((= bits +scm-unspecified+) *unspecified*)
	((= bits +scm-undefined+) +unbound+)
	((= bits +scm-eof+) ps:eof-object)
	((= bits +scm-nil+) *elisp-nil*)
	((and (zerop (logand bits 7)) image) (image-object image bits))
	(t (vm-error "can't decode SCM bits ~X" bits))))

(defun scm-bits (x)
  "X's SCM bits, as Guile would encode it.  A heap object's are those of
a pointer whose low bits are clear (so that only its first word tells
its type)."
  (cond ((and (integerp x) (<= (- (ash 1 61)) x (1- (ash 1 61))))
	 (logior (ldb (byte 64 0) (ash x 2)) 2))
	((characterp x) (logior (ash (char-code x) 8) 12))
	((eq x ps:false) +scm-false+)
	((null x) +scm-null+)
	((eq x ps:true) +scm-true+)
	((eq x *unspecified*) +scm-unspecified+)
	((eq x +unbound+) +scm-undefined+)
	((eq x ps:eof-object) +scm-eof+)
	((eq x *elisp-nil*) +scm-nil+)
	(t 8)))

(defun vm-error (format &rest args)
  (guile-error (ssym "vm-error") ps:false (apply #'format nil format args) '()))

;;; ------------------------------------------------------------------
;;; Programs: compiled procedures

(defclass vm-program ()
  ((code :initarg :code :accessor program-code)	; a code-pointer
   (free :initarg :free :accessor program-free))	; simple-vector of free variables
  (:metaclass sb-mop:funcallable-standard-class))

(defun make-vm-program (code nfree)
  (let ((p (make-instance 'vm-program :code code :free (make-array nfree :initial-element ps:false))))
    (sb-mop:set-funcallable-instance-function p (lambda (&rest args) (vm-apply p args)))
    p))

;;; ------------------------------------------------------------------
;;; Static objects: the words of an image, decoded

(defun image-cell (image address)
  (unless (zerop (mod address 8)) (vm-error "unaligned static address ~X" address))
  (svref (vm-image-cells image) (floor address 8)))

(defun (setf image-cell) (value image address)
  (unless (zerop (mod address 8)) (vm-error "unaligned static address ~X" address))
  (remhash address (vm-image-objects image))
  (setf (svref (vm-image-cells image) (floor address 8)) value))

(defun image-scm (image address)
  "The SCM value in the cell at ADDRESS."
  (let ((cell (image-cell image address)))
    (if (integerp cell) (scm-from-bits cell image) cell)))

(defun image-object (image address)
  "The object whose memory is at ADDRESS in IMAGE, decoded once."
  (or (gethash address (vm-image-objects image))
      (setf (gethash address (vm-image-objects image)) (decode-static-object image address))))

(defun decode-static-object (image address)
  (let ((word0 (image-cell image address)))
    (flet ((raw (i) (let ((c (image-cell image (+ address (* 8 i)))))
		      (if (integerp c) c (vm-error "expected raw word at ~X" (+ address (* 8 i))))))
	   (scm (i) (image-scm image (+ address (* 8 i)))))
      (cond ((not (integerp word0)) (cons word0 (scm 1))) ; a pair whose car was stored
	    ((zerop (logand word0 1)) (cons (scm 0) (scm 1))) ; a pair
	    (t
	     (let ((tc7 (logand word0 #x7f)))
	       (case tc7
		 (69 (let ((program (make-vm-program
				     (make-code-pointer image (floor (raw 1) 4))
				     (ash word0 -16))))
		       (dotimes (i (length (program-free program)) program)
			 (setf (svref (program-free program) i) (scm (+ 2 i))))))
		 (13 (let* ((n (ash word0 -8)) (v (make-array n)))
		       (dotimes (i n v) (setf (svref v i) (scm (+ 1 i))))))
		 (21 (decode-static-string image (raw 1) (raw 2) (raw 3)))
		 (61 (make-syntax-object (scm 1) (scm 2) (scm 3) (scm 4)))
		 (77 (let* ((n (raw 1))
			    (contents (raw 2))
			    (bytes (vm-image-bytes image)))
		       (subseq bytes contents (+ contents n))))
		 (otherwise (vm-error "static object of type ~D at ~X not decoded yet" tc7 address)))))))))

(defun decode-static-string (image stringbuf start length)
  (let* ((header (image-cell image stringbuf))
	 (wide (logtest header #x400))
	 (bytes (vm-image-bytes image))
	 (chars (+ stringbuf 16))
	 (s (make-string length)))
    (dotimes (i length s)
      (setf (char s i)
	    (code-char (if wide
			   (let ((at (+ chars (* 4 (+ start i)))))
			     (logior (aref bytes at) (ash (aref bytes (+ at 1)) 8)
				     (ash (aref bytes (+ at 2)) 16) (ash (aref bytes (+ at 3)) 24)))
			   (aref bytes (+ chars start i))))))))

;;; ------------------------------------------------------------------
;;; The machine

(defstruct (vm (:constructor %make-vm))
  (stack (make-array 4096 :initial-element ps:false) :type simple-vector)
  (fp 0 :type fixnum)
  (sp 0 :type fixnum)
  (compare :none))			; :less, :equal, :none or :invalid

(defvar *vm* nil "The thread's VM.")

(defun current-vm () (or *vm* (setq *vm* (%make-vm))))

(defun ensure-stack (vm height)
  (let ((stack (vm-stack vm)))
    (when (>= height (length stack))
      (let ((new (make-array (max (* 2 (length stack)) (+ height 1024)) :initial-element ps:false)))
	(replace new stack)
	(setf (vm-stack vm) new)))))

(defmacro local (i) `(svref stack (+ fp 1 ,i)))
(defmacro sp-slot (j) `(svref stack (- sp ,j)))

(defvar *vm-trace* nil "True: print each instruction as it runs.")

(defun vm-apply (program args)
  "Call PROGRAM, a vm-program, on ARGS, from Lisp: its values."
  (let* ((vm (current-vm))
	 (saved-fp (vm-fp vm)) (saved-sp (vm-sp vm))
	 (fp (+ saved-sp 3)))
    (ensure-stack vm (+ fp 2 (length args)))
    (let ((stack (vm-stack vm)))
      (setf (svref stack fp) nil
	    (svref stack (- fp 1)) :boundary
	    (svref stack (- fp 2)) (- fp saved-fp)
	    (local 0) program)
      (loop for a in args for i from 1 do (setf (local i) a))
      (setf (vm-fp vm) fp (vm-sp vm) (+ fp 1 (length args))))
    (unwind-protect
	 (values-list (run-vm vm (program-code program)))
      (setf (vm-fp vm) saved-fp (vm-sp vm) saved-sp))))

;;; ------------------------------------------------------------------
;;; Instructions
;;;
;;; Each instruction's handler takes the machine, the current image, the
;;; instruction's word index (IP), its operands and the index after it
;;; (NEXT), and returns where to go: a word index in the same image, a
;;; code-pointer, or a list of values when a frame returns to Lisp.

(defvar *vm-handlers* (make-hash-table :test 'equal))
(defvar *vm-dispatch* nil "Opcode -> handler.")

(defmacro defop (name operands &body body)
  "Define instruction NAME's handler.  OPERANDS name its operands, in the
order of its fields; FP, SP and STACK are the machine's."
  (let ((ops (gensym "OPERANDS")))
    `(setf (gethash ,name *vm-handlers*)
	   (lambda (vm image ip ,ops next)
	     (declare (ignorable vm image ip next))
	     (destructuring-bind ,operands ,ops
	       (declare (ignorable ,@(remove '&rest operands)))
	       (symbol-macrolet ((fp (vm-fp vm)) (sp (vm-sp vm)) (stack (vm-stack vm)))
		 ,@body))))))

(defun vm-dispatch-table ()
  (or *vm-dispatch*
      (let ((table (make-array 256 :initial-element nil)))
	(unless *vm-ops* (load-vm-ops))
	(loop for op across *vm-ops*
	      when op do (setf (svref table (vm-op-opcode op))
			       (or (gethash (vm-op-name op) *vm-handlers*)
				   (let ((name (vm-op-name op)))
				     (lambda (&rest args)
				       (declare (ignore args))
				       (vm-error "VM instruction ~A isn't implemented" name))))))
	(setq *vm-dispatch* table))))

(defstruct (extent-exit (:constructor make-extent-exit (location)))
  "The end of a dynamic extent a nested run of the machine is in: where
to go on."
  location)

(defun run-vm (vm code &optional nested)
  "Run from CODE, a code-pointer, until a frame returns to Lisp: the
values it returns.  If NESTED, the run is a dynamic extent's body (a
prompt's, a dynamic-wind's, a fluid binding's), and ends with the
extent-exit of the instruction that ends the extent."
  (let ((dispatch (vm-dispatch-table))
	(image (code-pointer-image code))
	(ip (code-pointer-word code)))
    (loop
      (let ((bytes (vm-image-bytes image)))
	(multiple-value-bind (op operands next) (decode-instruction bytes ip)
	  (when *vm-trace*
	    (format *trace-output* "~&;; ~5D ~A ~{~S~^ ~}  fp ~D sp ~D~%" ip (vm-op-name op) operands
		    (vm-fp vm) (vm-sp vm)))
	  (let ((to (funcall (svref dispatch (vm-op-opcode op)) vm image ip operands next)))
	    (etypecase to
	      (fixnum (setq ip to))
	      (code-pointer (setq image (code-pointer-image to) ip (code-pointer-word to)))
	      (list (return to))
	      (extent-exit
	       (unless nested (vm-error "a dynamic extent ended that wasn't begun"))
	       (return to)))))))))

;;; Locals

(defun vm-local (vm i) (svref (vm-stack vm) (+ (vm-fp vm) 1 i)))
(defun (setf vm-local) (value vm i) (setf (svref (vm-stack vm) (+ (vm-fp vm) 1 i)) value))
(defun vm-slot (vm j) (svref (vm-stack vm) (- (vm-sp vm) j)))
(defun (setf vm-slot) (value vm j) (setf (svref (vm-stack vm) (- (vm-sp vm) j)) value))

(defun set-frame-size (vm n &optional fill)
  "Make the frame N locals; new locals hold FILL (unless it is NIL)."
  (let ((old (vm-sp vm)) (new (+ (vm-fp vm) n)))
    (ensure-stack vm (+ new 1))
    (when (and fill (> new old))
      (fill (vm-stack vm) fill :start (+ old 1) :end (+ new 1)))
    (setf (vm-sp vm) new)))

(defun frame-size (vm) (- (vm-sp vm) (vm-fp vm)))

;;; Calls and returns

(defun return-from-frame (vm)
  "Pop the frame, its values left in place: where to go next."
  (let* ((stack (vm-stack vm)) (fp (vm-fp vm))
	 (ra (svref stack (- fp 1))))
    (if (eq ra :boundary)
	(loop for i from 1 to (- (vm-sp vm) fp) collect (svref stack (+ fp i)))
	(progn (setf (vm-fp vm) (- fp (svref stack (- fp 2))))
	       ra))))

(defun call-lisp-procedure (vm f)
  "Apply F, not a compiled program, to the frame's arguments; leave its
values in the frame, and return from it."
  (let* ((n (frame-size vm))
	 (args (loop for i from 1 below n collect (vm-local vm i)))
	 (values (multiple-value-list
		  (if (functionp f)
		      (apply f args)
		      (guile-error (ssym "wrong-type-arg") ps:false "Wrong type to apply: ~S"
				   (list f) (list f))))))
    (set-frame-size vm (length values))
    (loop for v in values for i from 0 do (setf (vm-local vm i) v))
    (return-from-frame vm)))

(defun push-call-frame (vm proc nlocals return-to)
  (let ((new-fp (+ (vm-fp vm) proc)))
    (ensure-stack vm (+ new-fp nlocals 1))
    (let ((stack (vm-stack vm)))
      (setf (svref stack new-fp) nil
	    (svref stack (- new-fp 1)) return-to
	    (svref stack (- new-fp 2)) proc))
    (setf (vm-fp vm) new-fp (vm-sp vm) (+ new-fp nlocals))))

(defun enter-procedure (vm callee)
  "Go to CALLEE in the frame just set up for it."
  (if (typep callee 'vm-program)
      (program-code callee)
      (call-lisp-procedure vm callee)))

(defop "call" (proc nlocals)
  (push-call-frame vm proc nlocals (make-code-pointer image next))
  (enter-procedure vm (vm-local vm 0)))

(defop "call-label" (proc nlocals label)
  (push-call-frame vm proc nlocals (make-code-pointer image next))
  (+ ip label))

(defop "tail-call" ()
  (enter-procedure vm (vm-local vm 0)))

(defop "tail-call-label" (label)
  (+ ip label))

(defop "return-values" ()
  (return-from-frame vm))

(defun values-error (key message)
  (guile-error (ssym key) ps:false message '()))

(defop "receive" (dst proc nlocals)
  (when (< (- sp (+ fp proc)) 1) (values-error "vm-error" "Zero values returned to single-valued continuation"))
  (setf (vm-local vm dst) (vm-local vm proc))
  (set-frame-size vm nlocals)
  next)

(defop "receive-values" (proc allow-extra nvalues)
  (let ((n (- sp (+ fp proc))))
    (when (if (= allow-extra 1) (< n nvalues) (/= n nvalues))
      (values-error "vm-error" "Wrong number of values returned to continuation")))
  next)

;;; Prologues

(defun wrong-num-args (vm)
  (guile-error (ssym "wrong-number-of-args") ps:false "Wrong number of arguments to ~A"
	       (list (vm-local vm 0)) ps:false))

(defop "assert-nargs-ee" (n) (unless (= (frame-size vm) n) (wrong-num-args vm)) next)
(defop "assert-nargs-ge" (n) (unless (>= (frame-size vm) n) (wrong-num-args vm)) next)
(defop "assert-nargs-le" (n) (unless (<= (frame-size vm) n) (wrong-num-args vm)) next)
(defop "assert-nargs-ee/locals" (expected nlocals)
  (unless (= (frame-size vm) expected) (wrong-num-args vm))
  (set-frame-size vm (+ expected nlocals) +unbound+)
  next)
(defop "alloc-frame" (n) (set-frame-size vm n +unbound+) next)
(defop "reset-frame" (n) (set-frame-size vm n) next)
(defop "bind-optionals" (n)
  (when (< (frame-size vm) n) (set-frame-size vm n +unbound+))
  next)
(defop "bind-rest" (dst)
  (let ((rest (loop for i from dst below (frame-size vm) collect (vm-local vm i))))
    (set-frame-size vm (+ dst 1) +unbound+)
    (setf (vm-local vm dst) rest))
  next)
(defop "arguments<=?" (n)
  (let ((nargs (frame-size vm)))
    (setf (vm-compare vm) (cond ((< nargs n) :less) ((= nargs n) :equal) (t :none))))
  next)

;;; Moving values

(defop "mov" (dst src) (setf (vm-slot vm dst) (vm-slot vm src)) next)
(defop "long-mov" (dst src) (setf (vm-slot vm dst) (vm-slot vm src)) next)
(defop "long-fmov" (dst src) (setf (vm-local vm dst) (vm-local vm src)) next)
(defop "push" (src)
  (let ((v (vm-slot vm src)))
    (set-frame-size vm (+ (frame-size vm) 1))
    (setf (vm-slot vm 0) v))
  next)
(defop "pop" (dst)
  (let ((v (vm-slot vm 0)))
    (set-frame-size vm (- (frame-size vm) 1))
    (setf (vm-slot vm dst) v))
  next)
(defop "drop" (n) (set-frame-size vm (- (frame-size vm) n)) next)
(defop "shuffle-down" (from to)
  (let ((n (frame-size vm)))
    (loop for i from from below n for j from to
	  do (setf (vm-local vm j) (vm-local vm i)))
    (set-frame-size vm (- n (- from to))))
  next)

;;; Constants

(defop "make-immediate" (dst bits) (setf (vm-slot vm dst) (scm-from-bits (ldb (byte 64 0) bits))) next)
(defop "make-short-immediate" (dst bits) (setf (vm-slot vm dst) (scm-from-bits bits)) next)
(defop "make-long-immediate" (dst bits) (setf (vm-slot vm dst) (scm-from-bits bits)) next)
(defop "make-long-long-immediate" (dst high low)
  (setf (vm-slot vm dst) (scm-from-bits (logior (ash high 32) low)))
  next)
(defop "make-non-immediate" (dst offset)
  (setf (vm-slot vm dst) (image-object image (* 4 (+ ip offset))))
  next)
(defop "static-ref" (dst offset)
  (setf (vm-slot vm dst) (image-scm image (* 4 (+ ip offset))))
  next)
(defop "static-set!" (src offset)
  (setf (image-cell image (* 4 (+ ip offset))) (vm-slot vm src))
  next)
(defop "static-patch!" (dst-offset src-offset)
  (setf (image-cell image (* 4 (+ ip dst-offset))) (* 4 (+ ip src-offset)))
  next)
(defop "load-label" (dst offset)
  (setf (vm-slot vm dst) (make-code-pointer image (+ ip offset)))
  next)
(defop "load-u64" (dst high low) (setf (vm-slot vm dst) (logior (ash high 32) low)) next)
(defop "load-s64" (dst high low) (setf (vm-slot vm dst) (logior (ash high 32) low)) next)
(defop "load-f64" (dst high low)
  (setf (vm-slot vm dst) (sb-kernel:make-double-float (sign-extend high 32) low))
  next)

;;; Instrumentation and interrupts: nothing to do here

(defop "instrument-entry" (data) next)
(defop "instrument-loop" (data) next)
(defop "handle-interrupts" () next)

;;; ------------------------------------------------------------------
;;; The heap, by words
;;;
;;; Compiled code reads and writes heap objects by word index.  For
;;; each type it touches, word N maps to the Lisp object's field.  An
;;; object allocate-words makes is a raw-object of words until word 0
;;; says what it is; then it becomes that type's Lisp object (in the
;;; frame's slots too).

(defstruct (raw-object (:constructor make-raw-object (words)))
  (words nil :type simple-vector))

(defun heap-header (x)
  "The first word of heap object X, as Guile lays it out."
  (typecase x
    (cons (logand (scm-bits (car x)) (lognot 1)))
    (simple-vector (logior (ash (length x) 8) 13))
    (vm-program (logior (ash (length (program-free x)) 16) 69))
    (gvariable 7)
    (string 21)
    (vm-stringbuf (logior 39 (if (wide-string-p* (vm-stringbuf-string x)) #x400 0)))
    (syntax-object 61)
    (raw-object (let ((w (svref (raw-object-words x) 0))) (if (integerp w) w 0)))
    (t (cond ((struct-p x) 1)
	     ((and (symbolp x) x (not (keywordp x))) 5)
	     ((keywordp x) 53)
	     ((typep x 'double-float) 535)
	     ((integerp x) 279)
	     ((rationalp x) 1047)
	     ((complexp x) 791)
	     ((typep x 'ps-r6rs::octets) 77)
	     ((functionp x) 69)
	     (t 0)))))

(defun replace-in-frame (vm old new)
  (let ((stack (vm-stack vm)))
    (loop for h from (+ (vm-fp vm) 1) to (vm-sp vm)
	  when (eq (svref stack h) old) do (setf (svref stack h) new))))

(defun realize (vm raw header)
  "RAW, whose word 0 is now HEADER, as the Lisp object it is."
  (let* ((words (raw-object-words raw))
	 (object
	   (case (logand header #x7f)
	     (69 (make-vm-program nil (ash header -16)))
	     (13 (make-array (ash header -8) :initial-element ps:false))
	     (7 (make-gvariable))
	     (t (vm-error "allocating objects of type ~D isn't supported yet" (logand header #x7f))))))
    ;; words already written (before the header) go to their fields
    (loop for i from 1 below (length words)
	  for w = (svref words i)
	  unless (eql w 0) do (heap-set object i w))
    (replace-in-frame vm raw object)
    object))

(defun heap-ref (x i)
  "Word I of X, as an SCM value or raw word."
  (typecase x
    (cons (ecase i (0 (car x)) (1 (cdr x))))
    (simple-vector (svref x (- i 1)))
    (vm-program (if (= i 1) (program-code x) (svref (program-free x) (- i 2))))
    (gvariable (gvariable-value x))
    (raw-object (svref (raw-object-words x) i))
    (syntax-object (ecase i
		     (1 (syntax-object-expression x)) (2 (syntax-object-wrap x))
		     (3 (syntax-object-module x)) (4 (syntax-object-sourcev x))))
    ;; a string: its buffer, start and length
    (string (ecase i (1 (make-vm-stringbuf x)) (2 0) (3 (length x))))
    (vm-stringbuf (ecase i (1 (length (vm-stringbuf-string x)))))
    ;; a bytevector: its length, and a pointer to its contents
    ((simple-array (unsigned-byte 8) (*))
     (ecase i (1 (length x)) (2 (make-vm-pointer x 0))))
    (t (if (struct-p x)
	   (svref (struct-slots-of x) (- i 1))
	   (vm-error "reading word ~D of ~S isn't supported yet" i x)))))

(defun heap-set (x i value)
  (typecase x
    (cons (ecase i (0 (setf (car x) value)) (1 (setf (cdr x) value))))
    (simple-vector (setf (svref x (- i 1)) value))
    (vm-program (if (= i 1)
		    (setf (program-code x) value)
		    (setf (svref (program-free x) (- i 2)) value)))
    (gvariable (setf (gvariable-value x) value))
    (raw-object (setf (svref (raw-object-words x) i) value))
    (t (if (struct-p x)
	   (setf (svref (struct-slots-of x) (- i 1)) value)
	   (vm-error "writing word ~D of ~S isn't supported yet" i x)))))

(defop "allocate-words/immediate" (dst count)
  (setf (vm-slot vm dst) (make-raw-object (make-array count :initial-element 0)))
  next)
(defop "allocate-words" (dst count)
  (setf (vm-slot vm dst) (make-raw-object (make-array (vm-slot vm count) :initial-element 0)))
  next)

(defun scm-store (vm obj i value)
  (if (and (raw-object-p obj) (= i 0))
      ;; a pair: its car comes first
      (let ((pair (cons value (let ((cdr (svref (raw-object-words obj) 1))) (if (eql cdr 0) ps:false cdr)))))
	(replace-in-frame vm obj pair))
      (heap-set obj i value)))

(defun word-store (vm obj i value)
  (if (and (raw-object-p obj) (= i 0))
      (realize vm obj value)
      (heap-set obj i value)))

(defun struct-from-raw (vm raw vtable)
  "RAW, whose first word now points to VTABLE, as a struct of VTABLE."
  (let* ((words (raw-object-words raw))
	 (slots (make-array (1- (length words)) :initial-element ps:false)))
    (loop for i from 1 below (length words)
	  for w = (svref words i)
	  unless (eql w 0) do (setf (svref slots (1- i)) w))
    (let* ((flags (vtable-flags vtable))
	   (struct (cond ((logtest flags +vtable-flag-vtable+) (make-vtable-from-slots vtable slots))
			 ((logtest flags +vtable-flag-applicable+) (make-applicable-struct vtable slots))
			 (t (%make-gstruct vtable slots)))))
      (replace-in-frame vm raw struct)
      struct)))

(defop "scm-ref/tag" (dst obj tag)
  (let ((x (vm-slot vm obj)))
    (setf (vm-slot vm dst)
	  (if (and (struct-p x) (= tag 1))
	      (struct-vtable-of x)
	      (vm-error "scm-ref/tag ~D of ~S isn't supported yet" tag x))))
  next)
(defop "scm-set!/tag" (obj tag val)
  (let ((x (vm-slot vm obj)))
    (cond ((and (raw-object-p x) (= tag 1)) (struct-from-raw vm x (vm-slot vm val)))
	  (t (vm-error "scm-set!/tag ~D of ~S isn't supported yet" tag x))))
  next)

(defop "scm-ref/immediate" (dst obj i) (setf (vm-slot vm dst) (heap-ref (vm-slot vm obj) i)) next)
(defop "scm-ref" (dst obj i) (setf (vm-slot vm dst) (heap-ref (vm-slot vm obj) (vm-slot vm i))) next)
(defop "scm-set!/immediate" (obj i val) (scm-store vm (vm-slot vm obj) i (vm-slot vm val)) next)
(defop "scm-set!" (obj i val) (scm-store vm (vm-slot vm obj) (vm-slot vm i) (vm-slot vm val)) next)
(defop "word-ref/immediate" (dst obj i)
  (let ((x (vm-slot vm obj)))
    (setf (vm-slot vm dst) (if (= i 0) (heap-header x) (heap-ref x i))))
  next)
(defop "word-ref" (dst obj i)
  (let ((x (vm-slot vm obj)) (i (vm-slot vm i)))
    (setf (vm-slot vm dst) (if (= i 0) (heap-header x) (heap-ref x i))))
  next)
(defop "word-set!/immediate" (obj i val) (word-store vm (vm-slot vm obj) i (vm-slot vm val)) next)
(defop "word-set!" (obj i val) (word-store vm (vm-slot vm obj) (vm-slot vm i) (vm-slot vm val)) next)

;;; ------------------------------------------------------------------
;;; Comparisons and branches

(defmacro compare (test result)
  `(progn (setf (vm-compare vm) (if ,test ,result :none)) next))

(defop "eq?" (a b) (compare (let ((x (vm-slot vm a)) (y (vm-slot vm b))) (or (eq x y) (and (numberp x) (eql x y)) (and (characterp x) (eql x y)))) :equal))
(defop "eq-immediate?" (a bits) (compare (= (scm-bits (vm-slot vm a)) (ldb (byte 64 0) bits)) :equal))
(defop "immediate-tag=?" (obj mask tag) (compare (= (logand (scm-bits (vm-slot vm obj)) mask) tag) :equal))
(defop "heap-tag=?" (obj mask tag) (compare (= (logand (heap-header (vm-slot vm obj)) mask) tag) :equal))
(defop "u64=?" (a b) (compare (= (vm-slot vm a) (vm-slot vm b)) :equal))
(defop "u64<?" (a b) (compare (< (vm-slot vm a) (vm-slot vm b)) :less))
(defop "s64<?" (a b) (compare (< (s64 (vm-slot vm a)) (s64 (vm-slot vm b))) :less))
(defop "s64-imm=?" (a imm) (compare (= (s64 (vm-slot vm a)) imm) :equal))
(defop "u64-imm<?" (a imm) (compare (< (vm-slot vm a) imm) :less))
(defop "imm-u64<?" (a imm) (compare (< imm (vm-slot vm a)) :less))
(defop "s64-imm<?" (a imm) (compare (< (s64 (vm-slot vm a)) imm) :less))
(defop "imm-s64<?" (a imm) (compare (< imm (s64 (vm-slot vm a))) :less))
(defop "f64=?" (a b) (compare (= (vm-slot vm a) (vm-slot vm b)) :equal))
(defop "f64<?" (a b)
  (let ((x (vm-slot vm a)) (y (vm-slot vm b)))
    (setf (vm-compare vm) (cond ((or (nan-p x) (nan-p y)) :invalid) ((< x y) :less) (t :none))))
  next)
(defop "=?" (a b) (compare (= (vm-slot vm a) (vm-slot vm b)) :equal))
(defop "heap-numbers-equal?" (a b) (compare (= (vm-slot vm a) (vm-slot vm b)) :equal))
(defop "<?" (a b)
  (let ((x (vm-slot vm a)) (y (vm-slot vm b)))
    (setf (vm-compare vm) (cond ((or (nan-p x) (nan-p y)) :invalid) ((< x y) :less) (t :none))))
  next)

(defmacro branch (test) `(if ,test (+ ip offset) next))
(defop "j" (offset) (+ ip offset))
(defop "jl" (offset) (branch (eq (vm-compare vm) :less)))
(defop "je" (offset) (branch (eq (vm-compare vm) :equal)))
(defop "jnl" (offset) (branch (not (eq (vm-compare vm) :less))))
(defop "jne" (offset) (branch (not (eq (vm-compare vm) :equal))))
(defop "jge" (offset) (branch (eq (vm-compare vm) :none)))
(defop "jnge" (offset) (branch (not (eq (vm-compare vm) :none))))
(defop "jtable" (idx count offsets)
  (let ((i (min (vm-slot vm idx) (1- count))))
    (+ ip (nth i offsets))))

;;; ------------------------------------------------------------------
;;; Intrinsics: the runtime's procedures, by Guile's intrinsic index

(defvar *intrinsics* nil "Index -> Lisp function.")
(defvar *intrinsic-definitions* (make-hash-table :test 'equal))

(defmacro defintrinsic (name lambda-list &body body)
  `(setf (gethash ,name *intrinsic-definitions*) (lambda ,lambda-list ,@body)))

(defun intrinsic (index)
  (unless *intrinsics*
    (let ((table (make-array 256 :initial-element nil)))
      (dolist (entry (bytecode-table "intrinsic-list"))
	(let ((name (ps:scheme-symbol-name (car entry))))
	  (setf (svref table (cdr entry))
		(or (gethash name *intrinsic-definitions*)
		    (lambda (&rest args)
		      (declare (ignore args))
		      (vm-error "VM intrinsic ~A isn't implemented" name))))))
      (setq *intrinsics* table)))
  (svref *intrinsics* index))

(defun root-procedure (name) (or (root-value name) (vm-error "no ~A" name)))

(defintrinsic "add" (a b) (funcall (root-procedure "+") a b))
(defintrinsic "add/immediate" (a b) (funcall (root-procedure "+") a b))
(defintrinsic "sub" (a b) (funcall (root-procedure "-") a b))
(defintrinsic "sub/immediate" (a b) (funcall (root-procedure "-") a b))
(defintrinsic "mul" (a b) (funcall (root-procedure "*") a b))
(defintrinsic "div" (a b) (funcall (root-procedure "/") a b))
(defintrinsic "quo" (a b) (funcall (root-procedure "quotient") a b))
(defintrinsic "rem" (a b) (funcall (root-procedure "remainder") a b))
(defintrinsic "mod" (a b) (funcall (root-procedure "modulo") a b))
(defintrinsic "string->symbol" (s) (funcall (root-procedure "string->symbol") s))
(defintrinsic "symbol->keyword" (s) (funcall (root-procedure "symbol->keyword") s))
(defintrinsic "current-module" () (funcall (root-procedure "current-module")))
(defintrinsic "define!" (module name)
  (funcall (root-procedure "module-ensure-local-variable!") module name))
(defintrinsic "module-variable" (module name)
  (funcall (root-procedure "module-variable") module name))

(defun interned (x)
  "X, or the symbol named X if it is a string: names in static data are
strings, since symbols can't be allocated statically."
  (if (stringp x) (ssym x) x))

(defun lookup-bound (module-name name public)
  "The bound variable NAME in the module named MODULE-NAME (its public
interface if PUBLIC)."
  (let* ((name (interned name))
	 (module-name (if (listp module-name) (mapcar #'interned module-name) module-name))
	 (module (funcall (root-procedure (if public "resolve-interface" "resolve-module")) module-name))
	 (v (funcall (root-procedure "module-variable") module name)))
    (unless (and (gvariable-p v) (not (eq (gvariable-value v) +unbound+)))
      (guile-error (ssym "unbound-variable") ps:false "Unbound variable: ~S" (list name) ps:false))
    v))

(defintrinsic "lookup-bound-public" (module-name name) (lookup-bound module-name name t))
(defintrinsic "lookup-bound-private" (module-name name) (lookup-bound module-name name nil))
(defintrinsic "lookup" (module name)
  (let ((v (funcall (root-procedure "module-variable") module name)))
    (if (gvariable-p v) v ps:false)))
(defintrinsic "lookup-bound" (module name)
  (let ((v (funcall (root-procedure "module-variable") module name)))
    (unless (and (gvariable-p v) (not (eq (gvariable-value v) +unbound+)))
      (guile-error (ssym "unbound-variable") ps:false "Unbound variable: ~S" (list name) ps:false))
    v))
(defintrinsic "resolve-module" (name public)
  (funcall (root-procedure (if (eql public 0) "resolve-module" "resolve-interface")) name))

(defop "call-scm<-scmn-scmn" (dst a b idx)
  (setf (vm-slot vm dst) (funcall (intrinsic idx)
				  (image-object image (* 4 (+ ip a)))
				  (image-object image (* 4 (+ ip b)))))
  next)

(defop "call-scm<-thread" (dst idx) (setf (vm-slot vm dst) (funcall (intrinsic idx))) next)
(defop "call-scm<-scm" (dst a idx) (setf (vm-slot vm dst) (funcall (intrinsic idx) (vm-slot vm a))) next)
(defop "call-scm<-scm-scm" (dst a b idx)
  (setf (vm-slot vm dst) (funcall (intrinsic idx) (vm-slot vm a) (vm-slot vm b)))
  next)
(defop "call-scm<-scm-uimm" (dst a b idx)
  (setf (vm-slot vm dst) (funcall (intrinsic idx) (vm-slot vm a) b))
  next)
(defop "call-scm-scm" (a b idx) (funcall (intrinsic idx) (vm-slot vm a) (vm-slot vm b)) next)
(defop "call-scm-scm-scm" (a b c idx)
  (funcall (intrinsic idx) (vm-slot vm a) (vm-slot vm b) (vm-slot vm c))
  next)
(defop "call-thread" (idx) (extent-step vm image next (funcall (intrinsic idx))))
(defop "call-thread-scm" (a idx) (extent-step vm image next (funcall (intrinsic idx) (vm-slot vm a))))

;;; ------------------------------------------------------------------
;;; Loading images

(defun dynamic-entry (elf tag)
  (cdr (assoc tag (elf-dynamic-entries elf))))

(defconstant +dt-init+ 12)
(defconstant +dt-guile-entry+ #x37146002)

(defun load-image (bytes)
  "Load the ELF image in BYTES: run its init thunk, and return its entry
thunk."
  (let* ((elf (parse-elf bytes))
	 (image (make-vm-image bytes)))
    (setf (vm-image-elf image) elf)
    (flet ((thunk (address) (make-vm-program (make-code-pointer image (floor address 4)) 0)))
      (let ((init (dynamic-entry elf +dt-init+))
	    (entry (or (dynamic-entry elf +dt-guile-entry+) (elf-error "ELF file has no entry thunk"))))
	(when init (funcall (thunk init)))
	(thunk entry)))))

;;; ------------------------------------------------------------------
;;; Unboxed values
;;;
;;; A u64 or s64 local holds its 64 bits as a nonnegative integer (the
;;; manual: "the bits for a u64 value are the same as those for an s64
;;; value"); an instruction that takes it as signed sign-extends it.  An
;;; f64 local holds a double-float.

(declaim (inline u64 s64))
(defun u64 (n) (ldb (byte 64 0) n))
(defun s64 (n) (sign-extend (ldb (byte 64 0) n) 64))

(macrolet ((binary (name op &optional immediate)
	     `(defop ,name (dst a b)
		(setf (vm-slot vm dst) (u64 (,op (vm-slot vm a) ,(if immediate 'b '(vm-slot vm b)))))
		next)))
  (binary "uadd" +) (binary "usub" -) (binary "umul" *)
  (binary "uadd/immediate" + t) (binary "usub/immediate" - t) (binary "umul/immediate" * t)
  (binary "ulogand" logand) (binary "ulogior" logior) (binary "ulogxor" logxor)
  (binary "ulogand/immediate" logand t))
(defop "ulogsub" (dst a b) (setf (vm-slot vm dst) (u64 (logandc2 (vm-slot vm a) (vm-slot vm b)))) next)
(macrolet ((shift (name kind &optional immediate)
	     `(defop ,name (dst a b)
		(let ((n (logand ,(if immediate 'b '(vm-slot vm b)) 63)) (x (vm-slot vm a)))
		  (setf (vm-slot vm dst)
			(u64 ,(ecase kind
				(:left '(ash x n))
				(:right '(ash x (- n)))
				(:signed-right '(ash (s64 x) (- n)))))))
		next)))
  (shift "ulsh" :left) (shift "ursh" :right) (shift "srsh" :signed-right)
  (shift "ulsh/immediate" :left t) (shift "ursh/immediate" :right t) (shift "srsh/immediate" :signed-right t))
(macrolet ((float-op (name op)
	     `(defop ,name (dst a b) (setf (vm-slot vm dst) (,op (vm-slot vm a) (vm-slot vm b))) next)))
  (float-op "fadd" +) (float-op "fsub" -) (float-op "fmul" *))
(defop "fdiv" (dst a b)
  (setf (vm-slot vm dst) (sb-int:with-float-traps-masked (:divide-by-zero :invalid :overflow)
			   (/ (vm-slot vm a) (vm-slot vm b))))
  next)
(defop "s64->f64" (dst src) (setf (vm-slot vm dst) (coerce (s64 (vm-slot vm src)) 'double-float)) next)

(defop "tag-char" (dst src) (setf (vm-slot vm dst) (code-char (vm-slot vm src))) next)
(defop "untag-char" (dst src) (setf (vm-slot vm dst) (char-code (vm-slot vm src))) next)
(defop "tag-fixnum" (dst src) (setf (vm-slot vm dst) (s64 (vm-slot vm src))) next)
(defop "untag-fixnum" (dst src) (setf (vm-slot vm dst) (u64 (vm-slot vm src))) next)

;;; ------------------------------------------------------------------
;;; Raw memory: pointers into a bytevector's or a string's contents
;;;
;;; A pointer is a base (a byte vector, or a string whose characters are
;;; its "bytes": one per character if narrow, four if wide) and a byte
;;; offset.

(defstruct (vm-pointer (:constructor make-vm-pointer (base offset)))
  base (offset 0))

(defstruct (vm-stringbuf (:constructor make-vm-stringbuf (string)))
  "A string's character buffer, as compiled code sees it."
  string)

(defun wide-string-p* (s) (some (lambda (c) (> (char-code c) 255)) s))

(defun pointer-bytes (p who)
  (let ((base (vm-pointer-base p)))
    (unless (typep base '(simple-array (unsigned-byte 8) (*)))
      (vm-error "~A of a pointer to ~S isn't supported" who base))
    base))

(defun raw-ref (p offset size signed)
  (let ((base (vm-pointer-base p)) (at (+ (vm-pointer-offset p) offset)))
    (if (stringp base)
	(char-code (char base (if (= size 4) (floor at 4) at)))
	(let ((n 0))
	  (loop for i from (1- size) downto 0 do (setq n (logior (ash n 8) (aref base (+ at i)))))
	  (if signed (u64 (sign-extend n (* 8 size))) n)))))

(defun raw-set (p offset size value)
  (let ((base (vm-pointer-base p)) (at (+ (vm-pointer-offset p) offset)))
    (if (stringp base)
	(setf (char base (if (= size 4) (floor at 4) at)) (code-char value))
	(dotimes (i size)
	  (setf (aref base (+ at i)) (ldb (byte 8 (* 8 i)) value))))))

(macrolet ((refs (&rest specs)
	     `(progn
		,@(loop for (name size signed) in specs
			collect `(defop ,name (dst ptr idx)
				   (setf (vm-slot vm dst) (raw-ref (vm-slot vm ptr) (vm-slot vm idx) ,size ,signed))
				   next)))))
  (refs ("u8-ref" 1 nil) ("u16-ref" 2 nil) ("u32-ref" 4 nil) ("u64-ref" 8 nil)
	("s8-ref" 1 t) ("s16-ref" 2 t) ("s32-ref" 4 t) ("s64-ref" 8 t)))
(macrolet ((sets (&rest specs)
	     `(progn
		,@(loop for (name size) in specs
			collect `(defop ,name (ptr idx val)
				   (raw-set (vm-slot vm ptr) (vm-slot vm idx) ,size (u64 (vm-slot vm val)))
				   next)))))
  (sets ("u8-set!" 1) ("u16-set!" 2) ("u32-set!" 4) ("u64-set!" 8)
	("s8-set!" 1) ("s16-set!" 2) ("s32-set!" 4) ("s64-set!" 8)))
(defop "f32-ref" (dst ptr idx)
  (setf (vm-slot vm dst) (coerce (sb-kernel:make-single-float (sign-extend (raw-ref (vm-slot vm ptr) (vm-slot vm idx) 4 nil) 32))
				 'double-float))
  next)
(defop "f64-ref" (dst ptr idx)
  (let ((bits (raw-ref (vm-slot vm ptr) (vm-slot vm idx) 8 nil)))
    (setf (vm-slot vm dst) (sb-kernel:make-double-float (sign-extend (ash bits -32) 32) (ldb (byte 32 0) bits))))
  next)
(defop "f32-set!" (ptr idx val)
  (raw-set (vm-slot vm ptr) (vm-slot vm idx) 4
	   (ldb (byte 32 0) (sb-kernel:single-float-bits (coerce (vm-slot vm val) 'single-float))))
  next)
(defop "f64-set!" (ptr idx val)
  (let ((d (coerce (vm-slot vm val) 'double-float)))
    (raw-set (vm-slot vm ptr) (vm-slot vm idx) 8
	     (logior (ash (ldb (byte 32 0) (sb-kernel:double-float-high-bits d)) 32)
		     (sb-kernel:double-float-low-bits d))))
  next)

(defop "pointer-ref/immediate" (dst obj i)
  (let ((x (vm-slot vm obj)))
    (setf (vm-slot vm dst)
	  (cond ((and (typep x 'ps-r6rs::octets) (= i 2)) (make-vm-pointer x 0))
		(t (vm-error "pointer-ref/immediate ~D of ~S isn't supported yet" i x)))))
  next)
(defop "tail-pointer-ref/immediate" (dst obj i)
  (let ((x (vm-slot vm obj)))
    (setf (vm-slot vm dst)
	  (cond ((and (vm-stringbuf-p x) (= i 2)) (make-vm-pointer (vm-stringbuf-string x) 0))
		(t (vm-error "tail-pointer-ref/immediate ~D of ~S isn't supported yet" i x)))))
  next)

;;; ------------------------------------------------------------------
;;; More calls to intrinsics

(defop "call-thread-scm-scm" (a b idx)
  (extent-step vm image next (funcall (intrinsic idx) (vm-slot vm a) (vm-slot vm b))))
(defop "call-scm-sz-u32" (a b c idx) (funcall (intrinsic idx) (vm-slot vm a) (vm-slot vm b) (vm-slot vm c)) next)
(defop "call-s64<-scm" (dst a idx) (setf (vm-slot vm dst) (u64 (funcall (intrinsic idx) (vm-slot vm a)))) next)
(defop "call-u64<-scm" (dst a idx) (setf (vm-slot vm dst) (u64 (funcall (intrinsic idx) (vm-slot vm a)))) next)
(defop "call-f64<-scm" (dst a idx) (setf (vm-slot vm dst) (funcall (intrinsic idx) (vm-slot vm a))) next)
(defop "call-scm<-u64" (dst a idx) (setf (vm-slot vm dst) (funcall (intrinsic idx) (vm-slot vm a))) next)
(defop "call-scm<-s64" (dst a idx) (setf (vm-slot vm dst) (funcall (intrinsic idx) (s64 (vm-slot vm a)))) next)
(defop "call-scm<-thread-scm" (dst a idx) (setf (vm-slot vm dst) (funcall (intrinsic idx) (vm-slot vm a))) next)
(defop "call-scm<-scm-u64" (dst a b idx)
  (setf (vm-slot vm dst) (funcall (intrinsic idx) (vm-slot vm a) (vm-slot vm b)))
  next)
(defop "call-f64<-f64" (dst a idx) (setf (vm-slot vm dst) (funcall (intrinsic idx) (vm-slot vm a))) next)
(defop "call-f64<-f64-f64" (dst a b idx)
  (setf (vm-slot vm dst) (funcall (intrinsic idx) (vm-slot vm a) (vm-slot vm b)))
  next)
(defop "call-scm-uimm-scm" (a b c idx) (funcall (intrinsic idx) (vm-slot vm a) b (vm-slot vm c)) next)

(defop "current-thread" (dst) (setf (vm-slot vm dst) sb-thread:*current-thread*) next)
(defop "allocate-pointerless-words/immediate" (dst count)
  (setf (vm-slot vm dst) (make-raw-object (make-array count :initial-element 0)))
  next)
(defop "allocate-pointerless-words" (dst count)
  (setf (vm-slot vm dst) (make-raw-object (make-array (vm-slot vm count) :initial-element 0)))
  next)

(defop "builtin-ref" (dst idx)
  (setf (vm-slot vm dst)
	(root-procedure (svref #("apply" "values" "abort-to-prompt" "call-with-values"
				 "call-with-current-continuation")
			       idx)))
  next)

;;; Errors

(defop "throw" (key args) (apply (root-procedure "throw") (vm-slot vm key) (vm-slot vm args)))
(defun throw-value (vm image ip offset value data)
  (let ((v (image-object image (* 4 (+ ip offset)))))
    (funcall (root-procedure "throw") (interned (svref v 0)) (svref v 1) (svref v 2) (list value)
	     (if data (list value) ps:false))
    (vm-error "throw returned")))
(defop "throw/value" (value offset) (throw-value vm image ip offset (vm-slot vm value) nil))
(defop "throw/value+data" (value offset) (throw-value vm image ip offset (vm-slot vm value) t))
(defop "unreachable" () (vm-error "unreachable instruction reached"))

;;; Arguments

(defop "expand-apply-argument" ()
  (let* ((n (frame-size vm)) (list (vm-local vm (1- n))))
    (set-frame-size vm (+ (1- n) (length list)))
    (loop for x in list for i from (1- n) do (setf (vm-local vm i) x)))
  next)

(defun positional-count (vm nreq)
  "NREQ, and the arguments after them that aren't keywords."
  (let ((n (frame-size vm)))
    (loop for i from nreq below n
	  while (not (keywordp (vm-local vm i)))
	  finally (return i))))

(defop "positional-arguments<=?" (nreq expected)
  (let ((npos (positional-count vm nreq)))
    (setf (vm-compare vm) (cond ((< npos expected) :less) ((= npos expected) :equal) (t :none))))
  next)

(defop "bind-kwargs" (nreq flags nreq-and-opt ntotal kw-offset)
  (let* ((allow-other-keys (logbitp 0 flags))
	 (has-rest (logbitp 1 flags))
	 (nargs (frame-size vm))
	 (npos (min (positional-count vm nreq) nreq-and-opt))
	 (rest (loop for i from npos below nargs collect (vm-local vm i)))
	 (alist (image-object image (* 4 (+ ip kw-offset)))))
    (set-frame-size vm (max ntotal npos) +unbound+)
    (loop for i from npos below ntotal do (setf (vm-local vm i) +unbound+))
    (set-frame-size vm ntotal)
    (loop with tail = rest
	  while tail
	  do (let ((k (car tail)))
	       (cond ((keywordp k)
		      (unless (consp (cdr tail))
			(guile-error (ssym "keyword-argument-error") ps:false "Keyword argument has no value"
				     '() (list k)))
		      (let ((entry (assoc k alist)))
			(cond (entry (setf (vm-local vm (cdr entry)) (cadr tail)))
			      ((not allow-other-keys)
			       (guile-error (ssym "keyword-argument-error") ps:false "Unrecognized keyword"
					    '() (list k)))))
		      (setq tail (cddr tail)))
		     (has-rest (setq tail (cdr tail)))
		     (t (guile-error (ssym "keyword-argument-error") ps:false "Invalid keyword"
				     '() (list k))))))
    (when has-rest (setf (vm-local vm nreq-and-opt) rest)))
  next)

;;; ------------------------------------------------------------------
;;; More intrinsics

(macrolet ((by-root (&rest specs)
	     `(progn
		,@(loop for (name root args) in specs
			collect `(defintrinsic ,name ,args (funcall (root-procedure ,root) ,@args))))))
  (by-root ("string->number" "string->number" (s)) ("class-of" "class-of" (x))
	   ("logand" "logand" (a b)) ("logior" "logior" (a b)) ("logxor" "logxor" (a b))
	   ("abs" "abs" (x)) ("sqrt" "sqrt" (x)) ("floor" "floor" (x)) ("ceiling" "ceiling" (x))
	   ("sin" "sin" (x)) ("cos" "cos" (x)) ("tan" "tan" (x)) ("asin" "asin" (x))
	   ("acos" "acos" (x)) ("atan" "atan" (x)) ("atan2" "atan" (a b))
	   ("inexact" "exact->inexact" (x)) ("symbol->string" "symbol->string" (x))
	   ("string->utf8" "string->utf8" (x)) ("utf8->string" "utf8->string" (x))
	   ("fluid-ref" "fluid-ref" (f)) ("fluid-set!" "fluid-set!" (f x))
	   ("$car" "car" (x)) ("$cdr" "cdr" (x)) ("$set-car!" "set-car!" (x v)) ("$set-cdr!" "set-cdr!" (x v))
	   ("$variable-ref" "variable-ref" (v)) ("$variable-set!" "variable-set!" (v x))
	   ("$vector-ref" "vector-ref" (v i)) ("$vector-set!" "vector-set!" (v i x))
	   ("$vector-ref/immediate" "vector-ref" (v i)) ("$vector-set!/immediate" "vector-set!" (v i x))
	   ("$struct-vtable" "struct-vtable" (s)) ("$struct-ref" "struct-ref" (s i))
	   ("$struct-set!" "struct-set!" (s i x)) ("$struct-ref/immediate" "struct-ref" (s i))
	   ("$struct-set!/immediate" "struct-set!" (s i x))))

(defintrinsic "logsub" (a b) (funcall (root-procedure "logand") a (funcall (root-procedure "lognot") b)))
(defintrinsic "lsh" (a n) (funcall (root-procedure "ash") a n))
(defintrinsic "rsh" (a n) (funcall (root-procedure "ash") a (- n)))
(defintrinsic "lsh/immediate" (a n) (funcall (root-procedure "ash") a n))
(defintrinsic "rsh/immediate" (a n) (funcall (root-procedure "ash") a (- n)))
(defintrinsic "$vector-length" (v) (funcall (root-procedure "vector-length") v))
(defintrinsic "string-utf8-length" (s) (length (sb-ext:string-to-octets s :external-format :utf-8)))
(defintrinsic "string-set!" (s i c) (funcall (root-procedure "string-set!") s i (code-char c)))
(defintrinsic "$allocate-struct" (vtable n)
  (make-struct* vtable (make-list n :initial-element ps:false)))
(defintrinsic "s64->f64" (x) (coerce (s64 x) 'double-float))
(defintrinsic "expand-stack" (&rest args) (declare (ignore args)) nil)

(defun conversion-error (who x)
  (guile-error (ssym "wrong-type-arg") who "Wrong type argument in position 1: ~S" (list x) (list x)))

(defintrinsic "scm->f64" (x)
  (if (realp x) (coerce x 'double-float) (conversion-error "scm->f64" x)))
(defintrinsic "scm->u64" (x)
  (if (and (integerp x) (<= 0 x (1- (ash 1 64)))) x (conversion-error "scm->u64" x)))
(defintrinsic "scm->u64/truncate" (x)
  (if (integerp x) (u64 x) (conversion-error "scm->u64/truncate" x)))
(defintrinsic "scm->s64" (x)
  (if (and (integerp x) (<= (- (ash 1 63)) x (1- (ash 1 63)))) x (conversion-error "scm->s64" x)))
(defintrinsic "u64->scm" (x) x)
(defintrinsic "s64->scm" (x) x)

(macrolet ((float-fns (&rest specs)
	     `(progn
		,@(loop for (name fn n) in specs
			collect `(defintrinsic ,name ,(if (= n 1) '(x) '(a b))
				   (sb-int:with-float-traps-masked (:invalid :divide-by-zero :overflow)
				     ,(if (= n 1)
					  `(let ((r (,fn x))) (if (realp r) (coerce r 'double-float) r))
					  `(coerce (,fn a b) 'double-float))))))))
  (float-fns ("fabs" abs 1) ("fsqrt" sqrt 1) ("ffloor" ffloor 1) ("fceiling" fceiling 1)
	     ("fsin" sin 1) ("fcos" cos 1) ("ftan" tan 1) ("fasin" asin 1) ("facos" acos 1)
	     ("fatan" atan 1) ("fatan2" atan 2)))

;;; ------------------------------------------------------------------
;;; Dynamic extents: prompts, dynamic-wind, fluid bindings, dynamic states
;;;
;;; Each is the runtime's own.  Its body runs in a nested run of the
;;; machine, inside the runtime's call-with-prompt, winder or fluid
;;; binding, until the instruction that ends it (the unwind, pop-fluid
;;; and pop-dynamic-state intrinsics); then the outer run goes on after
;;; that instruction.  An abort or a throw out of the body leaves it as
;;; it leaves any Lisp code, undoing the runtime's dynamic state.

(defstruct (extent-push (:constructor make-extent-push (establish)))
  "An intrinsic's request to run the rest of the body in an extent:
ESTABLISH takes the body, a thunk, and calls it in the extent."
  establish)

(defun extent-step (vm image next result)
  "Where to go after an intrinsic call that returned RESULT."
  (cond ((extent-push-p result)
	 (let ((exit (funcall (extent-push-establish result)
			      (lambda () (run-vm vm (make-code-pointer image next) t)))))
	   (extent-exit-location exit)))
	((eq result :pop-extent) (make-extent-exit (make-code-pointer image next)))
	(t next)))

(defintrinsic "wind" (winder unwinder)
  ;; the compiled code calls WINDER before, and UNWINDER after a normal
  ;; exit, itself; the runtime's winder calls UNWINDER on any other exit
  (make-extent-push
   (lambda (body)
     (let ((popped nil))
       (psx::winder-extent (cons winder (lambda () (unless popped (funcall unwinder))))
			   (lambda () (prog1 (funcall body) (setq popped t))))))))
(defintrinsic "unwind" () :pop-extent)
(defintrinsic "push-fluid" (fluid value)
  (make-extent-push (lambda (body) (call-with-fluid fluid value body))))
(defintrinsic "pop-fluid" () :pop-extent)
(defintrinsic "push-dynamic-state" (state)
  (make-extent-push (lambda (body) (funcall (root-procedure "with-dynamic-state") state body))))
(defintrinsic "pop-dynamic-state" () :pop-extent)

(defop "prompt" (tag escape-only proc-slot handler)
  (let ((saved-fp fp) (saved-sp sp)
	(handler-at (make-code-pointer image (+ ip handler))))
    (extent-exit-location
     (psx::call-with-prompt
      (vm-slot vm tag)
      (lambda () (run-vm vm (make-code-pointer image next) t))
      (lambda (k &rest values)
	;; as if returned from a call with the procedure in PROC-SLOT
	(setf (vm-fp vm) saved-fp (vm-sp vm) saved-sp)
	(let ((all (cons k values)))
	  (set-frame-size vm (+ proc-slot 1 (length all)))
	  (loop for v in all for i from (+ proc-slot 1) do (setf (vm-local vm i) v)))
	(make-extent-exit handler-at))))))

(defop "abort" ()
  ;; a tail call of abort-to-prompt: the tag in local 1, the values after
  (let* ((n (frame-size vm))
	 (args (loop for i from 1 below n collect (vm-local vm i)))
	 (values (multiple-value-list (apply (root-procedure "abort-to-prompt") args))))
    (set-frame-size vm (length values))
    (loop for v in values for i from 0 do (setf (vm-local vm i) v))
    (return-from-frame vm)))

(defun read-file-bytes (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence v in)
      v)))

(defun load-go-file (path)
  "The entry thunk of the compiled file at PATH, loaded."
  (load-image (read-file-bytes path)))

(defun installed-go-file (name)
  "The installed Guile's compiled file for module file NAME (ice-9/q)."
  (let ((dirs (root-value "%load-compiled-path")))
    (or (loop for dir in (if (listp dirs) dirs '())
	      for path = (format nil "~A/~A.go" dir name)
	      when (probe-file path) return path)
	(let ((libdir (string-trim '(#\Newline) (uiop:run-program '("guile" "-c" "(display (car %load-compiled-path))")
								  :output :string :ignore-error-status t))))
	  (format nil "~A/~A.go" libdir name)))))
