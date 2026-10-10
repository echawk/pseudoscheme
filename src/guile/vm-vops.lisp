; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-GUILE -*-

;;;; Guile's VM: SBCL VOPs for translated code (the :vop backend)
;;;;
;;;; The :vop backend translates bytecode as the :jit backend does
;;;; (vm-jit.lisp), with these differences:
;;;; - its regions are compiled at (SAFETY 0), so that the VM stack's
;;;;   slots are read and written unchecked (the translation computes the
;;;;   indexes);
;;;; - the stack and the stack pointer are kept in variables of the
;;;;   region's function, %STACK and %SP, which the instructions that only
;;;;   read and write slots use; after any other instruction (or a call
;;;;   out of Lisp code that might run the VM again) they are loaded again;
;;;; - a comparison and the conditional jump after it become one branch,
;;;;   and jumps in the region GO to their targets;
;;;; - most instructions are translated inline: by VOPs written here,
;;;;   after the manner of Paul Khuong's "SBCL: the ultimate assembly code
;;;;   breadboard" (pvk.ca, 2014) -- arithmetic on the tagged words
;;;;   themselves, a tag test and an operation that branches on overflow,
;;;;   giving NIL when the general case must run instead, and predicates
;;;;   that branch -- or by typed Lisp, which SBCL's own VOPs compile.
;;;;   tests/guile-vm/vops.lisp tests the VOPs.
;;;;
;;;; The VOPs are for SBCL on arm64 only (the feature :PSEUDOSCHEME-VOPS);
;;;; elsewhere :vop is :jit.

(in-package "PSEUDOSCHEME-GUILE")

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+(and sbcl arm64) (pushnew :pseudoscheme-vops *features*))

(defvar *vm-vops* nil "True once the VOPs are defined (SBCL, arm64).")

#+pseudoscheme-vops
(progn
  (sb-ext:unlock-package "SB-VM")
  (sb-ext:unlock-package "SB-C")

  (defmacro define-out-of-line (name args form)
    "NAME called, not open-coded, does FORM, its VOP's operation.  It is
compiled when loaded, once the VOP is defined (a file's VOPs aren't
there to compile the file's code, which would call itself)."
    `(setf (fdefinition ',name)
	   (compile nil '(lambda ,args (declare (optimize speed (safety 0))) ,form))))

  (defmacro define-tagged-vop (name args (&rest temps) &body body)
    "NAME, a function of ARGS (any objects) giving a fixnum or NIL.  BODY,
arm64 assembly, leaves the fixnum's tagged word in RES (or branches to
SLOW: NIL, the general case)."
    `(progn
       (sb-c:defknown ,name ,(mapcar (constantly t) args) t
	   (sb-c:flushable sb-c:movable)
	 :overwrite-fndb-silently t)
       (sb-c:define-vop (,name)
	 (:translate ,name)
	 (:policy :fast-safe)
	 ;; tagged words, fixnums or not: any-reg holds a fixnum as one
	 (:args ,@(loop for a in args collect `(,a :scs (sb-vm::descriptor-reg sb-vm::any-reg))))
	 (:results (r :scs (sb-vm::descriptor-reg)))
	 (:temporary (:sc sb-vm::non-descriptor-reg) res ,@temps)
	 (:generator 4
	   ,@body
	   (sb-vm::move r res)
	   (sb-assem:inst b done)
	   slow
	   (sb-vm::move r sb-vm::null-tn)
	   done))
       (define-out-of-line ,name ,args (,name ,@args))))

  (defmacro define-tagged-predicate (name args (&rest temps) condition &body body)
    "NAME, a predicate of ARGS (any objects): BODY, arm64 assembly, sets
the flags so that CONDITION holds when it is true."
    `(progn
       (sb-c:defknown ,name ,(mapcar (constantly t) args) boolean
	   (sb-c:flushable sb-c:movable)
	 :overwrite-fndb-silently t)
       (sb-c:define-vop (,name)
	 (:translate ,name)
	 (:policy :fast-safe)
	 (:args ,@(loop for a in args collect `(,a :scs (sb-vm::descriptor-reg sb-vm::any-reg))))
	 ,@(and temps `((:temporary (:sc sb-vm::non-descriptor-reg) ,@temps)))
	 (:conditional ,condition)
	 (:generator 2 ,@body))
       (define-out-of-line ,name ,args (if (,name ,@args) t nil))))

  ;; Fixnums are words whose low bit (the tag) is clear; an unboxed u64
  ;; that is a fixnum is never negative, so a word is one when its low
  ;; bit and its sign bit are clear.
  (defconstant +u64-tag-mask+ (logior (ash 1 63) sb-vm:fixnum-tag-mask))

  (macrolet ((both-fixnums (x y)
	       `(progn (sb-assem:inst orr res ,x ,y)
		       (sb-assem:inst tst res sb-vm:fixnum-tag-mask)
		       (sb-assem:inst b :ne slow)))
	     (both-u64 (x y)
	       `(progn (sb-assem:inst orr res ,x ,y)
		       (sb-assem:inst tst res +u64-tag-mask+)
		       (sb-assem:inst b :ne slow)))
	     (untag (dst src)
	       `(sb-assem:inst asr ,dst ,src sb-vm:n-fixnum-tag-bits))
	     (divisor (y)
	       ;; neither 0 nor -1 (whose quotient of the most negative
	       ;; fixnum isn't one)
	       `(progn (sb-assem:inst cbz ,y slow)
		       (sb-assem:inst cmn ,y (sb-vm:fixnumize 1))
		       (sb-assem:inst b :eq slow))))

    ;;; Scheme integers that are fixnums: the result, or NIL when it isn't
    ;;; one (or the operands aren't)

    (macrolet ((add-or-sub (name inst)
		 `(progn
		    (define-tagged-vop ,name (x y) ()
		      ;; tagged fixnums add (or subtract) as they are
		      (both-fixnums x y)
		      (sb-assem:inst ,inst res x y)
		      (sb-assem:inst b :vs slow))
		    ;; and with a small constant, whose tagged word is an
		    ;; immediate operand: one tag to test
		    (sb-c:define-vop (,(sb-int:symbolicate name "/C"))
		      (:translate ,name)
		      (:policy :fast-safe)
		      (:args (x :scs (sb-vm::descriptor-reg sb-vm::any-reg)))
		      (:info y)
		      (:arg-types * (:constant (integer 0 1023)))
		      (:results (r :scs (sb-vm::descriptor-reg)))
		      (:temporary (:sc sb-vm::non-descriptor-reg) res)
		      (:generator 2
			(sb-assem:inst tst x sb-vm:fixnum-tag-mask)
			(sb-assem:inst b :ne slow)
			(sb-assem:inst ,inst res x (sb-vm:fixnumize y))
			(sb-assem:inst b :vs slow)
			(sb-vm::move r res)
			(sb-assem:inst b done)
			slow
			(sb-vm::move r sb-vm::null-tn)
			done)))))
      (add-or-sub %vm-fixnum+ adds)
      (add-or-sub %vm-fixnum- subs))

    ;; one of them untagged times the other tagged is the product tagged,
    ;; and it overflows when the high word of the 128-bit product isn't
    ;; the low word's sign
    (define-tagged-vop %vm-fixnum* (x y) (tmp hi)
      (both-fixnums x y)
      (untag tmp x)
      (sb-assem:inst mul res tmp y)
      (sb-assem:inst smulh hi tmp y)
      (sb-assem:inst cmp hi (sb-vm::asr res 63))
      (sb-assem:inst b :ne slow))

    ;; tagged over tagged is the quotient untagged; tagged less the
    ;; quotient times tagged is the remainder tagged
    (define-tagged-vop %vm-fixnum-quotient (x y) ()
      (both-fixnums x y)
      (divisor y)
      (sb-assem:inst sdiv res x y)
      (sb-assem:inst lsl res res sb-vm:n-fixnum-tag-bits))
    (define-tagged-vop %vm-fixnum-remainder (x y) (q)
      (both-fixnums x y)
      (divisor y)
      (sb-assem:inst sdiv q x y)
      (sb-assem:inst msub res q y x))
    ;; the remainder, plus the divisor when their signs differ
    (define-tagged-vop %vm-fixnum-modulo (x y) (q)
      (both-fixnums x y)
      (divisor y)
      (sb-assem:inst sdiv q x y)
      (sb-assem:inst msub res q y x)
      (sb-assem:inst cbz res done-mod)
      (sb-assem:inst eor q res y)
      (sb-assem:inst tbz q 63 done-mod)
      (sb-assem:inst add res res y)
      done-mod)

    ;; the tag bits of both are clear, and stay so
    (define-tagged-vop %vm-fixnum-logand (x y) ()
      (both-fixnums x y)
      (sb-assem:inst and res x y))
    (define-tagged-vop %vm-fixnum-logior (x y) ()
      (both-fixnums x y))		; RES is X ORed with Y already
    (define-tagged-vop %vm-fixnum-logxor (x y) ()
      (both-fixnums x y)
      (sb-assem:inst eor res x y))
    (define-tagged-vop %vm-fixnum-logsub (x y) ()
      (both-fixnums x y)
      (sb-assem:inst bic res x y))

    ;; X shifted left by N, a fixnum from 0 to 63: overflow is shifting
    ;; back not giving X
    (define-tagged-vop %vm-fixnum-ash-left (x n) (count tmp)
      (both-fixnums x n)
      (untag count n)
      (sb-assem:inst cmp count 63)
      (sb-assem:inst b :hi slow)		; negative too
      (sb-assem:inst lsl res x count)
      (sb-assem:inst asr tmp res count)
      (sb-assem:inst cmp tmp x)
      (sb-assem:inst b :ne slow))
    ;; X shifted right by N, a nonnegative fixnum (63 or more as 63): the
    ;; tag bit cleared of what comes into it
    (define-tagged-vop %vm-fixnum-ash-right (x n) (count)
      (both-fixnums x n)
      (sb-assem:inst tbnz n 63 slow)
      (untag count n)
      (sb-assem:inst cmp count 63)
      (sb-assem:inst b :ls in-range)
      (sb-assem:inst mov count 63)
      in-range
      (sb-assem:inst asr res x count)
      (sb-assem:inst and res res (lognot sb-vm:fixnum-tag-mask)))

    ;;; Unboxed u64s that are fixnums (nonnegative): the result, or NIL when
    ;;; it isn't one (or the operands aren't)

    (define-tagged-vop %vm-u64+ (x y) ()
      (both-u64 x y)
      (sb-assem:inst adds res x y)
      (sb-assem:inst b :mi slow))
    (define-tagged-vop %vm-u64- (x y) ()
      (both-u64 x y)
      (sb-assem:inst subs res x y)
      (sb-assem:inst b :mi slow))	; it wraps around: a bignum
    (define-tagged-vop %vm-u64* (x y) (tmp hi)
      (both-u64 x y)
      (untag tmp x)
      (sb-assem:inst mul res tmp y)
      (sb-assem:inst umulh hi tmp y)
      (sb-assem:inst cbnz hi slow)
      (sb-assem:inst tbnz res 63 slow))
    (define-tagged-vop %vm-u64-logand (x y) ()
      (both-u64 x y)
      (sb-assem:inst and res x y))
    (define-tagged-vop %vm-u64-logior (x y) ()
      (both-u64 x y))
    (define-tagged-vop %vm-u64-logxor (x y) ()
      (both-u64 x y)
      (sb-assem:inst eor res x y))
    (define-tagged-vop %vm-u64-logsub (x y) ()
      (both-u64 x y)
      (sb-assem:inst bic res x y))
    ;; shifts by N modulo 64, as the instructions' are
    (define-tagged-vop %vm-u64-lsh (x n) (count tmp)
      (both-u64 x n)
      (sb-assem:inst ubfm count n sb-vm:n-fixnum-tag-bits (+ sb-vm:n-fixnum-tag-bits 5))
      (sb-assem:inst lsl res x count)
      (sb-assem:inst tbnz res 63 slow)
      (sb-assem:inst lsr tmp res count)
      (sb-assem:inst cmp tmp x)
      (sb-assem:inst b :ne slow))
    (define-tagged-vop %vm-u64-rsh (x n) (count)
      (both-u64 x n)
      (sb-assem:inst ubfm count n sb-vm:n-fixnum-tag-bits (+ sb-vm:n-fixnum-tag-bits 5))
      (sb-assem:inst lsr res x count)
      (sb-assem:inst and res res (lognot sb-vm:fixnum-tag-mask))))

  ;;; Predicates

  ;; both fixnums: their tag bits, ORed, clear
  (define-tagged-predicate %vm-fixnums-p (x y) (tmp) :eq
    (sb-assem:inst orr tmp x y)
    (sb-assem:inst tst tmp sb-vm:fixnum-tag-mask))
  ;; an unboxed u64 that is a fixnum; two of them
  (define-tagged-predicate %vm-u64-p (x) () :eq
    (sb-assem:inst tst x +u64-tag-mask+))
  (define-tagged-predicate %vm-u64s-p (x y) (tmp) :eq
    (sb-assem:inst orr tmp x y)
    (sb-assem:inst tst tmp +u64-tag-mask+))
  ;; a fixnum of Guile's, of 62 bits: a fixnum whose top two bits are the
  ;; same
  (define-tagged-predicate %vm-guile-fixnum-p (x) (tmp) :eq
    (sb-assem:inst eor tmp x (sb-vm::lsl x 1))
    (sb-assem:inst tst tmp +u64-tag-mask+))

  ;; X a simple vector and I a fixnum from 1 to its length: word I of X
  ;; is element I - 1
  (sb-c:defknown %vm-vector-word-p (t t) boolean (sb-c:flushable sb-c:movable)
    :overwrite-fndb-silently t)
  (sb-c:define-vop (%vm-vector-word-p)
    (:translate %vm-vector-word-p)
    (:policy :fast-safe)
    (:args (x :scs (sb-vm::descriptor-reg))
	   (i :scs (sb-vm::descriptor-reg sb-vm::any-reg)))
    (:temporary (:sc sb-vm::non-descriptor-reg) tmp index)
    (:conditional)
    (:info target not-p)
    (:generator 6
      (let ((no (if not-p target (sb-assem:gen-label))))
	(sb-assem:inst and tmp x sb-vm:lowtag-mask)
	(sb-assem:inst cmp tmp sb-vm:other-pointer-lowtag)
	(sb-assem:inst b :ne no)
	(sb-assem:inst ldrb tmp (sb-vm::@ x (- sb-vm:other-pointer-lowtag)))
	(sb-assem:inst cmp tmp sb-vm:simple-vector-widetag)
	(sb-assem:inst b :ne no)
	(sb-assem:inst tst i sb-vm:fixnum-tag-mask)
	(sb-assem:inst b :ne no)
	(sb-assem:inst ldr tmp (sb-vm::@ x (- (* sb-vm:vector-length-slot sb-vm:n-word-bytes)
						 sb-vm:other-pointer-lowtag)))
	;; I - 1 below the length, unsigned: I from 1 to the length
	(sb-assem:inst sub index i (sb-vm:fixnumize 1))
	(sb-assem:inst cmp index tmp)
	(cond (not-p (sb-assem:inst b :hs target))
	      (t (sb-assem:inst b :lo target)
		 (sb-assem:emit-label no))))))
  (define-out-of-line %vm-vector-word-p (x i) (if (%vm-vector-word-p x i) t nil))

  (sb-ext:lock-package "SB-C")
  (sb-ext:lock-package "SB-VM")
  (setq *vm-vops* t))

;;; ------------------------------------------------------------------
;;; The translation (used only when the VOPs are defined)

(defparameter +reload+ '(setq %stack (vm-stack vm) %sp (vm-sp vm))
  "Load the stack and the stack pointer again.")

(defun cached (form)
  "FORM, an instruction's, reading and writing slots through %STACK and %SP."
  `(macrolet ((vm-slot (vm j) (declare (ignore vm)) (list 'svref '%stack (list '- '%sp j))))
     ,form))

(defun out-call (form)
  "FORM, which may run the VM (growing its stack), then the reload."
  `(prog1 ,form ,+reload+))

(defparameter +cached-ops+
  '("mov" "long-mov" "make-immediate" "make-short-immediate" "make-long-immediate"
    "make-long-long-immediate" "instrument-entry" "instrument-loop" "handle-interrupts"
    "untag-fixnum" "tag-fixnum" "untag-char" "tag-char" "s64->f64"
    "uadd" "usub" "umul" "uadd/immediate" "usub/immediate" "umul/immediate"
    "ulogand" "ulogior" "ulogxor" "ulogand/immediate" "ulogsub"
    "ulsh" "ursh" "srsh" "ulsh/immediate" "ursh/immediate" "srsh/immediate"
    "fadd" "fsub" "fmul" "fdiv" "scm-ref" "scm-ref/immediate" "word-ref/immediate"
    "eq?" "eq-immediate?" "immediate-tag=?" "heap-tag=?" "u64=?" "u64<?" "s64<?"
    "s64-imm=?" "u64-imm<?" "imm-u64<?" "s64-imm<?" "imm-s64<?" "f64=?" "f64<?"
    "=?" "heap-numbers-equal?" "<?")
  "Instructions that change nothing but slots (and the comparison result),
calling nothing that might run the VM.")

(defvar *vop-translators* (make-hash-table :test 'equal)
  "Instruction name -> a function of its operands, IP and NEXT returning a
form (as a handler's: its value is where to go) better than the handler's,
in which VM-SLOT is the cached stack's, or NIL.")

(defmacro deftranslator (name lambda-list &body body)
  `(setf (gethash ,name *vop-translators*) (lambda ,lambda-list ,@body)))

(defvar *vop-comparisons* (make-hash-table :test 'equal)
  "Comparison instruction name -> a function of its operands and DEST
returning a form that branches: DEST, of :less, :equal, :none or
:invalid (the comparison's result), gives the form going where the
following conditional jump does then; and the instruction.")

(defmacro defcomparison (name (operands dest &optional (op (gensym))) &body body)
  `(setf (gethash ,name *vop-comparisons*)
	 (lambda (,operands ,dest ,op) (declare (ignorable ,op)) ,@body)))

(defun jump-taken-p (jump result)
  "Whether conditional jump JUMP is taken when the comparison gave RESULT."
  (let ((name (vm-op-name jump)))
    (cond ((string= name "jl") (eq result :less))
	  ((string= name "jnl") (not (eq result :less)))
	  ((string= name "je") (eq result :equal))
	  ((string= name "jne") (not (eq result :equal)))
	  ((string= name "jge") (eq result :none))
	  ((string= name "jnge") (not (eq result :none))))))

(defparameter +conditional-jumps+ '("jl" "jnl" "je" "jne" "jge" "jnge"))

(defun vop-instruction-form (instructions tags)
  "The first of INSTRUCTIONS, translated for the :vop backend: a form
going where it does (a statement of the region's TAGBODY)."
  (destructuring-bind (ip op operands next) (first instructions)
    (let ((name (vm-op-name op))
	  (following (second instructions)))
      (flet ((goto (target)
	       (let ((tag (gethash target tags)))
		 (if tag `(go ,tag) `(return-from region ,target)))))
	(cond
	  ;; a comparison and the conditional jump after it: one branch
	  ((and following (eql (first following) next) (gethash name *vop-comparisons*)
		(member (vm-op-name (second following)) +conditional-jumps+ :test #'string=))
	   (destructuring-bind (jip jop (offset) jnext) following
	     (cached
	      (funcall (gethash name *vop-comparisons*) operands
		       (lambda (result)
			 (goto (if (jump-taken-p jop result) (+ jip offset) jnext)))
		       op))))
	  ((gethash name *vop-control*)
	   (cached (funcall (gethash name *vop-control*) operands ip next #'goto)))
	  (t
	   (let ((special (let ((f (gethash name *vop-translators*)))
			    (and f (funcall f operands ip next)))))
	     (cond (special (transfer-form (cached special) next tags))
		   ((member name +cached-ops+ :test #'string=)
		    (transfer-form (cached (instruction-form op ip operands next)) next tags))
		   (t (transfer-form (instruction-form op ip operands next) next tags +reload+))))))))))

;;; Jumps, calls and returns

(defvar *vop-control* (make-hash-table :test 'equal)
  "Instruction name -> a function of its operands, IP, NEXT and GOTO (a
function of an IP giving a form going there) returning a form that goes
where the instruction does.")

(defmacro defcontrol (name lambda-list &body body)
  `(setf (gethash ,name *vop-control*) (lambda ,lambda-list ,@body)))

(defcontrol "j" (operands ip next goto)
  (declare (ignore next))
  (funcall goto (+ ip (first operands))))

(dolist (name +conditional-jumps+)
  (let ((name name))
    (defcontrol name (operands ip next goto)
      (let ((target (+ ip (first operands))) (op (gethash name *vm-ops-by-name*)))
	`(case (vm-compare vm)
	   ,@(loop for result in '(:less :equal :none)
		   collect `(,result ,(funcall goto (if (jump-taken-p op result) target next))))
	   (t ,(funcall goto (if (jump-taken-p op :invalid) target next))))))))

(defvar *region-image* nil
  "While a region of it is translated for the :vop backend, the image.")

(defun return-address (next)
  "A form whose value is the code pointer to NEXT, in the region's image."
  (if *region-image*
      `',(make-code-pointer *region-image* next)
      `(make-code-pointer image ,next)))

(defun frame-size-form (n &optional fill)
  "Set the frame's size to N locals, as SET-FRAME-SIZE, through %STACK and %SP."
  `(let ((new (+ (vm-fp vm) ,n)))
     (declare (fixnum new))
     (when (>= (+ new 1) (length %stack))
       (grow-stack vm (+ new 1))
       (setq %stack (vm-stack vm)))
     ,@(and fill `((when (> new %sp) (fill %stack ,fill :start (+ %sp 1) :end (+ new 1)))))
     (setf (vm-sp vm) new)
     (setq %sp new)))

(defcontrol "call-label" (operands ip next goto)
  (destructuring-bind (proc nlocals label) operands
    `(let ((new-fp (+ (vm-fp vm) ,proc)))
       (declare (fixnum new-fp))
       (when (>= (+ new-fp ,nlocals 1) (length %stack))
	 (grow-stack vm (+ new-fp ,nlocals 1))
	 (setq %stack (vm-stack vm)))
       (setf (svref %stack new-fp) nil
	     (svref %stack (- new-fp 1)) ,(return-address next)
	     (svref %stack (- new-fp 2)) ,proc)
       (setf (vm-fp vm) new-fp)
       (setf (vm-sp vm) (+ new-fp ,nlocals))
       (setq %sp (+ new-fp ,nlocals))
       ,(funcall goto (+ ip label)))))

(defcontrol "tail-call-label" (operands ip next goto)
  (declare (ignore next))
  (funcall goto (+ ip (first operands))))

;; a return to code in the image goes on from there
(defcontrol "return-values" (operands ip next goto)
  (declare (ignore operands ip next goto))
  `(let* ((fp (vm-fp vm)) (ra (svref %stack (- fp 1))))
     (declare (fixnum fp))
     (if (and (code-pointer-p ra) (eq (code-pointer-image ra) image))
	 (progn (setf (vm-fp vm) (- fp (the fixnum (svref %stack (- fp 2)))))
		(setq ip (code-pointer-word ra))
		(go dispatch))
	 (return-from region (return-from-frame vm)))))

(defcontrol "reset-frame" (operands ip next goto)
  (declare (ignore ip))
  `(progn ,(frame-size-form (first operands)) ,(funcall goto next)))

(defcontrol "alloc-frame" (operands ip next goto)
  (declare (ignore ip))
  `(progn ,(frame-size-form (first operands) '+unbound+) ,(funcall goto next)))

(defcontrol "assert-nargs-ee/locals" (operands ip next goto)
  (declare (ignore ip))
  (destructuring-bind (expected nlocals) operands
    `(progn (unless (= (- %sp (vm-fp vm)) ,expected)
	      (wrong-num-args vm))
	    ,(frame-size-form (+ expected nlocals) '+unbound+)
	    ,(funcall goto next))))

;;; Comparisons

(defun compare< (x y)
  "<? and f64<?'s result, in general."
  (cond ((< x y) :less)
	((or (nan-p x) (nan-p y)) :invalid)
	(t :none)))

(macrolet ((both (name test fast general)
	     ;; comparing two slots: FAST when both are fixnums
	     `(defcomparison ,name (operands dest)
		(destructuring-bind (a b) operands
		  `(let ((x (vm-slot vm ,a)) (y (vm-slot vm ,b)))
		     (if (%vm-fixnums-p x y)
			 (if (,',fast (sb-ext:truly-the fixnum x) (sb-ext:truly-the fixnum y))
			     ,(funcall dest ,test) ,(funcall dest :none))
			 (if ,',general ,(funcall dest ,test) ,(funcall dest :none)))))))
	   (immediate (name test fast general)
	     ;; a slot and an immediate
	     `(defcomparison ,name (operands dest)
		(destructuring-bind (a imm) operands
		  `(let ((x (vm-slot vm ,a)))
		     (if (typep x 'fixnum)
			 (if (let ((x (sb-ext:truly-the fixnum x))) ,,fast)
			     ,(funcall dest ,test) ,(funcall dest :none))
			 (if ,,general ,(funcall dest ,test) ,(funcall dest :none))))))))
  (both "=?" :equal eq (= x y))
  (both "u64=?" :equal eq (= x y))
  (both "u64<?" :less < (< x y))
  (both "s64<?" :less < (< (s64 x) (s64 y)))
  ;; an unboxed u64 is never negative; an s64 that is a fixnum is itself
  (immediate "s64-imm=?" :equal `(= x ,imm) `(= (s64 x) ,imm))
  (immediate "u64-imm<?" :less `(< x ,imm) `(< x ,imm))
  (immediate "imm-u64<?" :less `(< ,imm x) `(< ,imm x))
  (immediate "s64-imm<?" :less `(< x ,imm) `(< (s64 x) ,imm))
  (immediate "imm-s64<?" :less `(< ,imm x) `(< ,imm (s64 x))))

(defcomparison "<?" (operands dest)
  (destructuring-bind (a b) operands
    `(let ((x (vm-slot vm ,a)) (y (vm-slot vm ,b)))
       (if (%vm-fixnums-p x y)
	   (if (< (sb-ext:truly-the fixnum x) (sb-ext:truly-the fixnum y))
	       ,(funcall dest :less) ,(funcall dest :none))
	   (case (compare< x y)
	     (:less ,(funcall dest :less))
	     (:invalid ,(funcall dest :invalid))
	     (t ,(funcall dest :none)))))))

(defcomparison "f64<?" (operands dest)
  (destructuring-bind (a b) operands
    `(let ((x (vm-slot vm ,a)) (y (vm-slot vm ,b)))
       (if (and (typep x 'double-float) (typep y 'double-float))
	   (cond ((< x y) ,(funcall dest :less))
		 ((and (= x x) (= y y)) ,(funcall dest :none))
		 (t ,(funcall dest :invalid)))
	   (case (compare< x y)
	     (:less ,(funcall dest :less))
	     (:invalid ,(funcall dest :invalid))
	     (t ,(funcall dest :none)))))))

;;; Type tests: Guile's tags, as tests of the Lisp objects

(defparameter +immediate-constants+
  `((,+scm-false+ . ps:false) (,+scm-nil+ . *elisp-nil*) (,+scm-null+ . nil) (,+scm-true+ . ps:true)
    (,+scm-unspecified+ . *unspecified*) (,+scm-undefined+ . +unbound+) (,+scm-eof+ . ps:eof-object))
  "The SCM bits of Guile's immediate constants, and forms whose values are
their objects here.")

(defun immediate-tag-test (x mask tag)
  "A form true when X's SCM bits, masked by MASK, are TAG (as
immediate-tag=?), or NIL if there's none better than the handler's."
  (flet ((constants ()
	   (loop for (bits . form) in +immediate-constants+
		 when (= (logand bits mask) tag) collect `(eq ,x ,form))))
    (cond ((and (= mask 3) (= tag 2)) `(%vm-guile-fixnum-p ,x)) ; fixnum?
	  ((and (= mask 255) (= tag 12)) `(characterp ,x))	     ; char?
	  ((and (= mask 7) (= tag 0))				     ; heap-object?
	   `(not (or (%vm-guile-fixnum-p ,x) (characterp ,x)
		     ,@(loop for (nil . form) in +immediate-constants+ collect `(eq ,x ,form)))))
	  ;; the constants: their low byte is 4, which no fixnum's, char's
	  ;; or heap object's is
	  ((and (= (logand mask 15) 15) (= (logand tag 15) 4))
	   `(or ,@(constants))))))

(defun heap-tag-answer (type mask tag)
  "Whether heap-tag=? of MASK and TAG is true of every object of TYPE
(cons or simple-vector): :yes, :no or NIL (it depends)."
  (ecase type
    ;; a pair's first word is its car's bits, the low one clear
    (cons (cond ((logbitp 0 tag) :no)
		((= mask 1) :yes)))
    ;; a vector's is its length, shifted, and 13
    (simple-vector
     (cond ((zerop (logand mask (lognot 255))) (if (= (logand 13 mask) tag) :yes :no))
	   ((/= (logand tag 255) (logand 13 mask 255)) :no)))))

(defcomparison "immediate-tag=?" (operands dest op)
  (destructuring-bind (obj mask tag) operands
    (let ((test (immediate-tag-test 'x mask tag)))
      `(if ,(if test
		`(let ((x (vm-slot vm ,obj))) ,test)
		`(macrolet ((compare (test result) (declare (ignore result)) test))
		   ,(instruction-form op 0 operands 0)))
	   ,(funcall dest :equal) ,(funcall dest :none)))))

(defcomparison "heap-tag=?" (operands dest op)
  (destructuring-bind (obj mask tag) operands
    (flet ((answer (type)
	     (ecase (heap-tag-answer type mask tag)
	       (:yes (funcall dest :equal))
	       (:no (funcall dest :none))
	       ((nil) `(go general)))))
      `(let ((x (vm-slot vm ,obj)))
	 (tagbody
	    (cond ((consp x) ,(answer 'cons))
		  ((simple-vector-p x) ,(answer 'simple-vector)))
	  general
	    (if (= (logand (heap-header x) ,mask) ,tag) ,(funcall dest :equal) ,(funcall dest :none)))))))

(defcomparison "eq-immediate?" (operands dest op)
  (destructuring-bind (a bits) operands
    (let* ((bits (ldb (byte 64 0) bits))
	   (constant (assoc bits +immediate-constants+))
	   (literal (handler-case (list (scm-from-bits bits)) (error () nil))))
      `(if ,(cond (constant `(eq (vm-slot vm ,a) ,(cdr constant)))
		  ((and literal (literal-immediate-p (first literal)))
		   `(eq (vm-slot vm ,a) ',(first literal)))
		  (t `(macrolet ((compare (test result) (declare (ignore result)) test))
			,(instruction-form op 0 operands 0))))
	   ,(funcall dest :equal) ,(funcall dest :none)))))

;; the rest, by their handlers: true if equal
(macrolet ((by-handler (&rest names)
	     `(progn
		,@(loop for name in names
			collect `(defcomparison ,name (operands dest op)
				   `(if (macrolet ((compare (test result) (declare (ignore result)) test))
					  ,(instruction-form op 0 operands 0))
					,(funcall dest :equal) ,(funcall dest :none)))))))
  (by-handler "eq?" "f64=?" "heap-numbers-equal?"))

;;; Arithmetic: the VOPs, else the general case

(defparameter +scm-vops+
  '(("add" . %vm-fixnum+) ("sub" . %vm-fixnum-) ("mul" . %vm-fixnum*)
    ("quo" . %vm-fixnum-quotient) ("rem" . %vm-fixnum-remainder) ("mod" . %vm-fixnum-modulo)
    ("logand" . %vm-fixnum-logand) ("logior" . %vm-fixnum-logior)
    ("logxor" . %vm-fixnum-logxor) ("logsub" . %vm-fixnum-logsub)
    ("add/immediate" . %vm-fixnum+) ("sub/immediate" . %vm-fixnum-)
    ("lsh" . %vm-fixnum-ash-left) ("rsh" . %vm-fixnum-ash-right)
    ("lsh/immediate" . %vm-fixnum-ash-left) ("rsh/immediate" . %vm-fixnum-ash-right))
  "Intrinsics of two Scheme values (or a value and an unsigned count) and
the VOPs that are their fixnum cases.")

(defun intrinsic-translator (immediate)
  "A translator of intrinsic calls of a result and two operands, the
second IMMEDIATE (a constant) or not: the VOP, the intrinsic when it
gives NIL."
  (lambda (operands ip next)
    (declare (ignore ip))
    (destructuring-bind (dst a b idx) operands
      (let ((vop (cdr (assoc (intrinsic-name idx) +scm-vops+ :test #'equal))))
	(when (and vop immediate (eq vop '%vm-fixnum-ash-left) (> b 63)) (setq vop nil))
	(and vop
	     `(let* ((x (vm-slot vm ,a))
		     (y ,(cond ((not immediate) `(vm-slot vm ,b))
			       ((eq vop '%vm-fixnum-ash-right) (min b 63))
			       (t b)))
		     (r (or (,vop x y) ,(out-call `(funcall (the function (intrinsic ,idx)) x ,(if immediate b 'y))))))
		(setf (vm-slot vm ,dst) r)
		,next))))))

(setf (gethash "call-scm<-scm-scm" *vop-translators*) (intrinsic-translator nil)
      (gethash "call-scm<-scm-u64" *vop-translators*) (intrinsic-translator nil)
      (gethash "call-scm<-scm-uimm" *vop-translators*) (intrinsic-translator t))

;; unboxed u64s: those that are fixnums by the VOPs, else as the handlers
(defun unsigned-translator (name vop immediate general)
  "Translate instruction NAME, of operands DST, A and B (IMMEDIATE, or a
slot), by VOP, or, when it gives NIL, GENERAL: a form of X and Y."
  (setf (gethash name *vop-translators*)
	(lambda (operands ip next)
	  (declare (ignore ip))
	  (destructuring-bind (dst a b) operands
	    `(let ((x (vm-slot vm ,a)) (y ,(if immediate b `(vm-slot vm ,b))))
	       (setf (vm-slot vm ,dst) (or (,vop x y) ,general))
	       ,next)))))

(loop for (name vop general) in '(("uadd" %vm-u64+ (u64 (+ x y))) ("usub" %vm-u64- (u64 (- x y)))
				  ("umul" %vm-u64* (u64 (* x y))) ("ulogand" %vm-u64-logand (u64 (logand x y)))
				  ("ulogior" %vm-u64-logior (u64 (logior x y)))
				  ("ulogxor" %vm-u64-logxor (u64 (logxor x y)))
				  ("ulogsub" %vm-u64-logsub (u64 (logandc2 x y)))
				  ("ulsh" %vm-u64-lsh (u64 (ash x (logand y 63))))
				  ("ursh" %vm-u64-rsh (u64 (ash x (- (logand y 63)))))
				  ;; a u64 that is a fixnum is its s64
				  ("srsh" %vm-u64-rsh (u64 (ash (s64 x) (- (logand y 63))))))
      do (unsigned-translator name vop nil general)
	 (let ((immediate (format nil "~A/immediate" name)))
	   (when (gethash immediate *vm-op-sources*)
	     (unsigned-translator immediate vop t general))))

;;; Conversions

(defun conversion-translator (name type fast general &optional intrinsics)
  "Translate NAME, of operands DST and SRC (and an intrinsic's index), when
the intrinsic is one of INTRINSICS (if given): FAST when X, the source, is
of TYPE, else GENERAL, a function of the index giving a form of X."
  (setf (gethash name *vop-translators*)
	(lambda (operands ip next)
	  (declare (ignore ip))
	  (destructuring-bind (dst src &optional idx) operands
	    (and (or (null intrinsics) (member (intrinsic-name idx) intrinsics :test #'equal))
		 `(let ((x (vm-slot vm ,src)))
		    (setf (vm-slot vm ,dst)
			  (if ,(if (eq type :u64) '(%vm-u64-p x) `(typep x ',type))
			      (let ((x (sb-ext:truly-the ,(if (eq type :u64) 'fixnum type) x))) ,fast)
			      ,(funcall general idx)))
		    ,next))))))

(flet ((call-intrinsic (idx) `(u64 (funcall (the function (intrinsic ,idx)) x))))
  ;; a u64 or s64 holds its bits, nonnegative: one that is a fixnum is its
  ;; own integer, and a fixnum that isn't negative its own u64 or s64
  (conversion-translator "untag-fixnum" :u64 'x (constantly '(u64 x)))
  (conversion-translator "tag-fixnum" 'fixnum 'x (constantly '(s64 x)))
  (conversion-translator "call-u64<-scm" :u64 'x #'call-intrinsic '("scm->u64" "scm->u64/truncate"))
  (conversion-translator "call-s64<-scm" :u64 'x #'call-intrinsic '("scm->s64"))
  (conversion-translator "call-scm<-u64" :u64 'x (constantly 'x) '("u64->scm"))
  (conversion-translator "call-scm<-s64" 'fixnum 'x (constantly '(s64 x)) '("s64->scm"))
  (conversion-translator "untag-char" 'character '(char-code x) (constantly '(char-code x)))
  (conversion-translator "tag-char" '(mod #.char-code-limit) '(code-char x) (constantly '(code-char x)))
  (conversion-translator "s64->f64" 'fixnum '(coerce x 'double-float)
			 (constantly '(coerce (s64 x) 'double-float))))

;; flonums: unboxed, as SBCL's VOPs do them, and boxed again
(dolist (name '("fadd" "fsub" "fmul"))
  (let ((op (cdr (assoc name '(("fadd" . +) ("fsub" . -) ("fmul" . *)) :test #'equal))))
    (setf (gethash name *vop-translators*)
	  (lambda (operands ip next)
	    (declare (ignore ip))
	    (destructuring-bind (dst a b) operands
	      `(let ((x (vm-slot vm ,a)) (y (vm-slot vm ,b)))
		 (setf (vm-slot vm ,dst)
		       (if (and (typep x 'double-float) (typep y 'double-float))
			   (,op (sb-ext:truly-the double-float x) (sb-ext:truly-the double-float y))
			   (,op x y)))
		 ,next))))))

;;; Memory: a vector's elements, a pair's car and cdr

(defun word-ref-form (x i)
  "Word I (a form) of X, as HEAP-REF."
  (cond ((eql i 0) `(if (consp ,x) (car (sb-ext:truly-the cons ,x)) (heap-ref ,x 0)))
	(t `(cond ,@(and (eql i 1) `(((consp ,x) (cdr (sb-ext:truly-the cons ,x)))))
		  ((%vm-vector-word-p ,x ,i)
		   (svref (sb-ext:truly-the simple-vector ,x) (1- (sb-ext:truly-the fixnum ,i))))
		  (t (heap-ref ,x ,i))))))

(defun word-set-form (x i value)
  "Set word I (a form) of X to VALUE, as SCM-STORE."
  (cond ((eql i 0) `(if (consp ,x) (setf (car (sb-ext:truly-the cons ,x)) ,value) (scm-store vm ,x 0 ,value)))
	(t `(cond ,@(and (eql i 1) `(((consp ,x) (setf (cdr (sb-ext:truly-the cons ,x)) ,value))))
		  ((%vm-vector-word-p ,x ,i)
		   (setf (svref (sb-ext:truly-the simple-vector ,x) (1- (sb-ext:truly-the fixnum ,i))) ,value))
		  (t (scm-store vm ,x ,i ,value))))))

(deftranslator "scm-ref" (operands ip next)
  (declare (ignore ip))
  (destructuring-bind (dst obj i) operands
    `(let ((x (vm-slot vm ,obj)) (i (vm-slot vm ,i)))
       (setf (vm-slot vm ,dst) ,(word-ref-form 'x 'i))
       ,next)))

(deftranslator "scm-ref/immediate" (operands ip next)
  (declare (ignore ip))
  (destructuring-bind (dst obj i) operands
    `(let ((x (vm-slot vm ,obj)))
       (setf (vm-slot vm ,dst) ,(word-ref-form 'x i))
       ,next)))

(deftranslator "scm-set!" (operands ip next)
  (declare (ignore ip))
  (destructuring-bind (obj i val) operands
    `(let ((x (vm-slot vm ,obj)) (i (vm-slot vm ,i)) (v (vm-slot vm ,val)))
       ,(word-set-form 'x 'i 'v)
       ,next)))

(deftranslator "scm-set!/immediate" (operands ip next)
  (declare (ignore ip))
  (destructuring-bind (obj i val) operands
    `(let ((x (vm-slot vm ,obj)) (v (vm-slot vm ,val)))
       ,(word-set-form 'x i 'v)
       ,next)))

;; a vector's first word: its length, shifted, and its tag
(deftranslator "word-ref/immediate" (operands ip next)
  (declare (ignore ip))
  (destructuring-bind (dst obj i) operands
    (and (eql i 0)
	 `(let ((x (vm-slot vm ,obj)))
	    (setf (vm-slot vm ,dst)
		  (if (simple-vector-p x) (logior (ash (length x) 8) 13) (heap-header x)))
	    ,next))))

;;; Raw memory: a bytevector's bytes, by SBCL's SAP-REF VOPs (one load or
;;; store, little-endian as the machine is), within the vector

(defun raw-access-form (ptr idx size access general)
  "A form doing ACCESS, a function of a SAP form and an offset form giving
a form, at byte IDX (a slot's) after pointer PTR's (a slot's) when it is
into a simple octet vector and in it; else GENERAL."
  `(let* ((p (vm-slot vm ,ptr)) (i (vm-slot vm ,idx))
	  (base (and (vm-pointer-p p) (vm-pointer-base p))))
     (if (and (typep base '(simple-array (unsigned-byte 8) (*))) (typep i 'fixnum)
	      (typep (vm-pointer-offset p) 'fixnum))
	 (let ((at (+ (sb-ext:truly-the fixnum (vm-pointer-offset p)) (sb-ext:truly-the fixnum i))))
	   (declare (fixnum at))
	   (if (<= 0 at (- (length base) ,size))
	       (sb-sys:with-pinned-objects (base)
		 ,(funcall access '(sb-sys:vector-sap base) 'at))
	       ,general))
	 ,general)))

(loop for (name size ref) in '(("u8-ref" 1 sb-sys:sap-ref-8) ("u16-ref" 2 sb-sys:sap-ref-16)
			       ("u32-ref" 4 sb-sys:sap-ref-32) ("u64-ref" 8 sb-sys:sap-ref-64)
			       ("s8-ref" 1 sb-sys:signed-sap-ref-8) ("s16-ref" 2 sb-sys:signed-sap-ref-16)
			       ("s32-ref" 4 sb-sys:signed-sap-ref-32) ("s64-ref" 8 sb-sys:signed-sap-ref-64)
			       ("f32-ref" 4 sb-sys:sap-ref-single) ("f64-ref" 8 sb-sys:sap-ref-double))
      do (let ((size size) (ref ref) (name name))
	   (setf (gethash name *vop-translators*)
		 (lambda (operands ip next)
		   (destructuring-bind (dst ptr idx) operands
		     `(progn
			(setf (vm-slot vm ,dst)
			      ,(raw-access-form
				ptr idx size
				(lambda (sap at)
				  (let ((value `(,ref ,sap ,at)))
				    (cond ((eq ref 'sb-sys:sap-ref-single) `(coerce ,value 'double-float))
					  ((char= (char name 0) #\s) `(u64 ,value))
					  (t value))))
				;; the handler's
				`(progn ,(instruction-form (gethash name *vm-ops-by-name*) ip operands next)
					(vm-slot vm ,dst))))
			,next))))))

(loop for (name size ref) in '(("u8-set!" 1 sb-sys:sap-ref-8) ("u16-set!" 2 sb-sys:sap-ref-16)
			       ("u32-set!" 4 sb-sys:sap-ref-32) ("u64-set!" 8 sb-sys:sap-ref-64)
			       ("s8-set!" 1 sb-sys:sap-ref-8) ("s16-set!" 2 sb-sys:sap-ref-16)
			       ("s32-set!" 4 sb-sys:sap-ref-32) ("s64-set!" 8 sb-sys:sap-ref-64)
			       ("f32-set!" 4 sb-sys:sap-ref-single) ("f64-set!" 8 sb-sys:sap-ref-double))
      do (let ((size size) (ref ref) (name name))
	   (setf (gethash name *vop-translators*)
		 (lambda (operands ip next)
		   (destructuring-bind (ptr idx val) operands
		     (let ((general `(progn ,(instruction-form (gethash name *vm-ops-by-name*) ip operands next) nil))
			   (float (member ref '(sb-sys:sap-ref-single sb-sys:sap-ref-double))))
		       `(let ((v (vm-slot vm ,val)))
			  (if ,(if float '(typep v 'double-float) '(typep v 'fixnum))
			      ,(raw-access-form
				ptr idx size
				(lambda (sap at)
				  `(setf (,ref ,sap ,at)
					 ,(cond ((eq ref 'sb-sys:sap-ref-single)
						 '(coerce (sb-ext:truly-the double-float v) 'single-float))
						(float '(sb-ext:truly-the double-float v))
						;; the low bits of the u64 (or s64) the fixnum is
						(t `(ldb (byte ,(* 8 size) 0) (sb-ext:truly-the fixnum v))))))
				general)
			      ,general)
			  ,next)))))))

;;; Constants, decoded when translating

(macrolet ((constant (name lambda-list form)
	     `(deftranslator ,name (operands ip next)
		(declare (ignorable ip))
		(destructuring-bind ,lambda-list operands
		  (let ((value ,form))
		    (and value `(progn (setf (vm-slot vm ,dst) ,(first value)) ,next)))))))
  (constant "load-u64" (dst high low) (list (logior (ash high 32) low)))
  (constant "load-s64" (dst high low) (list (logior (ash high 32) low)))
  (constant "load-f64" (dst high low) (list (sb-kernel:make-double-float (sign-extend high 32) low)))
  (constant "load-label" (dst offset) (list (return-address (+ ip offset))))
  ;; an object of the image, if it has been decoded (running the image's
  ;; code may yet patch it before)
  (constant "make-non-immediate" (dst offset)
	    (and *region-image*
		 (multiple-value-bind (object found)
		     (gethash (* 4 (+ ip offset)) (vm-image-objects *region-image*))
		   (and found (list `',object))))))

;;; Jump tables and the frame

(defcontrol "jtable" (operands ip next goto)
  (declare (ignore next))
  (destructuring-bind (idx count offsets) operands
    `(let ((i (vm-slot vm ,idx)))
       (case (if (and (typep i 'fixnum) (< -1 i ,count)) i ,(1- count))
	 ,@(loop for offset in offsets for j from 0
		 collect `(,j ,(funcall goto (+ ip offset))))))))

(defun frame-translator (name function)
  "Translate NAME by FUNCTION, of its operands, giving a form."
  (setf (gethash name *vop-translators*)
	(lambda (operands ip next)
	  (declare (ignore ip))
	  `(progn ,(apply function operands) ,next))))

(defmacro defframe (name lambda-list &body body)
  `(frame-translator ,name (lambda ,lambda-list ,@body)))

(defframe "drop" (n) (frame-size-form `(- %sp (vm-fp vm) ,n)))
(defframe "push" (src)
  `(let ((v (vm-slot vm ,src)))
     ,(frame-size-form '(+ (- %sp (vm-fp vm)) 1))
     (setf (vm-slot vm 0) v)))
(defframe "pop" (dst)
  `(let ((v (vm-slot vm 0)))
     ,(frame-size-form '(- %sp (vm-fp vm) 1))
     (setf (vm-slot vm ,dst) v)))
(defframe "receive" (dst proc nlocals)
  `(progn
     (when (< (- %sp (+ (vm-fp vm) ,proc)) 1)
       (values-error "vm-error" "Zero values returned to single-valued continuation"))
     (setf (vm-local vm ,dst) (vm-local vm ,proc))
     ,(frame-size-form nlocals)))
(defframe "bind-optionals" (n)
  `(when (< (- %sp (vm-fp vm)) ,n) ,(frame-size-form n '+unbound+)))

;; instructions that read the frame, or change only the heap, the image
;; or the comparison result, calling nothing that runs the VM: their
;; handlers, reading slots through %STACK and %SP
(setq +cached-ops+
      (union +cached-ops+
	     '("receive-values" "assert-nargs-ee" "assert-nargs-ge" "assert-nargs-le" "arguments<=?"
	       "positional-arguments<=?" "allocate-words" "allocate-words/immediate"
	       "allocate-pointerless-words" "allocate-pointerless-words/immediate"
	       "word-ref" "word-set!" "word-set!/immediate" "scm-ref/tag"
	       "atomic-scm-ref/immediate" "atomic-scm-set!/immediate" "atomic-scm-swap!/immediate"
	       "atomic-scm-compare-and-swap!/immediate" "current-thread" "builtin-ref"
	       "static-ref" "static-set!" "static-patch!" "long-fmov"
	       "pointer-ref/immediate" "tail-pointer-ref/immediate")
	     :test #'string=))

(conversion-translator "call-f64<-scm" 'double-float 'x
		       (lambda (idx) `(funcall (the function (intrinsic ,idx)) x)) '("scm->f64"))
