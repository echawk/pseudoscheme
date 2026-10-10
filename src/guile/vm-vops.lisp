; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-GUILE -*-

;;;; Guile's VM: SBCL VOPs for translated code (the :vop backend)
;;;;
;;;; The :vop backend translates bytecode as the :jit backend does
;;;; (vm-jit.lisp), with two differences:
;;;; - its regions are compiled at (SAFETY 0), so that the VM stack's
;;;;   slots are read and written unchecked (the translation computes the
;;;;   indexes);
;;;; - the add and sub intrinsics are VOPs written here, after the manner
;;;;   of Paul Khuong's "SBCL: the ultimate assembly code breadboard"
;;;;   (pvk.ca, 2014): arithmetic on the tagged words themselves, a tag
;;;;   test and an add that branches on overflow, giving NIL when the
;;;;   general case (Lisp's generic arithmetic) must run instead.
;;;;
;;;; SBCL on arm64 only; elsewhere :vop is :jit.

(in-package "PSEUDOSCHEME-GUILE")

(defvar *vm-vops* nil "True once the VOPs are defined (SBCL, arm64).")

#+(and sbcl arm64)
(progn
  (sb-ext:unlock-package "SB-VM")
  (sb-ext:unlock-package "SB-C")

  (sb-c:defknown (%vm-fixnum+ %vm-fixnum-) (t t) t
      (sb-c:flushable sb-c:movable)
    :overwrite-fndb-silently t)

  (macrolet ((tagged-op (name inst)
	       `(sb-c:define-vop (,name)
		  (:translate ,name)
		  (:policy :fast-safe)
		  ;; tagged words, fixnums or not: any-reg holds a fixnum as one
		  (:args (x :scs (sb-vm::descriptor-reg sb-vm::any-reg))
			 (y :scs (sb-vm::descriptor-reg sb-vm::any-reg)))
		  (:results (r :scs (sb-vm::descriptor-reg)))
		  (:temporary (:sc sb-vm::non-descriptor-reg) tmp)
		  (:generator 4
		    ;; both fixnums: their tag bits clear
		    (sb-assem:inst orr tmp x y)
		    (sb-assem:inst tst tmp sb-vm:fixnum-tag-mask)
		    (sb-assem:inst b :ne slow)
		    ;; tagged fixnums add (or subtract) as they are
		    (sb-assem:inst ,inst tmp x y)
		    (sb-assem:inst b :vs slow)
		    (sb-vm::move r tmp)
		    (sb-assem:inst b done)
		    slow
		    (sb-vm::move r sb-vm::null-tn)
		    done))))
    (tagged-op %vm-fixnum+ adds)
    (tagged-op %vm-fixnum- subs))

  ;; called, not open-coded (in code compiled before the VOPs, or not at
  ;; speed), they are the same operations
  (defun %vm-fixnum+ (x y) (%vm-fixnum+ x y))
  (defun %vm-fixnum- (x y) (%vm-fixnum- x y))

  (sb-ext:lock-package "SB-C")
  (sb-ext:lock-package "SB-VM")
  (setq *vm-vops* t))
