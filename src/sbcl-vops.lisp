; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SB-VM -*-

;;;; Faster code from SBCL for what the translator emits: the optional
;;;; layer of docs/optimizations.md ("VOPs"), loaded by bin/quasischeme's
;;;; build (system quasischeme/fast) after everything else, so that it
;;;; changes only the code compiled at run time -- programs, libraries,
;;;; --racket's linklets -- never the runtime's own or a bootstrap's.
;;;; QUASISCHEME_FAST=0 turns it off for a process.  Code compiled with
;;;; and without it is as correct, and goes together.
;;;;
;;;; For SBCL 2.6.9 on arm64 (sbcl/sbcl at tag sbcl-2.6.9), whose
;;;; internals it uses; elsewhere it does nothing.
;;;;
;;;; A small stack vector, inline.  A frame (src/continuations.lisp,
;;;; %SITE) is (vector ...) declared DYNAMIC-EXTENT, which SBCL compiles
;;;; on arm64 to a call of the assembly routine ALLOCATE-VECTOR-ON-STACK
;;;; (src/assembly/arm64/array.lisp), whatever the length; with a
;;;; constant length of at most 60 words (in one page, so the guard page
;;;; catches an overflow, as MAKE-VECTOR-CHECK-OVERFLOW-P reasons) it is
;;;; the same instructions inline: the stack pointer bumped, the header
;;;; and length stored and the elements zeroed (as the routine does,
;;;; before they are stored), without the call or the loop.  Lattice in
;;;; Racket mode spent an eighth of its time in the routine.

(in-package #+sbcl "SB-VM" #-sbcl "CL-USER")

#+(and sbcl arm64)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (string= (lisp-implementation-version) "2.6.9")
    (pushnew :quasischeme-sbcl-vops *features*)))

(defvar cl-user::*quasischeme-fast* :unknown
  "Whether this layer is on: QUASISCHEME_FAST=0 turns it off.  Read once
per process.")

#+sbcl (pushnew (lambda () (setq cl-user::*quasischeme-fast* :unknown)) sb-ext:*init-hooks*)

(defun quasischeme-fast-p ()
  (when (eq cl-user::*quasischeme-fast* :unknown)
    (setq cl-user::*quasischeme-fast* (not (equal (uiop:getenv "QUASISCHEME_FAST") "0"))))
  cl-user::*quasischeme-fast*)

#+quasischeme-sbcl-vops
(progn
  (sb-ext:unlock-package "SB-VM")
  (sb-ext:unlock-package "SB-C")

  (define-vop (qs-allocate-small-vector-on-stack)
    (:info type length words)
    (:arg-types (:constant fixnum) (:constant fixnum) (:constant fixnum))
    (:results (result :scs (descriptor-reg)))
    (:temporary (:sc non-descriptor-reg) header)
    (:temporary (:sc any-reg) len)
    (:generator 2
      (let ((bytes (logandc2 (+ (* (+ words vector-data-offset) n-word-bytes) lowtag-mask) lowtag-mask)))
	(allocation nil bytes other-pointer-lowtag result :stack-allocate-p t)
	(load-immediate-word header type)
	(load-immediate-word len (fixnumize length))
	(inst stp header len (@ tmp-tn))
	(loop for off from (* 2 n-word-bytes) below bytes by (* 2 n-word-bytes)
	      do (inst stp zr-tn zr-tn (@ tmp-tn off))))))

  ;; ALLOCATE-VECTOR's LTN-ANNOTATE optimizer chooses its VOP: this one
  ;; for a small constant vector on the stack, else SBCL's
  (let ((info (sb-c::fun-info-or-lose 'allocate-vector)))
    (unless (get 'allocate-vector 'qs-original-ltn-annotate)
      (setf (get 'allocate-vector 'qs-original-ltn-annotate) (sb-c::fun-info-ltn-annotate info)))
    (let ((original (get 'allocate-vector 'qs-original-ltn-annotate)))
      (setf (sb-c::fun-info-ltn-annotate info)
	    (lambda (call)
	      (let ((args (sb-c::basic-combination-args call)))
		(if (and (quasischeme-fast-p)
			 (sb-c::node-stack-allocate-p call)
			 (= (length args) 3)
			 (every #'sb-c::constant-lvar-p args)
			 (every (lambda (a) (typep (sb-c::lvar-value a) 'fixnum)) args)
			 (<= (sb-c::lvar-value (third args)) 60)
			 (not (sb-c::msan-unpoison sb-c::*compilation*)))
		    (sb-c::vectorish-ltn-annotate-helper
		     call 'qs-allocate-small-vector-on-stack 'allocate-vector-on-heap)
		    (funcall original call)))))))

  ;; so that its elements are stored without GC card marks, as those of
  ;; SBCL's stack vectors are (STACK-CONSED-P knows its VOP by name)
  (unless (get 'stack-consed-p 'qs-original)
    (setf (get 'stack-consed-p 'qs-original) #'stack-consed-p))
  (sb-ext:without-package-locks
    (handler-bind ((warning #'muffle-warning))
      (let ((original (get 'stack-consed-p 'qs-original)))
	(setf (fdefinition 'stack-consed-p)
	      (lambda (object)
		(or (funcall original object)
		    (let ((write (sb-c::tn-writes (remove-moves object))))
		      (and write (not (sb-c::tn-ref-next write)) (sb-c::tn-ref-vop write)
			   (eq (sb-c::vop-name (sb-c::tn-ref-vop write)) 'qs-allocate-small-vector-on-stack)))))))))

  (sb-ext:lock-package "SB-C")
  (sb-ext:lock-package "SB-VM"))
