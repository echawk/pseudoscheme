; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-GUILE -*-

;;;; Guile's VM: bytecode translated to Lisp
;;;;
;;;; A region of an image's code that runs often -- from an instruction
;;;; jumped to (a procedure's entry, a loop's head) up to where it ends --
;;;; becomes one Lisp function for SBCL to compile.  Its body is a
;;;; TAGBODY with a tag per instruction, each instruction its handler's
;;;; source (vm.lisp's DEFOP) with the operands as constants; going on to
;;;; the next instruction is a GO, as is a jump in the region.  A call's
;;;; return comes back in through a dispatch on IP, so the function can be
;;;; entered at any instruction it holds.  Where the code goes outside the
;;;; region (another image, a return to Lisp, an instruction not in the
;;;; region), the function returns what the handler did, and the
;;;; interpreter (RUN-VM-1) goes on from there.
;;;;
;;;; The interpreter stays the reference: the translation runs the same
;;;; handlers, and *VM-JIT* NIL (PSEUDOSCHEME_GUILE_JIT=0) turns it off.

(in-package "PSEUDOSCHEME-GUILE")

(defun backend-from-environment ()
  (let ((name (uiop:getenv "PSEUDOSCHEME_GUILE_VM_BACKEND")))
    (cond ((equal (uiop:getenv "PSEUDOSCHEME_GUILE_JIT") "0") :interpret)
	  ((null name) :jit)
	  (t (or (find name '(:interpret :jit :aot :vop) :test #'string-equal)
		 (error "Unknown Guile VM backend ~A" name))))))

(defvar *vm-backend* (backend-from-environment)
  "How bytecode runs (PSEUDOSCHEME_GUILE_VM_BACKEND):
  :interpret  the interpreter alone (vm.lisp), the reference;
  :jit        regions that run often translated to Lisp (below);
  :aot        every region of an image translated when it loads, the
              translation compiled with COMPILE-FILE and cached;
  :vop        as :jit, with SBCL VOPs for the hottest instructions
              (vm-vops.lisp).")

(defvar *vm-vops*)			; vm-vops.lisp

(defvar *vm-jit* (not (eq *vm-backend* :interpret))
  "True: run translations of code to Lisp (any backend but :interpret).")

(defun set-vm-backend (backend)
  "Use BACKEND for images loaded from now on (and code not yet translated)."
  (setq *vm-backend* backend
	*vm-jit* (not (eq backend :interpret))))

(defvar *vm-jit-threshold*
  (or (ignore-errors (parse-integer (uiop:getenv "PSEUDOSCHEME_GUILE_JIT_THRESHOLD"))) 20)
  "How many times an instruction is jumped to before the region from it
is translated (PSEUDOSCHEME_GUILE_JIT_THRESHOLD; 0 translates all code
run, for testing).")

(defvar *vm-jit-max* 2000
  "The most instructions in a region (SBCL compiles a function of no more
than about a megabyte of code on arm64).")

(defparameter +region-ends+
  '("j" "jtable" "return-values" "tail-call" "tail-call-label"
    "throw" "throw/value" "throw/value+data" "halt")
  "Instructions after which control doesn't fall through.")

(defun branch-targets (name ip operands)
  "Where instruction NAME at IP may jump in its procedure (word indexes)."
  (cond ((member name '("j" "jl" "je" "jnl" "jne" "jge" "jnge") :test #'string=)
	 (list (+ ip (first operands))))
	((string= name "jtable") (mapcar (lambda (o) (+ ip o)) (third operands)))
	((string= name "prompt") (list (+ ip (fourth operands))))
	(t '())))

(defun region-instructions (image entry &optional limit)
  "The instructions from ENTRY to the end of the code they're in: until
one that doesn't fall through, past which nothing before jumps (or LIMIT,
a word index).  A list of (ip op operands next), in order."
  (let* ((bytes (vm-image-bytes image))
	 (end (or limit (floor (length bytes) 4)))
	 (horizon entry)
	 (instructions '()))
    (loop with ip = entry
	  repeat *vm-jit-max*
	  while (< ip end)
	  do (multiple-value-bind (op operands next)
		 (handler-case (decode-instruction bytes ip)
		   (error () (loop-finish)))
	       (push (list ip op operands next) instructions)
	       (let ((name (vm-op-name op)))
		 (dolist (target (branch-targets name ip operands))
		   (setq horizon (max horizon target)))
		 (when (and (member name +region-ends+ :test #'string=) (> next horizon))
		   (loop-finish)))
	       (setq ip next)))
    (nreverse instructions)))

(defvar *specializers* (make-hash-table :test 'equal)
  "Instruction name -> a function of its operands, IP and NEXT returning
a form better than its handler's source for them, or NIL.")

(defmacro defspecializer (name lambda-list &body body)
  `(setf (gethash ,name *specializers*) (lambda ,lambda-list ,@body)))

(defun literal-immediate-p (x)
  (or (typep x 'fixnum) (characterp x) (eq x ps:true) (eq x ps:false) (null x)))

;; an immediate's bits are decoded once, here
(macrolet ((immediates (&rest names)
	     `(progn
		,@(loop for name in names
			collect `(defspecializer ,name (operands ip next)
				   (declare (ignore ip))
				   (destructuring-bind (dst bits &optional low) operands
				     (let ((x (scm-from-bits (if low (logior (ash bits 32) low) (ldb (byte 64 0) bits)))))
				       (and (literal-immediate-p x)
					    `(progn (setf (vm-slot vm ,dst) ',x) ,next)))))))))
  (immediates "make-immediate" "make-short-immediate" "make-long-immediate" "make-long-long-immediate"))

(defun intrinsic-name (index)
  "The name of Guile's intrinsic INDEX, a string."
  (let ((entry (rassoc index (bytecode-table "intrinsic-list"))))
    (and entry (ps:scheme-symbol-name (car entry)))))

(defun vop-backend-p () (and (eq *vm-backend* :vop) *vm-vops*))

;; :vop: fixnum addition and subtraction by the VOPs of vm-vops.lisp, the
;; intrinsic when they give NIL
(defspecializer "call-scm<-scm-scm" (operands ip next)
  (declare (ignore ip))
  (destructuring-bind (dst a b idx) operands
    (let* ((name (intrinsic-name idx))
	   (vop (and (vop-backend-p) (cdr (assoc name '(("add" . %vm-fixnum+) ("sub" . %vm-fixnum-))
						  :test #'equal)))))
      (and vop
	   `(let ((x (vm-slot vm ,a)) (y (vm-slot vm ,b)))
	      (setf (vm-slot vm ,dst) (or (,vop x y) (funcall (the function (intrinsic ,idx)) x y)))
	      ,next)))))

(defspecializer "call-scm<-scm-uimm" (operands ip next)
  (declare (ignore ip))
  (destructuring-bind (dst a b idx) operands
    (let* ((name (intrinsic-name idx))
	   (vop (and (vop-backend-p) (cdr (assoc name '(("add/immediate" . %vm-fixnum+)
							 ("sub/immediate" . %vm-fixnum-))
						  :test #'equal)))))
      (and vop
	   `(let ((x (vm-slot vm ,a)))
	      (setf (vm-slot vm ,dst) (or (,vop x ,b) (funcall (the function (intrinsic ,idx)) x ,b)))
	      ,next)))))

(defun instruction-form (op ip operands next)
  "Instruction OP at IP, its handler's source with OPERANDS in place: a
form whose value is where to go, as the handler's is."
  (let ((source (gethash (vm-op-name op) *vm-op-sources*))
	(special (let ((s (gethash (vm-op-name op) *specializers*)))
		   (and s (funcall s operands ip next)))))
    (cond
      (special special)
      ((null source)
	`(funcall (the function (svref (vm-dispatch-table) ,(vm-op-opcode op))) vm image ,ip ',operands ,next))
      (t
	(destructuring-bind (lambda-list &rest body) source
	  `(let ((ip ,ip) (next ,next))
	     (declare (ignorable ip next))
	     ,(if (intersection lambda-list lambda-list-keywords)
		  `(destructuring-bind ,lambda-list ',operands
		     (declare (ignorable ,@(set-difference lambda-list lambda-list-keywords)))
		     (symbol-macrolet ((fp (vm-fp vm)) (sp (vm-sp vm)) (stack (vm-stack vm)))
		       ,@body))
		  `(let ,(mapcar (lambda (var value) (list var (if (consp value) `',value value)))
				 lambda-list operands)
		     (declare (ignorable ,@lambda-list))
		     (symbol-macrolet ((fp (vm-fp vm)) (sp (vm-sp vm)) (stack (vm-stack vm)))
		       ,@body)))))))))

(defun region-form (instructions)
  "A function of the machine, the image and an IP in INSTRUCTIONS that
runs from IP: where to go when control leaves the region."
  (let ((tags (make-hash-table)))
    (dolist (i instructions) (setf (gethash (first i) tags) (gensym (format nil "I~D-" (first i)))))
    `(lambda (vm image ip)
       (declare (type vm vm) (fixnum ip) (ignorable image)
		(optimize (speed 2) (safety ,(if (vop-backend-p) 0 1)) (debug 0) (compilation-speed 0)))
       (block region
	 (tagbody
	  dispatch
	    (case ip
	      ,@(loop for (ip) in instructions collect `(,ip (go ,(gethash ip tags))))
	      (t (return-from region ip)))
	    ,@(loop for (ip op operands next) in instructions
		    collect (gethash ip tags)
		    collect `(let ((to ,(instruction-form op ip operands next)))
			       (cond ((eql to ,next)
				      ,(if (gethash next tags)
					   `(go ,(gethash next tags))
					   `(return-from region to)))
				     ((typep to 'fixnum) (setq ip to) (go dispatch))
				     ;; a call or return in the image: on, if
				     ;; it's in the region
				     ((and (code-pointer-p to) (eq (code-pointer-image to) image))
				      (setq ip (code-pointer-word to))
				      (go dispatch))
				     (t (return-from region to))))))))))

(defun compile-region (image entry)
  "Translate the region from ENTRY, mark its instructions as entering the
translation, and return it; or NIL if it can't be compiled."
  (let* ((jit (vm-image-jit image))
	 (instructions (region-instructions image entry))
	 (code (and instructions
		    (handler-case
			(handler-bind ((warning #'muffle-warning))
			  (let ((*error-output* (make-broadcast-stream)))
			    (compile nil (region-form instructions))))
		      (error () nil)))))
    (if code
	(dolist (i instructions)
	  (unless (functionp (svref jit (first i)))
	    (setf (svref jit (first i)) code)))
	(setf (svref jit entry) :interpret))
    code))

(defun jit-entry (image ip)
  "The translation to run at IP, jumped to, if there is one; counting
jumps there until there should be."
  (let ((jit (or (vm-image-jit image)
		 (setf (vm-image-jit image)
		       (make-array (ceiling (length (vm-image-bytes image)) 4) :initial-element 0)))))
    (let ((j (svref jit ip)))
      (cond ((functionp j) j)
	    ((not (typep j 'fixnum)) nil)
	    ((< j *vm-jit-threshold*) (setf (svref jit ip) (1+ j)) nil)
	    (t (compile-region image ip))))))

;;; ------------------------------------------------------------------
;;; Ahead of time: every region of an image, when it loads
;;;
;;; The image's code (its .rtl-text section) is cut into regions as
;;; above, one after another.  Each region's function is a top-level form
;;; of a Lisp file, compiled with COMPILE-FILE (one function per
;;; component, within the code SBCL can compile) into a fasl kept under
;;; the cache directory, by the image's contents: an installed module's
;;; code is translated once.

(defvar *aot-regions* '()
  "While an image's translation loads: (start end function), each region's.")

(defun aot-region (start end function)
  (push (list start end function) *aot-regions*))

(defun image-text-range (image)
  "The word indexes of IMAGE's code: its .rtl-text section."
  (let* ((elf (vm-image-elf image))
	 (text (and elf (elf-section-named elf ".rtl-text"))))
    (and text
	 (values (floor (elf-section-offset text) 4)
		 (floor (+ (elf-section-offset text) (elf-section-size text)) 4)))))

(defun image-regions (image)
  "IMAGE's code, cut into regions: lists of (ip op operands next)."
  (multiple-value-bind (start end) (image-text-range image)
    (when start
      (loop with ip = start
	    while (< ip end)
	    collect (let ((region (region-instructions image ip end)))
		      (if region
			  (setq ip (fourth (car (last region))))
			  (setq ip end))
		      region)
	      into regions
	    finally (return (remove nil regions))))))

(defun octets-hash (bytes)
  "FNV-1a of BYTES, 64 bits."
  (let ((h #xcbf29ce484222325))
    (loop for b across bytes
	  do (setq h (ldb (byte 64 0) (* (logxor h b) #x100000001b3))))
    h))

(defun aot-fasl (image)
  (merge-pathnames
   (format nil "pseudoscheme/guile-aot/~36R-~36R.fasl"
	   (octets-hash (vm-image-bytes image))
	   (psx::fnv-1a (psx::build-signature)))
   (uiop:xdg-cache-home)))

(defun write-aot-file (image lisp)
  (with-open-file (out lisp :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (let ((*package* (find-package "PSEUDOSCHEME-GUILE"))
	    (*print-circle* t)	; the tags of a region, uninterned, shared
	    (*print-readably* t))
	(format out ";;; Guile VM code translated to Lisp ahead of time (vm-jit.lisp)~%")
	(prin1 '(in-package "PSEUDOSCHEME-GUILE") out)
	(terpri out)
	(dolist (region (image-regions image))
	  (prin1 `(aot-region ,(first (first region)) ,(fourth (car (last region)))
			      ,(region-form region))
		 out)
	  (terpri out))))))

(defun aot-compile-image (image)
  "Translate IMAGE's code ahead of time, or load the translation made
before, and make it what runs.  True if it did."
  (let ((fasl (aot-fasl image)))
    (handler-case
	(progn
	  (unless (probe-file fasl)
	    (ensure-directories-exist fasl)
	    (let ((lisp (make-pathname :type "lisp" :defaults fasl))
		  (temp (make-pathname :name (format nil "~A-~D" (pathname-name fasl) (random (expt 2 40)))
				       :defaults fasl)))
	      (write-aot-file image lisp)
	      (handler-bind ((warning #'muffle-warning))
		(let ((*error-output* (make-broadcast-stream)) (*standard-output* (make-broadcast-stream)))
		  (with-compilation-unit (:override t)
		    (compile-file lisp :output-file temp :verbose nil :print nil))))
	      (rename-file temp fasl)
	      (ignore-errors (delete-file lisp))))
	  (let ((*aot-regions* '()))
	    (cl:load fasl)
	    (let ((jit (or (vm-image-jit image)
			   (setf (vm-image-jit image)
				 (make-array (ceiling (length (vm-image-bytes image)) 4) :initial-element 0)))))
	      (dolist (r *aot-regions*)
		(destructuring-bind (start end function) r
		  (loop for ip from start below end do (setf (svref jit ip) function))))))
	  t)
      (error (e)
	(warn "Guile VM: no ahead-of-time translation of an image: ~A" e)
	nil))))

(setq *image-loaded-hook*
      (lambda (image) (when (eq *vm-backend* :aot) (aot-compile-image image))))
