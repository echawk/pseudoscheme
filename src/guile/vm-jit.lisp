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

(defvar *vm-jit* (not (equal (uiop:getenv "PSEUDOSCHEME_GUILE_JIT") "0"))
  "True: translate code that runs often to Lisp.")

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

(defun region-instructions (image entry)
  "The instructions from ENTRY to the end of the code they're in: until
one that doesn't fall through, past which nothing before jumps.  A list
of (ip op operands next), in order."
  (let* ((bytes (vm-image-bytes image))
	 (end (floor (length bytes) 4))
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

(defun instruction-form (op ip operands next)
  "Instruction OP at IP, its handler's source with OPERANDS in place: a
form whose value is where to go, as the handler's is."
  (let ((source (gethash (vm-op-name op) *vm-op-sources*)))
    (if (null source)
	`(funcall (the function (svref (vm-dispatch-table) ,(vm-op-opcode op))) vm image ,ip ',operands ,next)
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
		       ,@body))))))))

(defun region-form (instructions)
  "A function of the machine, the image and an IP in INSTRUCTIONS that
runs from IP: where to go when control leaves the region."
  (let ((tags (make-hash-table)))
    (dolist (i instructions) (setf (gethash (first i) tags) (gensym (format nil "I~D-" (first i)))))
    `(lambda (vm image ip)
       (declare (type vm vm) (fixnum ip) (ignorable image)
		(optimize (speed 1) (safety 1) (debug 0) (compilation-speed 2)))
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
