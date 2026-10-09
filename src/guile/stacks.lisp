; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-GUILE -*-

;;;; Stacks and frames: make-stack, and what (system vm frame) is built on
;;;;
;;;; A stack is the frames of the calls in progress, innermost first.
;;;; They come from two places:
;;;;
;;;; - Lisp's stack (SBCL's debugger interface): Scheme compiled to Lisp,
;;;;   and the runtime's primitives.  A frame is one of a procedure's
;;;;   code, known by name (*CODE-NAMES*, the root module's primitives) or
;;;;   by being compiled from Scheme (a name made of the SCHEME package's
;;;;   symbols).  Its arguments are the ones SBCL kept; a procedure's
;;;;   machine (src/continuations.lisp) has three of its own first.
;;;; - The VM's stack (vm.lisp), where a run of the machine is on Lisp's
;;;;   stack: its frames, with their locals and instruction pointers, so
;;;;   that (system vm frame) reads them as Guile does, by the debugging
;;;;   information of the image.
;;;;
;;;; A frame from Lisp has an instruction pointer of no image, which
;;;; primitive-code-name knows its name by, and its locals are its
;;;; procedure (#f) and its arguments: what (system vm frame) makes of a
;;;; primitive's frame.

(in-package "PSEUDOSCHEME-GUILE")

(defstruct (gframe (:constructor make-gframe (kind name code locals ip)) (:copier nil))
  kind					; :lisp or :vm
  name					; for :lisp, the Scheme name or #f
  code					; for :lisp, its code; for :vm, its program
  locals				; simple-vector: local 0 the procedure
  ip					; an address in an image, or a negative id
  (stack nil)
  (index 0))

(defmethod print-object ((f gframe) stream)
  (format stream "#<frame ~(~X~) ~A>" (logand (sb-kernel:get-lisp-obj-address f) #xffffffffff)
	  (let ((n (gframe-name f))) (if (and n (not (eq n ps:false))) (ps:scheme-symbol-name n) "_"))))

(defstruct (gstack (:constructor make-gstack (id frames)) (:copier nil))
  id
  (frames #() :type simple-vector))

(defmethod print-object ((s gstack) stream)
  (format stream "#<stack ~(~X~):~D>" (logand (sb-kernel:get-lisp-obj-address s) #xffffffffff)
	  (length (gstack-frames s))))

;;; Instruction pointers of frames from Lisp: negative, so in no image.

(defvar *lisp-frame-ips* (trivial-garbage:make-weak-hash-table :weakness :value :synchronized t))

(defun lisp-frame-ip (frame)
  ;; ATOMIC-DECF returns the value before
  (let ((ip (1- (sb-ext:atomic-decf (car (load-time-value (list 0)))))))
    (setf (gethash ip *lisp-frame-ips*) frame)
    ip))

;;; Names of code

(defvar *root-code-names* nil "(obarray . table): root code -> name.")

(defun root-code-name (code)
  (unless (and *root-code-names* (eq (car *root-code-names*) *obarray*))
    (let ((table (make-hash-table :test 'eq)))
      (dolist (handle (ghash-handles *obarray*))
	(let ((v (cdr handle)))
	  (when (and (gvariable-p v) (functionp (gvariable-value v)))
	    (let ((f (gvariable-value v)))
	      (unless (gethash (procedure-code f) table)
		(setf (gethash (procedure-code f) table) (or (root-procedure-name f) (car handle))))))))
      (setq *root-code-names* (cons *obarray* table))))
  (gethash code (cdr *root-code-names*)))

(defvar *lisp-source-root*
  (namestring (asdf:system-source-directory :pseudoscheme)))

(defun scheme-debug-name-p (name)
  "Whether NAME, a Lisp debug name, is that of code compiled from Scheme:
it has a symbol of the SCHEME package, or it's a thunk compiled from a
file that isn't Lisp's (a Scheme file, or the module cache's)."
  (let ((scheme (load-time-value (find-package "SCHEME"))))
    (labels ((walk (x)
	       (cond ((symbolp x) (eq (symbol-package x) scheme))
		     ((consp x) (or (walk (car x)) (walk (cdr x))))
		     (t nil))))
      (or (walk name)
	  (and (consp name) (eq (first name) 'lambda) (null (second name))
	       (eq (third name) :in) (stringp (fourth name))
	       (let ((file (fourth name)))
		 (not (or (and (>= (length file) (length *lisp-source-root*))
			       (string= *lisp-source-root* file :end2 (length *lisp-source-root*))
			       (not (search "/tests/" file)))
			  (search "/sbcl/" file)
			  (search "quicklisp" file)))))))))

(defun machine-debug-name-p (name)
  "Whether NAME is that of a machine: (labels %machine... ...)."
  (and (consp name) (eq (car name) 'labels) (psx::machine-name-p (cadr name))))

;;; Frames from Lisp's stack

(defun lisp-frame-arguments (frame)
  "FRAME's arguments as SBCL kept them; one it didn't is _."
  (let ((args (handler-case (sb-debug::frame-args-as-list frame most-positive-fixnum)
		(error () '()))))
    (loop for (a . more) on args
	  if (and (symbolp a) (member (symbol-name a) '("&REST" "&MORE") :test #'string=) (listp (car more)))
	    append (car more) and do (loop-finish)
	  else collect (if (typep a 'sb-debug::unprintable-object) (ssym "_") a))))

(defun lisp-scheme-frame (frame)
  "A gframe for FRAME of Lisp's stack, if it is a Scheme procedure's."
  (let* ((df (sb-di:frame-debug-fun frame))
	 (debug-name (sb-di:debug-fun-name df))
	 (code (ignore-errors (sb-di:debug-fun-fun df)))
	 (name (or (and code (or (gethash code *code-names*) (root-code-name code)))
		   (and (machine-debug-name-p debug-name)
			(let ((n (psx::machine-procedure-name (cadr debug-name))))
			  (cond ((null n) nil)
				((string= n +inline-body-name+) :inline)
				(t (ssym n))))))))
    (when (and (not (eq name :inline)) (or name (scheme-debug-name-p debug-name)))
      (let ((args (lisp-frame-arguments frame)))
	(when (machine-debug-name-p debug-name)
	  (setq args (nthcdr 3 args))
	  ;; a rest list, spread as it was passed
	  (when (and (psx::machine-rest-p (cadr debug-name)) args (listp (car (last args))))
	    (setq args (append (butlast args) (car (last args))))))
	(let ((g (make-gframe :lisp (or name ps:false) code (coerce (cons ps:false args) 'simple-vector) nil)))
	  (setf (gframe-ip g) (lisp-frame-ip g))
	  g)))))

(defun prompt-marker (frame)
  "For FRAME of psx::prompt-extent, while its thunk runs: (:prompt . tag)."
  (let ((pframe (car (ignore-errors (sb-debug::frame-args-as-list frame 1)))))
    (when (and (simple-vector-p pframe) (member pframe psx::*fstack* :test #'eq))
      (let ((cell (svref pframe 2)))
	(cons :prompt (if (consp cell) (cdr cell) cell))))))

;;; Frames from the VM's stack

(defun vm-frame-ip (vm fp next-ip)
  "The address frame FP of VM is at: NEXT-IP (from the frame it called),
or its procedure's entry."
  (or next-ip
      (let ((proc (svref (vm-stack vm) (+ fp 1))))
	(if (typep proc 'vm-program) (code-address (program-code proc)) 0))))

(defun vm-run-frames (vm cursor run)
  "The frames of RUN, (fp . nested), of VM from CURSOR, its current frame:
innermost first, and the frame the next run out is at."
  (let ((stack (vm-stack vm)) (frames '()) (next-ip nil) (top nil)
	(start (car run)) (nested (cdr run)))
    (flet ((ok (fp) (and (integerp fp) (< 1 fp (length stack)))))
      (loop with fp = cursor
	    while (ok fp)
	    do (when (and nested (<= fp start)) (return (values (nreverse frames) start)))
	       (let* ((proc (svref stack (+ fp 1)))
		      (ra (svref stack (- fp 1)))
		      (link (svref stack (- fp 2)))
		      (end (or top (vm-sp vm))))
		 (when (typep proc 'vm-program)
		   (let ((g (make-gframe :vm (procedure-name* proc) proc
					 (subseq stack (+ fp 1) (max (+ fp 1) (1+ end)))
					 (vm-frame-ip vm fp next-ip))))
		     (push g frames)))
		 (setq top (- fp 3)
		       next-ip (and (code-pointer-p ra) (code-address ra)))
		 (cond ((eq ra :boundary)
			(return (values (nreverse frames) (and (integerp link) (- fp link)))))
		       ((not (integerp link)) (return (values (nreverse frames) nil)))
		       (t (setq fp (- fp link)))))
	    finally (return (values (nreverse frames) nil))))))

(defun procedure-name* (p)
  (let ((name (funcall (gethash "procedure-name" *guile-primitives*) p)))
    name))

;;; The current stack

(defun current-frame-entries ()
  "The frames in progress, innermost first, with (:prompt . tag) where
a prompt is."
  (let ((entries '())
	(runs *vm-runs*)
	(vm *vm*)
	(cursor (and *vm* (vm-fp *vm*))))
    (sb-debug:map-backtrace
     (lambda (frame)
       (let ((name (sb-di:debug-fun-name (sb-di:frame-debug-fun frame))))
	 (cond ((eq name 'run-vm-1)
		(let ((run (pop runs)))
		  (when (and run vm cursor)
		    (multiple-value-bind (frames next) (vm-run-frames vm cursor run)
		      (dolist (g frames) (push g entries))
		      (setq cursor next)))))
	       ((eq name 'psx::prompt-extent)
		(let ((marker (prompt-marker frame)))
		  (when marker (push marker entries))))
	       (t (let ((g (lisp-scheme-frame frame)))
		    (when g (push g entries))))))))
    (nreverse entries)))

;;; Cutting

(defun frame-entry-p (e) (gframe-p e))

(defun frame-matches-p (g cut)
  (cond ((functionp cut)
	 (if (eq (gframe-kind g) :vm)
	     (eq (gframe-code g) cut)
	     (eq (gframe-code g) (procedure-code cut))))
	((and (consp cut) (integerp (car cut)) (integerp (cdr cut)))
	 (<= (car cut) (gframe-ip g) (cdr cut)))))

(defun prompt-cut-p (cut)
  (not (or (integerp cut) (functionp cut) (eq cut ps:true)
	   (and (consp cut) (integerp (car cut)) (integerp (cdr cut))))))

(defun cut-inner (entries cut)
  (cond ((eq cut ps:true) entries)
	((integerp cut)
	 (let ((n cut))
	   (loop while (and entries (plusp n))
		 do (when (frame-entry-p (car entries)) (decf n))
		    (pop entries))
	   entries))
	((prompt-cut-p cut)
	 (let ((at (position-if (lambda (e) (and (consp e) (eq (cdr e) cut))) entries)))
	   (if at (nthcdr (1+ at) entries) entries)))
	(t (let ((at (position-if (lambda (e) (and (frame-entry-p e) (frame-matches-p e cut))) entries)))
	     (if at (nthcdr at entries) entries)))))

(defun cut-outer (entries cut)
  (nreverse
   (let ((reversed (reverse entries)))
     (cond ((eq cut ps:true) reversed)
	   ((integerp cut)
	    (let ((n cut))
	      (loop while (and reversed (plusp n))
		    do (when (frame-entry-p (car reversed)) (decf n))
		       (pop reversed))
	      reversed))
	   ((prompt-cut-p cut)
	    (let ((at (position-if (lambda (e) (and (consp e) (eq (cdr e) cut))) reversed)))
	      (if at (nthcdr (1+ at) reversed) reversed)))
	   (t (let ((at (position-if (lambda (e) (and (frame-entry-p e) (frame-matches-p e cut))) reversed)))
		(if at (nthcdr at reversed) reversed)))))))

(defun stack-from-entries (id entries cuts)
  (loop while cuts
	do (setq entries (cut-inner entries (pop cuts)))
	   (setq entries (cut-outer entries (if cuts (pop cuts) 0))))
  (let* ((frames (coerce (remove-if-not #'frame-entry-p entries) 'simple-vector))
	 (stack (make-gstack id frames)))
    (loop for g across frames for i from 0
	  do (setf (gframe-stack g) stack (gframe-index g) i))
    stack))

(defun stacks-fluid-value ()
  (let ((v (root-value "%stacks")))
    (and (fluid-p v) (fluid-value v))))

(defguile "make-stack" (obj &rest cuts)
  (cond ((eq obj ps:true)
	 (let* ((started (stacks-fluid-value))
		(entries (current-frame-entries))
		(entries (if (consp started)
			     ;; start-stack: nothing outside its prompt
			     (cut-outer entries (cdr started))
			     entries)))
	   (stack-from-entries (if (consp started) (car started) ps:false) entries cuts)))
	((gframe-p obj)
	 (let ((stack (gframe-stack obj)))
	   (stack-from-entries (gstack-id stack)
			       (coerce (subseq (gstack-frames stack) (gframe-index obj)) 'list)
			       cuts)))
	((psx::continuation-procedure-p obj)
	 ;; its frames are copies of the runtime's, not calls in progress
	 (make-gstack ps:false #()))
	(t (wrong-type "make-stack" 1 obj))))

(defun check-stack (who s) (unless (gstack-p s) (wrong-type who 1 s)) s)
(defun check-frame (who f) (unless (gframe-p f) (wrong-type who 1 f)) f)

(defguile "stack?" (x) (bool (gstack-p x)))
(defguile "stack-id" (s)
  (cond ((gstack-p s) (gstack-id s))
	((or (eq s ps:true) (psx::continuation-procedure-p s))
	 (let ((started (stacks-fluid-value)))
	   (if (consp started) (car started) ps:false)))
	(t (wrong-type "stack-id" 1 s))))
(defguile "stack-length" (s) (length (gstack-frames (check-stack "stack-length" s))))
(defguile "stack-ref" (s i)
  (let ((frames (gstack-frames (check-stack "stack-ref" s))))
    (unless (and (integerp i) (< -1 i (length frames)))
      (guile-error (ssym "out-of-range") "stack-ref" "Argument 2 out of range: ~S" (list i) (list i)))
    (svref frames i)))

(defguile "frame?" (x) (bool (gframe-p x)))
(defguile "frame-previous" (f)
  (let* ((f (check-frame "frame-previous" f))
	 (frames (gstack-frames (gframe-stack f)))
	 (i (1+ (gframe-index f))))
    (if (< i (length frames)) (svref frames i) ps:false)))
(defguile "frame-instruction-pointer" (f) (gframe-ip (check-frame "frame-instruction-pointer" f)))
(defguile "frame-address" (f)
  (let ((f (check-frame "frame-address" f)))
    (- (length (gstack-frames (gframe-stack f))) (gframe-index f))))
(defguile "frame-stack-pointer" (f)
  (let ((f (check-frame "frame-stack-pointer" f)))
    (+ (- (length (gstack-frames (gframe-stack f))) (gframe-index f)) (length (gframe-locals f)))))
(defguile "frame-dynamic-link" (f)
  (let ((previous (funcall (gethash "frame-previous" *guile-primitives*) f)))
    (if (gframe-p previous) (funcall (gethash "frame-address" *guile-primitives*) previous) ps:false)))
(defguile "frame-return-address" (f)
  (let ((previous (funcall (gethash "frame-previous" *guile-primitives*) f)))
    (if (gframe-p previous) (gframe-ip previous) ps:false)))
(defguile "frame-source" (f) (check-frame "frame-source" f) ps:false)

(defun frame-module-procedure (name)
  "(system vm frame)'s NAME, if the module is loaded."
  (let ((m (resolve-module* (list (ssym "system") (ssym "vm") (ssym "frame")))))
    (and m (let ((v (module-variable* m (ssym name))))
	     (and v (not (eq (gvariable-value v) +unbound+)) (gvariable-value v))))))

(defguile "frame-procedure-name" (f &rest keys)
  (let ((f (check-frame "frame-procedure-name" f)))
    (if (eq (gframe-kind f) :lisp)
	(gframe-name f)
	(let ((p (frame-module-procedure "frame-procedure-name")))
	  (if p (apply p f keys) (gframe-name f))))))

(defun frame-arguments* (f)
  (let ((p (frame-module-procedure "frame-call-representation")))
    (if p
	(cdr (funcall p f))
	(cdr (coerce (gframe-locals f) 'list)))))

(defguile "frame-arguments" (f) (frame-arguments* (check-frame "frame-arguments" f)))

;;; (system vm frame)'s C half

(defun frame-local (who f i)
  (let ((locals (gframe-locals (check-frame who f))))
    (unless (and (integerp i) (< -1 i (length locals)))
      (guile-error (ssym "out-of-range") who "Argument 2 out of range: ~S" (list i) (list i)))
    locals))

(defextension "scm_init_frames_builtins"
  (list (cons "frame-num-locals" (lambda (f) (length (gframe-locals (check-frame "frame-num-locals" f)))))
	(cons "frame-local-ref"
	      (lambda (f i &optional repr)
		(declare (ignore repr))
		(svref (frame-local "frame-local-ref" f i) i)))
	(cons "frame-local-set!"
	      (lambda (f i v &optional repr)
		(declare (ignore repr))
		(setf (svref (frame-local "frame-local-set!" f i) i) v)
		*unspecified*))))

;;; A Lisp frame's instruction pointer names it, as a primitive's does

(defun lisp-frame-at (ip)
  (and (integerp ip) (minusp ip) (gethash ip *lisp-frame-ips*)))
