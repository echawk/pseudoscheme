; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-PSYNTAX -*-

;;;; Full continuations from generalized stack inspection
;;;;
;;;; See docs/continuations.md.  With *FULL-CONTINUATIONS* true, each
;;;; core form psyntax produces goes through CC-TRANSFORM before the
;;;; translator.
;;;;
;;;; A procedure whose body makes a call that may capture a continuation,
;;;; other than in tail position, becomes a state machine: one Lisp
;;;; function whose body is a flat TAGBODY, with a label after each such
;;;; call (a "site") and the procedure's local variables hoisted to its
;;;; top.  A site pushes a frame, a stack-allocated vector
;;;;
;;;;    #(machine promoted site-number parameter-count live-variable ...)
;;;;
;;;; onto *FSTACK* for the extent of the call.  Capturing a continuation
;;;; copies the frames up to the base.  Re-entering one rebuilds them,
;;;; outermost first: each frame's machine is called with the site number
;;;; and the value returned to it, restores the live variables and jumps
;;;; to the label after the call.  So the normal path allocates nothing
;;;; on the heap and creates no closures, and the code stays flat, which
;;;; SBCL needs: it compiles a function with a thousand closures in it in
;;;; minutes, or not at all.
;;;;
;;;; Variables that are assigned, and that a frame may hold across an
;;;; assignment, are boxed, so that a re-entered frame shares them with
;;;; whatever else refers to them, as Scheme requires.
;;;;
;;;; Invoking a continuation within the extent of its call/cc throws to
;;;; that call/cc.  Otherwise it throws to the base, established around
;;;; each top-level evaluation, which runs the befores of the
;;;; continuation's dynamic-winds and rebuilds its frames.
;;;;
;;;; Most calls can't capture, and aren't sites: see "Procedures that
;;;; can't capture" and "Continuations that are only ever invoked during
;;;; the extent of their call/cc" below.  Lisp code that calls Scheme
;;;; procedures pushes frames of its own (dynamic-wind, exception
;;;; handlers, parameterize, map and the other loops), or, where it has
;;;; no frame-aware version, a barrier that makes re-entering through it
;;;; an error; so do Scheme procedures handed to Lisp through the bridge
;;;; (src/interop.lisp, LISP-FACING).  Known gap: Scheme code compiled
;;;; without the transformation (escape-only) isn't a barrier.

(in-package "PSEUDOSCHEME-PSYNTAX")

;;; ------------------------------------------------------------------
;;; The run-time side

(defvar *fstack* '()
  "The frames of the calls in progress, innermost first.  A base binds
it to (:BASE).")

(defvar *winders* '()
  "The active dynamic-winds' (before . after) pairs, innermost first.")

(defvar *base-tag* nil
  "The catch tag of the innermost base, to which a re-entered
continuation throws.")

(defvar *shared-winders* '()
  "While a re-entered continuation throws to the base: the winders it
shares with the current continuation, whose afters don't run.")

(defconstant +binding-stack-deep+ (* 512 1024)
  "Bytes of binding stack in use past which a frame isn't pushed by
binding *FSTACK*: SBCL's binding stack is of fixed size (about a
megabyte), while recursion may go as deep as the control stack allows.")

(defmacro binding-stack-deep-p ()
  #+sbcl `(> (sb-sys:sap- (sb-kernel:binding-stack-pointer-sap)
			  (sb-vm::current-thread-offset-sap sb-vm::thread-binding-stack-start-slot))
	     +binding-stack-deep+)
  #-sbcl nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *compact-frames* nil
    "True while compiling code that must be small: WITH-PUSHED-FRAME then
expands to a call of CALL-WITH-PUSHED-FSTACK, not the push in line.  For
a form whose code SBCL couldn't otherwise compile (src/guile/cache.lisp)."))

(defmacro with-pushed-frame ((frame) &body body)
  "Run BODY with FRAME (a vector, already allocated) pushed on *FSTACK*.
*FSTACK* is bound, which is cheap, until the binding stack is deep; then
CALL-WITH-ASSIGNED-FSTACK pushes it, taking only control stack.  That
is out of line: an UNWIND-PROTECT in a component makes SBCL compile all
of the component's functions more slowly.  Every thread that runs Scheme
binds *FSTACK* once (WITH-THREAD-STATE)."
  (let ((cell (gensym "CELL")) (thunk (gensym "BODY")))
    (when *compact-frames*
      (return-from with-pushed-frame
	`(let ((,cell (cons ,frame *fstack*)))
	   (declare (dynamic-extent ,cell))
	   (flet ((,thunk () ,@body))
	     (declare (dynamic-extent #',thunk))
	     (call-with-pushed-fstack ,cell #',thunk)))))
    ;; BODY once (code size matters: SBCL can't compile a code object
    ;; much past a megabyte on arm64), called by either branch
    `(let ((,cell (cons ,frame *fstack*)))
       (declare (dynamic-extent ,cell))
       (flet ((,thunk () ,@body))
	 (declare (dynamic-extent #',thunk))
	 (if (binding-stack-deep-p)
	     (call-with-assigned-fstack ,cell #',thunk)
	     (let ((*fstack* ,cell))
	       (,thunk)))))))

(defvar *stack-limit* nil
  "NIL, or (start-address . bytes): how much control stack recursion may
use from START-ADDRESS before CALL-WITH-ASSIGNED-FSTACK signals
STACK-LIMIT-REACHED (Guile's call-with-stack-overflow-handler).")

(define-condition stack-limit-reached (storage-condition) ()
  (:report "Stack overflow"))

(defun call-with-pushed-fstack (cell thunk)
  "WITH-PUSHED-FRAME, out of line: call THUNK with CELL pushed on *FSTACK*."
  (declare (function thunk))
  (if (binding-stack-deep-p)
      (call-with-assigned-fstack cell thunk)
      (let ((*fstack* cell)) (funcall thunk))))

(defun call-with-assigned-fstack (cell thunk)
  "Call THUNK with *FSTACK* assigned CELL, restored however THUNK is left."
  (declare (function thunk))
  (when *stack-limit*
    (let ((sp #+sbcl (sb-sys:sap-int (sb-vm::current-sp)) #-sbcl 0))
      (when (> (abs (- sp (car *stack-limit*))) (cdr *stack-limit*))
	(let ((*stack-limit* nil))	; handling it takes stack
	  (error 'stack-limit-reached)))))
  (let ((old *fstack*))
    (unwind-protect
	 (progn (setq *fstack* cell)
		(funcall thunk))
      (setq *fstack* old))))

(defmacro with-frame ((frame) &body body)
  "Run BODY with FRAME (a vector, already allocated) pushed on *FSTACK*."
  `(with-pushed-frame (,frame) ,@body))

(defmacro with-thread-state (&body body)
  "Run BODY with the thread's own continuation state: what a thread that
runs Scheme code starts with."
  `(let ((*fstack* '()) (*winders* '()) (*base-tag* nil) (*shared-winders* '()))
     ,@body))

;; threads made with bordeaux-threads (SRFI 18's, Chez's) bind them too,
;; through either version's default bindings
(dolist (package '("BORDEAUX-THREADS" "BORDEAUX-THREADS-2"))
  (let ((var (and (find-package package) (find-symbol "*DEFAULT-SPECIAL-BINDINGS*" package))))
    (when (and var (boundp var))
      (dolist (s '(*fstack* *winders* *base-tag* *shared-winders*))
	(unless (assoc s (symbol-value var))
	  (setf (symbol-value var) (acons s nil (symbol-value var))))))))

(defun call-with-frame (thunk k)
  "Call THUNK with a frame whose continuation is K, a Lisp function."
  (let ((frame (vector :k nil k)))
    (declare (dynamic-extent frame))
    (multiple-value-call k (with-frame (frame) (funcall thunk)))))

(defun call-with-continuation-base (thunk)
  "Call THUNK as the outermost frame continuations can capture.  A
continuation re-entered inside throws back here with a thunk that
rebuilds it."
  (when *base-tag*
    ;; Already inside one (eval, or load, called from a program): the
    ;; continuations captured inside must reach the frames outside too.
    (return-from call-with-continuation-base (funcall thunk)))
  (let* ((tag (list 'base))
	 (*base-tag* tag)
	 (*fstack* (list :base))
	 (*winders* '())
	 (*shared-winders* '()))
    (loop
      (setq thunk (catch tag
		    (return-from call-with-continuation-base (funcall thunk))))
      (setq *shared-winders* '()))))

(defun winder-extent (winder thunk)
  "Call THUNK inside dynamic-wind WINDER (whose before has run): its
after runs however THUNK is left."
  (let ((frame (vector :winder nil winder))
	(normal nil))
    (declare (dynamic-extent frame))
    (multiple-value-prog1
	(unwind-protect
	     (multiple-value-prog1
		 (let ((*winders* (cons winder *winders*)))
		   (with-frame (frame) (funcall thunk)))
	       (setq normal t))
	  (unless (or normal (member winder *shared-winders* :test #'eq))
	    (funcall (cdr winder))))
      (funcall (cdr winder)))))

(defun full-dynamic-wind (before thunk after)
  (funcall before)
  (winder-extent (cons before after) thunk))

(defun full-call-with-values (producer consumer)
  (call-with-frame producer (lambda (&rest values) (apply consumer values))))

;;; Exception handlers (the R7RS layer's *HANDLERS*) are dynamic state
;;; too: with-exception-handler's thunk runs in a :HANDLER frame, and a
;;; handler RAISE calls in a :HANDLERS frame (the outer handlers) under
;;; a :K frame (what RAISE does when the handler returns), so that a
;;; continuation captured in a handler or under with-exception-handler
;;; re-establishes them.  GUARD re-enters a handler this way (R6RS's
;;; expansion), to re-raise in the dynamic environment of the RAISE.
;;; These frames are pushed in escape mode too, where nothing captures
;;; them.

;;; What goes in a DYNAMIC-EXTENT frame that a captured copy keeps must be
;;; on the heap; but SBCL also stack-allocates what a LET variable used
;;; only in the frame's initializer is bound to.  Made by this function,
;;; it isn't.
(declaim (notinline heap-cons))
(defun heap-cons (a b) (cons a b))

(defun handler-extent (handler outer thunk)
  (let* ((handlers (heap-cons handler outer))	; on the heap: a copy of the frame keeps it
	 (frame (vector :handler nil handlers)))
    (declare (dynamic-extent frame))
    (ps-r7rs::call-with-handler handler outer (lambda () (with-frame (frame) (funcall thunk))))))

(defun handlers-extent (handlers thunk)
  (let ((frame (vector :handlers nil handlers)))
    (declare (dynamic-extent frame))
    (let ((ps-r7rs::*handlers* handlers))
      (with-frame (frame) (funcall thunk)))))

(defun full-call-handler (handler outer obj continuable)
  (call-with-frame
   (lambda () (handlers-extent outer (lambda () (funcall handler obj))))
   (if continuable
       #'values
       (lambda (&rest values)
	 (declare (ignore values))
	 (handlers-extent
	  outer
	  (lambda ()
	    (ps-r7rs:raise-object (funcall ps-r7rs::*non-continuable-condition* obj) nil)))))))

(setq ps-r7rs::*call-handler* 'full-call-handler
      ps-r7rs::*call-with-handler* 'handler-extent)

;;; Other dynamic contexts a Lisp primitive sets up around a call of a
;;; Scheme procedure (parameterize's) go in an :EXTENT frame holding the
;;; function that sets the context up, which rebuilding calls again.

(defun call-in-extent (establish thunk)
  "Call ESTABLISH with a thunk that calls THUNK, in a frame that calls
ESTABLISH again when rebuilt."
  (let ((frame (vector :extent nil establish)))
    (declare (dynamic-extent frame))
    (funcall establish (lambda () (with-frame (frame) (funcall thunk))))))

(setq ps-r7rs::*call-in-extent* 'call-in-extent)

;;; Loops written in Lisp that call Scheme procedures (map, for-each,
;;; vector-map, ...): each call is made in a :RESUME frame holding a
;;; function and the loop's state, #(:resume promoted function arg ...),
;;; which rebuilding calls as (function value arg ...) to go on with the
;;; loop.  A frame is pushed per call, and the frames are on the stack,
;;; so the loops allocate nothing more than escape-only ones.  Results
;;; are accumulated in reverse and reversed at the end, destructively
;;; unless a continuation was captured in the loop (the frame was
;;; promoted), whose re-entry must not change what an earlier return
;;; returned (R7RS 6.10, map).

(defmacro with-resume-frame ((frame function &rest args) &body body)
  ;; ARGS must be existing objects, not made here: DYNAMIC-EXTENT puts
  ;; what the initial value form allocates on the stack too.
  `(let ((,frame (vector :resume nil ,function ,@args)))
     (declare (dynamic-extent ,frame))
     (with-frame (,frame) ,@body)))

;;; A top-level form's forms (HOST-EVAL), each evaluated in a :RESUME
;;; frame holding the rest, so that a continuation captured in one goes
;;; on to them.

(defun eval-top-level-forms (forms)
  (let ((values '()))
    (loop for (form . rest) on forms
	  do (setq values
		   (multiple-value-list
		    (with-resume-frame (frame 'continue-top-level-forms rest)
		      (eval-compiled-or-interpreted (translate-core form t))))))
    (values-list values)))

(defun continue-top-level-forms (value rest)
  (if rest (eval-top-level-forms rest) value))

(defun list-cars (lists) (mapcar #'car lists))
(defun list-cdrs (lists) (mapcar #'cdr lists))

(defun map1-loop (f list acc captured)
  (loop
    (when (atom list) (return (if captured (reverse acc) (nreverse acc))))
    (let ((x (car list)) (promoted nil))
      (push (let ((frame (vector :resume nil 'map1-continue f list acc)))
	      (declare (dynamic-extent frame))
	      (multiple-value-prog1 (with-frame (frame) (funcall f x))
		(setq promoted (svref frame 1))))
	    acc)
      (when promoted (setq captured t)))
    (setq list (cdr list))))

(defun map1-continue (value f list acc)
  (map1-loop f (cdr list) (cons value acc) t))

(defun mapn-loop (f lists acc captured)
  (loop
    (when (some #'atom lists) (return (if captured (reverse acc) (nreverse acc))))
    (let ((xs (list-cars lists)) (promoted nil))
      (push (let ((frame (vector :resume nil 'mapn-continue f lists acc)))
	      (declare (dynamic-extent frame))
	      (multiple-value-prog1 (with-frame (frame) (apply f xs))
		(setq promoted (svref frame 1))))
	    acc)
      (when promoted (setq captured t)))
    (setq lists (list-cdrs lists))))

(defun mapn-continue (value f lists acc)
  (mapn-loop f (list-cdrs lists) (cons value acc) t))

(defun full-map (f list &rest lists)
  (if lists (mapn-loop f (cons list lists) '() nil) (map1-loop f list '() nil)))

(defun for-each-loop (f lists)
  ;; LISTS: a list of lists
  (loop
    (when (some #'atom lists) (return ps:unspecific))
    (let ((xs (list-cars lists)))
      (with-resume-frame (frame 'for-each-continue f lists)
	(if (cdr xs) (apply f xs) (funcall f (car xs)))))
    (setq lists (list-cdrs lists))))

(defun for-each-continue (value f lists)
  (declare (ignore value))
  (for-each-loop f (list-cdrs lists)))

(defun for-each1-loop (f list)
  (loop for l on list
	do (let ((x (car l)))
	     (with-resume-frame (frame 'for-each1-continue f l)
	       (funcall f x)))
	finally (return ps:unspecific)))

(defun for-each1-continue (value f list)
  (declare (ignore value))
  (for-each1-loop f (cdr list)))

(defun full-for-each (f list &rest lists)
  (if lists (for-each-loop f (cons list lists)) (for-each1-loop f list)))

;;; vector-map, vector-for-each, string-map, string-for-each: KIND says
;;; which; SEQUENCES are walked in step, to the shortest's length N.

(defun index-loop (kind f sequences i n acc captured)
  (loop
    (when (>= i n)
      (return (let ((acc (if captured (reverse acc) (nreverse acc))))
		(case kind
		  (:vector-map (coerce acc 'simple-vector))
		  (:string-map (coerce acc 'simple-string))
		  (t ps:unspecific)))))
    (let ((promoted nil) (j i))
      (let ((value (let ((frame (vector :resume nil 'index-continue kind f sequences i n acc)))
		     (declare (dynamic-extent frame))
		     (multiple-value-prog1
			 (with-frame (frame)
			   (if (cdr sequences)
			       (apply f (mapcar (lambda (s) (aref s j)) sequences))
			       (funcall f (aref (car sequences) j))))
		       (setq promoted (svref frame 1))))))
	(when (member kind '(:vector-map :string-map)) (push value acc))
	(when promoted (setq captured t))))
    (incf i)))

(defun index-continue (value kind f sequences i n acc)
  (index-loop kind f sequences (1+ i) n
	      (if (member kind '(:vector-map :string-map)) (cons value acc) acc)
	      t))

(defun full-index-loop (kind f sequence more)
  (let ((sequences (cons sequence more)))
    (index-loop kind f sequences 0 (reduce #'min sequences :key #'length) '() nil)))

;;; Calls to the other primitives that call procedures (*BARRIER-PRIMITIVES*)
;;; are made in a :BARRIER frame: re-entering a continuation captured in
;;; the procedure such a primitive called would resume as if the
;;; primitive had returned at once, so rebuilding the frame raises an
;;; error instead.  (Escaping through one is fine.)

(defmacro %barrier (name call)
  (let ((frame (gensym "FRAME")))
    `(let ((,frame (vector :barrier nil ',name)))
       (declare (dynamic-extent ,frame))
       (with-frame (,frame) ,call))))

(define-condition continuation-not-reentrant (control-error) ()
  (:report "continuation invoked after its extent ended, and not re-entrant (no base around its capture)"))

(defun escape-call/cc (f)
  "call/cc for a continuation only invoked during its extent: a catch,
in a frame that re-establishes the catch if a continuation captured
inside is re-entered (and so the continuation still works there)."
  (let* ((tag (heap-cons 'continuation nil))
	 (frame (vector :escape nil tag)))
    (declare (dynamic-extent frame))
    (catch tag
      (with-frame (frame)
	(funcall f (lambda (&rest values)
		     (if (and values (eq (car values) :pseudoscheme-continuation-query))
			 (cons tag nil)	; its identity (CONTINUATION=)
			 (throw tag (values-list values)))))))))

(defun continuation-procedure-p (f)
  "Whether F is a continuation FULL-CALL/CC or ESCAPE-CALL/CC made (as
its function's name tells; asking it would call it)."
  #+sbcl
  (and (sb-kernel:closurep f)
       (let ((name (sb-kernel:%fun-name (sb-kernel:%closure-fun f))))
	 (and (consp name) (member (car name) '(flet lambda))
	      (let ((in (member :in name)))
		(and in (member (second in) '(full-call/cc escape-call/cc)))))))
  #-sbcl nil)

(defun guard-reraise (k thunk)
  "GUARD's re-raise when no clause matches: re-enter the handler's
continuation K with THUNK, which re-raises there; or, where K can't be
re-entered (escape mode), call THUNK in the guard's own context."
  (handler-case (funcall k thunk)
    (control-error () (funcall thunk))))

;;; A frame's slot 1, PROMOTED, is (complete . copies) once a capture has
;;; copied it: COPIES the heap copies of it and of every frame outside
;;; it, innermost first, and COMPLETE whether they reach a base.  What is
;;; outside a frame doesn't change while the frame is on the stack, so a
;;; later capture copies only the frames pushed since, and shares the
;;; rest: repeated captures (generators, ctak) cost the new frames only.

(defvar *frame-copier* nil
  "NIL, or a function copying a frame of another kind than the ones here,
(FRAME INNERMOST-OF-ITS-KIND-P): the Guile VM's :VM frames, which copy the
VM's stack (src/guile/vm.lisp).")

(defvar *frame-resumer* nil
  "NIL, or a function resuming such a frame, as RESUME-FRAME does.")

(defun copy-frame (frame seen)
  "A copy of FRAME; SEEN is the frames inside it being copied too."
  (if (and *frame-copier* (eq (svref frame 0) :vm))
      (funcall *frame-copier* frame seen)
      (copy-seq frame)))

(defun capture-frames ()
  "Copies of the current frames, innermost first, and whether they reach
a base."
  (let ((new '()) (tail '()) (complete nil))
    (do ((s *fstack* (cdr s)))
	((null s))
      (let ((frame (car s)))
	(cond ((eq frame :base) (setq complete t) (return))
	      ((svref frame 1)
	       (setq complete (car (svref frame 1)) tail (cdr (svref frame 1)))
	       (return))
	      (t (push frame new)))))
    ;; NEW is outermost first: copy each onto TAIL, and promote it
    (dolist (frame new)
      (let ((copy (copy-frame frame (cdr (member frame new)))))
	(setq tail (cons copy tail))
	(setf (svref copy 1) (cons complete tail)
	      (svref frame 1) (svref copy 1))))
    (values tail complete)))

(defvar *rewinding* '()
  "While a continuation's frames are rebuilt: the winders being re-entered,
whose befores run as their frames are rebuilt.")

(defun resume-frame (frame inner)
  "Re-establish FRAME around INNER, a thunk computing what the frame's
call returns, then continue the frame."
  (let ((head (svref frame 0)))
    (case head
      (:k (multiple-value-call (svref frame 2) (with-frame (frame) (funcall inner))))
      (:winder (let ((winder (svref frame 2)))
		 (when (member winder *rewinding* :test #'eq)
		   (funcall (car winder)))
		 (winder-extent winder inner)))
      (:handler (handler-extent (car (svref frame 2)) (cdr (svref frame 2)) inner))
      (:handlers (handlers-extent (svref frame 2) inner))
      (:extent (call-in-extent (svref frame 2) inner))
      (:escape (catch (svref frame 2) (with-frame (frame) (funcall inner))))
      (:prompt (prompt-extent (vector :prompt nil (svref frame 2) (svref frame 3) *winders*) inner))
      (:resume (let ((value (with-frame (frame) (funcall inner))))
		 (apply (svref frame 2) value (coerce (subseq frame 3) 'list))))
      (:barrier (ps:scheme-error "a continuation can't be re-entered through ~A: it's written in Lisp"
				 (svref frame 2)))
      ((:vm :vm-prompt) (funcall *frame-resumer* frame inner))
      (t (let ((value (with-frame (frame) (funcall inner))))
	   (apply head (svref frame 2) value frame (make-list (svref frame 3))))))))

(defun rebuild-frames (frames values)
  "Re-establish FRAMES, outermost first, and return VALUES to the
innermost."
  (if (null frames)
      (values-list values)
      (resume-frame (car frames) (lambda () (rebuild-frames (cdr frames) values)))))

(defun shared-winders (a b)
  "The winders lists A and B (innermost first) have in common at their
outer ends, innermost first."
  (let ((ra (reverse a)) (rb (reverse b)) (shared '()))
    (loop while (and ra rb (eq (car ra) (car rb)))
	  do (push (pop ra) shared) (pop rb))
    shared))

(defun reenter (frames winders values shared)
  ;; Throwing to the base ran the afters of the winders that were active
  ;; but for SHARED, which the continuation's WINDERS end with.  The
  ;; befores of the others run as their frames are rebuilt, outermost
  ;; first, inside the frames outside them (a catch, say, that a before
  ;; throws to).
  (let ((*rewinding* (butlast winders (length shared))))
    (rebuild-frames (reverse frames) values)))

(defun check-no-barrier (here)
  "Signal an error if a :BARRIER frame that blocks escapes too, #(:barrier
promoted name :escapes), is pushed since HERE, a tail of *FSTACK*: a
continuation can't leave such a barrier (Guile's
with-continuation-barrier).  Other barriers only block re-entry."
  (loop for s on *fstack*
	until (eq s here)
	when (and (vectorp (car s)) (eq (svref (car s) 0) :barrier)
		  (> (length (car s)) 3) (eq (svref (car s) 3) :escapes))
	  do (ps:scheme-error "a continuation can't cross the continuation barrier of ~A"
			      (svref (car s) 2))))

(defun full-call/cc (f)
  (multiple-value-bind (frames rebuildable) (capture-frames)
    (let ((winders *winders*)
	  (live (list t))
	  (here *fstack*)
	  (tag (list 'continuation)))
      (flet ((k (&rest values)
	       ;; asked its identity (CONTINUATION=)
	       (when (and values (eq (car values) :pseudoscheme-continuation-query))
		 (return-from k (cons frames winders)))
	       (setq *shared-winders* '())
	       (cond ((car live) (check-no-barrier here) (throw tag (values-list values)))
		     ((and rebuildable *base-tag*)
		      (let ((shared (shared-winders *winders* winders)))
			(setq *shared-winders* shared)
			(throw *base-tag* (lambda () (reenter frames winders values shared)))))
		     (t (error 'continuation-not-reentrant)))))
	(unwind-protect (catch tag (funcall f #'k))
	  (setf (car live) nil))))))

;;; Delimited continuations: prompts, as Guile has them (call-with-prompt,
;;; abort-to-prompt), and Racket and SRFI 226 too.  A prompt is a frame,
;;; #(:prompt promoted catch-tag handler winders), with a catch of its
;;; own: aborting to it copies the frames pushed since (the continuation
;;; up to the prompt, not including it) and throws to the catch, whose
;;; afters run on the way; the handler is then called with a composable
;;; continuation.  Calling one rebuilds its frames on top of the current
;;; stack, as RESUME-FRAME does when a full continuation is re-entered,
;;; and returns what the outermost of them returns: there is no throw to
;;; a base, since the frames outside are the caller's own.

(defun prompt-extent (frame thunk)
  "Call THUNK inside prompt FRAME; on an abort to it, call its handler with
the composable continuation and the abort's values."
  (let ((abort (catch (svref frame 2)
		 (return-from prompt-extent (with-frame (frame) (funcall thunk))))))
    (apply (svref frame 3) abort)))

(defun call-with-prompt (tag thunk handler)
  (let ((frame (vector :prompt nil (heap-cons 'prompt tag) handler *winders*)))
    (declare (dynamic-extent frame))
    (prompt-extent frame thunk)))

(defun find-prompt (tag)
  (dolist (frame *fstack*)
    (when (and (vectorp frame) (eq (svref frame 0) :prompt) (eq (cdr (svref frame 2)) tag))
      (return frame))))

(defun fresh-frame (frame &optional (inside '()))
  "A copy of FRAME, unpromoted: what is outside it when it's rebuilt is
another continuation than when it was captured.  INSIDE is the frames
inside it being copied too."
  (let ((copy (copy-frame frame inside)))
    (setf (svref copy 1) nil)
    copy))

(defvar *outermost-composed* nil
  "While a composable continuation's frames are rebuilt: the outermost,
just inside the prompt it was captured up to.")

(defun compose-continuation (frames winders values)
  "Rebuild FRAMES (innermost first) on top of the current continuation,
running the befores of their WINDERS, outermost first, and return
VALUES to the innermost."
  (let* ((frames (reverse (mapcar #'fresh-frame frames)))
	 (*outermost-composed* (car frames))
	 ;; their befores run as their frames are rebuilt
	 (*rewinding* winders))
    (rebuild-frames frames values)))

(defun abort-to-prompt (tag &rest values)
  (let ((prompt (find-prompt tag)))
    (unless prompt
      (ps:scheme-error "abort-to-prompt: no prompt with tag ~S in the current continuation" tag))
    (let* ((frames (loop with inside = '()
			 for frame in *fstack*
			 until (eq frame prompt)
			 collect (prog1 (fresh-frame frame inside) (push frame inside))))
	   (winders (ldiff *winders* (svref prompt 4)))
	   (full *full-continuations*))
      (flet ((k (&rest values)
	       (unless full
		 (ps:scheme-error "a delimited continuation can't be called without full continuations"))
	       (compose-continuation frames winders values)))
	(throw (svref prompt 2) (cons #'k values))))))

(defun continuation= (a b)
  "Whether continuations A and B are the same continuation: captured with
nothing pushed or wound between (as a call in tail position is).
SRFI 226's sample implementation tells tail calls so."
  (or (eq a b)
      (and (functionp a) (functionp b)
	   (let ((ia (ignore-errors (funcall a :pseudoscheme-continuation-query)))
		 (ib (ignore-errors (funcall b :pseudoscheme-continuation-query))))
	     (and (consp ia) (consp ib)
		  (eq (car ia) (car ib)) (eq (cdr ia) (cdr ib)))))))

;;; What the transformation's output uses.  Each takes Scheme
;;; expressions, translated in place by the translator, and quoted data.

(defmacro %site (frame call)
  "Make CALL with FRAME, a (vector ...) form, pushed: both on the stack."
  (let ((f (gensym "FRAME")))
    `(let ((,f ,frame))
       (declare (dynamic-extent ,f))
       (with-pushed-frame (,f) ,call))))

(defmacro %machine (entry dispatch &rest statements)
  "A machine's body: STATEMENTS (expressions, and quoted symbols as
labels), entered at the start if ENTRY is 0, else at the label
DISPATCH, '((site . label) ...), gives for site ENTRY, where statements
restore the site's live variables and continue after it."
  `(block %machine
     (tagbody
	(case ,entry
	  ,@(loop for (site . label) in (second dispatch)
		  collect `(,site (go ,label))))
	,@(mapcar (lambda (s)
		    (if (and (consp s) (eq (car s) 'quote)) (second s) s))
		  statements))))

(defmacro %return (x) `(return-from %machine ,x))

;;; SBCL compiles a top-level form, closures and all, as one component,
;;; and some of its costs grow with the product of the component's
;;; functions and blocks.  On arm64 and x86-64, with DEBUG >= 1 and
;;; DEBUG >= SPEED, each function saves its binding stack pointer in a
;;; stack slot live across the whole component (INSERT-DEBUG-CATCH), so
;;; the register allocator's tables are #functions x #blocks.
(defparameter *full-policy*
  '(optimize #+sbcl (sb-c::insert-debug-catch 0)))

(defun call-with-full-policy (thunk)
  "Call THUNK, which compiles full-continuation code, under *FULL-POLICY*.
The global policy is bound, rather than a compilation unit made: THUNK
may also run the code it compiles, and leave by a non-local exit (a
program's exit), which a compilation unit would report as aborted."
  #+sbcl (let ((sb-c::*policy* (sb-c::process-optimize-decl *full-policy* sb-c::*policy*)))
	   (funcall thunk))
  #-sbcl (funcall thunk))

(defmacro %lifted (lambda)
  "LAMBDA, a closed procedure, compiled on its own (its own component)."
  `(load-time-value (locally (declare ,*full-policy*) ,lambda) t))
(defmacro %go (label) `(go ,(second label)))
(defmacro %ignorable (&rest variables)
  "Refer to VARIABLES, which a machine may not otherwise (its value and
frame parameters): no unused-variable style warnings."
  `(progn ,@variables nil))

;;; A resumed site's live variables are restored with this, not SVREF:
;;; SBCL's compile time grows exponentially with the number of sites
;;; when they're restored with an inline SVREF of the frame (a body of
;;; 20 calls took 16 s), and only resuming runs it.
(declaim (notinline %frame-ref))
(defun %frame-ref (frame i) (svref frame i))

;;; ------------------------------------------------------------------
;;; Which calls may capture

(defvar *full-continuations* t
  "True (the default) to compile with full, re-entrant continuations;
false for escape-only ones (call/cc as a catch, a continuation usable
only during the extent of its call), which compile a little faster and
run a little faster in code that calls unknown procedures in loops.")

(defparameter *full-replacements*
  '(("call-with-current-continuation" . "%full-call/cc")
    ("call/cc" . "%full-call/cc")
    ("dynamic-wind" . "%full-dynamic-wind")
    ("call-with-values" . "%full-call-with-values")
    ("map" . "%full-map")
    ("for-each" . "%full-for-each")
    ("vector-map" . "%full-vector-map")
    ("vector-for-each" . "%full-vector-for-each")
    ("string-map" . "%full-string-map")
    ("string-for-each" . "%full-string-for-each"))
  "Primitives that call procedures, and their frame-aware versions.")

(defparameter *calling-primitives*
  '("apply" "call-with-current-continuation" "call/cc" "dynamic-wind"
    "call-with-values" "map" "for-each" "%full-call/cc" "%full-dynamic-wind" "%full-call-with-values"
    "%full-map" "%full-for-each" "%call-with-frame"
    "%full-vector-map" "%full-vector-for-each" "%full-string-map" "%full-string-for-each"
    "%escape-call/cc" "call-with-prompt" "abort-to-prompt"
    "vector-map" "vector-for-each" "string-map" "string-for-each"
    "with-exception-handler" "raise" "raise-continuable" "force"
    "call-with-port" "call-with-input-file" "call-with-output-file"
    "with-input-from-file" "with-output-to-file" "eval" "load"
    "list-sort" "vector-sort" "vector-sort!" "find" "filter" "partition"
    "fold-left" "fold-right" "remp" "memp" "assp" "exists" "for-all"
    "member" "assoc" "hashtable-update!" "make-parameter" "%parameterize"
    "call-with-string-output-port" "call-with-bytevector-output-port"
    "call-with-output-string" "with-output-to-string" "with-input-from-string")
  "Primitives that may call a procedure, so that a call to one is a
call site like any other.  Calls to every other primitive are not.")

(defparameter *barrier-primitives*
  '("force" "call-with-port" "call-with-input-file" "call-with-output-file"
    "with-input-from-file" "with-output-to-file"
    "list-sort" "vector-sort" "vector-sort!" "find" "filter" "partition"
    "fold-left" "fold-right" "remp" "memp" "assp" "exists" "for-all"
    "member" "assoc" "hashtable-update!" "make-parameter"
    "call-with-string-output-port" "call-with-bytevector-output-port"
    "call-with-output-string" "with-output-to-string" "with-input-from-string")
  "Calling primitives with no frame-aware version, which do something
after the procedure they call returns: calls to them are made in a
barrier frame (%BARRIER).  member and assoc only with a predicate.")

(defparameter *adapter-primitives*
  '("void" "gensym" "symbol-value" "set-symbol-value!" "lisp-keyword?"
    "host-literal?" "pretty-print")
  "Host primitives of psyntax's adapter (src/psyntax.lisp), which call
no procedure.")

(defvar *primitive-names* nil)

(defun keyword-p (x name)
  (and (symbolp x) (string= (symbol-name x) name)))

(defun scheme-symbol-p (x)
  (and (symbolp x) x (eq (symbol-package x) (find-package "SCHEME"))))

(defun primitive-names ()
  (or *primitive-names*
      (let ((table (make-hash-table :test 'equal)))
	(dolist (s (all-primitive-names)) (setf (gethash (ps:scheme-symbol-name s) table) t))
	(dolist (p ps-r6rs:*primitives*) (setf (gethash (car p) table) t))
	(dolist (p *full-replacements*)
	  (setf (gethash (car p) table) t (gethash (cdr p) table) t))
	(setf (gethash "%call-with-frame" table) t (gethash "%escape-call/cc" table) t
	      (gethash "call-with-prompt" table) t (gethash "abort-to-prompt" table) t)
	(dolist (name *adapter-primitives*) (setf (gethash name table) t))
	(dolist (name *registered-primitive-names*) (setf (gethash name table) t))
	(setq *primitive-names* table))))

(defvar *registered-primitive-names* '()
  "Host primitives other layers define (src/guile/) that call no
procedure, so that calls to them aren't call sites.")

(defun register-primitive-names (names)
  (setq *registered-primitive-names* (union names *registered-primitive-names* :test #'string=)
	*primitive-names* nil))

(defun primitive-name (x)
  "The name of primitive X, if X refers to one: a host global with a
primitive's name (psyntax renames everything else), or (primitive x)."
  (cond ((and (consp x) (keyword-p (car x) "PRIMITIVE")) (ps:scheme-symbol-name (cadr x)))
	((scheme-symbol-p x)
	 (let ((name (ps:scheme-symbol-name x)))
	   (and (gethash name (primitive-names)) name)))))

(defvar *known-procedures* nil
  "A hash table of the lexical variables, in the top-level form being
transformed, bound once and for all to a lambda, to the lambda; or NIL.")

(defvar *downward-parameters* nil
  "A hash table of the known procedures' variables to a list saying, for
each parameter, whether the procedure only calls it or passes it on to
such a parameter (see FIND-DOWNWARD-PARAMETERS); or NIL.")

(defvar *safe-procedures* nil
  "A hash table of the lexical variables, in the top-level form being
transformed, that are bound to procedures that can't capture a
continuation (see FIND-SAFE-PROCEDURES); or NIL.")

(defun calling-call-p (operator)
  "Whether a call with OPERATOR (normalized) may capture.  Operators that
are Lisp symbols are the transformation's own, or the translator's."
  (cond ((and (symbolp operator) (not (scheme-symbol-p operator))) nil)
	((and *safe-procedures* (symbolp operator) (gethash operator *safe-procedures*)) nil)
	(t (let ((name (primitive-name operator)))
	     (or (null name) (member name *calling-primitives* :test #'string=))))))

;;; Procedures that can't capture.  A call is a site only because the
;;; callee might, through some chain of calls, reach call/cc, so a call
;;; to a procedure known never to needs no frame.  Within one top-level form
;;; (a program's or a library's body is one: psyntax makes its
;;; definitions a letrec*), a variable bound to a lambda and never
;;; assigned otherwise is known.  Its procedure is safe if nothing it
;;; calls, in any position, can capture: primitives that don't call
;;; procedures, and other safe procedures.  Assuming every known
;;; procedure safe and striking out those that call something unsafe,
;;; until none changes, gives the largest consistent set, which is
;;; right: a cycle of calls among procedures that call nothing else
;;; never reaches call/cc.  So (define (fib n) ... (fib (- n 1)) ...)
;;; makes no frames and needs no machine.
;;;
;;; Raising an error isn't a site: the handler of a non-continuable
;;; raise can't return to the raiser, so a continuation captured in it
;;; never needs the raiser's frame.  (raise-continuable is a site.)

(defun find-safe-procedures (form)
  "A hash table of the variables bound in FORM to safe procedures."
  (let ((assignments (make-hash-table :test 'eq))	; variable -> number of set!s
	(lambdas (make-hash-table :test 'eq))		; variable -> its lambda, if known
	(placeholders (make-hash-table :test 'eq)))	; variable -> bound to '#f
    (labels ((false-p (x) (equal x `(,(sym "quote") ,ps:false)))
	     (scan (e)
	       (cond ((atom e))
		     ((quote-form-p e))
		     ((lambda-form-p e) (scan (caddr e)))
		     ((keyword-p (car e) "SET!")
		      (incf (gethash (cadr e) assignments 0))
		      (when (lambda-form-p (caddr e))
			(push (caddr e) (gethash (cadr e) lambdas)))
		      (scan (caddr e)))
		     ;; a definition HOIST-DEFINITIONS made of a body's
		     ((keyword-p (car e) "DEFINE")
		      (when (and (member (cadr e) *hoisted-procedures*) (lambda-form-p (caddr e)))
			(push (caddr e) (gethash (cadr e) lambdas)))
		      (scan (caddr e)))
		     ((keyword-p (car e) "LETREC")
		      (dolist (b (cadr e))
			(when (lambda-form-p (cadr b)) (push (cadr b) (gethash (car b) lambdas)))
			(scan (cadr b)))
		      (scan (caddr e)))
		     ((let-form-p e)
		      (loop for v in (cadr (car e)) for a in (cdr e)
			    do (cond ((lambda-form-p a) (push a (gethash v lambdas)))
				     ((false-p a) (setf (gethash v placeholders) t))))
		      (dolist (a (cdr e)) (scan a))
		      (scan (caddr (car e))))
		     (t (loop for x on e
			      do (scan (car x))
				 (unless (listp (cdr x)) (scan (cdr x)))))))
	     (known-lambda (v)
	       ;; A lambda bound to V once and for all: by its binding with
	       ;; no assignment, or by one assignment of a '#f placeholder.
	       (let ((ls (gethash v lambdas)) (n (gethash v assignments 0)))
		 (and ls (null (cdr ls))
		      (or (zerop n) (and (= n 1) (gethash v placeholders)))
		      (car ls)))))
      (scan form)
      (let ((safe (make-hash-table :test 'eq))
	    (known (make-hash-table :test 'eq))
	    (bodies '()))
	(maphash (lambda (v ls)
		   (declare (ignore ls))
		   (let ((l (known-lambda v)))
		     (when l
		       (setf (gethash v safe) t (gethash v known) l)
		       (push (cons v (caddr l)) bodies))))
		 lambdas)
	(setq *known-procedures* known)
	(let ((*safe-procedures* safe))
	  (loop while (loop with changed = nil
			    for (v . body) in bodies
			    when (and (gethash v safe) (may-capture-p body))
			      do (remhash v safe) (setq changed t)
			    finally (return changed))))
	safe))))

;;; Continuations that are only ever invoked during the extent of their
;;; call/cc.  In (call/cc (lambda (k) body)), if BODY only calls K, or
;;; passes it to a parameter of a known procedure that only calls it or
;;; passes it on in the same way, K can't outlive the call/cc: it isn't
;;; stored, returned, closed over or handed to unknown code.  Such a
;;; call/cc (ctak's and fibc's, early exits) needs no capture of the
;;; frames, only a catch (%ESCAPE-CALL/CC).

(defvar *assumed-only-called* '()
  "Local procedures' variables taken to be only called while checking
that they are.")

(defun only-called-p (x body)
  "Whether BODY refers to variable X only to call it, or to pass it to a
downward parameter of a known procedure.  A reference inside a lambda
counts only where the lambda can't outlive BODY's evaluation either: a
procedure argument of one of *PROCEDURE-ARGUMENTS*, or a letrec-bound
procedure that is itself only called."
  (labels ((ok (e)
	     (cond ((eq e x) nil)	; a reference that isn't a call
		   ((atom e) t)
		   ((quote-form-p e) t)
		   ((lambda-form-p e) (not (occurs-p x e)))
		   ((keyword-p (car e) "SET!") (and (not (eq (cadr e) x)) (ok (caddr e))))
		   ((or (keyword-p (car e) "IF") (keyword-p (car e) "BEGIN"))
		    (every #'ok (cdr e)))
		   ((keyword-p (car e) "LETREC") (ok-letrec e nil))
		   ((lambda-form-p (car e))
		    (and (ok (caddr (car e))) (every #'ok (cdr e))))
		   ;; a named let: ((letrec ((loop (lambda ...))) loop) arg ...)
		   ((and (consp (car e)) (keyword-p (caar e) "LETREC") (symbolp (caddr (car e))))
		    (and (ok-letrec (car e) (caddr (car e))) (every #'ok (cdr e))))
		   ((eq (car e) x) (every #'ok (cdr e)))
		   ((and (symbolp (car e)) *downward-parameters* (gethash (car e) *downward-parameters*))
		    (let ((flags (gethash (car e) *downward-parameters*)))
		      (loop for a in (cdr e) for i from 0
			    always (if (eq a x) (nth i flags) (ok a)))))
		   ((procedure-positions e)
		    (let ((positions (procedure-positions e)))
		      (and (ok (car e))
			   (loop for a in (cdr e) for i from 0
				 always (if (and (member i positions) (lambda-form-p a))
					    (ok (caddr a))
					    (ok a))))))
		   (t (every #'ok e))))
	   (ok-letrec (e called)
	     ;; CALLED: E's body, a bound variable that the caller calls
	     (let ((*assumed-only-called* (append (mapcar #'car (cadr e)) *assumed-only-called*)))
	       (and (every (lambda (b)
			     (if (and (lambda-form-p (cadr b))
				      (or (member (car b) *assumed-only-called*)))
				 (and (local-only-called-p (car b) e called)
				      (ok (caddr (cadr b))))
				 (ok (cadr b))))
			   (cadr e))
		    (or (eq (caddr e) called) (ok (caddr e)))))))
    (ok body)))

(defun local-only-called-p (v letrec called)
  "Whether LETREC's bound variable V is only called in it (counting its
body being V, when the letrec is called, as a call)."
  (and (every (lambda (b)
		(if (lambda-form-p (cadr b))
		    (only-called-p v (caddr (cadr b)))
		    (only-called-p v (cadr b))))
	      (cadr letrec))
       (or (and called (eq (caddr letrec) v))
	   (only-called-p v (caddr letrec)))))

(defun procedure-positions (call)
  "If CALL is to one of *PROCEDURE-ARGUMENTS*, the positions of its
procedure arguments."
  (let ((name (and (consp call) (primitive-name (car call)))))
    (and name (cdr (assoc name *procedure-arguments* :test #'string=)))))

(defun occurs-p (x e)
  (cond ((eq x e) t)
	((atom e) nil)
	((quote-form-p e) nil)
	(t (loop for y on e
		 thereis (or (occurs-p x (car y))
			     (and (not (listp (cdr y))) (eq x (cdr y))))))))

(defun find-downward-parameters ()
  "Fill *DOWNWARD-PARAMETERS* for *KNOWN-PROCEDURES*: assume every
parameter downward, and strike out those whose procedure lets them
escape, until none changes."
  (let ((table (make-hash-table :test 'eq)))
    (when *known-procedures*
      (maphash (lambda (v l)
		 (let ((formals (cadr l)))
		   (when (and (listp formals) (null (cdr (last formals))))
		     (setf (gethash v table) (make-list (length formals) :initial-element t)))))
	       *known-procedures*)
      (let ((*downward-parameters* table))
	(loop while (let ((changed nil))
		      (maphash (lambda (v flags)
				 (let ((l (gethash v *known-procedures*)))
				   (loop for cell on flags for x in (cadr l)
					 when (and (car cell) (not (only-called-p x (caddr l))))
					   do (setf (car cell) nil changed t))))
			       table)
		      changed))))
    table))

(defun escape-call/cc-p (call)
  "Whether CALL is (call/cc (lambda (k) body)) with K only called."
  (and (member (primitive-name (car call)) '("call-with-current-continuation" "call/cc")
	       :test #'equal)
       (= (length call) 2)
       (lambda-form-p (cadr call))
       (let ((formals (cadr (cadr call))))
	 (and (consp formals) (null (cdr formals)) (symbolp (car formals))
	      (only-called-p (car formals) (caddr (cadr call)))))))

(defparameter *procedure-arguments*
  '(("map" 0) ("for-each" 0) ("vector-map" 0) ("vector-for-each" 0)
    ("string-map" 0) ("string-for-each" 0) ("apply" 0) ("call-with-values" 0 1)
    ("find" 0) ("filter" 0) ("partition" 0) ("fold-left" 0) ("fold-right" 0)
    ("remp" 0) ("memp" 0) ("assp" 0) ("exists" 0) ("for-all" 0)
    ("list-sort" 0) ("vector-sort" 0) ("vector-sort!" 0)
    ("member" 2) ("assoc" 2) ("hashtable-update!" 2))
  "Calling primitives that call only the procedures passed to them, and
which arguments those are (counting from 0).  A call to one passing only
safe procedures can't capture.")

(defun safe-procedure-p (e)
  "Whether E evaluates to a procedure whose calls can't capture: a safe
procedure's variable, a primitive that calls no procedure, or a lambda
whose body can't capture."
  (cond ((lambda-form-p e) (not (may-capture-p (caddr e))))
	((and *safe-procedures* (symbolp e) (gethash e *safe-procedures*)) t)
	((or (scheme-symbol-p e) (and (consp e) (keyword-p (car e) "PRIMITIVE")))
	 (let ((name (primitive-name e)))
	   (and name (not (member name *calling-primitives* :test #'string=)))))))

(defun safe-primitive-call-p (call)
  "Whether CALL is to one of *PROCEDURE-ARGUMENTS* with safe procedures."
  (let* ((name (primitive-name (car call)))
	 (positions (and name (cdr (assoc name *procedure-arguments* :test #'string=)))))
    (and positions
	 (every (lambda (i) (let ((arg (nthcdr (1+ i) call))) (or (null arg) (safe-procedure-p (car arg)))))
		positions))))

(defun plain-primitive-call-p (call)
  "Whether CALL, to a primitive with a frame-aware version, can use the
primitive itself: no procedure passed to it can capture, or for
call-with-values, the producer can't (the consumer is called in tail
position)."
  (or (safe-primitive-call-p call)
      (and (equal (primitive-name (car call)) "call-with-values")
	   (cdr call) (safe-procedure-p (cadr call)))))

(defun capturing-call-p (call)
  "Whether CALL, a combination, may capture."
  (and (calling-call-p (car call)) (not (safe-primitive-call-p call))))

(defun may-capture-p (e)
  "Whether evaluating E (not the bodies of the lambdas in it, unless
called on the spot) may make a call that captures, in any position."
  (cond ((atom e) nil)
	((quote-form-p e) nil)
	((lambda-form-p e) nil)
	((keyword-p (car e) "PRIMITIVE") nil)
	((or (keyword-p (car e) "IF") (keyword-p (car e) "BEGIN"))
	 (some #'may-capture-p (cdr e)))
	((or (keyword-p (car e) "SET!") (keyword-p (car e) "DEFINE"))
	 (may-capture-p (caddr e)))
	((keyword-p (car e) "LETREC")
	 (or (some (lambda (b) (may-capture-p (cadr b))) (cadr e))
	     (may-capture-p (caddr e))))
	((lambda-form-p (car e))
	 (or (may-capture-p (caddr (car e))) (some #'may-capture-p (cdr e))))
	(t (or (capturing-call-p e)
	       (may-capture-p (car e))
	       (some #'may-capture-p (cdr e))))))

(defun lambda-form-p (x) (and (consp x) (keyword-p (car x) "LAMBDA")))
(defun quote-form-p (x) (and (consp x) (keyword-p (car x) "QUOTE")))

(defun let-form-p (e)
  "((lambda (v ...) body) arg ...), with as many arguments as variables."
  (and (consp e) (lambda-form-p (car e))
       (listp (cadr (car e))) (null (cdr (last (cadr (car e)))))
       (= (length (cadr (car e))) (length (cdr e)))))

(defun atomic-p (x)
  (or (atom x) (quote-form-p x) (lambda-form-p x)
      (and (consp x) (keyword-p (car x) "PRIMITIVE"))))

(defun simple-p (e)
  "Whether E makes no call that may capture (outside nested lambdas)."
  (cond ((atomic-p e) t)
	(t (let ((head (car e)))
	     (cond ((or (keyword-p head "IF") (keyword-p head "BEGIN"))
		    (every #'simple-p (cdr e)))
		   ((or (keyword-p head "SET!") (keyword-p head "DEFINE"))
		    (simple-p (caddr e)))
		   ((keyword-p head "LETREC")
		    (and (every (lambda (b) (simple-p (cadr b))) (cadr e))
			 (simple-p (caddr e))))
		   ((lambda-form-p head)
		    (and (simple-p (caddr head)) (every #'simple-p (cdr e))))
		   (t (and (not (capturing-call-p e)) (every #'simple-p (cdr e)))))))))

(defun wrap-barrier (call)
  "CALL, in a barrier frame if its operator is one of *BARRIER-PRIMITIVES*."
  (let ((name (and (consp call) (primitive-name (car call)))))
    (if (and name (member name *barrier-primitives* :test #'string=)
	     (not (and (member name '("member" "assoc") :test #'string=)
		       (< (length call) 4))))
	`(%barrier ,name ,call)
	call)))

(defun replacement (x)
  (let ((name (primitive-name x)))
    (let ((new (and name (cdr (assoc name *full-replacements* :test #'string=)))))
      (if new (sym new) x))))

(defun fresh ()
  (funcall (host-ref "gensym")))

(defun formal-variables (formals)
  (cond ((null formals) '())
	((symbolp formals) (list formals))
	(t (cons (car formals) (formal-variables (cdr formals))))))

;;; ------------------------------------------------------------------
;;; Boxing assigned variables
;;;
;;; A variable that is assigned, bound in a procedure that becomes a
;;; machine, is boxed (a cons): the frame then holds the box, which a
;;; re-entered frame shares.  Exempt are the variables psyntax's letrec*
;;; (internal definitions) assigns once, when no site can run between
;;; the variable's binding and its assignment.

(defun box-assigned (form)
  (let ((counts (make-hash-table :test 'eq))	; lexical -> assignments
	(binder (make-hash-table :test 'eq))	; lexical -> machine-p of its procedure
	(exempt (make-hash-table :test 'eq))
	(boxed (make-hash-table :test 'eq))
	(lifted (make-hash-table :test 'eq))	; lexical -> free in a chunk to be lifted
	(lexicals (and *lift-chunks* (lexical-variables form))))
    (labels ((scan (e machine-p)
	       ;; MACHINE-P: whether the enclosing procedure has sites
	       (when (and *lift-chunks* (chunk-call-p e))
		 (dolist (v (free-lexicals e lexicals)) (setf (gethash v lifted) t)))
	       (cond ((atom e))
		     ((quote-form-p e))
		     ((lambda-form-p e)
		      (let ((m (not (tail-simple-p (caddr e)))))
			(dolist (v (formal-variables (cadr e))) (setf (gethash v binder) m))
			(scan (caddr e) m)))
		     ((let-form-p e)
		      (dolist (v (cadr (car e))) (setf (gethash v binder) machine-p))
		      (note-letrec* e)
		      (dolist (a (cdr e)) (scan a machine-p))
		      (scan (caddr (car e)) machine-p))
		     ((keyword-p (car e) "SET!")
		      (incf (gethash (cadr e) counts 0))
		      (scan (caddr e) machine-p))
		     ((keyword-p (car e) "LETREC")
		      (dolist (b (cadr e))
			(setf (gethash (car b) binder) machine-p)
			(scan (cadr b) machine-p))
		      (scan (caddr e) machine-p))
		     (t (dolist (x e) (scan x machine-p)))))
	     (note-letrec* (e)
	       ;; ((lambda (v ...) (begin (set! v e) ... body)) '#f ...)
	       (let ((vars (cadr (car e))) (body (caddr (car e))))
		 (when (and (every (lambda (a) (equal a `(,(sym "quote") ,ps:false))) (cdr e))
			    (consp body) (keyword-p (car body) "BEGIN"))
		   (loop for s in (cdr body)
			 while (and (consp s) (keyword-p (car s) "SET!") (member (cadr s) vars))
			 while (simple-p (caddr s))
			 do (setf (gethash (cadr s) exempt) t)))))
	     (rewrite (e)
	       (cond ((and (symbolp e) (gethash e boxed)) `(car ,e))
		     ((atom e) e)
		     ((quote-form-p e) e)
		     ((lambda-form-p e)
		      (let* ((formals (cadr e))
			     (bvs (remove-if-not (lambda (v) (gethash v boxed)) (formal-variables formals)))
			     (body (rewrite (caddr e))))
			`(,(car e) ,formals
			  ,(if bvs
			       `((,(sym "lambda") ,bvs ,body) ,@(mapcar (lambda (v) `(list ,v)) bvs))
			       body))))
		     ((and (keyword-p (car e) "SET!") (gethash (cadr e) boxed))
		      `(rplaca ,(cadr e) ,(rewrite (caddr e))))
		     ((keyword-p (car e) "SET!")
		      `(,(car e) ,(cadr e) ,(rewrite (caddr e))))
		     ((keyword-p (car e) "LETREC")
		      `(,(car e) ,(mapcar (lambda (b)
					    (list (car b)
						  (if (gethash (car b) boxed)
						      `(list ,(rewrite (cadr b)))
						      (rewrite (cadr b)))))
					  (cadr e))
			,(rewrite (caddr e))))
		     (t (mapcar #'rewrite e)))))
      (scan form (not (tail-simple-p form)))
      (maphash (lambda (v n)
		 (when (and (plusp n)
			    (or (gethash v lifted)
				(and (gethash v binder)
				     (not (and (= n 1) (gethash v exempt))))))
		   (setf (gethash v boxed) t)))
	       counts)
      ;; A boxed letrec-bound variable is bound to a box of its value.
      (if (zerop (hash-table-count boxed)) form (rewrite form)))))

;;; ------------------------------------------------------------------
;;; Simple expressions, and procedures

(defun letrec-bindings (variable value)
  "Bindings for a letrec that binds VARIABLE to VALUE, transformed: a
machine, (letrec ((m (lambda ...))) (lambda ...)), contributes its own
binding too, so that a letrec of procedures stays one of lambdas, which
the translator makes LABELS functions."
  (if (and (consp value) (keyword-p (car value) "LETREC") (lambda-form-p (caddr value))
	   (every (lambda (b) (lambda-form-p (cadr b))) (cadr value)))
      (append (cadr value) (list (list variable (caddr value))))
      (list (list variable value))))

(defun simple (e)
  "Simple E with its lambdas transformed and its references to the
primitives in *FULL-REPLACEMENTS* replaced."
  (cond ((quote-form-p e) e)
	((and (consp e) (keyword-p (car e) "PRIMITIVE")) (replacement e))
	((symbolp e) (if (scheme-symbol-p e) (replacement e) e))
	((atom e) e)
	((lambda-form-p e) (transform-lambda e))
	((keyword-p (car e) "LETREC")
	 `(,(car e) ,(mapcan (lambda (b) (letrec-bindings (car b) (transform-bound-lambda (car b) (cadr b))))
			     (cadr e))
	   ,(simple (caddr e))))
	((or (keyword-p (car e) "SET!") (keyword-p (car e) "DEFINE"))
	 `(,(car e) ,(cadr e) ,(simple (caddr e))))
	((plain-primitive-call-p e) (cons (car e) (mapcar #'simple (cdr e))))
	((escape-call/cc-p e) (cons (sym "%escape-call/cc") (mapcar #'simple (cdr e))))
	(t (wrap-barrier (mapcar #'simple e)))))

(defun tail-simple-p (e)
  "Whether E makes no call that may capture except in tail position: a
procedure with such a body needs no machine."
  (cond ((simple-p e) t)
	(t (let ((head (car e)))
	     (cond ((keyword-p head "IF")
		    (and (simple-p (cadr e)) (tail-simple-p (caddr e)) (tail-simple-p (cadddr e))))
		   ((keyword-p head "BEGIN")
		    (and (every #'simple-p (butlast (cdr e))) (tail-simple-p (car (last e)))))
		   ((keyword-p head "LETREC")
		    (and (every (lambda (b) (simple-p (cadr b))) (cadr e))
			 (tail-simple-p (caddr e))))
		   ((let-form-p e)
		    (and (every #'simple-p (cdr e)) (tail-simple-p (caddr head))))
		   ((or (keyword-p head "SET!") (keyword-p head "DEFINE")) nil)
		   (t (every #'simple-p e)))))))

(defun transform-lambda (e &optional self)
  "Lambda E transformed.  SELF: the variable a letrec or definition binds
to E, if E is a known procedure."
  (let ((formals (cadr e)) (body (caddr e)))
    (if (tail-simple-p body)
	`(,(car e) ,formals ,(simple body))
	(build-machine formals body
		       (and self (listp formals) (null (cdr (last formals)))
			    (not (makes-closures-p body))
			    self)))))

(defun makes-closures-p (e)
  "Whether evaluating E can make a closure (a lambda not called on the
spot)."
  (cond ((atom e) nil)
	((quote-form-p e) nil)
	((lambda-form-p e) t)
	((lambda-form-p (car e)) (or (makes-closures-p (caddr (car e))) (some #'makes-closures-p (cdr e))))
	(t (loop for x on e thereis (makes-closures-p (car x)) while (consp (cdr x))))))

(defun transform-bound-lambda (variable value)
  "VALUE, bound to VARIABLE by a letrec or definition, transformed."
  (if (lambda-form-p value)
      (transform-lambda value (and *known-procedures* (gethash variable *known-procedures*) variable))
      (simple value)))

;;; ------------------------------------------------------------------
;;; Machines

(defstruct (machine (:conc-name m-))
  (self nil)				; the procedure's variable, for self tail calls
  (params '())
  (statements '())			; reversed
  (locals '())
  (letrecs '())				; (variable procedure), reversed
  (sites '())				; (site label variable)
  (count 0)
  (labels 0))

(defvar *m*)

(defun emit (statement) (push statement (m-statements *m*)))

(defun new-local ()
  (let ((v (fresh))) (push v (m-locals *m*)) v))

(defun new-label-named (name)
  (intern name "PSEUDOSCHEME-PSYNTAX"))

(defun new-label ()
  (intern (format nil "%L~D" (incf (m-labels *m*))) "PSEUDOSCHEME-PSYNTAX"))

(defun finish (x k)
  (cond ((eq k :return) (emit `(%return ,x)))
	((eq k :drop) (unless (atomic-p x) (emit x)))
	(t (emit `(,(sym "set!") ,(cdr k) ,x)))))

(defun value-of (e)
  "A simple expression for E's value, emitting what computes it."
  (if (simple-p e)
      (simple e)
      (let ((v (new-local)))
	(flat e (cons :set v))
	v)))

(defun flat (e k)
  "Emit statements that compute E and, as K says, return its value
(:RETURN), drop it (:DROP), or assign it to a variable ((:SET . v))."
  (if (simple-p e)
      (finish (simple e) k)
      (let ((head (car e)))
	(cond ((keyword-p head "IF") (flat-if e k))
	      ((keyword-p head "BEGIN")
	       (loop for (x . more) on (cdr e) do (flat x (if more :drop k))))
	      ((keyword-p head "SET!")
	       (finish `(,head ,(cadr e) ,(value-of (caddr e))) k))
	      ((and (keyword-p head "LETREC") (every (lambda (b) (lambda-form-p (cadr b))) (cadr e)))
	       ;; procedures: bound around the machine's statements, where
	       ;; everything they can refer to is in scope (the locals are
	       ;; hoisted), so that the translator makes them LABELS
	       ;; functions rather than assigned locals
	       (dolist (b (cadr e))
		 (dolist (binding (letrec-bindings (car b) (transform-bound-lambda (car b) (cadr b))))
		   (push binding (m-letrecs *m*))))
	       (flat (caddr e) k))
	      ((keyword-p head "LETREC")
	       (dolist (b (cadr e))
		 (push (car b) (m-locals *m*))
		 (flat `(,(sym "set!") ,(car b) ,(cadr b)) :drop))
	       (flat (caddr e) k))
	      ((let-form-p e)
	       (loop for v in (cadr head) for a in (cdr e)
		     do (push v (m-locals *m*))
			(flat a (cons :set v)))
	       (flat (caddr head) k))
	      (t (flat-call e k))))))

(defun flat-if (e k)
  (let ((c (value-of (cadr e)))
	(else (new-label)))
    (emit `(,(sym "if") ,c (,(sym "quote") ,ps:false) (%go ',else)))
    (if (eq k :return)
	(progn (flat (caddr e) :return)
	       (emit `',else)
	       (flat (cadddr e) :return))
	(let ((end (new-label)))
	  (flat (caddr e) k)
	  (emit `(%go ',end))
	  (emit `',else)
	  (flat (cadddr e) k)
	  (emit `',end)))))

(defun flat-call (e k)
  (let ((xs (cond ((plain-primitive-call-p e)
		   ;; the primitive itself, not its frame-aware version
		   (cons (car e) (cdr (operands e))))
		  ((escape-call/cc-p e) (cons (sym "%escape-call/cc") (cdr (operands e))))
		  (t (wrap-barrier (operands e))))))
    (cond ((not (capturing-call-p e))
	   (finish xs k))
	  ((and (eq k :return) (m-self *m*) (eq (car xs) (m-self *m*))
		(= (length (cdr xs)) (length (m-params *m*))))
	   ;; a tail call to itself: assign the parameters and start over
	   (let ((temps (mapcar (lambda (a)
				  (let ((v (new-local)))
				    (emit `(,(sym "set!") ,v ,a))
				    v))
				(cdr xs))))
	     (loop for p in (m-params *m*) for v in temps
		   do (emit `(,(sym "set!") ,p ,v)))
	     (emit `(%go ',(new-label-named "%ENTRY")))))
	  ((eq k :return) (emit `(%return ,xs)))
	  (t (let ((site (incf (m-count *m*)))
		   (label (new-label))
		   (var (and (consp k) (cdr k))))
	       (emit (if var
			 `(,(sym "set!") ,var (%site ,site ,xs))
			 `(%site ,site ,xs)))
	       (emit `',label)
	       (push (list site label var) (m-sites *m*)))))))

(defun operands (es)
  "Simple expressions for ES, left to right.  One that isn't atomic is
bound to a variable first if one after it may capture, to keep order."
  (let ((out '()))
    (loop for (e . more) on es
	  do (if (simple-p e)
		 (push (simple e) out)
		 (progn
		   (setq out (mapcar (lambda (x)
				       (if (atomic-p x)
					   x
					   (let ((v (new-local)))
					     (emit `(,(sym "set!") ,v ,x))
					     v)))
				     (reverse out)))
		   (setq out (reverse out))
		   (push (value-of e) out))))
    (reverse out)))

(defun variables-in (e table)
  "The variables of TABLE that E refers to (or assigns)."
  (let ((found '()))
    (labels ((walk (x)
	       (cond ((symbolp x) (when (gethash x table) (pushnew x found)))
		     ((atom x))
		     ((quote-form-p x))
		     (t (loop for y on x
			      do (walk (car y))
				 (unless (listp (cdr y)) (walk (cdr y))))))))
      (walk e))
    found))

(defun build-machine (formals body &optional self)
  "The machine for a procedure with FORMALS and BODY.  SELF: its
variable, if its tail calls to itself can be jumps back to its start
(its parameters are a proper list, and its body makes no closures,
which would capture bindings the jump reuses)."
  (let* ((params (formal-variables formals))
	 (*m* (make-machine :self self :params params)))
    (flat body :return)
    (let* ((statements (coerce (reverse (m-statements *m*)) 'vector))
	   (locals (remove-duplicates (set-difference (m-locals *m*) params)))
	   (vars (make-hash-table :test 'eq))
	   (first-def (make-hash-table :test 'eq))
	   (last-use (make-hash-table :test 'eq))
	   (m (fresh)) (entry (fresh)) (value (fresh)) (frame (fresh))
	   (n (length params)))
      (dolist (v params) (setf (gethash v vars) t (gethash v first-def) -1))
      (dolist (v locals) (setf (gethash v vars) t))
      ;; Liveness: control only jumps forward, so a variable is live at a
      ;; site if it was assigned before it and is referred to after it.
      (loop for s across statements for i from 0
	    do (let ((assigned (and (consp s) (keyword-p (car s) "SET!") (cadr s))))
		 (when (and assigned (gethash assigned vars) (not (gethash assigned first-def)))
		   (setf (gethash assigned first-def) i))
		 (dolist (v (variables-in (if assigned (caddr s) s) vars))
		   (setf (gethash v last-use) i))))
      ;; The procedures bound around the statements may be called after
      ;; any site: what they refer to is live to the end.
      (dolist (v (variables-in (mapcar #'cadr (m-letrecs *m*)) vars))
	(setf (gethash v last-use) (length statements)))
      (let ((dispatch '()) (resumes '()))
	(loop for s across statements for i from 0
	      do (let* ((site-form (cond ((and (consp s) (eq (car s) '%site)) s)
					 ((and (consp s) (keyword-p (car s) "SET!")
					       (consp (caddr s)) (eq (car (caddr s)) '%site))
					  (caddr s))))
			(site (and site-form (second site-form))))
		   (when site
		     (let* ((info (assoc site (m-sites *m*)))
			    (var (third info))
			    (live (loop for v being the hash-keys of vars
					when (and (not (eq v var))
						  (let ((d (gethash v first-def)) (u (gethash v last-use)))
						    (and d u (< d i) (> u i))))
					  collect v)))
		       (setf (second site-form)
			     `(vector ,m (,(sym "quote") ()) ,site ,n ,@live))
		       ;; Resuming the site: restore its live variables, set
		       ;; its variable to the value returned, and go on after it.
		       (let ((resume (new-label)))
			 (push (cons site resume) dispatch)
			 (setq resumes
			       (append resumes
				       `(',resume
					 ,@(loop for v in live for j from 4
						 collect `(,(sym "set!") ,v (%frame-ref ,frame ,j)))
					 ,@(when var `((,(sym "set!") ,var ,value)))
					 (%go ',(second info))))))))))
	`(,(sym "letrec")
	  ((,m (,(sym "lambda") (,entry ,value ,frame ,@params)
		((,(sym "lambda") ,locals
		  ,(let ((body `(%machine ,entry (,(sym "quote") ,dispatch)
					   (%ignorable ,value ,frame)
					   ,@(when (or resumes self) `((%go ',(new-label-named "%ENTRY"))))
					   ,@resumes
					   ,@(when (or resumes self) `(',(new-label-named "%ENTRY")))
					   ,@(coerce statements 'list))))
		     (if (m-letrecs *m*)
			 `(,(sym "letrec") ,(reverse (m-letrecs *m*)) ,body)
			 body)))
		 ,@(mapcar (lambda (v) (declare (ignore v)) `(,(sym "quote") ,ps:false)) locals)))))
	  (,(sym "lambda") ,formals
	   (,m 0 (,(sym "quote") ,ps:false) (,(sym "quote") ,ps:false) ,@params)))))))

;;; ------------------------------------------------------------------
;;; Keeping machines small
;;;
;;; A procedure's machine is one Lisp function, its locals hoisted to its
;;; top; SBCL's analyses are superlinear in the size of a function, so a
;;; body of thousands of calls (a test suite's) won't compile.  A long
;;; sequence is cut into chunks, each called as ((lambda () ...)) and so
;;; a machine of its own.  The chunks start at the first element that may
;;; capture: the definitions before it stay where letrec* left them.

(defparameter *chunk-sites* 32
  "About how many calls that may capture a chunk of a sequence holds.")

(defvar *chunk-markers*)
(setf (documentation '*chunk-markers* 'variable)
      "The rest parameters of the chunks CHUNK-SEQUENCES made, in the form
CC-TRANSFORM is transforming.")

(defvar *lift-chunks* t
  "True to compile each chunk on its own: closure-converted, its free
variables passed as arguments (assigned ones boxed), and wrapped in
%LIFTED.  Without that, a procedure of thousands of calls (a test suite)
is one SBCL component, and SBCL's compile time grows with the square of
a component's size; nor can a component have more than 2047 entry
points.")

(defun chunk-call-p (e)
  (and (consp e) (null (cdr e)) (lambda-form-p (car e))
       (symbolp (cadr (car e))) (gethash (cadr (car e)) *chunk-markers*)))

(defun lexical-variables (form)
  "The variables FORM binds (lambda parameters and letrec variables)."
  (let ((table (make-hash-table :test 'eq)))
    (labels ((walk (e)
	       (cond ((atom e))
		     ((quote-form-p e))
		     ((lambda-form-p e)
		      (dolist (v (formal-variables (cadr e))) (setf (gethash v table) t))
		      (walk (caddr e)))
		     ((keyword-p (car e) "LETREC")
		      (dolist (b (cadr e)) (setf (gethash (car b) table) t) (walk (cadr b)))
		      (walk (caddr e)))
		     (t (loop for x on e do (walk (car x)))))))
      (walk form))
    table))

(defun free-lexicals (e lexicals)
  "The variables of LEXICALS free in E, in a fixed order."
  (let ((found '()))
    (labels ((walk (e bound)
	       (cond ((symbolp e)
		      (when (and (gethash e lexicals) (not (member e bound)))
			(pushnew e found)))
		     ((atom e))
		     ((quote-form-p e))
		     ((lambda-form-p e)
		      (walk (caddr e) (append (formal-variables (cadr e)) bound)))
		     ((keyword-p (car e) "LETREC")
		      (let ((bound (append (mapcar #'car (cadr e)) bound)))
			(dolist (b (cadr e)) (walk (cadr b) bound))
			(walk (caddr e) bound)))
		     (t (loop for x on e
			      do (walk (car x) bound)
				 (unless (listp (cdr x)) (walk (cdr x) bound)))))))
      (walk e '()))
    (reverse found)))

(defun lift-chunks (e lexicals)
  "E with each chunk call made a call to a closed procedure, compiled on
its own, of the chunk's free variables."
  (labels ((lift (e)
	     (cond ((atom e) e)
		   ((quote-form-p e) e)
		   ((chunk-call-p e)
		    (let ((fvs (free-lexicals e lexicals)))
		      `((%lifted (,(sym "lambda") ,fvs ,(lift (caddr (car e))))) ,@fvs)))
		   (t (map-tree e))))
	   (map-tree (x)
	     (cond ((consp x) (cons (lift (car x)) (map-tree (cdr x))))
		   (t x))))
    (lift e)))

(defun count-sites (e)
  "Calls in E (outside nested lambdas) that may capture."
  (cond ((atomic-p e) 0)
	((keyword-p (car e) "SET!") (count-sites (caddr e)))
	((keyword-p (car e) "LETREC")
	 (+ (loop for b in (cadr e) sum (count-sites (cadr b))) (count-sites (caddr e))))
	((or (keyword-p (car e) "IF") (keyword-p (car e) "BEGIN"))
	 (loop for x in (cdr e) sum (count-sites x)))
	((lambda-form-p (car e))
	 (+ (count-sites (caddr (car e))) (loop for x in (cdr e) sum (count-sites x))))
	(t (+ (if (capturing-call-p e) 1 0) (loop for x in (cdr e) sum (count-sites x))))))

(defun chunk-sequences (e)
  (cond ((atomic-p e)
	 (if (lambda-form-p e)
	     `(,(car e) ,(cadr e) ,(chunk-sequences (caddr e)))
	     e))
	((keyword-p (car e) "BEGIN")
	 (let* ((elements (mapcar #'chunk-sequences (cdr e)))
		(counts (mapcar #'count-sites elements)))
	   (if (<= (reduce #'+ counts) (* 2 *chunk-sites*))
	       `(,(car e) ,@elements)
	       (let ((prefix '()) (chunks '()) (chunk '()) (n 0))
		 (loop while (and elements (zerop (car counts)))
		       do (push (pop elements) prefix) (pop counts))
		 (loop for x in elements for c in counts
		       do (when (and chunk (> (+ n c) *chunk-sites*))
			    (push (reverse chunk) chunks)
			    (setq chunk '() n 0))
			  (push x chunk) (incf n c))
		 (when chunk (push (reverse chunk) chunks))
		 `(,(car e) ,@(reverse prefix)
		   ;; (lambda args ...), not (lambda () ...), which FLAT
		   ;; would take for a let and put back in place
		   ,@(mapcar (lambda (c)
			       (let ((marker (fresh)))
					 (setf (gethash marker *chunk-markers*) t)
					 `((,(sym "lambda") ,marker (,(sym "begin") ,@c)))))
			     (reverse chunks)))))))
	((quote-form-p e) e)
	(t (mapcar #'chunk-sequences e))))

(defun prepare (form)
  "FORM, chunked, its assigned variables boxed, and its chunks lifted."
  (let ((form (box-assigned (chunk-sequences form))))
    (if *lift-chunks*
	(lift-chunks form (lexical-variables form))
	form)))

(defun cc-transform (form)
  "Top-level FORM for full continuations."
  (let* ((*chunk-markers* (make-hash-table :test 'eq))
	 (*safe-procedures* nil)
	 (*known-procedures* nil)
	 (*downward-parameters* nil)
	 (*safe-procedures* (find-safe-procedures form))
	 (*downward-parameters* (find-downward-parameters)))
    (cc-transform-1 form)))

(defparameter *cc-transform-limit* 175000
  "The largest top-level form, in conses after the transformation, that
is transformed.  The transformation multiplies a procedure's size (a
site restores each of its live variables), and SBCL can't compile a
code object past a megabyte on arm64; Guile's compiler passes (peval's
procedure, a Scheme form of 46,000 conses, is 297,000 transformed) are
left as they are, so continuations captured in them are escape-only.")

(defun cc-transform-1 (form)
  (cond ((and (consp form) (keyword-p (car form) "DEFINE") (consp (cadr form)))
	 ;; (define (f . formals) body ...), from the runtime's own sources
	 (cc-transform-1 `(,(car form) ,(car (cadr form))
			   (,(sym "lambda") ,(cdr (cadr form))
			    ,(if (cdddr form) `(,(sym "begin") ,@(cddr form)) (caddr form))))))
	((and (consp form) (keyword-p (car form) "BEGIN") (cdr form))
	 `(,(car form) ,@(mapcar #'cc-transform-1 (cdr form))))
	((and (consp form) (keyword-p (car form) "DEFINE"))
	 (if (simple-p (caddr form))
	     `(,(car form) ,(cadr form) ,(transform-bound-lambda (cadr form) (prepare (caddr form))))
	     ;; defined first, so that the assignment can be in a frame
	     `(,(sym "begin")
	       (,(car form) ,(cadr form) (,(sym "quote") ,ps:false))
	       ,(cc-transform-1 `(,(sym "set!") ,(cadr form) ,(caddr form))))))
	(t (let* ((prepared (prepare form))
		  (transformed (if (tail-simple-p prepared)
				   (simple prepared)
				   `(,(build-machine '() prepared)))))
	     (if (> (tree-size transformed) *cc-transform-limit*)
		 form
		 transformed)))))

;;; ------------------------------------------------------------------
;;; The host's side

(defun install-continuation-primitives ()
  (defhost "%call-with-frame" (thunk k) (call-with-frame thunk k))
  (defhost "%full-call/cc" (f) (full-call/cc f))
  (defhost "%full-dynamic-wind" (before thunk after) (full-dynamic-wind before thunk after))
  (defhost "%full-call-with-values" (producer consumer) (full-call-with-values producer consumer))
  (defhost "%guard-reraise" (k thunk) (guard-reraise k thunk))
  (defhost "%escape-call/cc" (f) (escape-call/cc f))
  (defhost "call-with-prompt" (tag thunk handler) (call-with-prompt tag thunk handler))
  (defhost "abort-to-prompt" (tag &rest values) (apply #'abort-to-prompt tag values))
  (defhost "%full-map" (f list &rest lists) (apply #'full-map f list lists))
  (defhost "%full-for-each" (f list &rest lists) (apply #'full-for-each f list lists))
  (defhost "%full-vector-map" (f v &rest more) (full-index-loop :vector-map f v more))
  (defhost "%full-vector-for-each" (f v &rest more) (full-index-loop :vector-for-each f v more))
  (defhost "%full-string-map" (f s &rest more) (full-index-loop :string-map f s more))
  (defhost "%full-string-for-each" (f s &rest more) (full-index-loop :string-for-each f s more)))
