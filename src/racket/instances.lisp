; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-RACKET -*-

;;;; Primitive instances: #%kernel, #%unsafe, #%flfxnum, #%paramz,
;;;; #%linklet, ... (docs/racket.md, "Linklets").  Linklets refer to
;;;; primitives by name; an instance is a table of them.
;;;;
;;;; Who fills them depends on the strategy:
;;;;   A, C  rumble and the layers above it, as in Racket CS: BOOT reads
;;;;         them from rumble's tables after it loads;
;;;;   B     native/ (DEFNATIVE) and the extracted layers' exports.
;;;; Linklets, variables and instances themselves (instance objects,
;;;; variable objects) are in linklet/api.lisp for B, and rumble's for A
;;;; and C.

(in-package "QUASISCHEME-RACKET")

(defparameter *instance-names*
  '("#%kernel" "#%unsafe" "#%flfxnum" "#%extfl" "#%paramz" "#%foreign" "#%network"
    "#%place" "#%futures" "#%terminal" "#%linklet")
  "The primitive instances, in Racket 9.3 (docs/racket.md has their sizes).")

(defvar *primitive-instances* (make-hash-table :test 'equal)
  "Instance name -> (hash table: primitive name -> value).")

(defun primitive-instance (name)
  (or (gethash name *primitive-instances*)
      (setf (gethash name *primitive-instances*) (make-hash-table :test 'equal))))

(defun define-primitive (instance name value)
  (setf (gethash name (primitive-instance instance)) value))

(defun primitive (name &optional (instances (append *instance-names* '("#%internal" "#%quasischeme"))))
  "NAME's value in the first of INSTANCES that has it, and T; else NIL, NIL."
  (dolist (i instances (values nil nil))
    (multiple-value-bind (v found) (gethash name (primitive-instance i))
      (when found (return (values v t))))))

(defun missing-primitives (instance names)
  "Which of NAMES INSTANCE doesn't have: the coverage report, against
Racket's own (primitive-table ...) lists."
  (remove-if (lambda (n) (nth-value 1 (gethash n (primitive-instance instance)))) names))

;;; ------------------------------------------------------------------
;;; Racket CS's primitive tables: what each instance must have
;;;
;;; racket/src/cs/primitive/NAME.ss in the pinned source has an entry
;;; [name (known-KIND MASK ...)] per primitive: MASK is its arity mask
;;; (bit N: accepts N arguments; negative: N or more).  These, not a
;;; hand-made list, are what coverage is measured against.

(defparameter *primitive-table-files*
  '(("kernel" . "#%kernel") ("unsafe" . "#%unsafe") ("flfxnum" . "#%flfxnum")
    ("extfl" . "#%extfl") ("paramz" . "#%paramz") ("foreign" . "#%foreign")
    ("network" . "#%network") ("place" . "#%place") ("futures" . "#%futures")
    ("terminal" . "#%terminal") ("linklet" . "#%linklet") ("internal" . "#%internal"))
  "racket/src/cs/primitive/FILE.ss -> the instance it fills.")

(defstruct (table-entry (:constructor make-table-entry (name kind mask)))
  name kind mask)

(defun read-primitive-table (file)
  "FILE's entries, in order."
  (with-open-file (in (racket-source (format nil "racket/src/cs/primitive/~A.ss" file)))
    (loop for line = (read-line in nil)
	  while line
	  for start = (position #\[ line)
	  when (and start (every (lambda (c) (member c '(#\Space #\Tab))) (subseq line 0 start)))
	    collect (let* ((text (subseq line (1+ start)))
			   (space (position #\Space text))
			   (name (subseq text 0 space))
			   (rest (string-left-trim " (" (subseq text space)))
			   (words (uiop:split-string (string-right-trim ")] " rest) :separator " "))
			   (mask (and (second words) (parse-integer (second words) :junk-allowed t))))
		      (make-table-entry name (first words) mask)))))

(defun primitive-tables ()
  "Instance name -> its table's entries, for every table."
  (loop for (file . instance) in *primitive-table-files*
	collect (cons instance (read-primitive-table file))))

(defvar *arity-masks* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "Procedures whose arity isn't their lambda list's -> their arity mask:
a case-lambda of several clauses, procedure-reduce-arity's.")

(defun arity-mask (function)
  "FUNCTION's arity mask: recorded, or from its lambda list."
  (or (values (gethash function *arity-masks*))
      (lambda-list-arity-mask function)))

(defun lambda-list-arity-mask (function)
  (multiple-value-bind (required optional rest)
      (let ((required 0) (optional 0) (rest nil) (state :required))
	(dolist (p (sb-introspect:function-lambda-list function) (values required optional rest))
	  (case p
	    (&optional (setq state :optional))
	    ((&rest &key) (setq rest t state :done))
	    (&aux (setq state :done))
	    (t (case state (:required (incf required)) (:optional (incf optional)))))))
    (if rest
	(- (ash 1 required))
	(loop for n from required to (+ required optional) sum (ash 1 n)))))

(defun real-primitive-p (instance name)
  "Whether INSTANCE has NAME, and not as a stub."
  (and (nth-value 1 (gethash name (primitive-instance instance)))
       (not (gethash name *stubs*))))

(defun coverage-report (&key (stream *standard-output*) (missing nil))
  "Per instance: how many of Racket CS's primitives this strategy's
instances have, and how many have the wrong arity.  MISSING: list them."
  (let ((total 0) (have 0))
    (dolist (table (primitive-tables))
      (destructuring-bind (instance . entries) table
	(let ((absent '()) (wrong '()))
	  (dolist (e entries)
	    (multiple-value-bind (v found) (gethash (table-entry-name e) (primitive-instance instance))
	      (cond ((or (not found) (gethash (table-entry-name e) *stubs*)) (push (table-entry-name e) absent))
		    ((and (functionp v) (table-entry-mask e)
			  (not (ignore-errors (= (arity-mask v) (table-entry-mask e))))
			  ;; a host procedure's lambda list may be (&rest args)
			  (not (eql (ignore-errors (arity-mask v)) -1)))
		     (push (table-entry-name e) wrong)))))
	  (incf total (length entries))
	  (incf have (- (length entries) (length absent)))
	  (format stream "~&~12A ~4D of ~4D~@[  (arity differs: ~{~A~^ ~})~]~%"
		  instance (- (length entries) (length absent)) (length entries) (reverse wrong))
	  (when missing
	    (format stream "~&  missing: ~{~A~^ ~}~%" (reverse absent))))))
    (format stream "~&~12A ~4D of ~4D~%" "total" have total)))

(defvar *stubs* (make-hash-table :test 'equal)
  "Name -> T for each primitive that is a stub (INSTALL-STUBS).")

(defun install-stubs ()
  "Each procedure of Racket CS's primitive tables that an instance lacks,
as a stub that raises not-yet when called (not when referenced: a
linklet may store a primitive without calling it).  Needs Racket's
source; without it, nothing."
  (when (ignore-errors (racket-source-directory))
    (dolist (table (primitive-tables))
      (destructuring-bind (instance . entries) table
	(dolist (e entries)
	  (let ((name (table-entry-name e)))
	    ;; defined, but in another instance: this one has it too
	    (multiple-value-bind (value found) (primitive name)
	      (when (and found (not (nth-value 1 (gethash name (primitive-instance instance)))))
		(define-primitive instance name value)))
	    (when (and (uiop:string-prefix-p "known-procedure" (table-entry-kind e))
		       ;; in no instance: one defined in another instance than
		       ;; the table's is still the primitive (the host global
		       ;; racket:NAME is shared)
		       (not (nth-value 1 (primitive name))))
	      (setf (gethash name *stubs*) t)
	      (define-primitive instance name
		(if (and (eql (table-entry-mask e) 3) (parameter-name-p name))
		    ;; parameter-shaped (a getter and setter, by name): a
		    ;; parameter holding #f, which linklets read as they
		    ;; instantiate
		    (make-parameter* qs:false)
		    (lambda (&rest args) (declare (ignore args)) (not-yet name)))))
	    ;; a constant we lack: a property for a property's name, else a
	    ;; value of its own, so that references to it compile and run
	    (unless (or (uiop:string-prefix-p "known-procedure" (table-entry-kind e))
			(nth-value 1 (primitive name)))
	      (setf (gethash name *stubs*) t)
	      (define-primitive instance name
		(cond ((search "prop:" name) (%make-struct-type-property (rsym name) nil '()))
		      ;; Racket CS lists parameters as constants
		      ((parameter-name-p name) (make-parameter* qs:false))
		      (t (make-missing-constant name)))))))))))

(defun parameter-name-p (name)
  "Whether NAME is shaped like a parameter's: Racket's parameters are named
current-..., print-..., read-... and so on."
  (and (not (find #\? name)) (not (find #\! name))
       (or (some (lambda (prefix) (uiop:string-prefix-p prefix name))
		 '("current-" "error-" "print-" "read-" "port-" "compile-" "use-" "load-"
		   "eval-" "executable-" "exit-" "global-port-" "uncaught-"))
	   (search "-handler" name) (search "enabled" name))))

(defstruct (missing-constant (:constructor make-missing-constant (name))) name)

(defun prune-extra-primitives ()
  "Move each primitive an instance has that Racket CS's table for it
doesn't list to the instance #%quasischeme: an instance exports exactly
Racket's names, or a module that requires #%kernel and defines one of
ours (racket/private's andmap, say) would find it already required.
Compiled code still reaches them, by their host globals."
  (when (ignore-errors (racket-source-directory))
    (dolist (table (primitive-tables))
      (destructuring-bind (instance . entries) table
	(let ((names (make-hash-table :test 'equal))
	      (extra '()))
	  (dolist (e entries) (setf (gethash (table-entry-name e) names) t))
	  (maphash (lambda (name value)
		     (unless (gethash name names) (push (cons name value) extra)))
		   (primitive-instance instance))
	  (loop for (name . value) in extra
		do (remhash name (primitive-instance instance))
		   (define-primitive "#%quasischeme" name value)))))))