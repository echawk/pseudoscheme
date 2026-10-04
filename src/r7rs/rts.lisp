; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-R7RS -*-

;;;; R7RS run-time primitives written in Common Lisp
;;;;
;;;; Things that are most naturally CL: typed vectors (bytevectors),
;;;; floor/truncate division, UTF-8, the exception machinery, record
;;;; types, parameters, process context.  Each is registered with
;;;; DEFPRIM under its Scheme name; r7rs.lisp installs them as variables
;;;; in the implementation environment that backs (scheme base) etc.
;;;; Scheme-level derived forms and the easy list/string/vector
;;;; procedures are in base.scm.
;;;;
;;;; Conventions: Scheme #f is the symbol PS:FALSE, #t is T, and NIL is
;;;; the empty list, so predicates return (PS:TRUE? <cl-boolean>) and
;;;; tests of Scheme values use PS:TRUEP.

(in-package "PSEUDOSCHEME-R7RS")

(defvar *primitives* '()
  "Alist of (scheme-name-string . function), in definition order.")

(defun register-primitive (name function)
  (let ((cell (assoc name *primitives* :test #'string=)))
    (if cell
	(setf (cdr cell) function)
	(setq *primitives* (nconc *primitives* (list (cons name function)))))
    name))

(defmacro defprim (name lambda-list &body body)
  `(register-primitive ,name (lambda ,lambda-list ,@body)))

(defun primitive (name)
  (or (cdr (assoc name *primitives* :test #'string=))
      (error "no R7RS primitive ~A" name)))

(defun bool (x) (ps:true? x))

(defun scheme-error (control &rest args)
  (apply #'ps:scheme-error control args))

;;; ------------------------------------------------------------------
;;; Booleans, symbols

(defprim "boolean=?" (a b &rest more)
  (flet ((check (x) (unless (or (eq x t) (eq x ps:false))
		      (scheme-error "boolean=?: not a boolean: ~S" x))))
    (check a) (check b) (mapc #'check more)
    (bool (and (eq a b) (every (lambda (x) (eq x a)) more)))))

(defprim "symbol=?" (a b &rest more)
  (flet ((check (x) (unless (ps:scheme-symbol-p x)
		      (scheme-error "symbol=?: not a symbol: ~S" x))))
    (check a) (check b) (mapc #'check more)
    (bool (and (eq a b) (every (lambda (x) (eq x a)) more)))))

;;; ------------------------------------------------------------------
;;; Numbers (R7RS 6.2.6)

(defprim "exact-integer?" (x) (bool (integerp x)))

(defprim "exact-integer-sqrt" (n)
  (unless (and (integerp n) (>= n 0))
    (scheme-error "exact-integer-sqrt: not a nonnegative exact integer: ~S" n))
  (let ((s (isqrt n)))
    (values s (- n (* s s)))))

(defun check-integers (who &rest xs)
  (dolist (x xs)
    (unless (integerp x)
      (scheme-error "~A: not an integer: ~S" who x))))

(defprim "floor/" (n d)
  (check-integers "floor/" n d)
  (when (zerop d) (scheme-error "floor/: division by zero"))
  (multiple-value-bind (q r) (floor n d) (values q r)))
(defprim "floor-quotient" (n d)
  (check-integers "floor-quotient" n d)
  (when (zerop d) (scheme-error "floor-quotient: division by zero"))
  (values (floor n d)))
(defprim "floor-remainder" (n d)
  (check-integers "floor-remainder" n d)
  (when (zerop d) (scheme-error "floor-remainder: division by zero"))
  (nth-value 1 (floor n d)))
(defprim "truncate/" (n d)
  (check-integers "truncate/" n d)
  (when (zerop d) (scheme-error "truncate/: division by zero"))
  (multiple-value-bind (q r) (truncate n d) (values q r)))
(defprim "truncate-quotient" (n d)
  (check-integers "truncate-quotient" n d)
  (when (zerop d) (scheme-error "truncate-quotient: division by zero"))
  (values (truncate n d)))
(defprim "truncate-remainder" (n d)
  (check-integers "truncate-remainder" n d)
  (when (zerop d) (scheme-error "truncate-remainder: division by zero"))
  (nth-value 1 (truncate n d)))

(defun float-nan-p (x)
  (and (floatp x) (float-features:float-nan-p x)))

(defun float-infinite-p (x)
  (and (floatp x) (float-features:float-infinity-p x)))

(defprim "nan?" (x)
  (unless (numberp x) (scheme-error "nan?: not a number: ~S" x))
  (bool (or (float-nan-p x)
	    (and (complexp x) (or (float-nan-p (realpart x)) (float-nan-p (imagpart x)))))))
(defprim "infinite?" (x)
  (unless (numberp x) (scheme-error "infinite?: not a number: ~S" x))
  (bool (or (float-infinite-p x)
	    (and (complexp x) (or (float-infinite-p (realpart x))
				  (float-infinite-p (imagpart x)))))))
(defprim "finite?" (x)
  (unless (numberp x) (scheme-error "finite?: not a number: ~S" x))
  (bool (not (or (float-nan-p x) (float-infinite-p x)
		 (and (complexp x)
		      (or (float-nan-p (realpart x)) (float-infinite-p (realpart x))
			  (float-nan-p (imagpart x)) (float-infinite-p (imagpart x))))))))

;;; ------------------------------------------------------------------
;;; Characters and strings (the Unicode-sensitive parts of (scheme char))

(defprim "char-foldcase" (c) (char-downcase c))
(defprim "string-foldcase" (s) (string-downcase s))
(defprim "string-upcase" (s) (coerce (string-upcase s) 'simple-string))
(defprim "string-downcase" (s) (coerce (string-downcase s) 'simple-string))
(defprim "digit-value" (c)
  (let ((w (digit-char-p c))) (if w w ps:false)))

;;; ------------------------------------------------------------------
;;; Bytevectors (R7RS 6.9).  CL has these natively.

(deftype bytevector () '(simple-array (unsigned-byte 8) (*)))

(defun check-bytevector (who x)
  (unless (typep x 'bytevector)
    (scheme-error "~A: not a bytevector: ~S" who x)))

(defun check-byte (who x)
  (unless (typep x '(unsigned-byte 8))
    (scheme-error "~A: not a byte (0..255): ~S" who x)))

(defun range (who length start end)
  (let ((start (or start 0)) (end (or end length)))
    (unless (and (integerp start) (integerp end) (<= 0 start end length))
      (scheme-error "~A: bad range ~S..~S for length ~S" who start end length))
    (values start end)))

(defprim "bytevector?" (x) (bool (typep x 'bytevector)))

(defprim "make-bytevector" (k &optional (fill 0))
  (check-byte "make-bytevector" fill)
  (make-array k :element-type '(unsigned-byte 8) :initial-element fill))

(defprim "bytevector" (&rest bytes)
  (mapc (lambda (b) (check-byte "bytevector" b)) bytes)
  (make-array (length bytes) :element-type '(unsigned-byte 8) :initial-contents bytes))

(defprim "bytevector-length" (bv)
  (check-bytevector "bytevector-length" bv)
  (length bv))

(defprim "bytevector-u8-ref" (bv k)
  (check-bytevector "bytevector-u8-ref" bv)
  (unless (and (integerp k) (< -1 k (length bv)))
    (scheme-error "bytevector-u8-ref: index out of range: ~S" k))
  (aref bv k))

(defprim "bytevector-u8-set!" (bv k byte)
  (check-bytevector "bytevector-u8-set!" bv)
  (check-byte "bytevector-u8-set!" byte)
  (unless (and (integerp k) (< -1 k (length bv)))
    (scheme-error "bytevector-u8-set!: index out of range: ~S" k))
  (setf (aref bv k) byte)
  ps:unspecific)

(defprim "bytevector-copy" (bv &optional start end)
  (check-bytevector "bytevector-copy" bv)
  (multiple-value-bind (start end) (range "bytevector-copy" (length bv) start end)
    (subseq bv start end)))

(defprim "bytevector-copy!" (to at from &optional start end)
  (check-bytevector "bytevector-copy!" to)
  (check-bytevector "bytevector-copy!" from)
  (multiple-value-bind (start end) (range "bytevector-copy!" (length from) start end)
    (unless (<= 0 at (- (length to) (- end start)))
      (scheme-error "bytevector-copy!: destination too small"))
    (replace to from :start1 at :start2 start :end2 end)
    ps:unspecific))

(defprim "bytevector-append" (&rest bvs)
  (mapc (lambda (b) (check-bytevector "bytevector-append" b)) bvs)
  (apply #'concatenate 'bytevector bvs))

;;; UTF-8 (R7RS: utf8->string, string->utf8)

(defun utf8-encode (string start end)
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer t)))
    (loop for i from start below end
	  for code = (char-code (char string i))
	  do (cond ((< code #x80) (vector-push-extend code out))
		   ((< code #x800)
		    (vector-push-extend (logior #xC0 (ash code -6)) out)
		    (vector-push-extend (logior #x80 (logand code #x3F)) out))
		   ((< code #x10000)
		    (vector-push-extend (logior #xE0 (ash code -12)) out)
		    (vector-push-extend (logior #x80 (logand (ash code -6) #x3F)) out)
		    (vector-push-extend (logior #x80 (logand code #x3F)) out))
		   (t
		    (vector-push-extend (logior #xF0 (ash code -18)) out)
		    (vector-push-extend (logior #x80 (logand (ash code -12) #x3F)) out)
		    (vector-push-extend (logior #x80 (logand (ash code -6) #x3F)) out)
		    (vector-push-extend (logior #x80 (logand code #x3F)) out))))
    (coerce out 'bytevector)))

(defun utf8-decode (bv start end)
  (let ((out (make-string-output-stream)) (i start))
    (loop while (< i end)
	  do (let* ((b (aref bv i))
		    (n (cond ((< b #x80) 0) ((< b #xC0) -1) ((< b #xE0) 1)
			     ((< b #xF0) 2) ((< b #xF8) 3) (t -1))))
	       (when (or (minusp n) (>= (+ i n) end))
		 (scheme-error "utf8->string: invalid UTF-8 at byte ~D" i))
	       (let ((code (if (zerop n) b (logand b (ash #x3F (- n))))))
		 (loop for k from 1 to n
		       do (let ((c (aref bv (+ i k))))
			    (unless (= (logand c #xC0) #x80)
			      (scheme-error "utf8->string: invalid UTF-8 at byte ~D" (+ i k)))
			    (setq code (logior (ash code 6) (logand c #x3F)))))
		 (write-char (code-char code) out)
		 (incf i (1+ n)))))
    (coerce (get-output-stream-string out) 'simple-string)))

(defprim "string->utf8" (s &optional start end)
  (multiple-value-bind (start end) (range "string->utf8" (length s) start end)
    (utf8-encode s start end)))

(defprim "utf8->string" (bv &optional start end)
  (check-bytevector "utf8->string" bv)
  (multiple-value-bind (start end) (range "utf8->string" (length bv) start end)
    (utf8-decode bv start end)))

;;; ------------------------------------------------------------------
;;; Record types (define-record-type is a macro in base.scm over these)

(defstruct (record-type (:constructor %make-record-type (name fields)))
  name fields)

(defstruct (record (:constructor %make-record (rtd values)))
  rtd values)

(defmethod print-object ((r record-type) stream)
  (print-unreadable-object (r stream)
    (format stream "Record-type ~A" (record-type-name r))))

(defmethod print-object ((r record) stream)
  (print-unreadable-object (r stream)
    (format stream "Record ~A" (record-type-name (record-rtd r)))))

(defun field-index (who type field)
  (or (position field (record-type-fields type))
      (scheme-error "~A: ~S has no field ~S" who (record-type-name type) field)))

(defprim "%make-record-type" (name fields)
  (%make-record-type (if (symbolp name) (symbol-name name) name) fields))

(defprim "%record-constructor" (type field-names)
  (let* ((n (length (record-type-fields type)))
	 (indexes (mapcar (lambda (f) (field-index "record constructor" type f)) field-names))
	 (arity (length indexes)))
    (lambda (&rest args)
      (unless (= (length args) arity)
	(scheme-error "~A constructor: expected ~D arguments, got ~D"
		      (record-type-name type) arity (length args)))
      (let ((values (make-array n :initial-element ps:false)))
	(loop for i in indexes for a in args do (setf (svref values i) a))
	(%make-record type values)))))

(defprim "%record-predicate" (type)
  (lambda (obj) (bool (and (record-p obj) (eq (record-rtd obj) type)))))

(defprim "%record-accessor" (type field)
  (let ((i (field-index "record accessor" type field)))
    (lambda (obj)
      (unless (and (record-p obj) (eq (record-rtd obj) type))
	(scheme-error "~A accessor: not a ~A: ~S" field (record-type-name type) obj))
      (svref (record-values obj) i))))

(defprim "%record-modifier" (type field)
  (let ((i (field-index "record modifier" type field)))
    (lambda (obj value)
      (unless (and (record-p obj) (eq (record-rtd obj) type))
	(scheme-error "~A modifier: not a ~A: ~S" field (record-type-name type) obj))
      (setf (svref (record-values obj) i) value)
      ps:unspecific)))

;;; ------------------------------------------------------------------
;;; Exceptions (R7RS 6.11)
;;;
;;; RAISE / WITH-EXCEPTION-HANDLER keep an explicit handler stack,
;;; *HANDLERS*, so a handler runs in the dynamic context of the RAISE
;;; minus itself, as R7RS requires, and RAISE-CONTINUABLE can return.
;;; Errors signalled by the Lisp underneath (CAR of a non-pair, ...)
;;; reach the same handlers through a HANDLER-BIND installed by
;;; WITH-EXCEPTION-HANDLER; the CL condition object becomes the raised
;;; object, so ERROR-OBJECT? is true of it.

(defvar *handlers* '())

(defstruct (error-object (:constructor make-error-object
					 (message irritants &optional who (kind :error))))
  message irritants
  who		; R6RS: who raised it, or #f/NIL
  kind)		; R6RS: :ERROR or :ASSERTION (an assertion violation)

(defmethod print-object ((e error-object) stream)
  (print-unreadable-object (e stream)
    (format stream "Error ~A~{ ~S~}" (error-object-message e) (error-object-irritants e))))

(defvar *condition-describer* nil
  "If set, a function (object stream) that prints a raised object
readably for humans and returns true, or returns NIL to decline.  (The
R6RS layer describes its conditions.)")

(define-condition uncaught-raise (error)
  ((payload :initarg :payload :reader uncaught-payload))
  (:report (lambda (c stream)
	     (let ((p (uncaught-payload c)))
	       (cond ((and *condition-describer* (funcall *condition-describer* p stream)))
		     ((error-object-p p)
		      (format stream "~A~{ ~S~}" (error-object-message p) (error-object-irritants p)))
		     (t (format stream "Uncaught exception: ~S" p)))))))

(defvar *non-continuable-condition*
  (lambda (obj)
    (make-error-object "handler returned from non-continuable raise" (list obj)))
  "What to raise when a handler returns from a non-continuable RAISE of
OBJ.  (The R6RS layer replaces it with a &non-continuable condition.)")

(defvar *foreign-condition-converter* #'identity
  "Maps a Lisp error caught by WITH-EXCEPTION-HANDLER to the object the
Scheme handler sees.  (The R6RS layer makes R6RS conditions of them.)")

(defun raise-object (obj continuable)
  (if (null *handlers*)
      (error 'uncaught-raise :payload obj)
      (let* ((handler (car *handlers*))
	     (outer (cdr *handlers*))
	     (value (let ((*handlers* outer)) (funcall handler obj))))
	(if continuable
	    value
	    (let ((*handlers* outer))
	      (raise-object (funcall *non-continuable-condition* obj) nil))))))

(defprim "raise" (obj) (raise-object obj nil))
(defprim "raise-continuable" (obj) (raise-object obj t))

(defprim "error" (message &rest irritants)
  (raise-object (make-error-object message irritants) nil))

(defprim "with-exception-handler" (handler thunk)
  (unless (functionp handler)
    (scheme-error "with-exception-handler: handler is not a procedure: ~S" handler))
  (let ((outer *handlers*))
    (let ((*handlers* (cons handler outer)))
      (handler-bind ((error (lambda (c)
			      ;; A Lisp error under this frame goes to *this*
			      ;; frame's handler, with the outer handlers
			      ;; installed, as for a RAISE; falling out of the
			      ;; handler is the same secondary error.
			      (unless (typep c 'uncaught-raise)
				(let ((*handlers* outer)
				      (obj (funcall *foreign-condition-converter* c)))
				  (funcall handler obj)
				  (raise-object (funcall *non-continuable-condition* obj) nil))))))
	(funcall thunk)))))

(defprim "error-object?" (x)
  (bool (or (error-object-p x) (typep x 'error))))

(defprim "error-object-message" (x)
  (cond ((error-object-p x)
	 (let ((m (error-object-message x))) (if (stringp m) m (princ-to-string m))))
	((typep x 'condition) (remove #\Newline (princ-to-string x)))
	(t "")))

(defprim "error-object-irritants" (x)
  (if (error-object-p x) (error-object-irritants x) '()))

(defprim "file-error?" (x) (bool (typep x 'file-error)))
(defprim "read-error?" (x) (bool (typep x 'reader-error)))

;;; ------------------------------------------------------------------
;;; Parameters

(defstruct (parameter-state (:constructor make-parameter-state (value converter)))
  value converter)

(defvar *parameter-states* (make-hash-table :test 'eq :weakness :key)
  "parameter procedure -> parameter-state")

(defprim "make-parameter" (init &optional converter)
  (let* ((state (make-parameter-state nil converter))
	 (param (lambda () (parameter-state-value state))))
    (setf (parameter-state-value state)
	  (if converter (funcall converter init) init))
    (setf (gethash param *parameter-states*) state)
    param))

(defvar *port-parameters* '()
  "(procedure . special variable) for the procedures that act as port
parameters: current-input-port and friends, which are host procedures
returning a CL stream variable.  Filled in at boot, when the procedure
objects are known.")

(defun parameterize* (params values thunk)
  ;; current-input-port and friends: rebind their CL stream variables.
  (let ((port (position-if (lambda (p) (assoc p *port-parameters*)) params)))
    (when port
      (return-from parameterize*
	(progv (list (cdr (assoc (nth port params) *port-parameters*))) (list (nth port values))
	  (parameterize* (append (subseq params 0 port) (nthcdr (1+ port) params))
			 (append (subseq values 0 port) (nthcdr (1+ port) values))
			 thunk)))))
  ;; Converters run before any parameter is rebound (R7RS 4.2.6).
  (let* ((states (mapcar (lambda (p)
			   (or (gethash p *parameter-states*)
			       (scheme-error "parameterize: not a parameter object: ~S" p)))
			 params))
	 (new (mapcar (lambda (s v)
			(if (parameter-state-converter s)
			    (funcall (parameter-state-converter s) v)
			    v))
		      states values))
	 (old (mapcar #'parameter-state-value states)))
    (unwind-protect
	 (progn (mapc (lambda (s v) (setf (parameter-state-value s) v)) states new)
		(funcall thunk))
      (mapc (lambda (s v) (setf (parameter-state-value s) v)) states old))))

(defprim "%parameterize" (params values thunk)
  (parameterize* params values thunk))

;;; ------------------------------------------------------------------
;;; Ports and files (the parts that need CL streams)

(defprim "eof-object" () ps:eof-object)
(defprim "current-error-port" () *error-output*)
(defprim "port?" (x) (bool (streamp x)))
(defprim "textual-port?" (x)
  (bool (and (streamp x) (subtypep (stream-element-type x) 'character))))
(defprim "binary-port?" (x)
  (bool (and (streamp x) (not (subtypep (stream-element-type x) 'character)))))
(defprim "input-port-open?" (x) (bool (and (input-stream-p x) (open-stream-p x))))
(defprim "output-port-open?" (x) (bool (and (output-stream-p x) (open-stream-p x))))
(defprim "close-port" (x) (close x) ps:unspecific)
(defprim "call-with-port" (port proc)
  (multiple-value-prog1 (funcall proc port) (close port)))
(defprim "flush-output-port" (&optional (port *standard-output*))
  (finish-output port) ps:unspecific)

(defprim "read-line" (&optional (port *standard-input*))
  (multiple-value-bind (line missing-newline-p) (read-line port nil nil)
    (declare (ignore missing-newline-p))
    (if line (coerce line 'simple-string) ps:eof-object)))

(defprim "read-string" (k &optional (port *standard-input*))
  (let* ((buf (make-string k))
	 (n (read-sequence buf port)))
    (if (and (zerop n) (plusp k)) ps:eof-object (coerce (subseq buf 0 n) 'simple-string))))

(defprim "write-string" (s &optional (port *standard-output*) start end)
  (multiple-value-bind (start end) (range "write-string" (length s) start end)
    (write-string s port :start start :end end)
    ps:unspecific))

(defprim "file-exists?" (name) (bool (probe-file name)))
(defprim "delete-file" (name) (delete-file name) ps:unspecific)

;;; ------------------------------------------------------------------
;;; Process context and time

(defprim "exit" (&optional (code 0))
  (uiop:quit (cond ((eq code t) 0) ((eq code ps:false) 1) ((integerp code) code) (t 0))))
(defprim "emergency-exit" (&optional (code 0))
  (uiop:quit (cond ((eq code t) 0) ((eq code ps:false) 1) ((integerp code) code) (t 0))))
(defvar *command-line* nil
  "What COMMAND-LINE returns, when set (the command-line program sets it
to the script name and its arguments); else the process's arguments.")

(defprim "command-line" ()
  (or *command-line*
      (cons "pseudoscheme" (copy-list (uiop:command-line-arguments)))))
(defprim "get-environment-variable" (name)
  (or (uiop:getenv name) ps:false))
;; No portability library lists the whole environment.
(defun environment-strings ()
  "The process environment as a list of \"NAME=value\" strings."
  #+sbcl (sb-ext:posix-environ)
  #+ecl (ext:environ)
  #+ccl (ccl::get-env-strings)
  #+abcl (loop for (k . v) in (ext:getenv-all) collect (format nil "~A=~A" k v))
  #-(or sbcl ecl ccl abcl) '())

(defprim "get-environment-variables" ()
  (mapcar (lambda (entry)
	    (let ((eq (position #\= entry)))
	      (cons (subseq entry 0 eq) (if eq (subseq entry (1+ eq)) ""))))
	  (environment-strings)))

(defprim "current-jiffy" () (get-internal-real-time))
(defprim "jiffies-per-second" () internal-time-units-per-second)
(defprim "current-second" ()
  ;; Seconds since the Unix epoch, as an inexact rational (TAI is
  ;; what R7RS asks for; UTC is close enough for a skeleton).
  (float (- (get-universal-time) 2208988800) 1d0))
