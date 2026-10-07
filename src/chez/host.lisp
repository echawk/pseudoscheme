; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-R6RS -*-

;;;; Host procedures behind Chez Scheme's (chezscheme) library
;;;; (src/chez/*.scm): what needs the operating system, threads, or Common
;;;; Lisp.  Named chez:..., they reach Scheme through (pseudoscheme chez
;;;; host), which src/chez/chez.lisp makes of every chez: primitive.

(in-package "PSEUDOSCHEME-R6RS")

(defun path-string (x) (if (pathnamep x) (namestring x) x))

(defprim "chez:system" (command)
  (nth-value 2 (uiop:run-program command :force-shell t :output :interactive
					 :error-output :interactive :ignore-error-status t)))

(defprim "chez:with-input-from-string" (string thunk)
  (let ((*standard-input* (make-string-input-stream string)))
    (funcall thunk)))

(defprim "chez:with-output-to-string" (thunk)
  (let ((s (make-string-output-stream)))
    (let ((*standard-output* s)) (funcall thunk))
    (coerce (get-output-stream-string s) 'simple-string)))

(defprim "chez:current-directory" (&optional dir)
  (if dir
      (progn (setf *default-pathname-defaults* (uiop:ensure-directory-pathname dir)) ps:unspecific)
      (namestring *default-pathname-defaults*)))

(defprim "chez:file-directory?" (name)
  (ps:true? (and (uiop:directory-exists-p (uiop:parse-native-namestring name)) t)))

(defprim "chez:file-regular?" (name)
  (ps:true? (and (uiop:file-exists-p (uiop:parse-native-namestring name)) t)))

(defprim "chez:file-symbolic-link?" (name)
  (let ((p (uiop:parse-native-namestring name)))
    (ps:true? (and (probe-file p) (not (equal (truename p) (merge-pathnames p)))))))

(defprim "chez:directory-list" (name)
  (let ((dir (uiop:ensure-directory-pathname (uiop:parse-native-namestring name))))
    (mapcar (lambda (p)
	      (if (uiop:directory-pathname-p p)
		  (car (last (pathname-directory p)))
		  (file-namestring p)))
	    (append (uiop:subdirectories dir) (uiop:directory-files dir)))))

(defprim "chez:mkdir" (name &optional mode)
  (declare (ignore mode))
  (ensure-directories-exist (uiop:ensure-directory-pathname name))
  ps:unspecific)

(defprim "chez:delete-directory" (name &optional error?)
  (declare (ignore error?))
  (uiop:delete-empty-directory (uiop:ensure-directory-pathname name))
  ps:unspecific)

(defprim "chez:rename-file" (old new)
  (rename-file (uiop:parse-native-namestring old) (uiop:parse-native-namestring new))
  ps:unspecific)

(defprim "chez:file-modification-time" (name)
  (or (file-write-date (uiop:parse-native-namestring name)) 0))

(defprim "chez:library-directories" ()
  (mapcar (lambda (d) (cons (path-string d) (path-string d))) psx:*library-path*))

(defprim "chez:timezone-offset" ()
  "Seconds east of UTC, now, daylight saving included."
  (multiple-value-bind (s m h d mo y dow dst tz) (decode-universal-time (get-universal-time))
    (declare (ignore s m h d mo y dow))
    (round (* -3600 (- tz (if dst 1 0))))))

;;; File modes and change times.  No portability library covers stat(2)
;;; and chmod(2) without a C toolchain (osicat needs one), so these run
;;; the stat and chmod commands: GNU stat first, then BSD's (macOS).

(defun stat-field (name gnu-format bsd-format)
  (let ((path (namestring (uiop:parse-native-namestring name))))
    (flet ((try (args)
	     (multiple-value-bind (out err status)
		 (uiop:run-program (cons "stat" args) :output :string
						      :error-output nil :ignore-error-status t)
	       (declare (ignore err))
	       (and (eql status 0) (parse-integer out :junk-allowed t)))))
      (or (try (list "-c" gnu-format "--" path))
	  (try (list "-f" bsd-format path))
	  (error "can't stat ~A" name)))))

(defprim "chez:get-mode" (name &optional (follow? t))
  (declare (ignore follow?))
  ;; GNU's %a is octal digits; BSD's %Lp too.
  (parse-integer (princ-to-string (stat-field name "%a" "%Lp")) :radix 8))

(defprim "chez:chmod" (name mode)
  (uiop:run-program (list "chmod" (format nil "~O" mode)
			  (namestring (uiop:parse-native-namestring name))))
  ps:unspecific)

(defprim "chez:file-change-time" (name)
  (stat-field name "%Z" "%c"))

(defprim "chez:machine-type" ()
  "Chez Scheme's name for this machine, threaded: ta6osx, tarm64le, ...
chez-srfi derives its platform features (posix, darwin, ...) from it."
  (let ((arch (let ((m (string-downcase (machine-type))))
		(cond ((or (search "x86-64" m) (search "x86_64" m) (search "amd64" m)) "a6")
		      ((or (search "arm64" m) (search "aarch64" m)) "arm64")
		      ((or (search "x86" m) (search "386" m)) "i3")
		      ((search "ppc" m) "ppc32")
		      (t m))))
	(os (let ((s (string-downcase (software-type))))
	      (cond ((search "darwin" s) "osx")
		    ((search "linux" s) "le")
		    ((search "freebsd" s) "fb")
		    ((search "openbsd" s) "ob")
		    ((search "netbsd" s) "nb")
		    ((search "sunos" s) "s2")
		    ((search "win" s) "nt")
		    (t s)))))
    (ps:intern-scheme-symbol (concatenate 'string "t" arch os))))

;;; ------------------------------------------------------------------
;;; Boxes (#&x reads as one: PS:BOX, src/core.lisp)

(defprim "chez:box" (x) (ps:make-box x))
(defprim "chez:box?" (x) (bool (ps:box-p x)))
(defprim "chez:unbox" (b)
  (unless (ps:box-p b) (r6rs-assertion-violation "unbox" "not a box" b))
  (ps:box-contents b))
(defprim "chez:set-box!" (b x)
  (unless (ps:box-p b) (r6rs-assertion-violation "set-box!" "not a box" b))
  (setf (ps:box-contents b) x)
  ps:unspecific)
(defprim "chez:box-cas!" (b old new)
  (unless (ps:box-p b) (r6rs-assertion-violation "box-cas!" "not a box" b))
  #+sbcl (bool (eq (sb-ext:compare-and-swap (ps:box-contents b) old new) old))
  #-sbcl (if (eq (ps:box-contents b) old)
	     (progn (setf (ps:box-contents b) new) t)
	     ps:false))

;;; ------------------------------------------------------------------
;;; fxvectors (#vfx(...)); flvectors are f64vectors

(defun check-fxvector (who v)
  (unless (ps:fxvector-p v) (r6rs-assertion-violation who "not an fxvector" v)))

(defprim "chez:fxvector?" (x) (bool (ps:fxvector-p x)))
(defprim "chez:make-fxvector" (n &optional (fill 0))
  (make-array n :element-type 'fixnum :initial-element fill))
(defprim "chez:fxvector" (&rest xs) (ps:list->fxvector xs))
(defprim "chez:list->fxvector" (xs) (ps:list->fxvector xs))
(defprim "chez:fxvector-length" (v) (check-fxvector "fxvector-length" v) (length v))
(defprim "chez:fxvector-ref" (v i) (check-fxvector "fxvector-ref" v) (aref v i))
(defprim "chez:fxvector-set!" (v i x)
  (check-fxvector "fxvector-set!" v)
  (setf (aref v i) x)
  ps:unspecific)
(defprim "chez:fxvector->list" (v) (check-fxvector "fxvector->list" v) (coerce v 'list))
(defprim "chez:fxvector-fill!" (v x) (check-fxvector "fxvector-fill!" v) (fill v x) ps:unspecific)
(defprim "chez:fxvector-copy" (v) (check-fxvector "fxvector-copy" v) (copy-seq v))

;;; ------------------------------------------------------------------
;;; Weak and ephemeron hashtables: R6RS hashtables whose CL table is weak
;;; on its keys (in SBCL an entry so held is an ephemeron: its value
;;; doesn't keep its key alive)

(defun make-weak-table (who kind test)
  (declare (ignore who))
  (%make-hashtable (trivial-garbage:make-weak-hash-table :test test :weakness :key)
		   kind nil nil t))

(defprim "chez:make-weak-eq-hashtable" (&optional k)
  (declare (ignore k))
  (make-weak-table "make-weak-eq-hashtable" :eq 'eq))
(defprim "chez:make-weak-eqv-hashtable" (&optional k)
  (declare (ignore k))
  (make-weak-table "make-weak-eqv-hashtable" :eqv 'eql))
(defprim "chez:hashtable-weak?" (h)
  (check-hashtable "hashtable-weak?" h)
  (bool (trivial-garbage:hash-table-weakness (hashtable-table h))))

;;; ------------------------------------------------------------------
;;; Threads, on bordeaux-threads.  A thread made here runs with float
;;; traps off and the current ports of the thread that made it, as
;;; SRFI 18's do.

(defstruct (chez-thread (:constructor make-chez-thread (native id)))
  native id)

(defmethod print-object ((x chez-thread) stream)
  (print-unreadable-object (x stream) (format stream "thread ~D" (chez-thread-id x))))

(defvar *thread-ids* 0)
(defvar *thread-id-lock* (bt2:make-lock :name "Chez thread ids"))
(defvar *current-chez-thread* nil)

(defprim "chez:fork-thread" (thunk)
  (let* ((in *standard-input*) (out *standard-output*) (err *error-output*)
	 (dir *default-pathname-defaults*)
	 (parameters (ps-r7rs::thread-parameters-snapshot))
	 (id (bt2:with-lock-held (*thread-id-lock*) (incf *thread-ids*)))
	 (thread (make-chez-thread nil id)))
    ;; bordeaux-threads' first API: its second ends a thread at the first
    ;; condition signalled in it, a warning included
    (setf (chez-thread-native thread)
	  (bt:make-thread
	   (lambda ()
	     (ps:disable-float-traps)
	     (let ((*standard-input* in) (*standard-output* out) (*error-output* err)
		   (*default-pathname-defaults* dir)
		   (ps-r7rs::*thread-parameters* parameters)
		   (*current-chez-thread* thread))
	       ;; an exception nothing handles ends the thread, reported as
	       ;; Chez reports it
	       (handler-case (funcall thunk)
		 (error (e)
		   (ignore-errors
		    (format *error-output* "~&~A~%"
			    (uiop:symbol-call "PSEUDOSCHEME-R7RS" "CHEZ-ERROR-TEXT" e))
		    (finish-output *error-output*))
		   ps:unspecific))))
	   :name (format nil "Chez thread ~D" id)))
    thread))

(defprim "chez:thread?" (x) (bool (chez-thread-p x)))
(defprim "chez:thread-join" (thread)
  (bt:join-thread (chez-thread-native thread)))
(defprim "chez:get-thread-id" ()
  (if *current-chez-thread* (chez-thread-id *current-chez-thread*) 0))

;;; Chez's mutexes are recursive; bordeaux-threads' recursive locks
;;; aren't on every Lisp (SBCL's aren't), so the recursion is counted
;;; here, around a plain lock.
(defstruct (chez-mutex (:constructor make-chez-mutex (lock name)))
  lock name (owner nil) (count 0))

(defun mutex-acquire (m wait)
  (let ((self (bt2:current-thread)))
    (cond ((eq (chez-mutex-owner m) self) (incf (chez-mutex-count m)) t)
	  ((bt2:acquire-lock (chez-mutex-lock m) :wait wait)
	   (setf (chez-mutex-owner m) self (chez-mutex-count m) 1)
	   t)
	  (t nil))))

(defun mutex-release (m)
  (unless (eq (chez-mutex-owner m) (bt2:current-thread))
    (r6rs-assertion-violation "mutex-release" "mutex not held by this thread" m))
  (when (zerop (decf (chez-mutex-count m)))
    (setf (chez-mutex-owner m) nil)
    (bt2:release-lock (chez-mutex-lock m))))
(defstruct (chez-condition (:constructor make-chez-condition (cv name))) cv name)

(defprim "chez:make-mutex" (&optional name)
  (make-chez-mutex (bt2:make-lock :name (if (or (null name) (eq name ps:false)) "mutex" (princ-to-string name)))
		   (or name ps:false)))
(defprim "chez:mutex?" (x) (bool (chez-mutex-p x)))
(defprim "chez:mutex-name" (m) (chez-mutex-name m))
(defprim "chez:mutex-acquire" (m &optional (block? t))
  (bool (mutex-acquire m (not (eq block? ps:false)))))
(defprim "chez:mutex-release" (m)
  (mutex-release m)
  ps:unspecific)

(defprim "chez:make-condition" (&optional name)
  (make-chez-condition (bt2:make-condition-variable) (or name ps:false)))
(defprim "chez:condition?" (x) (bool (chez-condition-p x)))
(defprim "chez:condition-wait" (c m &optional timeout)
  ;; TIMEOUT: a time object's seconds, or #f; the result is #t unless it
  ;; timed out.  The mutex is released fully while waiting, however many
  ;; times this thread holds it.
  (let ((seconds (and timeout (not (eq timeout ps:false)) timeout))
	(count (chez-mutex-count m)))
    (setf (chez-mutex-owner m) nil (chez-mutex-count m) 0)
    (prog1 (bool (bt2:condition-wait (chez-condition-cv c) (chez-mutex-lock m) :timeout seconds))
      (setf (chez-mutex-owner m) (bt2:current-thread) (chez-mutex-count m) count))))
(defprim "chez:condition-signal" (c)
  (bt2:condition-notify (chez-condition-cv c))
  ps:unspecific)
(defprim "chez:condition-broadcast" (c)
  (bt2:condition-broadcast (chez-condition-cv c))
  ps:unspecific)

(defprim "chez:sleep-seconds" (seconds)
  (sleep (max 0 seconds))
  ps:unspecific)

;;; ------------------------------------------------------------------
;;; The process

(defprim "chez:get-process-id" ()
  #+sbcl (sb-posix:getpid)
  #-sbcl 0)

(defprim "chez:putenv" (name value)
  #+sbcl (sb-posix:setenv name value 1)
  #-sbcl (setf (uiop:getenv name) value)
  ps:unspecific)

;;; ------------------------------------------------------------------
;;; format: Common Lisp's, which Chez's follows, with every object
;;; printed as Scheme prints it: ~a displays, ~s writes.  Integers and
;;; ratios are left to Lisp, so that ~x, ~b and ~r print them in their
;;; radix.  (A Scheme #f is true to ~[ and ~:[, being Lisp's PS:FALSE.)

(defvar *scheme-print-dispatch*
  (let ((table (copy-pprint-dispatch nil)))
    (set-pprint-dispatch '(not (or integer ratio))
			 (lambda (stream x)
			   ;; the Scheme writer prints some things with Lisp's
			   ;; printer, which mustn't come back here
			   (let ((escape *print-escape*)
				 (*print-pretty* nil))
			     (if escape
				 (funcall ps:*scheme-write* x stream)
				 (funcall ps:*scheme-display* x stream))))
			 0 table)
    table))

(defun chez-format (stream control args)
  (let ((*print-pretty* t)
	(*print-pprint-dispatch* *scheme-print-dispatch*)
	(*print-right-margin* most-positive-fixnum)
	(*print-lines* nil)
	(*print-circle* nil)
	(*read-default-float-format* 'double-float))
    (apply #'format stream control args)))

(defprim "chez:format-to-string" (control &rest args)
  (with-output-to-string (s) (chez-format s control args)))
(defprim "chez:format-to-port" (port control &rest args)
  (chez-format port control args)
  ps:unspecific)

;;; ------------------------------------------------------------------
;;; Numbers, symbols, procedures

(defvar *chez-random-state* (make-random-state t))

(defprim "chez:random" (n)
  (unless (and (realp n) (plusp n))
    (r6rs-assertion-violation "random" "not a positive real" n))
  (if (floatp n)
      (random (coerce n 'double-float) *chez-random-state*)
      (random n *chez-random-state*)))

(defprim "chez:random-seed" (&optional n)
  ;; CL can't seed from a number portably: a fresh state for each seed
  #+sbcl (when n (setq *chez-random-state* (sb-ext:seed-random-state n)))
  #-sbcl (declare (ignore n))
  ps:unspecific)

(macrolet ((def (name fn)
	     `(defprim ,name (x) (ps:inexact (,fn (if (rationalp x) (coerce x 'double-float) x))))))
  (def "chez:sinh" sinh) (def "chez:cosh" cosh) (def "chez:tanh" tanh)
  (def "chez:asinh" asinh) (def "chez:acosh" acosh) (def "chez:atanh" atanh))

(defvar *chez-gensym-counter* 0)

(defprim "chez:make-gensym" (prefix)
  ;; an uninterned symbol, named as Scheme names symbols (case inverted)
  (make-symbol (ps:invert-case (format nil "~A~D" (if (symbolp prefix) (ps:scheme-symbol-name prefix) prefix)
				       (incf *chez-gensym-counter*)))))
(defprim "chez:uninterned-symbol?" (x) (bool (and (symbolp x) x (null (symbol-package x)))))
(defprim "chez:string->uninterned-symbol" (s) (make-symbol (ps:invert-case s)))

(defprim "chez:procedure-arity-mask" (proc)
  ;; bit N set if PROC accepts N arguments (all bits from some on, a
  ;; negative mask, for a rest argument)
  (let ((lambda-list #+sbcl (sb-kernel:%fun-lambda-list (if (functionp proc) proc (fdefinition proc)))
		     #-sbcl :unknown))
    (if (not (listp lambda-list))
	-1
	(let ((required 0) (optional 0) (rest nil) (state :required))
	  (dolist (x lambda-list)
	    (case x
	      (&optional (setq state :optional))
	      ((&rest &body &key) (setq rest t state :done))
	      ((&aux) (setq state :done))
	      (t (case state
		   (:required (incf required))
		   (:optional (incf optional))))))
	  (if rest
	      (- (ash 1 required))
	      (- (ash 1 (+ required optional 1)) (ash 1 required)))))))

;;; ------------------------------------------------------------------
;;; Parameters, as Chez's: R7RS's (parameterize works on them), and
;;; called with a value, they set it (through the converter)

(defprim "chez:make-parameter" (init &optional converter)
  (let* ((converter (and converter (not (eq converter ps:false)) converter))
	 (state (ps-r7rs::make-parameter-state nil converter))
	 (param (lambda (&optional (value nil value-p))
		  (if value-p
		      (progn (setf (ps-r7rs::parameter-state-value state)
				   (if converter (funcall converter value) value))
			     ps:unspecific)
		      (ps-r7rs::parameter-state-value state)))))
    (setf (ps-r7rs::parameter-state-value state)
	  (if converter (funcall converter init) init))
    (setf (gethash param ps-r7rs::*parameter-states*) state)
    param))

;;; ------------------------------------------------------------------
;;; Ports on file descriptors: binary, or transcoded if a transcoder is
;;; given.  Closing one closes the descriptor.

(defclass chez-fd-output-port (standard-binary-output-port) ())
(defmethod close ((s chez-fd-output-port) &key abort)
  (declare (ignore abort))
  (finish-output (underlying s))
  (close (underlying s))
  (setf (port-open s) nil)
  t)

(defun fd-port-stream (fd direction)
  #+sbcl (sb-sys:make-fd-stream fd :element-type '(unsigned-byte 8)
				   :input (member direction '(:input :io))
				   :output (member direction '(:output :io))
				   :buffering :full :auto-close t)
  #-sbcl (open (format nil "/dev/fd/~D" fd) :element-type '(unsigned-byte 8)
	       :direction (if (eq direction :io) :io direction) :if-exists :append))

(defun maybe-transcoded (port transcoder)
  (if (and transcoder (not (eq transcoder ps:false)))
      (funcall (cdr (assoc "transcoded-port" *primitives* :test #'string=)) port transcoder)
      port))

(defprim "chez:open-fd-input-port" (fd &optional buffer-mode transcoder)
  (declare (ignore buffer-mode))
  (maybe-transcoded (make-instance 'octet-stream-input-port :stream (fd-port-stream fd :input))
		    transcoder))

(defprim "chez:open-fd-output-port" (fd &optional buffer-mode transcoder)
  (declare (ignore buffer-mode))
  (maybe-transcoded (make-instance 'chez-fd-output-port :stream (fd-port-stream fd :output))
		    transcoder))

(defprim "chez:open-fd-input/output-port" (fd &optional buffer-mode transcoder)
  (declare (ignore buffer-mode))
  ;; two ports' worth of one descriptor: binary input/output port types
  ;; aren't distinguished from their halves here, so reading uses the
  ;; input half
  (maybe-transcoded (make-instance 'octet-stream-input-port :stream (fd-port-stream fd :io))
		    transcoder))

(defprim "chez:port-file-descriptor" (port)
  (or (port-descriptor port) (r6rs-assertion-violation "port-file-descriptor" "not a file port" port)))

(defvar *nonblocking-ports* (trivial-garbage:make-weak-hash-table :weakness :key)
  "The ports set nonblocking: get-bytevector-some! returns 0 on them when
nothing is ready.")

(defprim "chez:port-nonblocking?" (port) (bool (gethash port *nonblocking-ports*)))

(defprim "chez:get-bytevector-some!" (port bv start count)
  (let ((n (read-some-bytes port bv start count (gethash port *nonblocking-ports*))))
    (if (eq n :eof) ps:eof-object n)))

(defprim "chez:set-port-nonblocking!" (port on?)
  (if (eq on? ps:false)
      (remhash port *nonblocking-ports*)
      (setf (gethash port *nonblocking-ports*) t))
  (let ((fd (port-descriptor port)))
    #+sbcl (when fd
	     (let ((flags (sb-posix:fcntl fd sb-posix:f-getfl)))
	       (sb-posix:fcntl fd sb-posix:f-setfl
			       (if (eq on? ps:false)
				   (logandc2 flags sb-posix:o-nonblock)
				   (logior flags sb-posix:o-nonblock)))))
    #-sbcl (declare (ignore fd on?)))
  ps:unspecific)

(defprim "chez:port-closed?" (port)
  (bool (not (open-stream-p port))))

;;; Chez's file-opening options: (open-output-file path 'append) and the
;;; like.  Of the existing-file options, error is the default (as in
;;; Chez), truncate empties the file, replace deletes it first, append
;;; writes at its end; compressed, buffered and the rest are accepted
;;; and ignored.

(defun chez-file-options (options)
  (mapcar #'ps:scheme-symbol-name (if (listp options) options (list options))))

(defprim "chez:open-output-file" (path &optional options)
  (let* ((names (chez-file-options options))
	 (if-exists (cond ((member "append" names :test #'string=) :append)
			  ((member "truncate" names :test #'string=) :supersede)
			  ((member "replace" names :test #'string=)
			   (when (probe-file path) (delete-file path))
			   :supersede)
			  (t nil))))
    (when (and (null if-exists) (probe-file path))
      (io-error-filename "open-output-file" path "make-i/o-file-already-exists-error"))
    (open path :direction :output :external-format :utf-8
	       :if-exists (or if-exists :supersede) :if-does-not-exist :create)))

(defprim "chez:open-input-file" (path)
  (unless (probe-file path)
    (io-error-filename "open-input-file" path "make-i/o-file-does-not-exist-error"))
  (open path :direction :input :external-format :utf-8))
