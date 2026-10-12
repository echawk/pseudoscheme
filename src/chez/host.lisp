; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-R6RS -*-

;;;; Host procedures behind Chez Scheme's (chezscheme) library
;;;; (src/chez/*.scm): what needs the operating system, threads, or Common
;;;; Lisp.  Named chez:..., they reach Scheme through (pseudoscheme chez
;;;; host), which src/chez/chez.lisp makes of every chez: primitive.

(in-package "PSEUDOSCHEME-R6RS")

(defun path-string (x) (if (pathnamep x) (namestring x) x))

(defparameter *posix-spawn-p*
  (find "USE-POSIX-SPAWN" (sb-kernel:%fun-lambda-list #'sb-ext:run-program)
	:key (lambda (x) (symbol-name (if (consp x) (car x) x))) :test #'string=)
  "Whether this SBCL's RUN-PROGRAM can use posix_spawn (2.3 and later):
much faster than fork for a process of a large heap, a few ms rather
than a few hundred.")

(defprim "chez:system" (command)
  ;; sh -c, its standard streams the process's own, written after what
  ;; is already ours
  (finish-output *standard-output*)
  (sb-ext:process-exit-code
   (apply #'sb-ext:run-program "/bin/sh" (list "-c" command)
	  :input t :output t :error t :wait t
	  (and *posix-spawn-p* '(:use-posix-spawn t)))))

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

;;; stat(2) and lstat(2), through sb-posix: Chez's file predicates
;;; follow symbolic links unless their follow? argument is #f.

(defun file-stat (name follow?)
  "The stat of file NAME (lstat unless FOLLOW?), or NIL if there is none."
  (let ((path (namestring (uiop:parse-native-namestring name))))
    (handler-case (if follow? (sb-posix:stat path) (sb-posix:lstat path))
      (sb-posix:syscall-error () nil))))

(defun file-type-p (name follow? type)
  (let ((stat (file-stat name follow?)))
    (ps:true? (and stat (= (logand (sb-posix:stat-mode stat) sb-posix:s-ifmt) type)))))

(defun follow-arg (follow?) (not (eq follow? ps:false)))

(defprim "chez:file-exists?" (name &optional (follow? t))
  (ps:true? (and (file-stat name (follow-arg follow?)) t)))

(defprim "chez:file-directory?" (name &optional (follow? t))
  (file-type-p name (follow-arg follow?) sb-posix:s-ifdir))

(defprim "chez:file-regular?" (name &optional (follow? t))
  (file-type-p name (follow-arg follow?) sb-posix:s-ifreg))

(defprim "chez:file-symbolic-link?" (name)
  (file-type-p name nil sb-posix:s-iflnk))

(defprim "chez:file-times" (name &optional (follow? t))
  "(access change modification), in seconds since the epoch."
  (let ((stat (or (file-stat name (follow-arg follow?))
		  (ps:scheme-error "failed for ~S: no such file or directory" name))))
    (list (sb-posix:stat-atime stat) (sb-posix:stat-ctime stat) (sb-posix:stat-mtime stat))))

(defun call-for-file (who name error? function)
  "Call FUNCTION on NAME's native path: #t, or on failure #f, or an
error if ERROR? is true."
  (handler-case (progn (funcall function (namestring (uiop:parse-native-namestring name))) ps:true)
    (sb-posix:syscall-error (e)
      (if (and error? (not (eq error? ps:false)))
	  (ps:scheme-error "~A: failed for ~A: ~A" who name
			   (sb-int:strerror (sb-posix:syscall-errno e)))
	  ps:false))))

(defprim "chez:delete-file" (name &optional error?)
  (call-for-file "delete-file" name error? #'sb-posix:unlink))

(defprim "chez:delete-directory" (name &optional error?)
  (call-for-file "delete-directory" name error? #'sb-posix:rmdir))

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

(defprim "chez:rename-file" (old new)
  (rename-file (uiop:parse-native-namestring old) (uiop:parse-native-namestring new))
  ps:unspecific)

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
  #+sbcl
  (let ((path (namestring (uiop:parse-native-namestring name))))
    (logand #o7777 (sb-posix:stat-mode (if (ps:truep follow?) (sb-posix:stat path) (sb-posix:lstat path)))))
  ;; GNU's %a is octal digits; BSD's %Lp too.
  #-sbcl (parse-integer (princ-to-string (stat-field name "%a" "%Lp")) :radix 8))

(defprim "chez:chmod" (name mode)
  #+sbcl (sb-posix:chmod (namestring (uiop:parse-native-namestring name)) mode)
  #-sbcl (uiop:run-program (list "chmod" (format nil "~O" mode)
				 (namestring (uiop:parse-native-namestring name))))
  ps:unspecific)

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

(defun vector-copy! (who check src src-start dst dst-start n)
  "Chez's fxvector-copy! and flvector-copy!: N elements, as memmove."
  (funcall check who src)
  (funcall check who dst)
  (unless (and (typep src-start '(integer 0)) (typep dst-start '(integer 0)) (typep n '(integer 0))
	       (<= (+ src-start n) (length src)) (<= (+ dst-start n) (length dst)))
    (r6rs-assertion-violation who "invalid range" (list src-start dst-start n)))
  (replace dst src :start1 dst-start :end1 (+ dst-start n) :start2 src-start)
  ps:unspecific)

(defprim "chez:fxvector-copy!" (src src-start dst dst-start n)
  (vector-copy! "fxvector-copy!" #'check-fxvector src src-start dst dst-start n))

(defun flvector-p (x) (typep x '(simple-array double-float (*))))
(defun check-flvector (who v)
  (unless (flvector-p v) (r6rs-assertion-violation who "not an flvector" v)))
(defun check-flonum (who x)
  (unless (typep x 'double-float) (r6rs-assertion-violation who "not a flonum" x)))
(defun list->flvector (who xs)
  (dolist (x xs) (check-flonum who x))
  (make-array (length xs) :element-type 'double-float :initial-contents xs))

(defprim "chez:flvector?" (x) (bool (flvector-p x)))
(defprim "chez:make-flvector" (n &optional (fill 0d0))
  (unless (typep n '(integer 0 #.array-dimension-limit))
    (r6rs-assertion-violation "make-flvector" "invalid length" n))
  (check-flonum "make-flvector" fill)
  (make-array n :element-type 'double-float :initial-element fill))
(defprim "chez:flvector" (&rest xs) (list->flvector "flvector" xs))
(defprim "chez:list->flvector" (xs)
  (unless (and (listp xs) (handler-case (list-length xs) (error () nil)))
    (r6rs-assertion-violation "list->flvector" "not a proper list" xs))
  (list->flvector "list->flvector" xs))
(defprim "chez:flvector-length" (v) (check-flvector "flvector-length" v) (length v))
(defun check-flvector-index (who v i)
  (check-flvector who v)
  (unless (and (integerp i) (< -1 i (length v)))
    (r6rs-assertion-violation who "invalid index" i)))
(defprim "chez:flvector-ref" (v i) (check-flvector-index "flvector-ref" v i) (aref v i))
(defprim "chez:flvector-set!" (v i x)
  (check-flvector-index "flvector-set!" v i)
  (check-flonum "flvector-set!" x)
  (setf (aref v i) x)
  ps:unspecific)
(defprim "chez:flvector->list" (v) (check-flvector "flvector->list" v) (coerce v 'list))
(defprim "chez:flvector-fill!" (v x)
  (check-flvector "flvector-fill!" v)
  (check-flonum "flvector-fill!" x)
  (fill v x)
  ps:unspecific)
(defprim "chez:flvector-copy" (v) (check-flvector "flvector-copy" v) (copy-seq v))
(defprim "chez:flvector-copy!" (src src-start dst dst-start n)
  (vector-copy! "flvector-copy!" #'check-flvector src src-start dst dst-start n))

;;; (flbit-field fl start end): bits start to end (exclusive) of fl's
;;; IEEE double representation

(defprim "chez:flbit-field" (x start end)
  (check-flonum "flbit-field" x)
  (unless (and (typep start '(integer 0 64)) (typep end '(integer 0 64)) (<= start end))
    (r6rs-assertion-violation "flbit-field" "invalid range" (list start end)))
  (ldb (byte (- end start) start)
       (logior (ash (ldb (byte 32 0) (sb-kernel:double-float-high-bits x)) 32)
	       (sb-kernel:double-float-low-bits x))))

;;; Fixnum operations that wrap around rather than overflow

(defun check-fixnum (who x)
  (unless (typep x 'fixnum) (r6rs-assertion-violation who "not a fixnum" x)))

(defun wrap-fixnum (n)
  (let ((m (ldb (byte sb-vm:n-fixnum-bits 0) n)))
    (if (logbitp (1- sb-vm:n-fixnum-bits) m) (- m (ash 1 sb-vm:n-fixnum-bits)) m)))

(macrolet ((wrapping (name op)
	     `(defprim ,name (x y)
		(check-fixnum ,name x)
		(check-fixnum ,name y)
		(wrap-fixnum (,op x y)))))
  (wrapping "chez:fx+/wraparound" +)
  (wrapping "chez:fx*/wraparound" *))

(defprim "chez:fx-/wraparound" (x &optional (y nil y-p))
  (check-fixnum "fx-/wraparound" x)
  (if y-p
      (progn (check-fixnum "fx-/wraparound" y) (wrap-fixnum (- x y)))
      (wrap-fixnum (- x))))

(defprim "chez:fxsll/wraparound" (x k)
  (check-fixnum "fxsll/wraparound" x)
  (unless (typep k `(integer 0 ,sb-vm:n-fixnum-bits))
    (r6rs-assertion-violation "fxsll/wraparound" "invalid shift" k))
  (wrap-fixnum (ash x k)))

(defun check-bit-index (who i)
  ;; below the sign bit
  (unless (typep i `(integer 0 (,(1- sb-vm:n-fixnum-bits))))
    (r6rs-assertion-violation who "invalid bit index" i)))

(defprim "chez:fxlogbit0" (i x)
  (check-bit-index "fxlogbit0" i) (check-fixnum "fxlogbit0" x)
  (logandc2 x (ash 1 i)))
(defprim "chez:fxlogbit1" (i x)
  (check-bit-index "fxlogbit1" i) (check-fixnum "fxlogbit1" x)
  (wrap-fixnum (logior x (ash 1 i))))

(macrolet ((popcount (name bits)
	     `(defprim ,name (x)
		(unless (typep x '(integer 0 ,(1- (expt 2 bits))))
		  (r6rs-assertion-violation ,name ,(format nil "not a nonnegative ~D-bit fixnum" bits) x))
		(logcount x))))
  (popcount "chez:fxpopcount32" 32)
  (popcount "chez:fxpopcount16" 16))

;;; statistics: bytes allocated, and collections and the time they took

(defvar *gc-count* 0)
(defun count-gc () (incf *gc-count*))
(pushnew 'count-gc sb-ext:*after-gc-hooks*)

(defprim "chez:bytes-consed" () (sb-ext:get-bytes-consed))
(defprim "chez:bytes-allocated" () (sb-kernel:dynamic-usage))
;;; Chez's virtual registers: 16 per thread
(defvar *virtual-registers* (make-hash-table :test 'eq :weakness :key :synchronized t))
(defun virtual-registers ()
  (or (gethash sb-thread:*current-thread* *virtual-registers*)
      (setf (gethash sb-thread:*current-thread* *virtual-registers*) (make-array 16 :initial-element 0))))
(defprim "chez:virtual-register" (i) (svref (virtual-registers) i))
(defprim "chez:set-virtual-register!" (i x) (setf (svref (virtual-registers) i) x) ps:unspecific)
(defprim "chez:collect" ()
  (sb-ext:gc :full t)
  (ps-r6rs::note-full-collection)
  ps:unspecific)

;;; Guardians (src/r6rs/guardians.lisp): a guardian is a procedure

(defvar *guardian-procedures* (trivial-garbage:make-weak-hash-table :weakness :key :test 'eq :synchronized t))

(defprim "chez:make-guardian" (&optional (ordered ps:false))
  (let* ((g (ps-r6rs::new-guardian (ps:true? ordered)))
	 (procedure (lambda (&optional (object nil object-p) (representative object))
		      (if object-p
			  (ps-r6rs::guardian-register g object representative)
			  (ps-r6rs::guardian-next g)))))
    (setf (gethash procedure *guardian-procedures*) g)
    procedure))

(defprim "chez:guardian?" (x) (bool (and (functionp x) (gethash x *guardian-procedures*))))

(defprim "chez:unregister-guardian" (procedure)
  (let ((g (and (functionp procedure) (gethash procedure *guardian-procedures*))))
    (unless g (ps:scheme-error "unregister-guardian: ~S is not a guardian" procedure))
    (ps-r6rs::guardian-unregister g)))
(defprim "chez:gc-statistics" ()
  (vector *gc-count* (/ sb-ext:*gc-run-time* internal-time-units-per-second)))

;;; ------------------------------------------------------------------
;;; Weak and ephemeron hashtables: R6RS hashtables whose CL table is weak
;;; on its keys (in SBCL an entry so held is an ephemeron: its value
;;; doesn't keep its key alive)

(defun make-weak-table (kind test weakness)
  (%make-hashtable (trivial-garbage:make-weak-hash-table :test test :weakness :key)
		   kind nil nil t weakness))

(defprim "chez:make-weak-eq-hashtable" (&optional k)
  (check-hashtable-size "make-weak-eq-hashtable" k)
  (make-weak-table :eq 'eq :weak))
(defprim "chez:make-weak-eqv-hashtable" (&optional k)
  (check-hashtable-size "make-weak-eqv-hashtable" k)
  (make-weak-table :eqv 'eql :weak))
(defprim "chez:make-ephemeron-eq-hashtable" (&optional k)
  (check-hashtable-size "make-ephemeron-eq-hashtable" k)
  (make-weak-table :eq 'eq :ephemeron))
(defprim "chez:make-ephemeron-eqv-hashtable" (&optional k)
  (check-hashtable-size "make-ephemeron-eqv-hashtable" k)
  (make-weak-table :eqv 'eql :ephemeron))

(defun make-weak-custom-table (who hash equiv weakness)
  (check-procedure who hash)
  (check-procedure who equiv)
  (%make-hashtable (make-hash-table :test 'eql) :custom hash equiv t weakness
		   (and (eq weakness :ephemeron)
			(trivial-garbage:make-weak-hash-table :test 'eq :weakness :key))))

(defprim "chez:make-weak-hashtable" (hash equiv &optional k)
  (declare (ignore k))
  (make-weak-custom-table "make-weak-hashtable" hash equiv :weak))
(defprim "chez:make-ephemeron-hashtable" (hash equiv &optional k)
  (declare (ignore k))
  (make-weak-custom-table "make-ephemeron-hashtable" hash equiv :ephemeron))

(defprim "chez:hashtable-weak?" (h)
  (check-hashtable "hashtable-weak?" h)
  (bool (eq (hashtable-weakness h) :weak)))
(defprim "chez:hashtable-ephemeron?" (h)
  (check-hashtable "hashtable-ephemeron?" h)
  (bool (eq (hashtable-weakness h) :ephemeron)))

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

;;; Chez 10's wrapper procedures: a procedure that calls another, with
;;; data and an arity mask of its own; an arity wrapper checks its
;;; arguments' count against the mask.

(defclass wrapper-procedure (sb-mop:funcallable-standard-object)
  ((proc :initarg :proc :accessor wrapper-proc)
   (mask :initarg :mask :reader wrapper-mask)
   (data :initarg :data :accessor wrapper-data))
  (:metaclass sb-mop:funcallable-standard-class))

(defun make-wrapper (who proc mask data check-arity)
  (unless (functionp proc) (r6rs-assertion-violation who "not a procedure" proc))
  (unless (integerp mask) (r6rs-assertion-violation who "not an exact integer" mask))
  (let ((w (make-instance 'wrapper-procedure :proc proc :mask mask :data data)))
    (sb-mop:set-funcallable-instance-function
     w (if check-arity
	   (lambda (&rest args)
	     (unless (logbitp (length args) mask)
	       (r6rs-assertion-violation nil "incorrect number of arguments" (length args)))
	     (apply (wrapper-proc w) args))
	   (lambda (&rest args) (apply (wrapper-proc w) args))))
    w))

(defun check-wrapper (who x)
  (unless (typep x 'wrapper-procedure) (r6rs-assertion-violation who "not a wrapper procedure" x)))

(defprim "chez:make-wrapper-procedure" (proc mask data)
  (make-wrapper "make-wrapper-procedure" proc mask data nil))
(defprim "chez:make-arity-wrapper-procedure" (proc mask data)
  (make-wrapper "make-arity-wrapper-procedure" proc mask data t))
(defprim "chez:wrapper-procedure?" (x) (bool (typep x 'wrapper-procedure)))
(defprim "chez:wrapper-procedure-procedure" (w)
  (check-wrapper "wrapper-procedure-procedure" w) (wrapper-proc w))
(defprim "chez:wrapper-procedure-data" (w)
  (check-wrapper "wrapper-procedure-data" w) (wrapper-data w))
(defprim "chez:set-wrapper-procedure-procedure!" (w proc)
  (check-wrapper "set-wrapper-procedure-procedure!" w)
  (unless (functionp proc) (r6rs-assertion-violation "set-wrapper-procedure-procedure!" "not a procedure" proc))
  (setf (wrapper-proc w) proc)
  ps:unspecific)
(defprim "chez:set-wrapper-procedure-data!" (w data)
  (check-wrapper "set-wrapper-procedure-data!" w)
  (setf (wrapper-data w) data)
  ps:unspecific)

(defun lambda-list-arity-mask (proc)
  "Bit N set if PROC accepts N arguments (all bits from some on, a
negative mask, for a rest argument)."
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

(defprim "chez:procedure-arity-mask" (proc)
  (if (typep proc 'wrapper-procedure)
      (wrapper-mask proc)
      (lambda-list-arity-mask proc)))

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

;;; Stencil vectors (ps:stencil-vector, src/core.lisp)

(defun check-stencil-vector (who x)
  (unless (ps:stencil-vector-p x)
    (ps:scheme-error "~A: ~S is not a stencil vector" who x))
  x)

(defun stencil-vector-index (who sv i)
  (unless (and (integerp i) (< -1 i (length (ps:stencil-vector-elements sv))))
    (ps:scheme-error "~A: invalid index ~S" who i))
  i)

(defprim "chez:stencil-vector" (mask &rest elements)
  (ps:make-stencil-vector mask elements))

(defprim "chez:stencil-vector?" (x) (ps:true? (ps:stencil-vector-p x)))

(defprim "chez:stencil-vector-mask" (sv)
  (ps:stencil-vector-mask (check-stencil-vector "stencil-vector-mask" sv)))

(defprim "chez:stencil-vector-length" (sv)
  (length (ps:stencil-vector-elements (check-stencil-vector "stencil-vector-length" sv))))

(defprim "chez:stencil-vector-ref" (sv i)
  (check-stencil-vector "stencil-vector-ref" sv)
  (svref (ps:stencil-vector-elements sv) (stencil-vector-index "stencil-vector-ref" sv i)))

(defprim "chez:stencil-vector-set!" (sv i x)
  (check-stencil-vector "stencil-vector-set!" sv)
  (setf (svref (ps:stencil-vector-elements sv) (stencil-vector-index "stencil-vector-set!" sv i)) x)
  ps:unspecific)

(defprim "chez:stencil-vector-mask-width" () ps:stencil-vector-mask-width)

(defprim "chez:stencil-vector-update" (sv remove add &rest new)
  (check-stencil-vector "stencil-vector-update" sv)
  (let* ((mask (ps:stencil-vector-mask sv))
	 (kept (logandc2 mask remove)))
    (unless (and (integerp remove) (integerp add) (>= remove 0) (>= add 0)
		 (= (logand mask remove) remove)
		 (zerop (logand kept add))
		 (= (logcount add) (length new)))
      (ps:scheme-error "stencil-vector-update: invalid masks ~S and ~S for ~S" remove add sv))
    (let ((old (ps:stencil-vector-elements sv)) (oi 0) (out '()))
      (loop for bit from 0 below ps:stencil-vector-mask-width
	    do (cond ((logbitp bit add) (push (pop new) out))
		     ((logbitp bit kept) (push (svref old oi) out)))
	       (when (logbitp bit mask) (incf oi)))
      (ps:make-stencil-vector (logior kept add) (nreverse out)))))

(defprim "chez:stencil-vector-truncate!" (sv mask)
  (check-stencil-vector "stencil-vector-truncate!" sv)
  (let ((old (ps:stencil-vector-elements sv)))
    (unless (and (integerp mask) (>= mask 0) (<= (logcount mask) (length old)))
      (ps:scheme-error "stencil-vector-truncate!: invalid mask ~S for ~S" mask sv))
    (setf (ps:stencil-vector-elements sv) (subseq old 0 (logcount mask))
	  (ps:stencil-vector-mask sv) mask)
    ps:unspecific))

;;; ------------------------------------------------------------------
;;; For format (src/chez/format.scm, Chez Scheme's own): its column-
;;; tracking ports, fresh-line, and decode-float.

(defclass format-port (trivial-gray-streams:fundamental-character-output-stream)
  ((target :initarg :target :reader format-port-target)
   (column :initform 0 :accessor format-port-column))
  (:documentation "Chez's format port: what is written goes to TARGET,
and the column is kept, from 0 when it's made, for ~t, ~& and ~<...~>."))

(defmethod trivial-gray-streams:stream-write-char ((p format-port) c)
  (write-char c (format-port-target p))
  (setf (format-port-column p) (if (char= c #\Newline) 0 (1+ (format-port-column p))))
  c)

(defmethod trivial-gray-streams:stream-write-string ((p format-port) s &optional (start 0) end)
  (let* ((end (or end (length s)))
	 (newline (position #\Newline s :start start :end end :from-end t)))
    (write-string s (format-port-target p) :start start :end end)
    (setf (format-port-column p)
	  (if newline (- end newline 1) (+ (format-port-column p) (- end start))))
    s))

(defmethod trivial-gray-streams:stream-line-column ((p format-port))
  (format-port-column p))

(defmethod trivial-gray-streams:stream-force-output ((p format-port))
  (force-output (format-port-target p)))

(defmethod trivial-gray-streams:stream-finish-output ((p format-port))
  (finish-output (format-port-target p)))

(defmethod close ((p format-port) &key abort)
  (declare (ignore abort))
  ;; the port, not its target, is closed
  (call-next-method))

(defprim "chez:make-format-port" (target)
  (make-instance 'format-port :target target))

(defprim "chez:format-port-column" (p)
  (if (typep p 'format-port) (format-port-column p) 0))

(defprim "chez:fresh-line" (&optional (port *standard-output*))
  (fresh-line port)
  ps:unspecific)

(defprim "chez:decode-float" (x)
  "Chez's decode-float: #(significand exponent sign), x being
significand * 2^exponent * sign."
  (multiple-value-bind (f e s) (integer-decode-float (float x 1d0))
    (vector f e (if (minusp s) -1 1))))

;;; Chez's older records (src/chez/records.scm): typed fields, as in
;;; (immutable double-float a), whose values constructors and mutators
;;; check; record-constructor of a record-type descriptor; and csv7:'s
;;; field procedures, which name a field or number it among all of a
;;; type's fields, its ancestors' first.

(defvar *record-field-types* (make-hash-table :test 'eq :weakness :key)
  "Record-type descriptor -> its own fields' types: names (strings), or
NIL for any value.")

(defun integer-field-bits (type)
  "For an integer field type: (signed-p . bits), else NIL."
  (flet ((suffix (prefix)
	   (and (> (length type) (length prefix)) (string= prefix type :end2 (length prefix))
		(parse-integer type :start (length prefix) :junk-allowed t))))
    (let ((alias (assoc type '(("short" t . 16) ("int" t . 32) ("long" t . 64) ("long-long" t . 64)
			       ("iptr" t . 64) ("ptrdiff_t" t . 64) ("ssize_t" t . 64)
			       ("unsigned-short" nil . 16) ("unsigned" nil . 32) ("unsigned-int" nil . 32)
			       ("unsigned-long" nil . 64) ("unsigned-long-long" nil . 64)
			       ("uptr" nil . 64) ("size_t" nil . 64) ("void*" nil . 64))
			:test #'string=)))
      (cond (alias (cdr alias))
	    ((suffix "integer-") (cons t (suffix "integer-")))
	    ((suffix "unsigned-") (cons nil (suffix "unsigned-")))))))

(defun field-type-ok-p (type x)
  (let ((bits (and type (integer-field-bits type))))
    (cond ((null type) t)
	  (bits (and (integerp x)
		     (if (car bits)
			 (typep x `(signed-byte ,(cdr bits)))
			 ;; Chez accepts a negative value of the width too
			 (typep x `(integer ,(- (expt 2 (1- (cdr bits)))) ,(1- (expt 2 (cdr bits))))))))
	  ((member type '("double-float" "double" "single-float" "float") :test #'string=) (floatp x))
	  ((string= type "fixnum") (typep x 'fixnum))
	  ((member type '("char" "wchar" "wchar_t") :test #'string=) (characterp x))
	  (t t))))

(defun check-field-value (who type x)
  (unless (field-type-ok-p type x)
    (ps:scheme-error "~A: invalid value ~S for ~A field" who x type)))

(defprim "chez:set-record-field-types!" (rtd types)
  "TYPES: a list, a type name or #f for each of RTD's own fields."
  (setf (gethash rtd *record-field-types*)
	(mapcar (lambda (ty) (if (eq ty ps:false) nil (ps:scheme-symbol-name ty))) types))
  ps:unspecific)

(defun all-fields (rtd)
  "Every field of RTD's records, (mutable-p name type), its ancestors' first."
  (let ((parent (rtd-parent rtd)))
    (append (and parent (all-fields parent))
	    (loop for (mutable-p . name) across (rtd-fields rtd)
		  for types = (gethash rtd *record-field-types*) then (cdr types)
		  collect (list mutable-p name (car types))))))

(defun rtd-record-constructor (rtd)
  "Chez's record-constructor of a record-type descriptor: a procedure of
every field's value, its ancestors' first."
  (check-rtd "record-constructor" rtd)
  (let ((types (mapcar #'third (all-fields rtd)))
	(n (rtd-count rtd))
	(name (scheme-name (rtd-name rtd))))
    (lambda (&rest args)
      (unless (= (length args) n)
	(ps:scheme-error "~A: incorrect number of arguments ~D (expected ~D)" name (length args) n))
      (loop for type in types for x in args do (check-field-value name type x))
      (%make-record rtd (coerce args 'simple-vector)))))

(defprim "chez:record-constructor" (rtd) (rtd-record-constructor rtd))

(defun field-position (who rtd field)
  "The index in RTD's records' values of FIELD, a name or an index, and
the field, (mutable-p name type)."
  (check-rtd who rtd)
  (let* ((fields (all-fields rtd))
	 (i (cond ((and (integerp field) (< -1 field (length fields))) field)
		  ((symbolp field) (position field fields :key #'second :from-end t)))))
    (unless i (ps:scheme-error "~A: invalid field ~S for ~A" who field (scheme-name (rtd-name rtd))))
    (values i (nth i fields))))

(defprim "chez:csv7-record-field-accessor" (rtd field)
  (let ((i (field-position "record-field-accessor" rtd field)))
    (lambda (r)
      (unless (record-of-type-p r rtd)
	(ps:scheme-error "~S is not of type ~A" r (scheme-name (rtd-name rtd))))
      (svref (record-values r) i))))

(defprim "chez:csv7-record-field-mutator" (rtd field)
  (multiple-value-bind (i f) (field-position "record-field-mutator" rtd field)
    (unless (first f)
      (ps:scheme-error "record-field-mutator: field ~S of ~A is immutable" field (scheme-name (rtd-name rtd))))
    (lambda (r x)
      (unless (record-of-type-p r rtd)
	(ps:scheme-error "~S is not of type ~A" r (scheme-name (rtd-name rtd))))
      (check-field-value (scheme-name (rtd-name rtd)) (third f) x)
      (setf (svref (record-values r) i) x)
      ps:unspecific)))

(defprim "chez:csv7-record-field-accessible?" (rtd field)
  (field-position "record-field-accessible?" rtd field)
  ps:true)

(defprim "chez:csv7-record-field-mutable?" (rtd field)
  (bool (first (nth-value 1 (field-position "record-field-mutable?" rtd field)))))

(defprim "chez:csv7-record-type-field-names" (rtd)
  (check-rtd "record-type-field-names" rtd)
  (mapcar #'second (all-fields rtd)))

(defprim "chez:csv7-record-type-field-decls" (rtd)
  (check-rtd "record-type-field-decls" rtd)
  (mapcar (lambda (f)
	    (list (ps:intern-scheme-symbol (if (first f) "mutable" "immutable"))
		  (ps:intern-scheme-symbol (or (third f) "scheme-object"))
		  (second f)))
	  (all-fields rtd)))

;;; record-type-equal-procedure and record-type-hash-procedure: equal?
;;; and equal-hash on records (through PS::*RECORD-EQUAL-HOOK* and
;;; *RECORD-HASH-HOOK*, in the Chez mode only).  Two records are compared
;;; by the procedure of the nearest ancestor type (inclusive) of both
;;; that has one, if that is the same type for both; else by the
;;; default-record-equal-procedure parameter's, else by eq?.

(defvar *record-equal-procedures* (make-hash-table :test 'eq :weakness :key))
(defvar *record-hash-procedures* (make-hash-table :test 'eq :weakness :key))
(defvar *record-procedure-defaults* nil
  "(default-record-equal-procedure . default-record-hash-procedure),
the parameters of src/chez/records.scm.")

(defun record-type-procedure (table who rtd args)
  (check-rtd who rtd)
  (if (null args)
      (gethash rtd table ps:false)
      (let ((proc (car args)))
	(cond ((eq proc ps:false) (remhash rtd table))
	      ((functionp proc) (setf (gethash rtd table) proc))
	      (t (ps:scheme-error "~A: ~S is not a procedure or #f" who proc)))
	ps:unspecific)))

(defprim "chez:record-type-equal-procedure" (rtd &rest args)
  (record-type-procedure *record-equal-procedures* "record-type-equal-procedure" rtd args))

(defprim "chez:record-type-hash-procedure" (rtd &rest args)
  (record-type-procedure *record-hash-procedures* "record-type-hash-procedure" rtd args))

(defprim "chez:set-record-procedure-defaults!" (equal hash)
  (setq *record-procedure-defaults* (cons equal hash))
  ps:unspecific)

(defun nearest-type-with (table rtd)
  (loop for r = rtd then (rtd-parent r) while r when (gethash r table) return r))

(defun record-procedure-default (key)
  (let ((param (and *record-procedure-defaults* (funcall key *record-procedure-defaults*))))
    (if param (funcall param) ps:false)))

(defun check-record (who x)
  (unless (record-p x) (ps:scheme-error "~A: ~S is not a record" who x)))

(defun record-equal-procedure (r1 r2)
  (check-record "record-equal-procedure" r1)
  (check-record "record-equal-procedure" r2)
  (let ((t1 (nearest-type-with *record-equal-procedures* (record-rtd r1)))
	(t2 (nearest-type-with *record-equal-procedures* (record-rtd r2))))
    (if (or t1 t2)
	(if (eq t1 t2) (gethash t1 *record-equal-procedures*) ps:false)
	(record-procedure-default #'car))))

(defun record-hash-procedure (r)
  (check-record "record-hash-procedure" r)
  (let ((rtd (nearest-type-with *record-hash-procedures* (record-rtd r))))
    (if rtd (gethash rtd *record-hash-procedures*) (record-procedure-default #'cdr))))

(defprim "chez:record-equal-procedure" (r1 r2) (record-equal-procedure r1 r2))
(defprim "chez:record-hash-procedure" (r) (record-hash-procedure r))

(defun chez-record-equal (a b recur)
  (if (and ps:*chez-reader* (record-p a) (record-p b))
      (let ((proc (record-equal-procedure a b)))
	(if (eq proc ps:false)
	    (eq a b)
	    (ps:truep (funcall proc a b (lambda (x y) (bool (funcall recur x y)))))))
      :pass))

(defun chez-record-hash (r walk)
  (when (and ps:*chez-reader* (record-p r))
    (let ((proc (record-hash-procedure r)))
      (unless (eq proc ps:false)
	(let ((h (funcall proc r (lambda (x) (funcall walk x)))))
	  (unless (integerp h)
	    (ps:scheme-error "record hash procedure returned ~S, not an exact integer" h))
	  (logand most-positive-fixnum h))))))

(setq ps::*record-equal-hook* 'chez-record-equal
      *record-hash-hook* 'chez-record-hash)

(defprim "chez:record-constructor-descriptor?" (x) (bool (rcd-p x)))

;;; record-reader and record-writer: #[name field ...], read and written
;;; in this mode (PS::*RECORD-LITERAL-HOOK*, PS::*WRITE-EXTENSION*).  A
;;; record is written as #[name field ...] unless its type, or an
;;; ancestor, has a writer of its own.

(defvar *record-readers* (make-hash-table :test 'eq)
  "Name -> record-type descriptor, for #[name field ...].")
(defvar *record-reader-names* (make-hash-table :test 'eq :weakness :key)
  "Record-type descriptor -> its name in *RECORD-READERS*.")
(defvar *record-writers* (make-hash-table :test 'eq :weakness :key))

(defun record-reader-name-p (x) (and (ps:scheme-symbol-p x) (not (eq x ps:false))))

(defprim "chez:record-reader" (x &optional (y nil y-p))
  ;; a name may be a string
  (when (stringp x) (setq x (ps:intern-scheme-symbol x)))
  (cond ((not y-p)
	 (cond ((rtd-p x) (gethash x *record-reader-names* ps:false))
	       ((record-reader-name-p x) (gethash x *record-readers* ps:false))
	       (t (r6rs-assertion-violation "record-reader" "not a name or record-type descriptor" x))))
	((and (record-reader-name-p x) (rtd-p y))
	 (let ((old (gethash x *record-readers*)))
	   (when old (remhash old *record-reader-names*)))
	 (let ((old-name (gethash y *record-reader-names*)))
	   (when old-name (remhash old-name *record-readers*)))
	 (setf (gethash x *record-readers*) y (gethash y *record-reader-names*) x)
	 ps:unspecific)
	((and (eq y ps:false) (or (rtd-p x) (record-reader-name-p x)))
	 (let ((other (if (rtd-p x) (gethash x *record-reader-names*) (gethash x *record-readers*))))
	   (when other
	     (if (rtd-p x)
		 (progn (remhash x *record-reader-names*) (remhash other *record-readers*))
		 (progn (remhash x *record-readers*) (remhash other *record-reader-names*)))))
	 ps:unspecific)
	(t (r6rs-assertion-violation "record-reader" "invalid arguments" (list x y)))))

(defun read-record-literal (elements)
  (let ((rtd (and (consp elements) (gethash (car elements) *record-readers*))))
    (unless rtd
      (ps:scheme-error "#[...]: no record type is named ~S" (if (consp elements) (car elements) elements)))
    ;; the record is made before its labels are resolved, so a field can't
    ;; be one: Chez's #0=#[foo #0#] is an error
    (when (some #'ps::label-placeholder-p (cdr elements))
      (ps:scheme-error "#[...]: a datum label can't be a field of the record it labels"))
    (apply (rtd-record-constructor rtd) (cdr elements))))

(defprim "chez:record-writer" (rtd &optional (proc nil proc-p))
  (check-rtd "record-writer" rtd)
  (cond ((not proc-p) (gethash rtd *record-writers* ps:false))
	((eq proc ps:false) (remhash rtd *record-writers*) ps:unspecific)
	((functionp proc) (setf (gethash rtd *record-writers*) proc) ps:unspecific)
	(t (r6rs-assertion-violation "record-writer" "not a procedure or #f" proc))))

(defun write-record (obj port recur)
  "Write record OBJ as Chez does, in the Chez mode; NIL otherwise.  The
unspecified value too: #<void>."
  (when (and ps:*chez-reader* (eq obj ps:unspecific))
    (write-string "#<void>" port)
    (return-from write-record t))
  (when (and ps:*chez-reader* (record-p obj) (not (condition-p* obj)))
    (let* ((rtd (record-rtd obj))
	   (writer (loop for r = rtd then (rtd-parent r) while r
			 thereis (gethash r *record-writers*))))
      (if writer
	  (funcall writer obj port (lambda (x p) (declare (ignore p)) (funcall recur x)))
	  (progn
	    (write-string "#[" port)
	    (funcall recur (rtd-name rtd))
	    (loop for x across (record-values obj)
		  do (write-char #\Space port) (funcall recur x))
	    (write-string "]" port)))
      t)))

(defun record-parts (x)
  "A record's fields, for datum labels (#0=#[foo #0#])."
  (and ps:*chez-reader* (record-p x) (not (condition-p* x)) (record-values x)))

(setq ps::*record-literal-hook* 'read-record-literal
      ps::*write-extension* 'write-record
      ps::*datum-label-parts* 'record-parts)

;;; ------------------------------------------------------------------
;;; Immutable vectors, strings and bytevectors, and the empty ones
;;;
;;; An immutable one carries a flag in its header that SBCL leaves to
;;; implementations (+vector-shareable-nonstd+; its fasl loader marks
;;; constants with the other one, +vector-shareable+), so telling costs a
;;; read of the header.  In the Chez mode the mutators check it
;;; (src/chez/chez.lisp, *CHEZ-CHECKED-MUTATORS*).  As in Chez, the
;;; empty vector, string and bytevector are each a single object, and
;;; immutable.

(defconstant +immutable-flag+ (ash sb-vm:+vector-shareable-nonstd+ 8))

(declaim (inline immutable-p))
(defun immutable-p (x)
  (and (typep x '(simple-array * (*)))
       (logtest +immutable-flag+ (sb-kernel:get-header-data x))))

(defun mark-immutable (x) (sb-impl::logically-readonlyize x nil))

(defvar *empty-vector* (mark-immutable (make-array 0)))
(defvar *empty-string* (mark-immutable (make-string 0)))
(defvar *empty-bytevector* (mark-immutable (make-array 0 :element-type '(unsigned-byte 8))))

(defun empty-like (x)
  "The single empty object of empty X's kind, or X if it has none."
  (typecase x
    (simple-vector *empty-vector*)
    (string *empty-string*)
    ((simple-array (unsigned-byte 8) (*)) *empty-bytevector*)
    (t x)))

(defun immutable (x)
  "X, a fresh vector, string or bytevector, made immutable (the empty
one if it is empty)."
  (if (zerop (length x)) (empty-like x) (mark-immutable x)))

(defun mutable (x)
  "X, a fresh vector, string or bytevector: the empty one if empty."
  (if (zerop (length x)) (empty-like x) x))

(defun check-mutable-object (who x kind)
  (unless (cond ((string= kind "vector") (simple-vector-p x))
		((string= kind "string") (stringp x))
		(t (typep x '(simple-array (unsigned-byte 8) (*)))))
    (r6rs-assertion-violation (ps:intern-scheme-symbol who) (format nil "~~s is not a ~A" kind) x))
  ;; the empty ones are immutable, but nothing of them can be changed
  (when (and (immutable-p x) (plusp (length x)))
    (r6rs-assertion-violation (ps:intern-scheme-symbol who) (format nil "~~s is not a mutable ~A" kind) x)))

(defun check-vector (who v)
  (unless (simple-vector-p v)
    (r6rs-assertion-violation (ps:intern-scheme-symbol who) "~s is not a vector" v)))

(defun vector-range (who v start n)
  "START and N, defaulted and checked: Chez's vector-copy takes a start
and a count."
  (check-vector who v)
  (let* ((len (length v))
	 (start (or start 0)))
    (unless (and (typep start 'fixnum) (<= 0 start len))
      (r6rs-assertion-violation (ps:intern-scheme-symbol who) (format nil "invalid start index ~S" start) start))
    (let ((n (or n (- len start))))
      (unless (and (typep n 'fixnum) (<= 0 n (- len start)))
	(r6rs-assertion-violation (ps:intern-scheme-symbol who) (format nil "invalid count ~S" n) n))
      (values start n))))

(defprim "chez:immutable?" (x) (bool (immutable-p x)))
(defprim "chez:vector-copy" (v &optional start n)
  (multiple-value-bind (start n) (vector-range "vector-copy" v start n)
    (mutable (subseq v start (+ start n)))))
(defprim "chez:immutable-vector-copy" (v &optional (start nil start-p) n)
  (multiple-value-bind (start n) (vector-range "immutable-vector-copy" v start n)
    (if (and (not start-p) (immutable-p v))
	v
	(immutable (subseq v start (+ start n))))))
(defprim "chez:vector-append" (&rest vs)
  (dolist (v vs) (check-vector "vector-append" v))
  (mutable (apply #'concatenate 'simple-vector vs)))
(defprim "chez:immutable-vector-append" (&rest vs)
  (dolist (v vs) (check-vector "immutable-vector-append" v))
  (if (and vs (null (cdr vs)) (immutable-p (car vs)))
      (car vs)
      (immutable (apply #'concatenate 'simple-vector vs))))
(defprim "chez:vector->immutable-vector" (v)
  (check-vector "vector->immutable-vector" v)
  (if (immutable-p v) v (immutable (copy-seq v))))
(defprim "chez:immutable-vector" (&rest xs) (immutable (coerce xs 'simple-vector)))
(defprim "chez:vector-set/copy" (v i x)
  (check-vector "vector-set/copy" v)
  (unless (and (typep i 'fixnum) (< -1 i (length v)))
    (r6rs-assertion-violation (ps:intern-scheme-symbol "vector-set/copy") (format nil "invalid index ~S" i) i))
  (let ((new (copy-seq v))) (setf (svref new i) x) new))
(defprim "chez:immutable-vector-set/copy" (v i x)
  (immutable (funcall (prim "chez:vector-set/copy") v i x)))
(defprim "chez:string->immutable-string" (s)
  (unless (stringp s) (r6rs-assertion-violation (ps:intern-scheme-symbol "string->immutable-string") "not a string" s))
  (if (immutable-p s) s (immutable (coerce s 'simple-string))))
(defprim "chez:string-append-immutable" (&rest ss)
  (dolist (s ss)
    (unless (stringp s) (r6rs-assertion-violation (ps:intern-scheme-symbol "string-append-immutable") "not a string" s)))
  (if (and ss (null (cdr ss)) (immutable-p (car ss)))
      (car ss)
      (immutable (coerce (apply #'concatenate 'string ss) 'simple-string))))
(defprim "chez:bytevector->immutable-bytevector" (bv)
  (unless (typep bv '(simple-array (unsigned-byte 8) (*)))
    (r6rs-assertion-violation (ps:intern-scheme-symbol "bytevector->immutable-bytevector") "not a bytevector" bv))
  (if (immutable-p bv) bv (immutable (copy-seq bv))))
(defprim "chez:empty-vector" () *empty-vector*)
(defprim "chez:empty-string" () *empty-string*)
(defprim "chez:empty-bytevector" () *empty-bytevector*)
(defprim "chez:check-mutable" (who x kind)
  (check-mutable-object (if (symbolp who) (ps:scheme-symbol-name who) who) x kind)
  ps:unspecific)
(defprim "chez:clear-port" (port input)
  (if (truthy input) (clear-input port) (clear-output port))
  ps:unspecific)
(defprim "chez:unread-char" (c port) (unread-char c port) ps:unspecific)

;;; Small system procedures: the oblist, file ports and their lengths,
;;; the installed libraries
(defprim "chez:oblist" ()
  (let ((symbols '()))
    (do-symbols (s (find-package "SCHEME") symbols)
      (when (eq (symbol-package s) (find-package "SCHEME")) (push s symbols)))))
(defprim "chez:file-port?" (x) (bool (typep x 'sb-sys:fd-stream)))
(defprim "chez:port-length" (p)
  (or (ignore-errors (file-length p))
      (r6rs-assertion-violation (ps:intern-scheme-symbol "port-length") "port does not support port-length" p)))
(defprim "chez:port-has-port-length?" (p) (bool (ignore-errors (file-length p))))
(defprim "chez:library-list" () (funcall (psx:host-ref "psyntax:library-names")))
(defprim "chez:library-version" (name)
  (let ((spec (ignore-errors (funcall (psx:host-ref "psyntax:library-spec-by-name") name))))
    (if (consp spec)
	(third spec)
	(r6rs-assertion-violation (ps:intern-scheme-symbol "library-version") "library not found" name))))
