; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

;;;; Strategy B's linklets from Racket's source tree.
;;;;
;;;; The expander: the expanded, flattened expander linklet that Racket's
;;;; own build extracts for Racket CS (compiled/expander.rktl, by
;;;; racket/src/expander/main.zuo), which utils/racket/extract.sh makes
;;;; with an installed Racket of the pinned version, into
;;;; build/external/racket-derived/expander.rktl.  Without it, the copy
;;;; in Racket BC's racket/src/bc/src/startup.inc, as C string literals,
;;;; is read instead: the same linklet with its local names renamed,
;;;; there for a machine with no Racket.  The thread, io and regexp
;;;; layers can be extracted the same way (docs/racket-linklet-compiler.md,
;;;; "Status").

(in-package "PSEUDOSCHEME-RACKET")

(defun c-string-literals (stream)
  "The concatenation of the C string literals in STREAM, unescaped."
  (with-output-to-string (out)
    (loop for c = (read-char stream nil)
	  while c
	  do (case c
	       (#\/ (when (eql (peek-char nil stream nil) #\*) ; a comment
		      (loop for p = nil then d
			    for d = (read-char stream nil)
			    until (or (null d) (and (eql p #\*) (eql d #\/))))))
	       (#\" (loop for d = (read-char stream)
			  until (char= d #\")
			  do (if (char= d #\\)
				 (let ((e (read-char stream)))
				   (case e
				     (#\n (write-char #\Newline out))
				     (#\t (write-char #\Tab out))
				     (#\r (write-char #\Return out))
				     ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
				      (let ((n (digit-char-p e 8)))
					(loop repeat 2
					      while (digit-char-p (peek-char nil stream) 8)
					      do (setq n (+ (* n 8) (digit-char-p (read-char stream) 8))))
					(write-char (code-char n) out)))
				     (t (write-char e out))))
				 (write-char d out))))))))

(defun derived-expander-file ()
  (asdf:system-relative-pathname :pseudoscheme "build/external/racket-derived/expander.rktl"))

(defun expander-linklet-text ()
  "The expander linklet's source: extracted (DERIVED-EXPANDER-FILE), or
else from BC's startup.inc, whose C string is bytes (UTF-8), decoded as
such."
  (let ((derived (probe-file (derived-expander-file))))
    (if derived
	(uiop:read-file-string derived :external-format :utf-8)
	(startup-inc-expander-text))))

(defun startup-inc-expander-text ()
  (let ((latin (with-open-file (in (racket-source "racket/src/bc/src/startup.inc")
				   :external-format :latin-1)
		 (c-string-literals in))))
    (sb-ext:octets-to-string (map '(vector (unsigned-byte 8)) #'char-code latin)
			     :external-format :utf-8)))

(defun read-expander-linklet ()
  (with-input-from-string (in (expander-linklet-text))
    (read-racket in)))

(defun unknown-primitives (core)
  "The names CORE (a lowered linklet) refers to as primitives that no
instance has, with how many times each."
  (let ((counts (make-hash-table :test 'eq)))
    (labels ((walk (x)
	       (when (consp x)
		 (cond ((and (eq (car x) (core "racket:unknown-primitive")) (consp (cdr x)))
			(incf (gethash (cadr (cadr x)) counts 0)))
		       ;; a stub's host global
		       ((and (symbolp (car x)) (uiop:string-prefix-p "racket:" (rname (car x)))
			     (gethash (subseq (rname (car x)) 7) *stubs*))
			(incf (gethash (rsym (subseq (rname (car x)) 7)) counts 0))
			(walk (cdr x)))
		       (t
		      (walk (car x)) (walk (cdr x)))))))
      (walk core))
    (sort (loop for k being the hash-keys of counts using (hash-value n) collect (cons (rname k) n))
	  #'> :key #'cdr)))

(defun expander-report (&key (stream *standard-output*) (missing nil))
  "Lower the expander linklet (passes 1 to 4, no SBCL compilation): say
whether it lowers, and which primitives it uses that we lack."
  (let* ((form (read-expander-linklet))
	 (compiled (compile-linklet-form form :name (rsym "expander")))
	 (unknown (unknown-primitives (compiled-linklet-form compiled))))
    (format stream "~&expander linklet: ~D body forms, ~D exports; lowered; ~D primitives it uses are missing~%"
	    (length (cdddr form)) (length (compiled-linklet-exports compiled)) (length unknown))
    (when missing
      (format stream "~&  missing (uses): ~{~A (~D)~^, ~}~%"
	      (loop for (name . n) in unknown append (list name n))))
    unknown))

(defvar *expander-instance* nil "The expander linklet's instance, once instantiated.")

(defun instantiate-expander (&key (stream *standard-output*))
  "Compile the expander linklet (form by form: it's big) and instantiate
it into *EXPANDER-INSTANCE*, saying how long each step took, or which
top-level form failed and why."
  (flet ((seconds (start) (/ (- (get-internal-real-time) start) internal-time-units-per-second)))
    (let* ((start (get-internal-real-time))
	   (text (expander-linklet-text))
	   (form (with-input-from-string (in text) (read-racket in)))
	   (compiled (compile-linklet-form form :name (rsym "expander"))))
      ;; its compiled form is cached, keyed by its text (cache.lisp)
      (setf (compiled-linklet-source-key compiled) text)
      (format stream "~&expander: read and lowered in ~,1Fs~%" (seconds start))
      (setq start (get-internal-real-time))
      (linklet-function compiled)
      (format stream "~&expander: compiled in ~,1Fs~%" (seconds start))
      (setq start (get-internal-real-time))
      (let ((instance (make-linklet-instance (rsym "expander"))))
	(setq *uncaught* nil)
	(handler-case
	    (progn
	      (call-with-continuation-prompt
	       (lambda () (with-racket-errors (instantiate-linklet compiled '() instance)))
	       *default-prompt-tag* nil)
	      (if *uncaught*
		  ;; the uncaught-exception handler printed it, and aborted
		  (progn (format stream "~&expander: instantiating failed in top-level form ~A~%"
				 *split-form-index*)
			 nil)
		  (progn (format stream "~&expander: instantiated in ~,1Fs~%" (seconds start))
			 (setq *expander-instance* instance))))
	  (error (e)
	    (format stream "~&expander: instantiating failed in top-level form ~A: ~A~%"
		    *split-form-index* e)
	    nil))))))

(defun expander-export (name)
  (instance-value *expander-instance* (rsym name)))

(defvar *expander-booted* nil)

(defun expander-eval (datum)
  "DATUM evaluated by the expander, in a namespace with #%kernel, as
Racket CS's main.sps starts: boot, then (namespace-require ''#%kernel)."
  (unless *expander-instance* (instantiate-expander))
  (unless *expander-booted*
    (funcall (expander-export "boot"))
    (funcall (expander-export "namespace-require")
	     (list (rsym "quote") (rsym "#%kernel")))
    ;; the collections, as main.sps sets them up
    (funcall (expander-export "eval")
	     (read-racket (make-string-input-stream
			   "(begin (current-library-collection-links (find-library-collection-links))
                                   (current-library-collection-paths (find-library-collection-paths)))")))
    (install-zo-writer)
    (setq *expander-booted* t))
  (funcall (expander-export "eval") datum))

;;; The port read handler: the io layer's default reads with the reader
;;; the expander installs (in Racket CS, install-reader!); here, the
;;; expander instance's read and read-syntax
(defvar *port-read-handlers* (make-hash-table :test 'eq :weakness :key :synchronized t))

(defun default-port-read-handler (port &optional (source nil source-p))
  (if source-p
      (funcall (expander-export "read-syntax") source port)
      (funcall (expander-export "read") port)))

(defnative "port-read-handler" (port &optional (handler nil handler-p)) (:leaf t)
  (if handler-p
      (progn (setf (gethash port *port-read-handlers*) handler) *void*)
      (or (gethash port *port-read-handlers*) #'default-port-read-handler)))

;;; ------------------------------------------------------------------
;;; .zo files: a module file's compiled form, written as it is compiled,
;;; and found by the expander's own load handler next time
;;;
;;; The expander compiles machine-independently here (current-compile-
;;; target-machine is #f), so a .zo holds correlated linklets, which the
;;; expander writes and reads itself; loading one compiles its linklets,
;;; whose machine code is in the fasl cache (cache.lisp).  The .zo files
;;; go under a cache root, current-compiled-file-roots, never into
;;; Racket's tree: in the Racket cache's generation (cache.lisp), so a
;;; change to our code starts afresh and the old ones are pruned.  PSEUDOSCHEME_RACKET_ZO=0 turns them off.

(defun zo-root ()
  (uiop:merge-pathnames* "zo/" (racket-cache-directory)))

(defun expander-value (name)
  "NAME's value in the expander's #%kernel namespace."
  (funcall (expander-export "eval") (rsym name)))

(defun install-zo-writer ()
  (unless (member (uiop:getenv "PSEUDOSCHEME_RACKET_ZO") '("0" "no" "off") :test #'equal)
    (let* ((root (namestring (zo-root)))
	   (current-compile (expander-export "current-compile"))
	   (compile (funcall current-compile))
	   (declare-name (expander-value "current-module-declare-name"))
	   (resolved-name (expander-value "resolved-module-path-name")))
      (ensure-directories-exist root)
      (funcall (expander-export "current-compiled-file-roots") (list (make-rpath root)))
      (funcall current-compile
	       (lambda (stx immediate-eval?)
		 ;; a module file's declaration is compiled serializable (as
		 ;; for immediate-eval? #f) to be written; the same code runs
		 (let* ((name (funcall declare-name))
			(file (and (truthy name) (rpath-p (funcall resolved-name name))))
			(compiled (funcall compile stx (if file ps:false immediate-eval?))))
		   (when (truthy name)
		     (let ((path (funcall resolved-name name)))
		       (when (rpath-p path)
			 (handler-case (write-zo compiled (rpath-string path) root)
			   (error (e)
			     (when (uiop:getenv "PSEUDOSCHEME_RACKET_BACKTRACE")
			       (format *error-output* "~&;; no .zo for ~A: ~A~%" (rpath-string path) e)))))))
		   compiled))))))

(defun zo-path (source root)
  "Where the expander looks for SOURCE's .zo under ROOT: ROOT/DIR/compiled/
NAME_EXT.zo, DIR being SOURCE's directory less its leading /."
  (let* ((slash (position #\/ source :from-end t))
	 (dir (subseq source 1 (1+ slash)))
	 (file (substitute #\_ #\. (subseq source (1+ slash)))))
    (format nil "~A~Acompiled/~A.zo" root dir file)))

(defun write-zo (compiled source root)
  (let* ((path (zo-path source root))
	 (temp (format nil "~A.~36R" path (random (expt 36 8) (make-random-state t))))
	 (port (make-routport (rsym "zo"))))
    (funcall (native-function (gethash "write" *natives*)) compiled port)
    (ensure-directories-exist path)
    (with-open-file (out temp :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
      (write-sequence (routport-buffer port) out))
    (rename-file temp path)))