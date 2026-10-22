; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-RACKET -*-

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

(in-package "QUASISCHEME-RACKET")

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
  (asdf:system-relative-pathname :quasischeme "build/external/racket-derived/expander.rktl"))

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

(defun wrap-expander-read (form)
  "The expander linklet FORM with the read its read-primitives table
holds (#%read's, and so #%kernel's) as (quasischeme-fast-read read):
read's fast path (fast-read.lisp) in front of it."
  (dolist (f (cdddr form) form)
    (when (and (form-p f "define-values") (consp (cadr f)) (symbolp (car (cadr f)))
	       (string= (rname (car (cadr f))) "read-primitives")
	       (form-p (caddr f) "hasheq"))
      (loop for tail on (cdr (caddr f)) by #'cddr
	    when (equal (car tail) (list (rsym "quote") (rsym "read")))
	      do (setf (cadr tail) (list (rsym "quasischeme-fast-read") (cadr tail))))
      (return form))))

(defvar *expander-instance* nil "The expander linklet's instance, once instantiated.")

(defun instantiate-expander (&key (stream *standard-output*))
  "Compile the expander linklet (form by form: it's big) and instantiate
it into *EXPANDER-INSTANCE*, saying how long each step took, or which
top-level form failed and why."
  (flet ((seconds (start) (/ (- (get-internal-real-time) start) internal-time-units-per-second)))
    (let* ((start (get-internal-real-time))
	   (text (progn
		   ;; Racket's regexps, which the expander's linklet uses
		   ;; (regexp-layer.lisp)
		   (install-regexp-layer :stream stream)
		   (expander-linklet-text)))
	   ;; its compiled form is cached, keyed by its text (cache.lisp), and
	   ;; what WRAP-EXPANDER-READ does; lowered only if the cache hasn't
	   ;; its machine code and meta file
	   (key (concatenate 'string text ";fast-read"))
	   (form (lambda () (wrap-expander-read (with-input-from-string (in text) (read-racket in)))))
	   (compiled (let ((c (make-compiled-linklet :deferred :name (rsym "expander") :strategy "linklet")))
		       (setf (compiled-linklet-source-key c) key
			     (compiled-linklet-source-form c) form)
		       (or (linklet-from-meta c)
			   (let ((c (compile-linklet-form (funcall form) :name (rsym "expander"))))
			     (setf (compiled-linklet-source-key c) key)
			     (write-linklet-meta c)
			     c)))))
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

;;; The packages of Racket's source tree (pkgs/NAME, each a multi-
;;; collection package: its directories are collections) are linked as
;;; an installation's pkgs are, by a links file: compiler/zo-structs.rkt
;;; is zo-lib's, and the "tests" collection of pkgs/racket-test-core/
;;; (tests/racket/...) is how Racket's own test suite finds its files.
;;; The file is regenerated at each boot, into the cache.

(defun collection-links-text ()
  (let ((pkgs (merge-pathnames "pkgs/" (racket-source-directory))))
    (with-output-to-string (out)
      (format out "(")
      (dolist (dir (sort (copy-list (uiop:subdirectories pkgs)) #'string< :key #'namestring))
	(let ((info (merge-pathnames "info.rkt" dir)))
	  (when (and (probe-file info)
		     (search "(define collection 'multi)" (uiop:read-file-string info)))
	    (format out "~%  (root ~S)" (namestring dir)))))
      (format out ")~%"))))

(defun write-collection-links ()
  "The links file of the source tree's packages, in the Racket cache:
its pathname."
  (let ((file (merge-pathnames "links.rktd" (racket-cache-directory)))
	(text (collection-links-text)))
    (unless (and (probe-file file) (string= (uiop:read-file-string file) text))
      (ensure-directories-exist file)
      (with-open-file (out file :direction :output :if-exists :supersede)
	(write-string text out)))
    file))

(defun expander-export (name)
  (instance-value *expander-instance* (rsym name)))

(defvar *expander-booted* nil)

(defvar *collection-paths-set* nil
  "Whether this process set the collection paths (SET-COLLECTION-PATHS):
an image saved with the expander booted (save-racket-core) has the paths
of the run that made it.")

(pushnew (lambda () (setq *collection-paths-set* nil)) sb-ext:*init-hooks*)

(defvar *collection-options* '()
  "The command line's say in the collection paths (command-line.lisp):
:SEARCH, directories after the main collects (-S); :NO-USER-PATHS, no
user-specific ones (-U); :NO-COLLECTS, none at all (-X \"\").")

(defun set-collection-paths ()
  "The collections, as main.sps sets them up, less the user's own (the
addon directory's collects and links: not the source tree's), plus a
links file for the packages of the source tree (pkgs/), and what
*COLLECTION-OPTIONS* says."
  (funcall (expander-export "eval")
	   (read-racket (make-string-input-stream
			 (format nil "(let-values ([(user) (use-user-specific-search-paths)])
                                        (begin
                                          (use-user-specific-search-paths #f)
                                          (current-library-collection-links
                                            (append (find-library-collection-links)
                                                    (list (string->path ~S))))
                                          (current-library-collection-paths ~A)
                                          (use-user-specific-search-paths ~A)))"
				 (namestring (write-collection-links))
				 (if (getf *collection-options* :no-collects)
				     "'()"
				     (format nil "(find-library-collection-paths '() (map string->path '(~{~S~^ ~})))"
					     (getf *collection-options* :search)))
				 (if (getf *collection-options* :no-user-paths) "#f" "user")))))
  (setq *collection-paths-set* t))

(defun expander-eval (datum)
  "DATUM evaluated by the expander, in a namespace with #%kernel, as
Racket CS's main.sps starts: boot, then (namespace-require ''#%kernel)."
  (boot-expander)
  (funcall (expander-export "eval") datum))

(defun boot-expander ()
  "The expander instantiated and booted (EXPANDER-EVAL), and this run's
collection paths set."
  (unless *expander-instance*
    (unless (instantiate-expander :stream nil)
      (racket-error "racket" "the expander didn't instantiate")))
  (unless *expander-booted*
    (funcall (expander-export "boot"))
    (funcall (expander-export "namespace-require")
	     (list (rsym "quote") (rsym "#%kernel")))
    (set-collection-paths)
    (install-zo-writer)
    ;; Racket's string->number is the reader's, which #%kernel re-exports
    ;; from #%read (data.lisp)
    (setq *expander-string->number*
	  (funcall (expander-export "dynamic-require")
		   (list (rsym "quote") (rsym "#%read")) (rsym "string->number")))
    ;; read's fast path (fast-read.lisp), given the reader's parameters
    (install-fast-read
     (lambda (name)
       (multiple-value-bind (v found) (primitive name)
	 (if found
	     v
	     (funcall (expander-export "dynamic-require")
		      (list (rsym "quote") (rsym "#%read")) (rsym name))))))
    (setq *expander-booted* t))
  (unless *collection-paths-set* (set-collection-paths)))

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
;;; change to our code starts afresh and the old ones are pruned.  QUASISCHEME_RACKET_ZO=0 turns them off.

(defun zo-root ()
  (uiop:merge-pathnames* "zo/" (racket-cache-directory)))

(defun expander-value (name)
  "NAME's value in the expander's #%kernel namespace."
  (funcall (expander-export "eval") (rsym name)))

(defun install-zo-writer ()
  (unless (member (uiop:getenv "QUASISCHEME_RACKET_ZO") '("0" "no" "off") :test #'equal)
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
			(compiled (funcall compile stx (if file qs:false immediate-eval?))))
		   (when (truthy name)
		     (let ((path (funcall resolved-name name)))
		       (when (rpath-p path)
			 (handler-case (write-zo compiled (rpath-string path) root)
			   (error (e)
			     (when (uiop:getenv "QUASISCHEME_RACKET_BACKTRACE")
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