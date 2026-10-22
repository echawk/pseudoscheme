; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-RACKET -*-

;;;; Paths, the filesystem and environment variables, natively: in Racket
;;;; CS these are the io layer's, over rktio; strategy B has no io layer
;;;; yet (docs/racket.md, "How Racket is layered"), so the ones the
;;;; expander uses are here, on SBCL and UIOP.  Unix paths only.
;;;;
;;;; A path is an RPATH holding its string; Racket's paths are bytes, and
;;;; these are UTF-8 strings, which is the same for every path a Unix
;;;; system gives us in practice.

(in-package "QUASISCHEME-RACKET")

(defstruct (rpath (:constructor make-rpath (string))) (string "" :type string))

(defmethod print-object ((p rpath) stream)
  (format stream "#<path:~A>" (rpath-string p)))

(defun path-string (who x)
  "X (a path or a string) as a string."
  (cond ((rpath-p x) (rpath-string x))
	((stringp x) x)
	(t (racket-error who "contract violation~%  expected: path-string?~%  given: ~A" (written x)))))

(defun utf8 (bytes) (sb-ext:octets-to-string bytes :external-format :utf-8))
(defun utf8-bytes (string) (sb-ext:string-to-octets string :external-format :utf-8))

(defun directory-string-p (s) (and (plusp (length s)) (char= (char s (1- (length s))) #\/)))

(defnative "path?" (x) (:leaf t) (bool (rpath-p x)))
(defnative "path-for-some-system?" (x) (:leaf t) (bool (rpath-p x)))
(defnative "string->path" (s) (:leaf t) (make-rpath (copy-seq s)))
(defnative "path->string" (p) (:leaf t) (copy-seq (rpath-string p)))
(defnative "bytes->path" (b &optional type) (:leaf t) (declare (ignore type)) (make-rpath (utf8 b)))
(defnative "path->bytes" (p) (:leaf t) (utf8-bytes (rpath-string p)))
(defnative "bytes->path-element" (b &optional type false-on-non-element?) (:leaf t)
  (declare (ignore type false-on-non-element?))
  (make-rpath (utf8 b)))
(defnative "path-element->bytes" (p) (:leaf t) (utf8-bytes (rpath-string p)))
(defnative "path-convention-type" (p) (:leaf t) (declare (ignore p)) (rsym "unix"))
(defnative "system-path-convention-type" () (:leaf t) (rsym "unix"))

(defun absolute-string-p (s) (and (plusp (length s)) (char= (char s 0) #\/)))

(defnative "complete-path?" (p) (:leaf t) (bool (absolute-string-p (path-string "complete-path?" p))))
(defnative "absolute-path?" (p) (:leaf t) (bool (absolute-string-p (path-string "absolute-path?" p))))
(defnative "relative-path?" (p) (:leaf t)
  (let ((s (path-string "relative-path?" p))) (bool (and (plusp (length s)) (not (absolute-string-p s))))))

(defun path-element-string (x)
  (cond ((and (symbolp x) (string= (rname x) "up")) "..")
	((and (symbolp x) (string= (rname x) "same")) ".")
	(t (path-string "build-path" x))))

(defun join (base sub)
  (cond ((absolute-string-p sub)
	 (racket-error "build-path" "absolute path cannot be added to a path~%  absolute path: ~A" sub))
	((directory-string-p base) (concatenate 'string base sub))
	(t (concatenate 'string base "/" sub))))

(defnative "build-path" (base &rest subs) (:leaf t)
  (make-rpath (reduce #'join (mapcar #'path-element-string subs)
		      :initial-value (path-element-string base))))

(defnative "path->directory-path" (p) (:leaf t)
  (let ((s (path-string "path->directory-path" p)))
    (make-rpath (if (directory-string-p s) s (concatenate 'string s "/")))))

(defnative "split-path" (p) (:leaf t)
  ;; base (a path, 'relative or #f), name (a path, 'up or 'same), must-be-dir?
  (let* ((s (path-string "split-path" p))
	 (dir (directory-string-p s))
	 (trimmed (if (and dir (> (length s) 1)) (subseq s 0 (1- (length s))) s))
	 (slash (position #\/ trimmed :from-end t)))
    (flet ((name (n)
	     (cond ((string= n "..") (rsym "up")) ((string= n ".") (rsym "same")) (t (make-rpath n)))))
      (cond ((string= trimmed "/") (values qs:false (make-rpath "/") qs:true))
	    ((null slash) (values (rsym "relative") (name trimmed) (bool dir)))
	    (t (values (make-rpath (subseq trimmed 0 (1+ slash)))
		       (name (subseq trimmed (1+ slash)))
		       (bool (or dir (member (subseq trimmed (1+ slash)) '("." "..") :test #'string=)))))))))

(defun explode (s)
  (remove "" (uiop:split-string s :separator "/") :test #'string=))

(defnative "explode-path" (p) (:leaf t)
  (let ((s (path-string "explode-path" p)))
    (append (if (absolute-string-p s) (list (make-rpath "/")) '())
	    (mapcar (lambda (e) (cond ((string= e "..") (rsym "up")) ((string= e ".") (rsym "same"))
				      (t (make-rpath e))))
		    (explode s)))))

(defun simplify (s)
  "S without . and with each name/.. removed, syntactically."
  (let ((out '()) (absolute (absolute-string-p s)))
    (dolist (e (explode s))
      (cond ((string= e "."))
	    ((and (string= e "..") out (not (string= (car out) ".."))) (pop out))
	    ((and (string= e "..") absolute))
	    (t (push e out))))
    (let ((joined (format nil "~{~A~^/~}" (reverse out))))
      (cond (absolute (concatenate 'string "/" joined))
	    ((string= joined "") ".")
	    (t joined)))))

(defnative "simplify-path" (p &optional use-filesystem?) (:leaf t)
  (declare (ignore use-filesystem?))
  (let ((s (path-string "simplify-path" p)))
    (make-rpath (let ((simple (simplify s)))
		  (if (and (directory-string-p s) (not (directory-string-p simple)))
		      (concatenate 'string simple "/")
		      simple)))))

(defnative "cleanse-path" (p) (:leaf t)
  (make-rpath (let ((s (path-string "cleanse-path" p)))
		(with-output-to-string (out)
		  (let ((prev nil))
		    ;; runs of / as one
		    (loop for c across s
			  unless (and (char= c #\/) (eql prev #\/)) do (write-char c out)
			  do (setq prev c)))))))

(defnative "resolve-path" (p) (:leaf t) (make-rpath (path-string "resolve-path" p)))

(defnative "expand-user-path" (p) (:leaf t)
  (let ((s (path-string "expand-user-path" p)))
    (make-rpath (if (and (plusp (length s)) (char= (char s 0) #\~))
		    (concatenate 'string (string-right-trim "/" (namestring (user-homedir-pathname)))
				 (subseq s 1))
		    s))))

(defvar *current-directory*
  (make-parameter* (make-rpath (namestring (uiop:getcwd)))
		   (lambda (p) (make-rpath (let ((s (path-string "current-directory" p)))
					     (if (directory-string-p s) s (concatenate 'string s "/")))))))
(define-primitive "#%kernel" "current-directory" *current-directory*)

(defun reset-current-directory ()
  "The current-directory parameter's root value: the process's working
directory when it starts, not when the image was saved."
  (setf (car (parameter-cell (gethash *current-directory* *parameters*)))
	(make-rpath (namestring (uiop:getcwd)))))

#+sbcl (pushnew 'reset-current-directory sb-ext:*init-hooks*)
(define-primitive "#%kernel" "current-directory-for-user" *current-directory*)
(define-primitive "#%kernel" "current-load-relative-directory" (make-parameter* qs:false))
(define-primitive "#%kernel" "current-write-relative-directory" (make-parameter* qs:false))

(defnative "path->complete-path" (p &optional (base nil base-p)) (:leaf t)
  (let ((s (path-string "path->complete-path" p)))
    (make-rpath (if (absolute-string-p s)
		    s
		    (join (path-string "path->complete-path"
				       (if base-p base (funcall *current-directory*)))
			  s)))))

;;; ------------------------------------------------------------------
;;; The filesystem

(defun native-path (who p)
  "P, a path or string, as the operating system is to be given it: a
relative one against Racket's current-directory parameter (which
parameterize changes without the process's working directory)."
  (let ((s (path-string who p)))
    (if (or (absolute-string-p s) (zerop (length s)))
	s
	(join (path-string who (funcall *current-directory*)) s))))

(defun file-kind (s)
  "What native path S names, following links: :DIRECTORY, :FILE (any
other kind) or NIL.  One stat(2), not PROBE-FILE's realpath and lstats
(and S isn't parsed as a Lisp pathname, wildcards and all): the module
name resolver asks a few times for each module required."
  (multiple-value-bind (ok dev ino mode) (sb-unix:unix-stat s)
    (declare (ignore dev ino))
    (and ok (if (= (logand mode #o170000) #o040000) :directory :file))))

(defnative "file-exists?" (p) (:leaf t)
  (bool (eq (file-kind (native-path "file-exists?" p)) :file)))
(defnative "directory-exists?" (p) (:leaf t)
  (bool (eq (file-kind (native-path "directory-exists?" p)) :directory)))
(defnative "link-exists?" (p) (:leaf t)
  (bool (ignore-errors (sb-posix:s-islnk (sb-posix:stat-mode (sb-posix:lstat (native-path "link-exists?" p)))))))
(defnative "file-size" (p) (:leaf t)
  (sb-posix:stat-size (sb-posix:stat (native-path "file-size" p))))
(defnative "file-or-directory-modify-seconds" (p &optional secs fail) ()
  (declare (ignore secs))
  (let ((st (ignore-errors (sb-posix:stat (native-path "file-or-directory-modify-seconds" p)))))
    (cond (st (sb-posix:stat-mtime st))
	  ((and fail (functionp fail)) (funcall fail))
	  (t (racket-error "file-or-directory-modify-seconds" "error getting file/directory time~%  path: ~A"
			   (native-path "" p))))))
(defnative "directory-list" (&optional (p (funcall *current-directory*)) &rest options) (:leaf t)
  (declare (ignore options))
  (let ((dir (uiop:ensure-directory-pathname (native-path "directory-list" p))))
    (sort (mapcar (lambda (x)
		    (make-rpath (let ((n (namestring x)))
				  (car (last (explode n))))))
		  (append (uiop:directory-files dir) (uiop:subdirectories dir)))
	  #'string< :key #'rpath-string)))

;;; ------------------------------------------------------------------
;;; find-system-path: Racket's collections and configuration are the
;;; pinned source tree's (racket/collects, racket/etc), when it's there

(defvar *system-paths* '()
  "Kind (a string) -> path string, for the kinds the command line sets:
run-file and exec-file (-N, -E, and the program run), collects-dir (-X)
and addon-dir (-A) (command-line.lisp).")

(defnative "find-system-path" (kind) (:leaf t)
  (let ((k (rname kind))
	(source (ignore-errors (namestring (racket-source-directory)))))
    (make-rpath
     (cond ((cdr (assoc k *system-paths* :test #'string=)))
	   ((string= k "home-dir") (namestring (user-homedir-pathname)))
	   ((member k '("pref-dir" "addon-dir") :test #'string=)
	    (namestring (uiop:xdg-data-home "racket/")))
	   ((string= k "pref-file") (namestring (uiop:xdg-data-home "racket/racket-prefs.rktd")))
	   ((string= k "cache-dir") (namestring (uiop:xdg-cache-home "racket/")))
	   ((string= k "temp-dir") (or (uiop:getenv "TMPDIR") "/tmp/"))
	   ((member k '("init-dir" "doc-dir" "desk-dir") :test #'string=) (namestring (user-homedir-pathname)))
	   ((string= k "init-file") (namestring (merge-pathnames ".racketrc" (user-homedir-pathname))))
	   ((string= k "sys-dir") "/")
	   ((member k '("exec-file" "run-file") :test #'string=) "racket")
	   ((member k '("collects-dir" "host-collects-dir") :test #'string=)
	    (if source (concatenate 'string source "racket/collects/") "collects/"))
	   ((member k '("config-dir" "host-config-dir") :test #'string=)
	    (if source (concatenate 'string source "racket/etc/") "etc/"))
	   ((string= k "orig-dir") (namestring (uiop:getcwd)))
	   (t (racket-error "find-system-path" "contract violation~%  given: ~A" k))))))

(defnative "system-library-subpath" (&optional mode) (:leaf t)
  (declare (ignore mode))
  (make-rpath (format nil "~A-~A" (if (member :arm64 *features*) "aarch64" "x86_64")
		      (if (uiop:os-macosx-p) "macosx" "linux"))))

;;; ------------------------------------------------------------------
;;; Environment variables: the process's, or a copy

(defstruct (environment-variables (:constructor make-environment-variables* (table)))
  table)				; NIL: the process's; else a hash table string -> string

(defvar *current-environment-variables*
  (make-parameter* (make-environment-variables* nil)))
(define-primitive "#%kernel" "current-environment-variables" *current-environment-variables*)

(defnative "environment-variables?" (x) (:leaf t) (bool (environment-variables-p x)))
(defnative "environment-variables-ref" (env name) (:leaf t)
  (let* ((n (utf8 name))
	 (v (if (environment-variables-table env)
		(gethash n (environment-variables-table env))
		(sb-posix:getenv n))))
    (if v (utf8-bytes v) qs:false)))
(defnative "environment-variables-set!" (env name value &optional fail) ()
  (declare (ignore fail))
  (let ((n (utf8 name)))
    (if (environment-variables-table env)
	(if (truthy value)
	    (setf (gethash n (environment-variables-table env)) (utf8 value))
	    (remhash n (environment-variables-table env)))
	(if (truthy value) (sb-posix:setenv n (utf8 value) 1) (sb-posix:unsetenv n)))
    *void*))
(defnative "environment-variables-names" (env) (:leaf t)
  (mapcar #'utf8-bytes
	  (if (environment-variables-table env)
	      (loop for k being the hash-keys of (environment-variables-table env) collect k)
	      (mapcar (lambda (s) (subseq s 0 (position #\= s))) (sb-ext:posix-environ)))))
(defnative "environment-variables-copy" (env) (:leaf t)
  (let ((table (make-hash-table :test 'equal)))
    (if (environment-variables-table env)
	(maphash (lambda (k v) (setf (gethash k table) v)) (environment-variables-table env))
	(dolist (s (sb-ext:posix-environ))
	  (let ((i (position #\= s))) (setf (gethash (subseq s 0 i) table) (subseq s (1+ i))))))
    (make-environment-variables* table)))
(defnative "make-environment-variables" (&rest names-and-values) (:leaf t)
  (let ((table (make-hash-table :test 'equal)))
    (loop for (n v) on names-and-values by #'cddr do (setf (gethash (utf8 n) table) (utf8 v)))
    (make-environment-variables* table)))
