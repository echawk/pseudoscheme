; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-R6RS -*-

;;;; Host procedures behind src/compat/chezscheme.scm: the parts of Chez
;;;; Scheme's (chezscheme) library that need the operating system.  Named
;;;; chez:..., exported to Scheme through (pseudoscheme host).

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
