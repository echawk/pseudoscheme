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
  (ps:true? (uiop:directory-exists-p (uiop:parse-native-namestring name))))

(defprim "chez:file-regular?" (name)
  (ps:true? (uiop:file-exists-p (uiop:parse-native-namestring name))))

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
