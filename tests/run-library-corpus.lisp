; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER -*-

;;;; Can Pseudoscheme load real-world Scheme libraries?
;;;;
;;;; Point this at a directory of library sources laid out the R6RS/R7RS
;;;; way -- (foo bar) in foo/bar.sls or .sld -- such as the .akku/lib of a
;;;; project where `akku install` has run, or a snow-chibi install
;;;; directory.  Every portable library file in it (implementation-specific
;;;; variants like foo.chezscheme.sls are skipped) is imported in turn, in
;;;; one psyntax host with the R7RS libraries installed; the result is a
;;;; list of what loads and why the rest don't.
;;;;
;;;; Usage:
;;;;   sbcl --dynamic-space-size 4GB --control-stack-size 500MB \
;;;;        --script tests/run-library-corpus.lisp DIR [-v] [-L LIBDIR ...]
;;;;        [--skip NAME ...]
;;;;
;;;; -L adds a directory the libraries in DIR may import from (searched
;;;; after DIR), without testing its own libraries.  --skip leaves out the
;;;; files in any directory named NAME (src/srfi/reference, say: upstream
;;;; sources kept for provenance).

(require :asdf)

;; The dependencies (float-features, cl-unicode, ...) come from
;; Quicklisp when it's installed; QUICKLOAD fetches any that are missing.
(let ((setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file setup) (load setup)))

(defun load-system (system)
  (if (find-package "QL")
      (uiop:symbol-call "QL" "QUICKLOAD" system :silent t)
      (asdf:load-system system)))

(defvar *root*
  (let ((here (make-pathname :name nil :type nil
			     :defaults (or *load-truename* *load-pathname*))))
    (truename (merge-pathnames (make-pathname :directory '(:relative :up)) here))))
(pushnew *root* asdf:*central-registry* :test #'equal)

(let ((*standard-output* (make-broadcast-stream))
      (*error-output* (make-broadcast-stream)))
  (handler-bind ((warning #'muffle-warning))
    (load-system :pseudoscheme/api)))

(ps:disable-float-traps)

(defparameter *verbose* (member "-v" (uiop:command-line-arguments) :test #'string=))
(defparameter *extra-dirs*
  (loop for (a b) on (uiop:command-line-arguments)
	when (string= a "-L") collect b))
(defparameter *skip*
  (loop for (a b) on (uiop:command-line-arguments)
	when (string= a "--skip") collect b))
(defparameter *dir*
  (or (loop for (prev a) on (cons nil (uiop:command-line-arguments))
	    when (and a (char/= (char a 0) #\-) (not (member prev '("-L" "--skip") :test #'equal)))
	      return a)
      (error "usage: run-library-corpus.lisp DIR [-v] [-L LIBDIR ...] [--skip NAME ...]")))

(defparameter *implementations*
  '("chezscheme" "guile" "ikarus" "mosh" "ypsilon" "larceny" "ironscheme" "vicare"
    "sagittarius" "loko" "digamma" "racket" "mzscheme" "chibi" "gauche" "chicken"
    "gambit" "cyclone" "kawa" "mit" "s7" "stklos" "capy" "foment" "skint" "nmosh"
    "ypsilon" "unsyntax" "ufo")
  "Second extensions marking implementation-specific variants.")

(defun portable-file-p (path)
  (let* ((name (pathname-name path))
	 (dot (position #\. name :from-end t)))
    (and (member (pathname-type path) '("sls" "sld") :test #'string=)
	 (not (and dot (member (subseq name (1+ dot)) *implementations* :test #'string=))))))

(defun library-names-in (path)
  "Names of the libraries a file defines (R6RS library or R7RS define-library)."
  (handler-case
      (let ((forms (ps-r7rs::read-forms path)))
	(loop for f in forms
	      when (or (ps-r7rs::head-is f "library") (ps-r7rs::head-is f "define-library"))
		collect (ps-r7rs::library-name-of f)))
    (error () nil)))

(defun try-import (name)
  (handler-case
      (progn
	(ps-r7rs::eval-forms (list (list (ps:intern-scheme-symbol "import") name)))
	:ok)
    (serious-condition (e)
      ;; Errors can carry whole expander environments: print them short.
      (let ((*print-length* 10) (*print-level* 4))
	(string-trim '(#\Space #\Newline)
		     (substitute #\Space #\Newline (princ-to-string e)))))))

(let* ((dir (uiop:ensure-directory-pathname *dir*))
       (files (remove-if (lambda (path)
			   (some (lambda (name) (member name (pathname-directory path) :test #'equal))
				 *skip*))
			 (remove-if-not #'portable-file-p (directory (merge-pathnames "**/*.*" dir)))))
       (names (remove-duplicates (loop for f in files append (library-names-in f)) :test #'equal))
       (ok 0) (failures '()))
  (setf psx:*library-path* (cons (namestring dir) *extra-dirs*))
  (pseudoscheme-interop:boot)		; R7RS, and the bridge's (pseudoscheme lisp)
  (dolist (name (sort names #'string< :key (lambda (n) (format nil "~S" n))))
    (let ((result (try-import name)))
      (cond ((eq result :ok) (incf ok)
	     (when *verbose* (format t "~&ok   ~A~%" (ps-r7rs::write-scheme-to-string name))))
	    (t (push (cons name result) failures)
	       (format t "~&FAIL ~A~%     ~A~%" (ps-r7rs::write-scheme-to-string name)
		       (subseq result 0 (min 200 (length result))))))
      (finish-output)))
  (format t "~&~%~D of ~D libraries load.~%" ok (+ ok (length failures))))
