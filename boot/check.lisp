; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER; -*-

;;;; Checks a bootstrap against Pseudoscheme itself.
;;;;
;;;; Usage, from the repository root:
;;;;   sbcl --script boot/check.lisp [DIR ...]
;;;;
;;;; Has the translator, as currently loaded from src/*.pso, translate
;;;; everything boot/bootstrap.scm translates, into boot/build/sbcl/.
;;;; Then reads each file there, and the file of the same name in each
;;;; DIR (default boot/build/out/), with the CL reader, and compares
;;;; them form by form.  Layout and header comments may differ; the
;;;; forms must be EQUAL (EQUALP for vectors).  Exits nonzero on any
;;;; difference.

(require :asdf)

(let ((setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file setup) (load setup)))

(pushnew (truename "./") asdf:*central-registry* :test #'equal)

(if (find-package "QL")
    (uiop:symbol-call "QL" "QUICKLOAD" :pseudoscheme :silent t)
    (asdf:load-system :pseudoscheme))

(defpackage :pseudoscheme-boot-check (:use :common-lisp))
(in-package :pseudoscheme-boot-check)

(defun st (name) (intern name "SCHEME-TRANSLATOR"))
(defun st-call (name &rest args) (apply (symbol-function (st name)) args))
(defun st-value (name) (symbol-value (st name)))

(defparameter *reference-dir* (merge-pathnames "boot/build/sbcl/" (truename "./")))

(defun translator-files ()
  (with-open-file (s "src/translator.files") (read s)))

(defun src (name type) (namestring (merge-pathnames (make-pathname :name name :type type) "src/")))
(defun ref (name type) (namestring (merge-pathnames (make-pathname :name name :type type) *reference-dir*)))

(defun make-reference ()
  (ensure-directories-exist *reference-dir*)
  (let ((ps:*scheme-read* #'ps:scheme-read-using-commonlisp-reader)
	(*standard-output* (make-broadcast-stream)))
    (st-call "WRITE-CLOSED-DEFINITIONS" (st-value "REVISED^4-SCHEME-STRUCTURE")
	     (ref "closed" "pso"))
    (dolist (f '("read" "write"))
      (st-call "REALLY-TRANSLATE-FILE" (src f "scm") (ref f "pso")
	       (st-value "REVISED^4-SCHEME-ENV")))
    (dolist (f (translator-files))
      (st-call "REALLY-TRANSLATE-FILE" (src f "scm") (ref f "pso")
	       (st-value "SCHEME-TRANSLATOR-ENV")))
    (st-call "WRITE-DEFPACKAGES"
	     (list (st-value "REVISED^4-SCHEME-STRUCTURE")
		   (st-value "SCHEME-TRANSLATOR-STRUCTURE"))
	     (ref "spack" "lisp"))))

(defun read-forms (file)
  ;; As LOAD would read it: (ps:in-package ...) changes *PACKAGE*.
  (with-open-file (s file)
    (let ((*package* (find-package "CL-USER"))
	  (*readtable* (copy-readtable nil))
	  (eof (list nil)))
      (loop for form = (read s nil eof)
	    until (eq form eof)
	    do (when (and (consp form)
			  (member (car form) '(ps::in-package in-package)))
		 (setq *package* (find-package (second form))))
	    collect form))))

(defun same (a b)
  (cond ((and (consp a) (consp b)) (and (same (car a) (car b)) (same (cdr a) (cdr b))))
	((and (vectorp a) (vectorp b) (not (stringp a)))
	 (and (= (length a) (length b)) (every #'same a b)))
	(t (equal a b))))

(defun compare-file (name dir)
  (let ((ours (merge-pathnames name dir))
	(theirs (merge-pathnames name *reference-dir*)))
    (if (not (probe-file ours))
	(progn (format t "~&  ~A: missing~%" name) nil)
	(let ((a (read-forms theirs))
	      (b (read-forms ours)))
	  (loop for i from 0
		for x in a
		for y in b
		unless (same x y)
		  do (format t "~&  ~A: form ~D differs~%    expected: ~S~%    got:      ~S~%"
			     name i x y)
		     (return-from compare-file nil))
	  (if (/= (length a) (length b))
	      (progn (format t "~&  ~A: ~D forms, expected ~D~%" name (length b) (length a))
		     nil)
	      t)))))

(defun main (dirs)
  (make-reference)
  (let ((names (mapcar #'file-namestring (directory (merge-pathnames "*.*" *reference-dir*))))
	(ok t))
    (dolist (dir dirs)
      (let ((dir (merge-pathnames (if (char= (char dir (1- (length dir))) #\/) dir
				      (concatenate 'string dir "/"))
				  (truename "./"))))
	(format t "~&Checking ~A against Pseudoscheme's own translation~%" dir)
	(let ((bad (remove-if (lambda (name) (compare-file name dir)) names)))
	  (if bad
	      (setq ok nil)
	      (format t "~&  all ~D files match~%" (length names))))))
    (uiop:quit (if ok 0 1))))

(main (or (uiop:command-line-arguments) '("boot/build/out/")))
