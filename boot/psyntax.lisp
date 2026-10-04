; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER; -*-

;;;; Rebuilds psyntax's image on Pseudoscheme, from a seed image, until
;;;; it reproduces itself.  Run by boot/psyntax.sh, from the repository
;;;; root:
;;;;
;;;;   sbcl --dynamic-space-size 4GB --control-stack-size 500MB \
;;;;        --script boot/psyntax.lisp SEED
;;;;
;;;; SEED is a psyntax image (.pp) built from these sources.  Stage N
;;;; runs psyntax-buildscript.ss with the image of stage N-1 (stage 0
;;;; being SEED) in boot/build/psyntax/stageN/, a copy of
;;;; vendor/psyntax's sources.  Stops when a stage's image is identical
;;;; to the previous one, and prints its pathname; fails if that doesn't
;;;; happen by stage 3.

(require :asdf)

(let ((setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file setup) (load setup)))

(pushnew (truename "./") asdf:*central-registry* :test #'equal)

(if (find-package "QL")
    (uiop:symbol-call "QL" "QUICKLOAD" :pseudoscheme/r6rs :silent t)
    (asdf:load-system :pseudoscheme/r6rs))

(defun stage-directory (n)
  (let ((dir (merge-pathnames (format nil "boot/build/psyntax/stage~D/" n)
			      (truename "./"))))
    (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)
    (ensure-directories-exist (merge-pathnames "psyntax/" dir))
    (dolist (file (directory "vendor/psyntax/psyntax/*.ss"))
      (uiop:copy-file file (merge-pathnames (file-namestring file)
					    (merge-pathnames "psyntax/" dir))))
    (uiop:copy-file "vendor/psyntax/psyntax-buildscript.ss"
		    (merge-pathnames "psyntax-buildscript.ss" dir))
    dir))

(defun same-file-p (a b)
  (string= (uiop:read-file-string a) (uiop:read-file-string b)))

(let* ((arg (first (uiop:command-line-arguments)))
       (seed (if arg (truename arg) (error "usage: psyntax.lisp SEED"))))
  (loop for n from 1 to 3
	for previous = nil then image
	for dir = (stage-directory n)
	for image = (merge-pathnames "psyntax-pseudoscheme.pp" dir)
	do (format t "~&Stage ~D: building psyntax with ~A~%" n
			   (enough-namestring seed (truename "./")))
	   (psx:rebuild :seed seed :directory dir)
	   (when (and previous (same-file-p previous image))
	     (format t "~&Stage ~D reproduced stage ~D: ~A~%" n (1- n)
		     (enough-namestring image (truename "./")))
	     (uiop:quit 0))
	   (setq seed image))
  (format t "~&No fixpoint by stage 3~%")
  (uiop:quit 1))
