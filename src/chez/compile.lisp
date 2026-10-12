; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-R7RS -*-

;;;; Chez's compile-program: a top-level program expanded (not run) and
;;;; compiled into an object file, which is an SBCL fasl (after the
;;;; source's #! line, if it has one, so that a program that can be run
;;;; still can be).  load, load-program, --script and --program run a
;;;; compiled file by loading the fasl (LOAD-FILE-AT-CHEZ-TOP-LEVEL).
;;;;
;;;; A compiled program first invokes the libraries it was expanded
;;;; against, in order, and checks that each is the one it was: the
;;;; built-in ones and libraries from the compiled-library cache keep
;;;; their ids from session to session (src/library-cache.lisp); one
;;;; expanded again since has a new id, and the program must be compiled
;;;; again, as with Chez.  So must one compiled by another build of
;;;; Pseudoscheme, whose translated code may call what this one lacks.

(in-package "PSEUDOSCHEME-R7RS")

(defun require-program-libraries (signature specs)
  "Called first by a compiled program: check that this is the build of
Pseudoscheme it was compiled by (SIGNATURE), and invoke the libraries of
SPECS, ((id name version) ...), each of which must still be the library
of that id."
  (unless (equal signature (psx::build-signature))
    (error "compiled program was compiled by another build of Pseudoscheme: compile it again"))
  (dolist (spec specs)
    (let ((current (funcall (psx:host-ref "psyntax:library-spec-by-name") (second spec))))
      (unless (eq (first current) (first spec))
	(error "compiled program was compiled against another version of library (~{~A~^ ~}): compile it again"
	       (mapcar #'ps:scheme-symbol-name (second spec))))
      (funcall (psx:host-ref "psyntax:library-export-bindings") (second spec)))))

(defun first-line-if-script-header (path)
  (with-open-file (in path)
    (let ((line (read-line in nil)))
      (and line (> (length line) 1) (string= "#!" line :end2 2)
	   ;; #!r6rs, #!chezscheme and the like are read syntax, not a header
	   (or (find #\/ line) (find #\Space line))
	   line))))

(defun copy-program-source (in out)
  (uiop:copy-file in out))

(defun compile-chez-program (in out)
  "Compile the top-level program in file IN into the object file OUT.
If it imports a library whose id is new in this session (the
compiled-library cache is off), or its code can't be dumped, OUT is a
copy of IN, which runs as the source does."
  (let* ((in (merge-pathnames in))
	 (out (merge-pathnames out))
	 (*include-directory* (make-pathname :name nil :type nil :defaults in))
	 (expanded (with-psyntax-parameter ("psyntax:mixed-bodies" ps:false)
		     (funcall (psx:host-ref "psyntax:expand-r6rs-top-level") (read-forms in))))
	 (specs (first expanded)))
    (if (every (lambda (spec) (psx::stable-id-p (first spec))) specs)
	(handler-case
	    (psx::write-compiled-file
	     out (cons `(require-program-libraries ,(psx::build-signature) ',specs)
		       (psx::translate-top-level (second expanded)))
	     :header (first-line-if-script-header in) :source (namestring in))
	  (error (e)
	    (format *error-output* "~&;; compile-program: ~A not compiled (~A); its object file is a copy~%"
		    (namestring in) e)
	    (copy-program-source in out)))
	(copy-program-source in out))
    ;; as Chez's: a program with a #! line can be run
    #+sbcl (sb-posix:chmod (namestring out)
			   (logand #o7777 (sb-posix:stat-mode (sb-posix:stat (namestring in)))))
    ps:unspecific))
