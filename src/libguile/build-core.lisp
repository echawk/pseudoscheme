;;;; Build quasischeme-guile.core, the SBCL core a C program linked
;;;; against our libguile starts at its first call (docs/libguile.md,
;;;; "Embedding"; core.c, boot_lisp).  `make core` runs this:
;;;;
;;;;   sbcl --non-interactive --no-userinit --load build-core.lisp OUTPUT
;;;;
;;;; Loading the command line's system boots Guile mode into the image,
;;;; as bin/quasischeme's build does.  The core has no toplevel: with a
;;;; callable export SBCL's start-up returns to C (initialize_lisp) after
;;;; the init hooks, and the export's address lands in core.c's
;;;; psg_entry.  The hook attaches the library already loaded.

(require :asdf)

(let* ((here (make-pathname :name nil :type nil
			    :defaults (or *load-truename* *load-pathname*)))
       (root (merge-pathnames (make-pathname :directory '(:relative :up :up)) here)))
  (pushnew (truename root) asdf:*central-registry* :test #'equal)
  (pushnew (truename (merge-pathnames "contrib/cli/" (truename root))) asdf:*central-registry* :test #'equal))

(let ((setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file setup)
    (load setup)
    (uiop:symbol-call "QL" "QUICKLOAD" :quasischeme-cli :silent t)))

(handler-bind ((warning #'muffle-warning))
  (asdf:load-system :quasischeme-cli))

(let ((output (or (car (last sb-ext:*posix-argv*)) "quasischeme-guile.core")))
  (push (lambda () (uiop:symbol-call "QUASISCHEME-LIBGUILE" "START-EMBEDDED"))
	sb-ext:*init-hooks*)
  (sb-ext:save-lisp-and-die output
			    :callable-exports '(quasischeme-libguile::psg-entry)))
