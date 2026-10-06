;;; SRFI 59: vicinity.  Aubrey Jaffer's implementation from the SRFI
;;; document (slib/Template.scm and slib/require.scm), unmodified
;;; (reference/srfi-59/vicinity.scm; MIT licence, in
;;; reference/srfi-59/LICENSE).  It was written for a case-folding
;;; reader ((case (software-type) ((UNIX) ...))), so it is included with
;;; include-ci.  What it expects of its host is in (srfi private
;;; srfi-59-support), shared with SRFI 96: software-type (unix, or
;;; ms-dos on Windows), getenv, slib:warn, slib:error and the load
;;; pathname.  Differences:
;;;
;;;   - implementation-vicinity is Pseudoscheme's source directory (the
;;;     ASDF system's), not the template's /usr/local/src/scheme/; #f
;;;     if ASDF doesn't know the system.
;;;   - *load-pathname*, which program-vicinity reads, is the pathname
;;;     bound by SRFI 96's with-load-pathname; otherwise the directory of
;;;     the program being run (or the file being loaded by the R7RS front
;;;     end); at the REPL, #f, so program-vicinity is an error there.
;;;   - library-vicinity is $SCHEME_LIBRARY_PATH, else SLIB's default,
;;;     /usr/local/lib/slib/, as in the reference code.
(define-library (srfi 59)
  (export program-vicinity library-vicinity
          (rename pseudoscheme-implementation-vicinity implementation-vicinity)
          user-vicinity home-vicinity in-vicinity sub-vicinity
          make-vicinity pathname->vicinity vicinity:suffix?)
  (import (scheme base)
          (only (rnrs base) identifier-syntax)
          (srfi private srfi-59-support)
          (prefix (only (cl common-lisp) namestring) cl:)
          (prefix (only (cl asdf) system-source-directory) asdf:))
  (begin
    (define-syntax *load-pathname*
      (identifier-syntax (current-load-pathname)))
    (define (pseudoscheme-implementation-vicinity)
      (guard (e (#t #f))
        (cl:namestring (asdf:system-source-directory "pseudoscheme")))))
  (include-ci "reference/srfi-59/vicinity.scm"))
