;;; SRFI 176: Version flag.  Written for Pseudoscheme: version-alist
;;; returns the properties that `pseudoscheme -V` prints, one to a line.
;;; They are command, scheme.id, languages, encodings, version,
;;; install-dir, scheme.srfi, scheme.features (the features other than
;;; srfi-N), scheme.path (the library search path) and build.platform.
(define-library (srfi 176)
  (export version-alist)
  (import (scheme base) (only (pseudoscheme lisp) lisp-eval-string))
  (begin
    (define version-alist
      (lisp-eval-string "(lambda () (ps-r7rs:version-alist))"))))
