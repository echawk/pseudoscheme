;;; SRFI 188: splicing binding constructs for syntactic keywords.  The
;;; sample implementation (Marc Nieper-Wißkirchen's, for Chibi; MIT
;;; licence, in reference/srfi-188/) re-exports Chibi's let-syntax and
;;; letrec-syntax, which splice.  Here the splicing forms are R6RS's
;;; let-syntax and letrec-syntax from (rnrs base), which psyntax splices
;;; into a definition context as the SRFI specifies.  R7RS's own
;;; let-syntax and letrec-syntax, in (scheme base), don't splice.
(define-library (srfi 188)
  (export (rename let-syntax splicing-let-syntax)
          (rename letrec-syntax splicing-letrec-syntax))
  (import (only (rnrs base) let-syntax letrec-syntax)))
