;;; Part of SRFI 166 (see ../166.sld): chibi-scheme's lib/srfi/166/pretty.sld,
;;; with the changes described there, marked PSEUDOSCHEME.

(define-library (srfi 166 pretty)
  (import (scheme base)
          (scheme char)
          (scheme write)
          (srfi private srfi-166-shared)  ; PSEUDOSCHEME: was (chibi show shared)
          (srfi 1)
          (srfi 69)
          (srfi 130)
          (srfi 166 base)
          (srfi 166 color))
  (export pretty pretty-shared pretty-simply pretty-with-color)
  (include "../reference/srfi-166/pretty.scm"))
