;;; Part of SRFI 166 (see ../166.sld): chibi-scheme's lib/srfi/166/unicode.sld,
;;; with the changes described there, marked PSEUDOSCHEME.

(define-library (srfi 166 unicode)
  (import (scheme base)
          (scheme char)
          (srfi 130)
          (srfi 151)
          (srfi 166 base))
  (export terminal-aware
          string-terminal-width string-terminal-width/wide
          substring-terminal-width substring-terminal-width/wide
          substring-terminal-preserve
          upcased downcased)
  (include "../reference/srfi-166/width.scm"
           "../reference/srfi-166/unicode.scm"))
