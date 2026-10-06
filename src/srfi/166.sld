;;; SRFI 166: monadic formatting.  Not the SRFI's sample implementation,
;;; but Alex Shinn's chibi-scheme one, which the sample is drawn from
;;; (lib/srfi/166.sld and lib/srfi/166/; 3-clause BSD licence in
;;; reference/srfi-166/LICENSE).  The .scm files are in
;;; reference/srfi-166/, unmodified; the sublibraries the SRFI
;;; specifies, (srfi 166 base), (srfi 166 pretty), (srfi 166 columnar),
;;; (srfi 166 unicode) and (srfi 166 color), are in 166/.  This file is
;;; chibi's 166.sld, unmodified but for this comment.
;;;
;;; Changes, in 166/*.sld only, marked PSEUDOSCHEME: include paths point
;;; into ../reference/srfi-166/; (chibi show shared) is
;;; (srfi private srfi-166-shared), a copy of chibi's
;;; lib/chibi/show/shared.sld (reference/srfi-166/shared.sld) under that
;;; name; and columnar's let-optionals*, from (chibi optional) in chibi,
;;; is the portable definition base.sld already uses on other hosts.
;;; SRFI 165 (the environment monad the formatters are built on) is
;;; Pseudoscheme's own (srfi 165).
;;;
;;; Limitations: written uses the host's write for strings, which
;;; writes a newline as itself rather than \n; and numbers formatted
;;; without a precision come from number->string, which writes large and
;;; small flonums with an exponent (299792458.0 is 2.99792458e8), so
;;; comma-rule doesn't apply to them.

(define-library (srfi 166)
  (import (srfi 166 base)
          (srfi 166 pretty)
          (srfi 166 columnar)
          (srfi 166 unicode)
          (srfi 166 color))
  (export
   ;; basic
   show displayed written written-shared written-simply escaped maybe-escaped
   numeric numeric/comma numeric/si numeric/fitted
   nl fl space-to tab-to nothing each each-in-list
   joined joined/prefix joined/suffix joined/last joined/dot
   joined/range padded padded/right padded/both
   trimmed trimmed/right trimmed/both trimmed/lazy
   fitted fitted/right fitted/both output-default
   ;; computations
   fn with with! forked call-with-output
   ;; state variables
   make-state-variable
   port row col width output writer pad-char ellipsis
   string-width substring/width substring/preserve
   radix precision decimal-sep decimal-align sign-rule
   comma-sep comma-rule word-separator? ambiguous-is-wide?
   pretty-environment
   ;; pretty
   pretty pretty-shared pretty-simply pretty-with-color
   ;; columnar
   columnar tabular wrapped wrapped/list wrapped/char
   justified from-file line-numbers
   ;; unicode
   terminal-aware
   string-terminal-width string-terminal-width/wide
   substring-terminal-width substring-terminal-width/wide
   substring-terminal-width substring-terminal-width/wide
   substring-terminal-preserve
   upcased downcased
   ;; color
   as-red as-blue as-green as-cyan as-yellow
   as-magenta as-white as-black
   as-bold as-italic as-underline
   as-color as-true-color
   on-red on-blue on-green on-cyan on-yellow
   on-magenta on-white on-black
   on-color on-true-color
   ))
