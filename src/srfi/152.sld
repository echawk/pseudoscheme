;;; SRFI 152: string library (reduced).  John Cowan's sample
;;; implementation, unmodified (reference/srfi-152/portable.scm, after
;;; Olin Shivers' SRFI 13 and its MIT Scheme / scsh licences, at the end
;;; of that file; reference/srfi-152/macros.scm, MIT licence in
;;; reference/srfi-152/LICENSE).  This library form follows the shipped
;;; srfi-152.sld, renamed (srfi 152), with one change: the shipped
;;; library defines its own string=?, string<?, ..., string-ci>=?
;;; (extend-comparisons.scm) that also accept zero or one argument,
;;; which the SRFI does not ask for; this library exports R7RS's own
;;; instead.  It also exports, as R7RS's own bindings, the R7RS string
;;; procedures that the SRFI lists (string?, make-string, string-ref,
;;; substring, string-map, read-string, string-copy!, ...), which the
;;; shipped library leaves out.  So (import (scheme base) (scheme char)
;;; (srfi 152)) raises no conflict.
;;;
;;; Many names are shared with SRFI 13 and SRFI 130 (string-index,
;;; string-pad, string-join, ...) with different contracts; import only
;;; one of those libraries.

(define-library (srfi 152)
  (import (scheme base)
          (scheme char)
          (scheme cxr)
          (scheme case-lambda))

  ;; R7RS's own procedures, as the SRFI specifies
  (export string? make-string string
          string->vector string->list vector->string list->string
          string-length string-ref substring string-copy
          string=? string<? string>? string<=? string>=?
          string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?
          string-append string-map string-for-each
          read-string write-string
          string-set! string-fill! string-copy!)

  ;; Remaining exports, grouped as in the SRFI
  (export string-null? string-every string-any)
  (export string-tabulate string-unfold string-unfold-right)
  (export reverse-list->string)
  (export string-take string-drop string-take-right string-drop-right
          string-pad string-pad-right
          string-trim string-trim-right string-trim-both)
  (export string-replace)
  (export string-prefix-length string-suffix-length
          string-prefix? string-suffix?)
  (export string-index string-index-right string-skip string-skip-right
          string-contains string-contains-right
          string-take-while string-take-while-right
          string-drop-while string-drop-while-right
          string-break string-span)
  (export string-concatenate string-concatenate-reverse
          string-join)
  (export string-fold string-fold-right string-count
          string-filter string-remove)
  (export string-replicate string-segment string-split)

  (include "reference/srfi-152/macros.scm")
  (include "reference/srfi-152/portable.scm"))
