;;; SRFI 210: procedures and syntax for multiple values.  Marc
;;; Nieper-Wißkirchen's sample implementation, unmodified
;;; (reference/srfi-210/210.scm, MIT licence in its header).  The library
;;; form follows the shipped srfi/210.sld, with (srfi 1) for
;;; (scheme list).  It needs SRFI 195's multiple-value boxes (box and
;;; unbox), which it takes from (srfi 195); so box/mv returns, and
;;; bind/box and box-values take, the same boxes as (srfi 111) and
;;; (srfi 195).  The cond-expand's else branch, a private multiple-value
;;; box type, is only a fallback for a build without (srfi 195).
(define-library (srfi 210)
  (export apply/mv call/mv list/mv vector/mv box/mv value/mv coarity
          set!-values with-values case-receive bind/mv
          list-values vector-values box-values value identity
          compose-left compose-right map-values bind/list bind/box bind)
  (import (scheme base)
          (scheme case-lambda)
          (srfi 1))
  (cond-expand
   ((library (srfi 195))
    (import (only (srfi 195) box unbox)))
   (else
    (begin
      (define-record-type multiple-value-box
        (make-multiple-value-box values)
        multiple-value-box?
        (values multiple-value-box-values))
      (define (box . values) (make-multiple-value-box values))
      (define (unbox b) (apply values (multiple-value-box-values b))))))
  (include "reference/srfi-210/210.scm"))
