;;; SRFI 195: multiple-value boxes.  Marc Nieper-Wißkirchen's sample
;;; implementation, unmodified (reference/srfi-195/195.scm, MIT licence
;;; in its header).  The library form follows the shipped srfi/195.sld
;;; (whose 111-declarations.scm is written out inline here).  (srfi 111)
;;; re-exports box, box?, unbox and set-box! from this library, so a
;;; SRFI 111 box is a SRFI 195 box and the reverse.
(define-library (srfi 195)
  (export box box? unbox set-box! box-arity unbox-value set-box-value!)
  (import (scheme base))
  (include "reference/srfi-195/195.scm"))
