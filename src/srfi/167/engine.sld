;;; Part of SRFI 167 (see ../167.sld): the sample implementation's
;;; srfi/engine.sld, with its include path pointing into
;;; ../reference/srfi-167/.
(define-library (srfi 167 engine)

  (export
   make-engine
   engine?
   engine-open
   engine-close
   engine-in-transaction
   engine-ref
   engine-set!
   engine-delete!
   engine-range-remove!
   engine-range
   engine-prefix-range
   engine-hook-on-transaction-begin
   engine-hook-on-transaction-commit
   engine-pack
   engine-unpack)

  (import (scheme base))

  (include "../reference/srfi-167/engine.scm"))
