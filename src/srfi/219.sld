;;; SRFI 219: define higher-order lambda.  Lassi Kortela's R7RS sample
;;; implementation (srfi/219.sld in the SRFI repository; MIT licence,
;;; per the SRFI document: Copyright (C) 2021 Lassi Kortela), verbatim.
;;; Its define replaces R7RS's, so import (except (scheme base) define).
(define-library (srfi 219)
  (export define)
  (import (rename (scheme base) (define native-define)))
  (begin  (define-syntax define
            (syntax-rules ()
              ((define ((head . outer-args) . args) . body)
               (define (head . outer-args) (lambda args . body)))
              ((define head . body)
               (native-define head . body))))))
