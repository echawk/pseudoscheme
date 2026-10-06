;;; Part of SRFI 166 (see ../166.sld): chibi-scheme's lib/srfi/166/columnar.sld,
;;; with the changes described there, marked PSEUDOSCHEME.

(define-library (srfi 166 columnar)
  (import (scheme base)
          (scheme char)
          (scheme file)
          (srfi 1)
          (srfi 117)
          (srfi 130)
          (srfi 166 base)
          )
  (begin
    ;; PSEUDOSCHEME: (chibi optional) supplied let-optionals*; this is
    ;; base.sld's portable definition.
    (define-syntax let-optionals*
      (syntax-rules ()
        ((let-optionals* opt-ls () . body)
         (begin . body))
        ((let-optionals* (op . args) vars . body)
         (let ((tmp (op . args)))
           (let-optionals* tmp vars . body)))
        ((let-optionals* tmp ((var default) . rest) . body)
         (let ((var (if (pair? tmp) (car tmp) default))
               (tmp2 (if (pair? tmp) (cdr tmp) '())))
           (let-optionals* tmp2 rest . body)))
        ((let-optionals* tmp tail . body)
         (let ((tail tmp)) . body)))))
  (export
   columnar tabular wrapped wrapped/list wrapped/char
   justified from-file line-numbers)
  (include "../reference/srfi-166/column.scm"))
