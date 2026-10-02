;;; extras-pseudoscheme.ss
;;; WHEN and UNLESS, built into Chez Scheme but absent from Dybvig &
;;; Hieb's macro-defs.ss, which nonetheless uses UNLESS (in its DELAY
;;; support code).  Load after expand.pp and before macro-defs.ss.

(define-syntax when
   (lambda (x)
      (syntax-case x ()
         ((_ test e1 e2 ...)
          (syntax (if test (begin e1 e2 ...)))))))

(define-syntax unless
   (lambda (x)
      (syntax-case x ()
         ((_ test e1 e2 ...)
          (syntax (if test #f (begin e1 e2 ...)))))))
