;;; SRFI 207: String-notated bytevectors.  The sample implementation by
;;; Wolfgang Corcoran-Mathe (MIT; reference/srfi-207/LICENSE), from the
;;; SRFI repository's srfi/207.sld and srfi/207/*.scm, unmodified but for
;;; the include paths.  Its cond-expands find Pseudoscheme's SRFIs 133,
;;; 145, 152, 158 and 160.
;;;
;;; The #u8"..." syntax is the reader's (src/read.scm).
(define-library (srfi 207)
  (import (scheme base)
          (scheme case-lambda)
          (only (scheme char) char-whitespace?)
          (srfi 1)
          (srfi 151))

  (cond-expand
    ((library (scheme bytevector))
     (import (only (scheme bytevector) bytevector->u8-list
                                       u8-list->bytevector)))
    (else
     (begin
      (define (u8-list->bytevector lis)
        (let* ((len (length lis))
               (bvec (make-bytevector len)))
          (let lp ((i 0) (lis lis))
            (cond ((null? lis) bvec)
                  (else (bytevector-u8-set! bvec i (car lis))
                        (lp (+ i 1) (cdr lis)))))))
      (define (bytevector->u8-list bvec)
        (list-tabulate (bytevector-length bvec)
                       (lambda (i)
                         (bytevector-u8-ref bvec i)))))))

  (cond-expand
    ((library (srfi 133))
     (import (only (srfi 133) vector-unfold)))
    (else
     (begin    ; We only need the "seedless" (tabulate) version
      (define (vector-unfold f len)
        (let ((res (make-vector len)))
          (let lp ((i 0))
            (cond ((= i len) res)
                  (else (vector-set! res i (f i))
                        (lp (+ i 1))))))))))

  (cond-expand
    ((library (srfi 145))
     (import (srfi 145)))
    (else
     (begin
      (define-syntax assume
        (syntax-rules ()
          ((_ expr . _)
           (or expr (car 0))))))))

  (cond-expand
    ((library (srfi 152))
     (import (srfi 152)))
    ((library (srfi 130))
     (import (srfi 130)))
    ((library (srfi 13))
     (import (srfi 13)))
    (else
     (error
      "No string library found (need one of SRFIs 152, 130, or 13).")))

  (cond-expand
    (guile       ; u8vectors and bytevectors are distinct in Guile
     (begin
      (define (u8vector-for-each proc bvec)
        (assume (procedure? proc))
        (assume (bytevector? bvec))
        (let ((len (bytevector-length bvec)))
          (let lp ((i 0))
            (cond ((= i len) (if #f #f))
                  (else
                   (proc (bytevector-u8-ref bvec i))
                   (lp (+ i 1)))))))
      (define (u8vector-unfold f len seed)
        (let ((u8vec (make-bytevector len)))
          (let lp ((i 0) (seed seed))
            (unless (= i len)
              (let-values (((b seed*) (f i seed)))
                (bytevector-u8-set! u8vec i b)
                (lp (+ i 1) seed*))))
          u8vec))))
    (else (import (only (srfi 160 u8) u8vector-for-each u8vector-unfold))))

  (cond-expand
    ((library (srfi 158))
     (import (only (srfi 158) list->generator)))
    (else
     (begin
      (define (list->generator xs)
        (lambda ()
          (if (null? xs)
              (eof-object)
              (let ((x (car xs)))
                (set! xs (cdr xs))
                x)))))))

  (export bytestring bytevector->hex-string bytestring->list
          make-bytestring make-bytestring!
          hex-string->bytevector bytevector->base64 base64->bytevector
          make-bytestring-generator
          bytestring-pad bytestring-pad-right bytestring-trim
          bytestring-trim-right bytestring-trim-both bytestring-replace
          bytestring-index bytestring-index-right bytestring-break
          bytestring-span
          bytestring>? bytestring<? bytestring<=? bytestring>=?
          bytestring-error? bytestring-error-message bytestring-error-irritants
          bytestring-join bytestring-split
          read-textual-bytestring write-textual-bytestring
          write-binary-bytestring)

  (include "reference/srfi-207/error.scm")
  (include "reference/srfi-207/parse.scm")
  (include "reference/srfi-207/base64.scm")
  (include "reference/srfi-207/bytestrings-impl.scm"))
