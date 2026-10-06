;;; SRFI 74: octet-addressed binary blocks.  Written for Pseudoscheme, as
;;; a thin layer over R6RS bytevectors, which grew out of this SRFI: a
;;; blob is a bytevector (and so a SRFI 4/66 u8vector).  The procedures
;;; are R6RS's with SRFI 74's argument order (size and endianness
;;; first).  The SRFI's reference implementation (Michael Sperber's
;;; blob.scm, on SRFI 66) is not used: it fixes (endianness native) as
;;; big, where here it is the machine's own, R6RS's native-endianness.
;;;
;;; endianness evaluates to the symbol big or little, as R6RS's does,
;;; and also accepts native.  (rnrs bytevectors) exports an endianness
;;; without native, so a program importing both excludes one.
(define-library (srfi 74)
  (export endianness
          blob? make-blob blob-length
          blob-u8-ref blob-u8-set! blob-s8-ref blob-s8-set!
          blob-uint-ref blob-sint-ref blob-uint-set! blob-sint-set!
          blob-u16-ref blob-u16-set! blob-s16-ref blob-s16-set!
          blob-u16-native-ref blob-u16-native-set!
          blob-s16-native-ref blob-s16-native-set!
          blob-u32-ref blob-u32-set! blob-s32-ref blob-s32-set!
          blob-u32-native-ref blob-u32-native-set!
          blob-s32-native-ref blob-s32-native-set!
          blob-u64-ref blob-u64-set! blob-s64-ref blob-s64-set!
          blob-u64-native-ref blob-u64-native-set!
          blob-s64-native-ref blob-s64-native-set!
          blob=? blob-copy! blob-copy
          blob->u8-list u8-list->blob blob->s8-list s8-list->blob
          blob->uint-list blob->sint-list uint-list->blob sint-list->blob)
  (import (scheme base)
          (only (rnrs bytevectors)
                native-endianness
                bytevector-uint-ref bytevector-sint-ref
                bytevector-uint-set! bytevector-sint-set!
                bytevector-u16-ref bytevector-u16-set!
                bytevector-s16-ref bytevector-s16-set!
                bytevector-u32-ref bytevector-u32-set!
                bytevector-s32-ref bytevector-s32-set!
                bytevector-u64-ref bytevector-u64-set!
                bytevector-s64-ref bytevector-s64-set!
                bytevector->uint-list bytevector->sint-list
                uint-list->bytevector sint-list->bytevector)
          (rename (only (rnrs bytevectors)
                        bytevector? bytevector-length
                        bytevector-u8-ref bytevector-u8-set!
                        bytevector-s8-ref bytevector-s8-set!
                        bytevector-u16-native-ref bytevector-u16-native-set!
                        bytevector-s16-native-ref bytevector-s16-native-set!
                        bytevector-u32-native-ref bytevector-u32-native-set!
                        bytevector-s32-native-ref bytevector-s32-native-set!
                        bytevector-u64-native-ref bytevector-u64-native-set!
                        bytevector-s64-native-ref bytevector-s64-native-set!
                        bytevector=? bytevector-copy!
                        bytevector->u8-list u8-list->bytevector)
                  (bytevector? blob?)
                  (bytevector-length blob-length)
                  (bytevector-u8-ref blob-u8-ref)
                  (bytevector-u8-set! blob-u8-set!)
                  (bytevector-s8-ref blob-s8-ref)
                  (bytevector-s8-set! blob-s8-set!)
                  (bytevector-u16-native-ref blob-u16-native-ref)
                  (bytevector-u16-native-set! blob-u16-native-set!)
                  (bytevector-s16-native-ref blob-s16-native-ref)
                  (bytevector-s16-native-set! blob-s16-native-set!)
                  (bytevector-u32-native-ref blob-u32-native-ref)
                  (bytevector-u32-native-set! blob-u32-native-set!)
                  (bytevector-s32-native-ref blob-s32-native-ref)
                  (bytevector-s32-native-set! blob-s32-native-set!)
                  (bytevector-u64-native-ref blob-u64-native-ref)
                  (bytevector-u64-native-set! blob-u64-native-set!)
                  (bytevector-s64-native-ref blob-s64-native-ref)
                  (bytevector-s64-native-set! blob-s64-native-set!)
                  (bytevector=? blob=?)
                  (bytevector-copy! blob-copy!)
                  (bytevector->u8-list blob->u8-list)
                  (u8-list->bytevector u8-list->blob)))
  (begin
    (define-syntax endianness
      (syntax-rules (little big native)
        ((_ little) 'little)
        ((_ big) 'big)
        ((_ native) (native-endianness))))

    (define (make-blob k) (make-bytevector k 0))
    (define (blob-copy blob) (bytevector-copy blob))

    (define (blob->s8-list blob)
      (bytevector->sint-list blob 'big 1))
    (define (s8-list->blob list)
      (sint-list->bytevector list 'big 1))

    (define (blob-uint-ref size endness blob index)
      (bytevector-uint-ref blob index endness size))
    (define (blob-sint-ref size endness blob index)
      (bytevector-sint-ref blob index endness size))
    (define (blob-uint-set! size endness blob index val)
      (bytevector-uint-set! blob index val endness size))
    (define (blob-sint-set! size endness blob index val)
      (bytevector-sint-set! blob index val endness size))

    (define (blob-u16-ref endness blob index) (bytevector-u16-ref blob index endness))
    (define (blob-u16-set! endness blob index val) (bytevector-u16-set! blob index val endness))
    (define (blob-s16-ref endness blob index) (bytevector-s16-ref blob index endness))
    (define (blob-s16-set! endness blob index val) (bytevector-s16-set! blob index val endness))
    (define (blob-u32-ref endness blob index) (bytevector-u32-ref blob index endness))
    (define (blob-u32-set! endness blob index val) (bytevector-u32-set! blob index val endness))
    (define (blob-s32-ref endness blob index) (bytevector-s32-ref blob index endness))
    (define (blob-s32-set! endness blob index val) (bytevector-s32-set! blob index val endness))
    (define (blob-u64-ref endness blob index) (bytevector-u64-ref blob index endness))
    (define (blob-u64-set! endness blob index val) (bytevector-u64-set! blob index val endness))
    (define (blob-s64-ref endness blob index) (bytevector-s64-ref blob index endness))
    (define (blob-s64-set! endness blob index val) (bytevector-s64-set! blob index val endness))

    (define (blob->uint-list size endness blob)
      (bytevector->uint-list blob endness size))
    (define (blob->sint-list size endness blob)
      (bytevector->sint-list blob endness size))
    (define (uint-list->blob size endness list)
      (uint-list->bytevector list endness size))
    (define (sint-list->blob size endness list)
      (sint-list->bytevector list endness size))))
