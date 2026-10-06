;;; SRFI 63: homogeneous and heterogeneous arrays.  Aubrey Jaffer's
;;; implementation, SLIB's array.scm as reproduced in the SRFI document
;;; (reference/srfi-63/array.scm; its own permissive SLIB notice at its
;;; head, and the SRFI's MIT licence, in reference/srfi-63/LICENSE).
;;;
;;; The host it expects is supplied below: SLIB's require (a no-op
;;; here), slib:error, and SLIB's procedural records (make-record-type,
;;; record-constructor, record-accessor, record-predicate, by field
;;; name), on R6RS's procedural records.
;;;
;;; One change, marked PSEUDOSCHEME: the SRFI's equal? (which must
;;; compare arrays) falls back to R7RS's equal? instead of returning #f
;;; for the types it doesn't know, so that bytevectors, for instance,
;;; are still compared by content.  This equal? clashes with R7RS's,
;;; so a program that imports both must leave one out:
;;; (import (except (scheme base) equal?) (srfi 63)).
;;;
;;; Arrays are records; vectors and strings are arrays of rank 1, as the
;;; SRFI allows.  As in SLIB, every uniform-array prototype (A:floR64b,
;;; A:fixN8b, A:bool, ...) is a vector: no uniform types are provided,
;;; which the SRFI permits ("resorting finally to vector").  There is
;;; no #nA read syntax (SRFI 58).
(define-library (srfi 63)
  (export array? equal? array-rank array-dimensions make-array
          make-shared-array list->array array->list vector->array
          array->vector array-in-bounds? array-ref array-set!
          A:floC128b A:floC64b A:floC32b A:floC16b
          A:floR128b A:floR64b A:floR32b A:floR16b
          A:floQ128d A:floQ64d A:floQ32d
          A:fixZ64b A:fixZ32b A:fixZ16b A:fixZ8b
          A:fixN64b A:fixN32b A:fixN16b A:fixN8b
          A:bool)
  (import (except (scheme base) equal?)
          (rename (only (scheme base) equal?) (equal? r7:equal?))
          (scheme cxr)
          (prefix (rnrs records procedural) r6:)
          (prefix (only (rnrs records inspection) record-type-field-names) r6:))
  (begin
    (define-syntax require
      (syntax-rules () ((_ feature) (begin))))

    (define (slib:error . args)
      (apply error "SRFI 63" args))

    ;; SLIB records, by field name.
    (define (make-record-type name fields)
      (r6:make-record-type-descriptor
       (string->symbol name) #f #f #f #f
       (list->vector (map (lambda (f) (list 'mutable f)) fields))))
    (define (field-index rtd field)
      (let loop ((i 0) (fs (vector->list (r6:record-type-field-names rtd))))
        (cond ((null? fs) (error "no such record field" field))
              ((eq? (car fs) field) i)
              (else (loop (+ i 1) (cdr fs))))))
    (define (record-constructor rtd fields)
      (let ((make (r6:record-constructor
                   (r6:make-record-constructor-descriptor rtd #f #f)))
            (all (vector->list (r6:record-type-field-names rtd))))
        (if (r7:equal? fields all)
            make
            (error "record-constructor: fields must be all of them, in order"))))
    (define (record-accessor rtd field)
      (r6:record-accessor rtd (field-index rtd field)))
    (define (record-predicate rtd)
      (r6:record-predicate rtd)))
  (include "reference/srfi-63/array.scm"))
