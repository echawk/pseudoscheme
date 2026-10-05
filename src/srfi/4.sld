;;; SRFI 4: homogeneous numeric vector datatypes.  Each is a Common Lisp
;;; specialized vector (src/r6rs/numeric-vectors.lisp): a u8vector is a
;;; bytevector, an s16vector a (simple-array (signed-byte 16) (*)), an
;;; f64vector a (simple-array double-float (*)).  f32vector elements are
;;; single floats, read back as double floats (Scheme's inexact reals).
;;; The reader and writer know the external syntax, #s16(1 2 3).
(define-library (srfi 4)
  (export
          (rename %u8vector? u8vector?) (rename %make-u8vector make-u8vector) (rename %u8vector u8vector) (rename %u8vector-length u8vector-length)
          (rename %u8vector-ref u8vector-ref) (rename %u8vector-set! u8vector-set!) (rename %u8vector->list u8vector->list) (rename %list->u8vector list->u8vector)
          (rename %s8vector? s8vector?) (rename %make-s8vector make-s8vector) (rename %s8vector s8vector) (rename %s8vector-length s8vector-length)
          (rename %s8vector-ref s8vector-ref) (rename %s8vector-set! s8vector-set!) (rename %s8vector->list s8vector->list) (rename %list->s8vector list->s8vector)
          (rename %u16vector? u16vector?) (rename %make-u16vector make-u16vector) (rename %u16vector u16vector) (rename %u16vector-length u16vector-length)
          (rename %u16vector-ref u16vector-ref) (rename %u16vector-set! u16vector-set!) (rename %u16vector->list u16vector->list) (rename %list->u16vector list->u16vector)
          (rename %s16vector? s16vector?) (rename %make-s16vector make-s16vector) (rename %s16vector s16vector) (rename %s16vector-length s16vector-length)
          (rename %s16vector-ref s16vector-ref) (rename %s16vector-set! s16vector-set!) (rename %s16vector->list s16vector->list) (rename %list->s16vector list->s16vector)
          (rename %u32vector? u32vector?) (rename %make-u32vector make-u32vector) (rename %u32vector u32vector) (rename %u32vector-length u32vector-length)
          (rename %u32vector-ref u32vector-ref) (rename %u32vector-set! u32vector-set!) (rename %u32vector->list u32vector->list) (rename %list->u32vector list->u32vector)
          (rename %s32vector? s32vector?) (rename %make-s32vector make-s32vector) (rename %s32vector s32vector) (rename %s32vector-length s32vector-length)
          (rename %s32vector-ref s32vector-ref) (rename %s32vector-set! s32vector-set!) (rename %s32vector->list s32vector->list) (rename %list->s32vector list->s32vector)
          (rename %u64vector? u64vector?) (rename %make-u64vector make-u64vector) (rename %u64vector u64vector) (rename %u64vector-length u64vector-length)
          (rename %u64vector-ref u64vector-ref) (rename %u64vector-set! u64vector-set!) (rename %u64vector->list u64vector->list) (rename %list->u64vector list->u64vector)
          (rename %s64vector? s64vector?) (rename %make-s64vector make-s64vector) (rename %s64vector s64vector) (rename %s64vector-length s64vector-length)
          (rename %s64vector-ref s64vector-ref) (rename %s64vector-set! s64vector-set!) (rename %s64vector->list s64vector->list) (rename %list->s64vector list->s64vector)
          (rename %f32vector? f32vector?) (rename %make-f32vector make-f32vector) (rename %f32vector f32vector) (rename %f32vector-length f32vector-length)
          (rename %f32vector-ref f32vector-ref) (rename %f32vector-set! f32vector-set!) (rename %f32vector->list f32vector->list) (rename %list->f32vector list->f32vector)
          (rename %f64vector? f64vector?) (rename %make-f64vector make-f64vector) (rename %f64vector f64vector) (rename %f64vector-length f64vector-length)
          (rename %f64vector-ref f64vector-ref) (rename %f64vector-set! f64vector-set!) (rename %f64vector->list f64vector->list) (rename %list->f64vector list->f64vector))
  (import (prefix (pseudoscheme host) %)))
