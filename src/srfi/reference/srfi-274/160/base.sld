;;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;;; SPDX-License-Identifier: MIT
(define-library (srfi 274 160 base)
  (import (scheme base) (scheme case-lambda)
          (srfi 274 internal)
          (prefix (only (srfi 160 base)
                        list->s8vector
                        list->u8vector
                        list->s16vector
                        list->u16vector
                        list->s32vector
                        list->u32vector
                        list->s64vector
                        list->u64vector
                        list->f32vector
                        list->f64vector
                        list->c64vector
                        list->c128vector)
                  srfi-160:)
          (except (srfi 160 s8) list->s8vector)
          (except (srfi 160 u8) list->u8vector)
          (except (srfi 160 s16) list->s16vector)
          (except (srfi 160 u16) list->u16vector)
          (except (srfi 160 s32) list->s32vector)
          (except (srfi 160 u32) list->u32vector)
          (except (srfi 160 s64) list->s64vector)
          (except (srfi 160 u64) list->u64vector)
          (except (srfi 160 f32) list->f32vector)
          (except (srfi 160 f64) list->f64vector)
          (except (srfi 160 c64) list->c64vector)
          (except (srfi 160 c128) list->c128vector))
  (export list->s8vector
          list->u8vector
          list->s16vector
          list->u16vector
          list->s32vector
          list->u32vector
          list->s64vector
          list->u64vector
          list->f32vector
          list->f64vector
          list->c64vector
          list->c128vector)
  (begin
    (define (converter who basic unfold)
      (case-lambda
        ((lst) (basic lst))
        ((lst start) (basic (list-tail lst start)))
        ((lst start end)
         (argcheck! who start end lst)
         (unfold (lambda (ignored seed)
                   (values (car seed) (cdr seed)))
                 (- end start)
                 (list-tail lst start)))))
    (define list->s8vector
      (converter 'list->s8vector srfi-160:list->s8vector s8vector-unfold))
    (define list->u8vector
      (converter 'list->u8vector srfi-160:list->u8vector u8vector-unfold))
    (define list->s16vector
      (converter 'list->s16vector srfi-160:list->s16vector s16vector-unfold))
    (define list->u16vector
      (converter 'list->u16vector srfi-160:list->u16vector u16vector-unfold))
    (define list->s32vector
      (converter 'list->s32vector srfi-160:list->s32vector s32vector-unfold))
    (define list->u32vector
      (converter 'list->u32vector srfi-160:list->u32vector u32vector-unfold))
    (define list->s64vector
      (converter 'list->s64vector srfi-160:list->s64vector s64vector-unfold))
    (define list->u64vector
      (converter 'list->u64vector srfi-160:list->u64vector u64vector-unfold))
    (define list->f32vector
      (converter 'list->f32vector srfi-160:list->f32vector f32vector-unfold))
    (define list->f64vector
      (converter 'list->f64vector srfi-160:list->f64vector f64vector-unfold))
    (define list->c64vector
      (converter 'list->c64vector srfi-160:list->c64vector c64vector-unfold))
    (define list->c128vector
      (converter 'list->c128vector srfi-160:list->c128vector c128vector-unfold))))