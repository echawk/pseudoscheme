;;; SRFI 274: extended list conversion procedures.  The SRFI names no
;;; (srfi 274) library, only (srfi 274 base), (srfi 274 41), (srfi 274
;;; 134), (srfi 274 158), (srfi 274 160 base) and (srfi 274 160 TYPE),
;;; which are Peter McGoron's sample implementation, unmodified, in 274/
;;; (MIT licence in reference/srfi-274/LICENSE).  This library gathers
;;; all their procedures.  Every one of them replaces a standard or SRFI
;;; procedure of the same name (list-copy, list->string, list->vector,
;;; list->stream, list->ideque, list->generator, list->u8vector, ...),
;;; extending it with optional start and end arguments, so a program
;;; excludes the originals from (scheme base) and the SRFIs.
(define-library (srfi 274)
  (import (srfi 274 base) (srfi 274 41) (srfi 274 134) (srfi 274 158)
          (srfi 274 160 base))
  (export list-copy list->string list->vector
          list->stream list->ideque list->generator
          list->s8vector list->u8vector list->s16vector list->u16vector
          list->s32vector list->u32vector list->s64vector list->u64vector
          list->f32vector list->f64vector list->c64vector list->c128vector))
