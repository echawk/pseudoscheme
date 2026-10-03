;;; SRFI 143: fixnums.  After John Cowan's reference implementation's
;;; R6RS module (srfi-143.sls, MIT license): fixnums are the host's
;;; (rnrs arithmetic fixnums) fixnums, so fx-width is (fixnum-width).
;;; Modifications: the .sls's R6RS `library' body is transcribed here
;;; into an R7RS define-library (it cannot be `include'd as is); the
;;; renames it does through (rnrs base) and (rnrs r5rs) imports are
;;; done as export renames of (scheme base)'s exact-integer-sqrt,
;;; quotient and remainder.  The definitions are verbatim except two,
;;; changed to pass the reference test suite (chibi-test.scm): R6RS
;;; requires a bit index below (fixnum-width) and a rotate count below
;;; the field width, which SRFI 143 does not, so fxbit-set? answers
;;; the sign bit for indexes past the width, and fxbit-field-rotate
;;; reduces the count modulo the field width.
(define-library (srfi 143)
  (export fx-width fx-greatest fx-least
          fixnum? fx=? fx<? fx>? fx<=? fx>=?
          fxzero? fxpositive? fxnegative?
          fxodd? fxeven? fxmax fxmin
          fx+ fx- fxneg fx* (rename quotient fxquotient)
          (rename remainder fxremainder)
          fxabs fxsquare (rename exact-integer-sqrt fxsqrt)
          fx+/carry fx-/carry fx*/carry
          fxnot fxand fxior fxxor fxarithmetic-shift
          fxarithmetic-shift-left fxarithmetic-shift-right
          fxbit-count fxlength fxif fxbit-set? fxcopy-bit
          fxfirst-set-bit fxbit-field
          fxbit-field-rotate fxbit-field-reverse)
  (import (scheme base)
          (rename (except (rnrs arithmetic fixnums) fxcopy-bit)
                  (fxfirst-bit-set fxfirst-set-bit)
                  (fxbit-count r6rs:fxbit-count)
                  (fxbit-set? r6rs:fxbit-set?)
                  (fxreverse-bit-field fxbit-field-reverse)))
  (begin
    ;; Constants not in R6RS
    (define fx-width (fixnum-width))
    (define fx-greatest (greatest-fixnum))
    (define fx-least (least-fixnum))

    ;; Procedures not in R6RS
    (define (fxneg i) (fx- i))

    (define (fxabs i)
      (if (fxnegative? i) (- i) i))

    (define (fxsquare i)
      (fx* i i))

    ;; Incompatible semantics
    (define (fxbit-count i)
      (if (fx>=? i 0)
        (r6rs:fxbit-count i)
        (r6rs:fxbit-count (fxnot i))))

    ;; R6RS fxcopy-bit is only loosely related: we don't use it
    (define (fxcopy-bit index to bool)
      (if bool
          (fxior to (fxarithmetic-shift-left 1 index))
          (fxand to (fxnot (fxarithmetic-shift-left 1 index)))))

    ;; Imcompatible argument orderings
    (define (fxbit-set? index i)
      (if (fx>=? index fx-width)        ; changed: was just the r6rs call
          (fxnegative? i)
          (r6rs:fxbit-set? i index)))

    (define (fxbit-field-rotate i count start end)
      ;; changed: was (fxrotate-bit-field i start end count), with
      ;; (+ count (- end start)) for a negative count
      (let ((width (- end start)))
        (if (fxzero? width)
            i
            (fxrotate-bit-field i start end (modulo count width)))))))
