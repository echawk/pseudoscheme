;;; SRFI 194: random data generators.  The SRFI's sample implementation
;;; by Arvydas Silanskas, Bradley J Lucier, Linas Vepštas and John Cowan,
;;; unmodified (reference/srfi-194/194-impl.scm, zipf-zri.scm and
;;; sphere.scm; MIT licence per their SPDX headers, in
;;; reference/srfi-194/LICENSE).  The library form is the shipped
;;; srfi/194.sld, with its cond-expands resolved for Pseudoscheme:
;;; (scheme base), (srfi 133) and (srfi 158).  Random sources are
;;; SRFI 27's.
(define-library (srfi 194)
  (import (scheme base)
          (srfi 133)
          (scheme case-lambda)
          (scheme inexact)
          (scheme complex)
          (scheme write)
          (srfi 27)
          (srfi 158))
  (export
    clamp-real-number

    current-random-source
    with-random-source

    make-random-integer-generator
    make-random-u1-generator
    make-random-u8-generator make-random-s8-generator
    make-random-u16-generator make-random-s16-generator
    make-random-u32-generator make-random-s32-generator
    make-random-u64-generator make-random-s64-generator
    make-random-boolean-generator
    make-random-char-generator
    make-random-string-generator
    make-random-real-generator
    make-random-rectangular-generator
    make-random-polar-generator

    make-bernoulli-generator
    make-binomial-generator
    make-categorical-generator
    make-normal-generator
    make-exponential-generator
    make-geometric-generator
    make-poisson-generator
    make-zipf-generator
    make-sphere-generator
    make-ellipsoid-generator
    make-ball-generator

    make-random-source-generator
    gsampling)
  (include "reference/srfi-194/194-impl.scm")
  (include "reference/srfi-194/zipf-zri.scm")
  (include "reference/srfi-194/sphere.scm"))
