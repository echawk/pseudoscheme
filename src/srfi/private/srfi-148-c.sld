;;; (srfi private srfi-148-c): part of SRFI 148's sample implementation
;;; (see 148.sld): the section Combinatorics.  It exports everything
;;; it defines, for the other parts and (srfi 148).
(define-library (srfi private srfi-148-c)
  (export em-0 em-1 em-2 em-3 em-4 em-5 em-6 em-7 em-8 em-9 em-10 em= em<
          em<= em> em>= em-zero? em-even? em-odd? em+ em- em* em*-aux
          em-quotient em-remainder em-binom em-fact em-fact-del
          em-fact-cons*)
  (import (except (scheme base) define-syntax let-syntax letrec-syntax syntax-rules)
          (srfi 147)
          (srfi 26)
          (srfi private srfi-148-a)
          (srfi private srfi-148-b))
  (include "../reference/srfi-148/148.macros.6.scm"))
