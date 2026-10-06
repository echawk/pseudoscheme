;;; SRFI 257: simple extendable pattern matcher with backtracking --
;;; the (srfi 257 box) sublibrary.  Sergei Egorov's R7RS sample library, unmodified apart from
;;; this comment (reference/srfi-257/257-box.sld; MIT licence per its SPDX header, in
;;; reference/srfi-257/LICENSE).
;;; SPDX-FileCopyrightText: 2024 Sergei Egorov
;;; SPDX-License-Identifier: MIT

(define-library (srfi 257 box)
  (import (scheme base) (srfi 111) (srfi 257))
  (export ~box? ~box)

(begin

(define-match-pattern ~box? ()
  ((_ p ...) (~and (~test box?) p ...)))


(define-match-pattern ~box ()
  ((_ p) (~and (~test box?) (~prop unbox => p))))

))
