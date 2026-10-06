;;; SRFI 271: random port libraries.  Wolfgang Corcoran-Mathe's sample
;;; implementation's srfi/271.sld (reference/srfi-271/; MIT licence in
;;; reference/srfi-271/LICENSE), unmodified but for this comment.
;;; (srfi 271) is (srfi 271 randomized); see 271/randomized.sld and
;;; 271/determinized.sld.
;;; SPDX-FileCopyrightText: 2026 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT
(define-library (srfi 271)
  (export make-random-port)
  (import (scheme base)
          (scheme file)
          (srfi 271 randomized)))
