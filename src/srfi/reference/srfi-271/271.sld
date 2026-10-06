;;; SPDX-FileCopyrightText: 2026 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT
(define-library (srfi 271)
  (export make-random-port)
  (import (scheme base)
          (scheme file)
          (srfi 271 randomized)))
