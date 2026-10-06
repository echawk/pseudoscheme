;;; SPDX-FileCopyrightText: 2026 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT
(define-library (srfi 271 randomized)
  (export make-random-port)
  (import (scheme base)
          (scheme file))
  (begin
    (define (make-random-port . junk)
      (open-binary-input-file "/dev/urandom"))
    ))
