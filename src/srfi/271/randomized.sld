;;; Part of SRFI 271 (see ../271.sld): the sample implementation's
;;; srfi/271/randomized.sld, unmodified but for this comment.  A
;;; randomized random port reads the operating system's entropy pool,
;;; /dev/urandom, so this needs a Unix-like system.
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
