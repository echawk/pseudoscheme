; SPDX-FileCopyrightText: 2026 Daniel Ziltener
;
; SPDX-License-Identifier: MIT

(define-library (srfi 263 syntax)
  (export set-method!
          derive-object
          copy-object
          define-object)
  (include "srfi-263-syntax.impl.scm"))
