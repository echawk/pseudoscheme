;;; Helper library for SRFI 146: the reference implementation's (gleckler vector-edit), renamed (srfi 146 private vector-edit); see ../../146.sld and ../hash.sld.

;;; SPDX-FileCopyrightText: 2021 Arthur A. Gleckler
;;; SPDX-License-Identifier: MIT

(define-library (srfi 146 private vector-edit)
  (import (scheme base))
  (export vector-edit vector-replace-one vector-without)
  (include "../../reference/srfi-146/vector-edit.scm"))
