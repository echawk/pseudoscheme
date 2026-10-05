;;; Helper library for SRFI 146: the reference implementation's (gleckler hamt-misc), renamed (srfi 146 private hamt-misc); see ../../146.sld and ../hash.sld.

;;; SPDX-FileCopyrightText: 2021 Arthur A. Gleckler
;;; SPDX-License-Identifier: MIT

(define-library (srfi 146 private hamt-misc)
  (import (scheme base)
	  (scheme case-lambda)
	  (only (srfi 125) make-hash-table string-hash)
	  (only (srfi 128) make-comparator))
  (export assert do-list
	  make-string-hash-table
	  with-output-to-string)
  (include "../../reference/srfi-146/hamt-misc.scm"))

