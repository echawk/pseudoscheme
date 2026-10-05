;;; Helper library for SRFI 146: the reference implementation's (gleckler hamt), renamed (srfi 146 private hamt); see ../../146.sld and ../hash.sld.

;;; SPDX-FileCopyrightText: 2021 Arthur A. Gleckler
;;; SPDX-License-Identifier: MIT

(define-library (srfi 146 private hamt)
  (import (scheme base)
	  (scheme case-lambda)
	  (only (srfi 1) find-tail)
	  (srfi 16)
	  (only (srfi 143) fx-width)
	  (srfi 151)
	  (srfi 146 private hamt-misc)
	  (srfi 146 private vector-edit))
  (export fragment->mask
	  hamt->list
	  hamt-fetch
	  hamt-null
	  hamt-null?
	  hamt/count
	  hamt/empty?
	  hamt/for-each
	  hamt/immutable
	  hamt/mutable
	  hamt/mutable?
	  hamt/payload?
	  hamt/put
	  hamt/put!
	  hamt/replace
	  hamt/replace!
	  hash-array-mapped-trie?
	  make-hamt

	  ;; These are only needed by tests:
	  collision?
	  hamt-bucket-size
	  hamt-hash-size
	  hamt/root
	  leaf-stride
	  narrow/array
	  narrow/leaves
	  narrow?
	  next-set-bit
	  wide/array
	  wide/children
	  wide?)
  (include "../../reference/srfi-146/hamt.scm"))

