;;; SRFI 274: extended list conversion procedures.  Peter McGoron's
;;; sample implementation of this library, unmodified (a copy of
;;; reference/srfi-274/160/u32.sld; MIT licence in reference/srfi-274/LICENSE).
;;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;;; SPDX-License-Identifier: MIT
(define-library (srfi 274 160 u32)
  (import (srfi 274 160 base))
  (export list->u32vector))
