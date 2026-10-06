;;; Part of SRFI 271 (see ../271.sld): the sample implementation's
;;; srfi/271/determinized.sld, unmodified but for this comment.  The
;;; generator is xoshiro256++, in determinized/xoshiro256++.sld.
;;; SPDX-FileCopyrightText: 2026 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT
(define-library (srfi 271 determinized)
  (export make-random-port
          random-port?
          random-port-state
          random-port-state?
          random-port-state=?
          random-port-initialization-error?
          )
  (import (srfi 271 determinized xoshiro256++)))
