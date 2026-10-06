;;; Copyright © 2024 Wolfgang Corcoran-Mathe
;;; Copyright © 2024 Marc Nieper-Wißkirchen
;;;
;;; SPDX-FileCopyrightText: 2024 Wolfgang Corcoran-Mathe, Marc Nieper-Wißkirchen
;;; SPDX-License-Identifier: MIT
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining
;;; a copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject
;;; to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included
;;; in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
;;; KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
;;; WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Part of SRFI 255 (restarting conditions): the R6RS sample
;;; implementation by Wolfgang Corcoran-Mathe and Marc Nieper-Wißkirchen,
;;; reference/srfi-255/srfi-255.sls (MIT licence above, and in
;;; reference/srfi-255/LICENSE), converted mechanically to R7RS: the
;;; library form became define-library with the body in a begin, and the
;;; library names changed -- (srfi :255) to (srfi 255), (srfi :39
;;; parameters) to (srfi 39), and the sample's helper libraries
;;; (srfi :255 NAME) to (srfi private srfi-255-NAME).  The code is
;;; otherwise unchanged.  Restarters are R6RS conditions (&restarter is
;;; an R6RS condition type).
(define-library (srfi 255)
  (export &restarter
          make-restarter
          restarter?
          restarter-tag
          restarter-formals
          restarter-description
          restarter-invoker
          restarter-who
          define-restartable
          restarter-guard
          restartable
          restart
          current-interactor
          with-current-interactor
          )

  (import (srfi private srfi-255-restarters))
  (begin))
