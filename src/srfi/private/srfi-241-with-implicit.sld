
;; Copyright (C) Marc Nieper-Wißkirchen (2022).  All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Part of SRFI 241 (match): Marc Nieper-Wißkirchen's R6RS sample
;;; implementation, reference/srfi-241/with-implicit.sls (MIT licence above, and in
;;; reference/srfi-241/LICENSE), converted mechanically to R7RS: the
;;; library form became define-library with the body in a begin, and the
;;; library names changed -- (srfi :241 match) and (srfi :241 match
;;; quasiquote) to (srfi 241 match) and (srfi 241 match quasiquote), and
;;; the sample's helper libraries (srfi :241 NAME) to (srfi private
;;; srfi-241-NAME).  The code is otherwise unchanged.
(define-library (srfi private srfi-241-with-implicit)
  (export
    with-implicit)
  (import
    (rnrs))
  (begin

  (define-syntax with-implicit
    (lambda (x)
      (syntax-case x ()
        [(_ (k x ...) e1 ... e2)
         #'(with-syntax ([x (datum->syntax #'k 'x)] ...)
             e1 ... e2)]
        [_ (syntax-violation 'with-implicit "invalid syntax" x)])))))
