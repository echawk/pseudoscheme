
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
;;; implementation, reference/srfi-241/helpers.sls (MIT licence above, and in
;;; reference/srfi-241/LICENSE), converted mechanically to R7RS: the
;;; library form became define-library with the body in a begin, and the
;;; library names changed -- (srfi :241 match) and (srfi :241 match
;;; quasiquote) to (srfi 241 match) and (srfi 241 match quasiquote), and
;;; the sample's helper libraries (srfi :241 NAME) to (srfi private
;;; srfi-241-NAME).  The code is otherwise unchanged.
(define-library (srfi private srfi-241-helpers)
  (export
    construct-name
    ellipsis?
    symbolic-identifier=?)
  (import
    (rnrs)
    (srfi private srfi-241-define-who))
  (begin

  (define/who symbolic-identifier=?
    (lambda (id1 id2)
      (unless (identifier? id1)
        (assertion-violation who "invalid first identifier argument" id1))
      (unless (identifier? id2)
        (assertion-violation who "invalid second identifier argument" id2))
      (symbol=? (syntax->datum id1)
                (syntax->datum id2))))

  (define/who construct-name
    (lambda (k . arg*)
      (unless (identifier? k)
        (assertion-violation who "invalid template identifier argument" k))
      (datum->syntax
       k
       (string->symbol
	(apply string-append
	       (map (lambda (x)
		      (cond
                       [(string? x) x]
                       [(identifier? x)
			(symbol->string (syntax->datum x))]
                       [else
                        (assertion-violation who "invalid argument" x)]))
		    arg*))))))

  (define ellipsis?
    (lambda (x)
      (and (identifier? x)
	   (free-identifier=? x #'(... ...)))))

  ))
