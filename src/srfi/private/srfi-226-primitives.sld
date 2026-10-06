;;; From the SRFI 226 sample implementation (reference/srfi-226/lib/control-features/primitives.chezscheme.sls;
;;; MIT), with the libraries renamed, its Chez Scheme
;;; make-ephemeron-eq-hashtable from (srfi private
;;; srfi-226-make-ephemeron-eq-hashtable), and %continuation=? not eq?
;;; (see below).

;; Copyright (C) Marc Nieper-Wißkirchen (2021).  All Rights Reserved.

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

(library (srfi private srfi-226-primitives)
  (export %call-with-current-continuation
	  %call-in-continuation
	  %continuation=?
	  %case-lambda-box %case-lambda-box-ref)
  (import (except (rnrs (6)) call-with-current-continuation)
	  (srfi private srfi-226-make-ephemeron-eq-hashtable)
	  (only (pseudoscheme lisp) lisp-eval-string))

  ;; PSEUDOSCHEME: was eq?; but each call/cc here makes a procedure, so
  ;; two of the same continuation (in tail position) are compared by
  ;; what they capture (src/continuations.lisp)
  (define %continuation=?
    (lisp-eval-string "(lambda (a b) (if (pseudoscheme-psyntax::continuation= a b) t ps:false))"))

  (define procedure-locations
    (let ([locations (make-ephemeron-eq-hashtable)])
      (lambda () locations)))

  (define-syntax %case-lambda-box
    (syntax-rules ()
      [(%case-lambda-box expr [formals body] ...)
       (let ([proc (case-lambda [formals body] ...)])
	 (hashtable-set! (procedure-locations) proc expr)
	 proc)]))

  (define %case-lambda-box-ref
    (lambda (proc default)
      (hashtable-ref (procedure-locations) proc default)))

  (define continuation
    (let ([continuations (make-ephemeron-eq-hashtable)])
      (case-lambda
       [(k) (hashtable-ref continuations k #f)]
       [(k c) (hashtable-update! continuations k values c)])))

  (define %call-with-current-continuation
    (lambda (proc)
      (call/cc
       (lambda (k)
	 ((call/cc
	   (lambda (abort-k)
	     (continuation k abort-k)
	     (lambda ()
	       (proc k)))))))))

  (define %call-in-continuation
    (lambda (k thunk)
      ((assert (continuation k)) thunk))))

;; Local Variables:
;; mode: scheme
;; End:
