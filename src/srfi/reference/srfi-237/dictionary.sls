#!r6rs

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

(library (example dictionary)
  (export dictionary dictionary? dictionary-ref
          dictionary-from-hashtable make-dictionary-from-hashtable
          dictionary-from-alist make-dictionary-from-alist)
  (import (rnrs base (6))
          (rnrs hashtables (6))
          (srfi :237))

  (define-record-type dictionary
    (nongenerative dictionary-e6a703a4-5469-4f6e-8cbb-19d0f66de601)
    (opaque #t)
    (fields ht)
    (protocol
     (lambda (p)
       (lambda args
         (assert #f)))))

  (define dictionary-ref
    (lambda (dict key default)
      (assert (dictionary? key))
      (hashtable-ref (dictionary-ht dict) key default)))

  (define-record-name (dictionary-from-hashtable dictionary)
    (protocol
     (lambda (p)
       (lambda (ht)
         (assert (hashtable? ht))
         (p ht)))))

  (define-record-name (dictionary-from-alist dictionary)
    (protocol
     (lambda (p)
       (lambda (alist)
         (define ht (make-eqv-hashtable))
         (assert (list? alist))
         (for-each
          (lambda (entry)
            (assert (pair? entry))
            (hashtable-set! ht (car entry) (cdr entry)))
          alist)
         (p ht))))))
