;; Copyright (C) Marc Nieper-Wißkirchen (2016).  All Rights Reserved. 

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


;; General

(define-syntax em-constant
  (em-syntax-rules ::: ()
    ((em-constant 'const)
     (em-cut 'em-constant-aux 'const <> ...))))

(define-syntax em-constant-aux
  (em-syntax-rules ()
    ((em-constant-aux 'const 'arg ...)
     'const)))

(define-syntax em-append
  (em-syntax-rules ()
    ((em-append) ''())
    ((em-append 'l) 'l)
    ((em-append 'm ... '(a ...) 'l) (em-append 'm ... '(a ... . l)))))

(define-syntax em-list
  (em-syntax-rules ()
    ((em-list 'a ...) '(a ...))))

(define-syntax em-cons
  (em-syntax-rules ()
    ((em-cons 'h 't) '(h . t))))

(define-syntax em-cons*
  (em-syntax-rules ()
    ((em-cons* 'e ... 't) '(e ... . t))))

(define-syntax em-car
  (em-syntax-rules ()
    ((em-car '(h . t)) 'h)))

(define-syntax em-cdr
  (em-syntax-rules ()
    ((em-cdr '(h . t)) 't)))

(define-syntax em-apply
  (em-syntax-rules ()
    ((em-apply 'keyword 'datum1 ... '(datum2 ...))
     (keyword 'datum1 ... 'datum2 ...))))

(define-syntax em-call
  (em-syntax-rules ()
    ((em-apply 'keyword 'datum ...)
     (keyword 'datum ...))))

(define-syntax em-eval
  (em-syntax-rules ()
    ((em-eval '(keyword datum ...))
     (keyword datum ...))))

(define-syntax em-error
  (em-syntax-rules ()
    ((em-error 'message 'arg ...)
     (em-suspend 'em-error-aux 'message 'arg ...))))

(define-syntax em-error-aux
  (syntax-rules ()
    ((em-error-aux s message arg ...)
     (syntax-error message arg ...))))

(define-syntax em-gensym
  (em-syntax-rules ()
    ((em-gensym) 'g)))

(define-syntax em-generate-temporaries
  (em-syntax-rules ()
    ((em-generate-temporaries '()) '())
    ((em-generate-temporaries '(h . t))
     (em-cons (em-gensym) (em-generate-temporaries 't)))))

(define-syntax em-quote
  (em-syntax-rules ()
    ((em-quote 'x) ''x)))

;; Boolean logic

(define-syntax em-if
  (em-syntax-rules ()
    ((em-if '#f consequent alternate)
     alternate)
    ((em-if 'test consequent alternate)
     consequent)))

(define-syntax em-not
  (em-syntax-rules ()
    ((em-not '#f)
     '#t)
    ((em-not '_)
     '#f)))

(define-syntax em-or
  (em-syntax-rules ()
    ((em-or)
     '#f)
    ((em-or 'x y ...)
     (em-if 'x 'x (em-or y ...)))))

(define-syntax em-and
  (em-syntax-rules ()
    ((em-and 'x)
     'x)
    ((em-and 'x y ...)
     (em-if 'x (em-and y ...) '#f))))

(define-syntax em-null?
  (em-syntax-rules ()
    ((em-null? '())
     '#t)
    ((em-null? '_)
     '#f)))

(define-syntax em-pair?
  (em-syntax-rules ()
    ((em-null? '(_ . _))
     '#t)
    ((em-null? '_)
     '#f)))

(define-syntax em-list?
  (em-syntax-rules ()
    ((em-list? '())
     '#t)
    ((em-list? '(_ . x))
     (em-list? 'x))
    ((em-list? '_)
     '#f)))

(define-syntax em-boolean?
  (em-syntax-rules ()
    ((em-boolean? '#f)
     '#t)
    ((em-boolean? '#t)
     '#t)
    ((em-boolean? '_)
     '#f)))

(define-syntax em-vector?
  (em-syntax-rules ()
    ((em-vector? '#(x ...))
     '#t)
    ((em-vector? '_)
     '#f)))

(define-syntax em-symbol?
  (em-syntax-rules ()
    ((em-symbol? '(x . y)) '#f)
    ((em-symbol? '#(x ...)) '#f)
    ((em-symbol? 'x)
     (em-suspend 'em-symbol?-aux 'x))))

(define-syntax em-symbol?-aux
  (syntax-rules ()
    ((em-symbol?-aux s x)
     (begin
       (define-syntax test
	 (syntax-rules ::: ()
		       ((test x %s) (em-resume %s '#t))
		       ((test y %s) (em-resume %s '#f))))
       (test symbol s)))))

(define-syntax em-bound-identifier=?
  (em-syntax-rules ()
    ((em-bound-identifier=? 'id 'b)
     (em-suspend em-bound-identifier=?-aux 'id 'b))))

(define-syntax em-bound-identifier=?-aux
  (syntax-rules ()
    ((em-bound-identifier=?-aux s id b)
     (bound-identifier=? id b (em-resume s '#t) (em-resume s '#f)))))

(define-syntax em-free-identifier=?
  (em-syntax-rules ()
    ((em-free-identifier=? 'id1 'id2)
     (em-suspend em-free-identifier=?-aux 'id1 'id2))))

(define-syntax em-free-identifier=?-aux
  (syntax-rules ()
    ((em-free-identifier=?-aux s id1 id2)
     (free-identifier=? id1 id2 (em-resume s '#t) (em-resume s '#f)))))

(define-syntax em-constant=?
  (em-syntax-rules ()
    ((ck= 'x 'y)
     (em-suspend em-constant=?-aux 'x 'y))))

(define-syntax em-constant=?-aux
  (syntax-rules ()
    ((em-constant=?-aux s x y)
     (begin
       (define-syntax test
	 (syntax-rules ::: ()
		       ((test x %s) (em-resume %s '#t))
		       ((test z %s) (em-resume %s '#f))))
       (test y s)))))

(define-syntax em-equal?
  (em-syntax-rules ()
    ((em-equal? '(x . y) '(u . v))
     (em-and (em-equal? 'x 'u) (em-equal? 'y 'v)))
    ((em-equal '#(x ...) '#(u ...))
     (em-and (em-equal? 'x 'u) ...))
    ((em-equal '() '())
     '#t)
    ((em-equal 'x 'u)
     (em-if (em-symbol? 'x)
	    (em-bound-identifier=? 'x 'u)
	    (em-constant=? 'x 'u)))
    ((em-equal '_ '_)
     '#f)))

