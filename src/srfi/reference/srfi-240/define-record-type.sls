#!r6rs

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

(library (srfi :240 define-record-type)
  (export define-record-type
	  fields
	  mutable
	  immutable
	  parent
	  protocol
	  sealed
	  opaque
	  nongenerative
          generative
	  parent-rtd)
  (import (rnrs base (6))
	  (rnrs syntax-case (6))
	  (rnrs lists (6))
	  (rnrs control (6))
	  (prefix (rnrs (6)) rnrs:)
	  (rename (srfi :237)
		  (define-record-type :237:define-record-type)))

  (define-syntax define-record-type
    (lambda (stx)
      (define who 'define-record-type)
      (define distinct-identifiers?
        (lambda (id*)
          (let f ((id* id*))
            (or (null? id*)
                (and (not (exists
                           (lambda (id)
                             (bound-identifier=? (car id*) id))
                           (cdr id*)))
                     (f (cdr id*)))))))
      (define gen-field-spec
        (lambda (field)
          (syntax-case field ()
            [(field-name accessor-name)
             #'(immutable field-name accessor-name)]
            [(field-name accessor-name mutator-name)
             #'(mutable field-name accessor-name mutator-name)]
            [_
             (syntax-violation who
                               "invalid field spec"
                               stx
                               field)])))
      (syntax-case stx ()
        [(_ name (constructor-name field-name ...) pred field ...)
         (and (identifier? #'name)
	      (identifier? #'constructor-name)
	      (for-all identifier? #'(field-name ...))
	      (identifier? #'pred)
	      (distinct-identifiers? #'(field-name ...)))
         (let* ([name* #'(field-name ...)]
                [tmp* (generate-temporaries name*)]
                [field-spec* (map gen-field-spec #'(field ...))])
           (define gen-init
             (lambda (spec)
	       (or (exists
                    (lambda (name tmp)
		      (and (bound-identifier=? name (cadr spec))
                           tmp))
                    name* tmp*)
                   #'#f)))
           (for-each
            (lambda (name)
	      (unless (exists
		       (lambda (spec)
                         (bound-identifier=? name (cadr spec)))
		       field-spec*)
                (syntax-violation who
                                  "undefined field name"
                                  stx
                                  name)))
            name*)
           (unless (distinct-identifiers? (map cadr field-spec*))
             (syntax-violation who
			       "multiple field specs with the same name"
			       stx))
           (with-syntax ([(tmp ...) tmp*]
                         [(init ...) (map gen-init field-spec*)]
                         [(spec ...) field-spec*])
             #'(define-record-type (name constructor-name pred)
                 (fields spec ...)
                 (protocol
                  (lambda (p)
                    (lambda (tmp ...)
		      (p init ...)))))))]
        [(_ name-spec record-clause ...)
	 #'(:237:define-record-type name-spec record-clause ...)]
        [_
         (syntax-violation who "invalid record-type definition" stx)])))

  )


;; Local Variables:
;; mode: scheme
;; End:
