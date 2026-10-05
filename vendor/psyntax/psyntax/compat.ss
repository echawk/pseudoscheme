;;; Copyright (c) 2006, 2007 Abdulaziz Ghuloum and Kent Dybvig
;;; 
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE. 

(library (psyntax compat)
  (export make-parameter parameterize define-record pretty-print
          gensym void eval-core symbol-value set-symbol-value!
          file-options-spec lisp-keyword? host-literal?)
  (import 
    (rnrs)
    (only (psyntax system $bootstrap)
          void gensym eval-core set-symbol-value! symbol-value 
          pretty-print lisp-keyword? host-literal?))

  (define make-parameter
    (case-lambda
      ((x) (make-parameter x (lambda (x) x)))
      ((x fender)
       (assert (procedure? fender))
       (let ((x (fender x)))
         (case-lambda
           (() x)
           ((v) (set! x (fender v))))))))

  (define-syntax parameterize 
    (lambda (x)
      (syntax-case x ()
        ((_ () b b* ...) (syntax (let () b b* ...)))
        ((_ ((olhs* orhs*) ...) b b* ...)
         (with-syntax (((lhs* ...) (generate-temporaries (syntax (olhs* ...))))
                       ((rhs* ...) (generate-temporaries (syntax (olhs* ...)))))
           (syntax (let ((lhs* olhs*) ...
                   (rhs* orhs*) ...)
               (let ((swap 
                      (lambda () 
                        (let ((t (lhs*)))
                          (lhs* rhs*)
                          (set! rhs* t))
                        ...)))
                 (dynamic-wind 
                   swap
                   (lambda () b b* ...)
                   swap)))))))))

  ;;; we represent records as vectors for portability but this is 
  ;;; not nice.  (PSEUDOSCHEME: no longer; see below.)  If your system supports compile-time generative
  ;;; records, replace the definition of define-record with your 
  ;;; system supplied definition (which you should support in the 
  ;;; expander first of course).
  ;;; if your system allows associating printers with records, 
  ;;; a printer procedure is provided (so you can use it in the 
  ;;; output of the macro).  The printers provided take two 
  ;;; arguments, a record instance and an output port.  They 
  ;;; output something like #<stx (foo bar)> or #<library (rnrs)> 
  ;;; to the port.
  ;;;
  ;;; The following should be good for full R6RS implementations.
  ;;;
  ;;;   (define-syntax define-record
  ;;;     (syntax-rules ()
  ;;;       [(_ name (field* ...) printer) 
  ;;;        (define-record name (field* ...))]
  ;;;       [(_ name (field* ...))
  ;;;        (define-record-type name 
  ;;;           (sealed #t)     ; for better performance
  ;;;           (opaque #t)     ; for security
  ;;;           (nongenerative) ; for sanity
  ;;;           (fields field* ...))]))

  (define-syntax define-record
    (lambda (stx)
      (define (iota i j)
        (cond
          ((= i j) '())
          (else (cons i (iota (+ i 1) j)))))
      (syntax-case stx ()
        ((_ name (field* ...) printer) 
         (syntax (define-record name (field* ...))))
        ((_ name (field* ...))
         (with-syntax ((constructor 
                        (datum->syntax (syntax name)
                          (string->symbol
                            (string-append "make-"
                              (symbol->string 
                                (syntax->datum (syntax name)))))))
                       (predicate 
                        (datum->syntax (syntax name)
                          (string->symbol
                            (string-append 
                              (symbol->string 
                                (syntax->datum (syntax name)))
                              "?")))) 
                       (uid
                        (datum->syntax (syntax name)
                          (string->symbol
                            (string-append "psyntax-record-"
                              (symbol->string (syntax->datum (syntax name)))))))
                       ((accessor ...)
                        (map 
                          (lambda (x) 
                            (datum->syntax (syntax name)
                              (string->symbol 
                                (string-append 
                                  (symbol->string (syntax->datum
                                                    (syntax name)))
                                  "-"
                                  (symbol->string (syntax->datum x))))))
                          (syntax (field* ...)))) 
                       ((mutator ...)
                        (map 
                          (lambda (x) 
                            (datum->syntax (syntax name)
                              (string->symbol 
                                (string-append "set-" 
                                  (symbol->string (syntax->datum
                                                    (syntax name)))
                                  "-"
                                  (symbol->string (syntax->datum x))
                                  "!"))))
                          (syntax (field* ...))))
                       ((idx ...)
                        (iota 0 (length (syntax (field* ...))))))
           ;;; PSEUDOSCHEME: R6RS records, opaque and nongenerative, not
           ;;; vectors, so that a syntax object isn't a vector to the
           ;;; code it's handed to, and so that one in compiled code
           ;;; finds its record type again by the uid.
           (syntax (begin
               (define rtd
                 (make-record-type-descriptor 'name #f 'uid #t #t
                   (list->vector '((mutable field*) ...))))
               (define constructor
                 (record-constructor
                   (make-record-constructor-descriptor rtd #f #f)))
               (define predicate (record-predicate rtd))
               (define accessor (record-accessor rtd idx))
               ...
               (define mutator (record-mutator rtd idx))
               ...)))))))

  ;; PSEUDOSCHEME: the option symbols of (file-options no-create ...),
  ;; checked; the expander makes them an enum set.
  (define (file-options-spec x)
    (for-each
      (lambda (o)
        (unless (memq o '(no-create no-fail no-truncate))
          (error 'file-options "invalid file option" o)))
      x)
    x)

)





