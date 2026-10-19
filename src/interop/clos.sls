;;; -*- Mode: Scheme -*-
;;;; (quasischeme clos): Common Lisp's object system in Scheme's syntax,
;;;; as GOOPS has it.  Classes and generic functions are CLOS's own, so
;;;; Lisp code sees them as it sees its own (named by the Scheme
;;;; symbols, as (cl:defclass ...) from Scheme names them); a method's
;;;; body is Scheme.
;;;;
;;;;   (define-class <point> ()
;;;;     (x #:init-value 0 #:init-keyword #:x #:accessor point-x)
;;;;     (y #:init-value 0 #:init-keyword #:y #:getter point-y #:setter set-point-y!))
;;;;   (define-class <point3> (<point>) (z #:init-keyword #:z #:accessor point-z))
;;;;
;;;;   (define-generic describe)
;;;;   (define-method (describe (p <point>)) (list 'point (point-x p) (point-y p)))
;;;;   (define-method (describe (p <point3>))
;;;;     (append (call-next-method) (list (point-z p))))
;;;;
;;;;   (describe (make <point3> #:x 1 #:y 2 #:z 3))  ; => (point 1 2 3)
;;;;
;;;; Slot options: #:init-value v, #:init-thunk f, #:init-keyword #:k,
;;;; #:getter g, #:setter s (s object value), #:accessor a (a getter).
;;;; In a method, (call-next-method) calls the next method with the same
;;;; arguments, (call-next-method a ...) with others, and (next-method?)
;;;; says whether there is one.  A parameter without a class is <top>'s.

(library (quasischeme clos)
  (export define-class define-generic define-method
          make slot-ref slot-set! slot-bound? class-of class-name class? is-a? generic?
          <top> <object> <number> <real> <integer> <string> <symbol> <pair> <null>
          <list> <vector> <procedure> <char> <hashtable>)
  (import (rnrs) (quasischeme clos primitives))

  (define-syntax define-class
    (lambda (x)
      ;; (name option value ...) => (name ((option value) ...)), and the
      ;; procedure definitions its #:getter, #:setter and #:accessor ask for
      (define (keyword-named? k name)
        (equal? (%clos:keyword-name k) name))
      (define (slot-procedures k slot options)
        (let loop ((o options) (acc '()))
          (if (or (null? o) (null? (cdr o)))
              (reverse acc)
              (let ((key (syntax->datum (car o))) (value (cadr o)))
                (loop (cddr o)
                      (cond ((or (keyword-named? key "getter") (keyword-named? key "accessor"))
                             (cons #`(define #,value (lambda (object) (slot-ref object '#,slot)))
                                   acc))
                            ((keyword-named? key "setter")
                             (cons #`(define #,value
                                       (lambda (object v) (slot-set! object '#,slot v)))
                                   acc))
                            (else acc)))))))
      (define (slot-spec s)
        (syntax-case s ()
          (id (identifier? #'id) (values #'(list 'id) '()))
          ((id option ...)
           (let loop ((o #'(option ...)) (spec '()))
             (if (or (null? o) (null? (cdr o)))
                 (values #`(list 'id #,@(reverse spec))
                         (slot-procedures #'id #'id #'(option ...)))
                 (loop (cddr o) (cons (cadr o) (cons #`(quote #,(car o)) spec))))))))
      (syntax-case x ()
        ((_ name (super ...) slot ...)
         (let loop ((slots #'(slot ...)) (specs '()) (procedures '()))
           (if (null? slots)
               #`(begin
                   (define name
                     (%clos:define-class 'name (list super ...) (list #,@(reverse specs))))
                   #,@procedures)
               (let-values (((spec procs) (slot-spec (car slots))))
                 (loop (cdr slots) (cons spec specs) (append procedures procs)))))))))

  (define-syntax define-generic
    (syntax-rules ()
      ((_ name) (define name (%clos:generic 'name)))))

  (define-syntax define-method
    (lambda (x)
      (define (parameters ps)
        ;; => (names classes rest-or-#f)
        (let loop ((ps ps) (names '()) (classes '()))
          (syntax-case ps ()
            (() (list (reverse names) (reverse classes) #f))
            (rest (identifier? #'rest) (list (reverse names) (reverse classes) #'rest))
            (((p class) . more) (loop #'more (cons #'p names) (cons #'class classes)))
            ((p . more) (loop #'more (cons #'p names) (cons #'<top> classes))))))
      (syntax-case x ()
        ((_ (name . ps) body1 body2 ...)
         (with-syntax ((((p ...) (class ...) rest) (parameters #'ps))
                       (call-next-method (datum->syntax #'name 'call-next-method))
                       (next-method? (datum->syntax #'name 'next-method?)))
           (if (syntax->datum #'rest)
               #'(%clos:add-method! 'name (list class ...)
                                    (lambda (call-next-method next-method? p ... . rest)
                                      body1 body2 ...)
                                    #t)
               #'(%clos:add-method! 'name (list class ...)
                                    (lambda (call-next-method next-method? p ...)
                                      body1 body2 ...)
                                    #f)))))))

  (define make %clos:make)
  (define slot-ref %clos:slot-ref)
  (define slot-set! %clos:slot-set!)
  (define slot-bound? %clos:slot-bound?)
  (define class-of %clos:class-of)
  (define class-name %clos:class-name)
  (define class? %clos:class?)
  (define is-a? %clos:is-a?)
  (define generic? %clos:generic?)

  (define <top> (%clos:builtin-class "<top>"))
  (define <object> (%clos:builtin-class "<object>"))
  (define <number> (%clos:builtin-class "<number>"))
  (define <real> (%clos:builtin-class "<real>"))
  (define <integer> (%clos:builtin-class "<integer>"))
  (define <string> (%clos:builtin-class "<string>"))
  (define <symbol> (%clos:builtin-class "<symbol>"))
  (define <pair> (%clos:builtin-class "<pair>"))
  (define <null> (%clos:builtin-class "<null>"))
  (define <list> (%clos:builtin-class "<list>"))
  (define <vector> (%clos:builtin-class "<vector>"))
  (define <procedure> (%clos:builtin-class "<procedure>"))
  (define <char> (%clos:builtin-class "<char>"))
  (define <hashtable> (%clos:builtin-class "<hashtable>")))
