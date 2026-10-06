;;; (srfi private srfi-44-records): R7RS define-record-type with SRFI
;;; 256's extension for subtypes, which Antero Mejr's SRFI 44
;;; implementation uses.  Written for Pseudoscheme on R6RS procedural
;;; records.
;;;
;;;   (define-record-type <type> (ctor field ...) pred (field acc [mod]) ...)
;;;   (define-record-type (<type> <parent>) (ctor parent-instance field ...)
;;;     pred (field acc [mod]) ...)
;;;
;;; A subtype's constructor takes an instance of the parent type and then
;;; the subtype's own fields; the new record gets copies of the parent
;;; instance's fields (all of its ancestors' fields).  SRFI 256 requires
;;; a direct instance of the parent; that is not checked.  Fields not in
;;; the constructor start as #f.
(define-library (srfi private srfi-44-records)
  (export define-record-type)
  (import (except (scheme base) define-record-type)
          (rnrs syntax-case)
          (rnrs records procedural)
          (rnrs records inspection))
  (begin
    ;; The values of OBJ's fields, as an instance of RTD and its
    ;; ancestors, root type's fields first.
    (define (record-values obj rtd)
      (let loop ((rtd rtd) (acc '()))
        (if (not rtd)
            acc
            (let* ((n (vector-length (record-type-field-names rtd)))
                   (own (let fields ((i (- n 1)) (vals '()))
                          (if (< i 0)
                              vals
                              (fields (- i 1)
                                      (cons ((record-accessor rtd i) obj)
                                            vals))))))
              (loop (record-type-parent rtd) (append own acc))))))

    (define (make-rtd name parent field-names)
      (make-record-type-descriptor
       name parent #f #f #f
       (list->vector (map (lambda (f) (list 'mutable f)) field-names))))

    (define (raw-constructor rtd)
      (record-constructor (make-record-constructor-descriptor rtd #f #f)))

    (define-syntax define-record-type
      (lambda (stx)
        (define (field-name spec)
          (syntax-case spec () ((f . _) #'f)))
        ;; for each field, the constructor argument of the same name, or #f
        (define (field-values fields args)
          (map (lambda (f)
                 (let loop ((args args))
                   (cond ((null? args) #'#f)
                         ((eq? (syntax->datum (car args)) (syntax->datum f))
                          (car args))
                         (else (loop (cdr args))))))
               fields))
        (define (accessor-definitions type specs)
          (let loop ((specs specs) (i 0) (defs '()))
            (if (null? specs)
                (reverse defs)
                (with-syntax ((type type)
                              (idx (datum->syntax type i)))
                  (syntax-case (car specs) ()
                    ((f acc)
                     (loop (cdr specs) (+ i 1)
                           (cons #'(define acc (record-accessor type idx)) defs)))
                    ((f acc mod)
                     (loop (cdr specs) (+ i 1)
                           (cons #'(define mod (record-mutator type idx))
                                 (cons #'(define acc (record-accessor type idx))
                                       defs)))))))))
        (syntax-case stx ()
          ((_ (type parent) (ctor parent-arg arg ...) pred spec ...)
           (with-syntax (((field ...) (map field-name #'(spec ...)))
                         ((value ...) (field-values (map field-name #'(spec ...))
                                                    #'(arg ...)))
                         ((def ...) (accessor-definitions #'type #'(spec ...))))
             #'(begin
                 (define type (make-rtd 'type parent '(field ...)))
                 (define ctor
                   (let ((raw (raw-constructor type)))
                     (lambda (parent-arg arg ...)
                       (apply raw (append (record-values parent-arg parent)
                                          (list value ...))))))
                 (define pred (record-predicate type))
                 def ...)))
          ((_ type (ctor arg ...) pred spec ...)
           (with-syntax (((field ...) (map field-name #'(spec ...)))
                         ((value ...) (field-values (map field-name #'(spec ...))
                                                    #'(arg ...)))
                         ((def ...) (accessor-definitions #'type #'(spec ...))))
             #'(begin
                 (define type (make-rtd 'type #f '(field ...)))
                 (define ctor
                   (let ((raw (raw-constructor type)))
                     (lambda (arg ...) (raw value ...))))
                 (define pred (record-predicate type))
                 def ...))))))))
