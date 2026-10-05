;;; SRFI 35: conditions.  Written for Pseudoscheme, on R6RS conditions.
;;;
;;; A SRFI 35 condition type is an R6RS record-type descriptor that
;;; descends from &condition, and a SRFI 35 condition is an R6RS
;;; condition (a simple condition is a record of such a type; a compound
;;; condition is what R6RS's condition procedure returns).  So:
;;;
;;; - &condition, &message, &serious and &error are bound to R6RS's own
;;;   descriptors, (record-type-descriptor &message) and so on, whose
;;;   hierarchy and field names (message) are those SRFI 35 specifies.
;;;   condition?, message-condition?, condition-message,
;;;   serious-condition? and error? are R6RS's procedures, re-exported.
;;; - Conditions made here are raised and caught by raise, guard and
;;;   with-exception-handler like any other; R7RS's error-object? and
;;;   error-object-message accept them, and the predicates and
;;;   condition-ref here work on conditions the system raises (error,
;;;   assertion-violation, a failed primitive): those are made of
;;;   &error or &assertion, &message, &irritants and &who components.
;;; - make-condition-type makes an R6RS record type.  An R6RS condition
;;;   type's descriptor, (record-type-descriptor &foo), is a SRFI 35
;;;   condition type, and R6RS's condition-predicate and
;;;   condition-accessor accept SRFI 35 types.
;;;
;;; Names that clash with (rnrs conditions): the type names (variables
;;; here, record-type names there), condition (syntax here, a procedure
;;; there) and define-condition-type (different syntax).  Import one of
;;; the two with a prefix or except.
(define-library (srfi 35)
  (export make-condition-type condition-type? make-condition condition?
          condition-has-type? condition-ref make-compound-condition
          extract-condition define-condition-type condition
          &condition &message &serious &error
          message-condition? condition-message serious-condition? error?)
  (import (scheme base)
          (prefix (rnrs conditions) r6:)
          (only (rnrs records syntactic) record-type-descriptor)
          (prefix (rnrs records procedural) r6:)
          (prefix (rnrs records inspection) r6:))
  (begin
    (define &condition (record-type-descriptor r6:&condition))
    (define &message (record-type-descriptor r6:&message))
    (define &serious (record-type-descriptor r6:&serious))
    (define &error (record-type-descriptor r6:&error))

    (define condition? r6:condition?)
    (define message-condition? r6:message-condition?)
    (define condition-message r6:condition-message)
    (define serious-condition? r6:serious-condition?)
    (define error? r6:error?)

    (define (descends? rtd ancestor)
      (let loop ((rtd rtd))
        (cond ((not rtd) #f)
              ((eq? rtd ancestor) #t)
              (else (loop (r6:record-type-parent rtd))))))

    (define (condition-type? x)
      (and (r6:record-type-descriptor? x) (descends? x &condition)))

    (define (check-type who x)
      (if (not (condition-type? x))
          (error (string-append (symbol->string who) ": not a condition type") x)))

    ;; The field names of TYPE, its ancestors' first, as R6RS's record
    ;; constructor takes its arguments.
    (define (all-field-names type)
      (let loop ((rtd type) (names '()))
        (if rtd
            (loop (r6:record-type-parent rtd)
                  (append (vector->list (r6:record-type-field-names rtd)) names))
            names)))

    (define (make-condition-type id parent field-names)
      (check-type 'make-condition-type parent)
      (let ((inherited (all-field-names parent)))
        (for-each (lambda (f)
                    (if (memq f inherited)
                        (error "make-condition-type: field already in the parent type" f)))
                  field-names))
      (r6:make-record-type-descriptor
       id parent #f #f #f
       (list->vector (map (lambda (f) (list 'immutable f)) field-names))))

    (define (make-condition type . field-plist)
      (check-type 'make-condition type)
      (let ((names (all-field-names type)))
        (let check ((plist field-plist))
          (cond ((null? plist))
                ((or (null? (cdr plist)) (not (memq (car plist) names)))
                 (error "make-condition: bad field list" type field-plist))
                (else (check (cddr plist)))))
        (apply (r6:record-constructor
                (r6:make-record-constructor-descriptor type #f #f))
               (map (lambda (name)
                      (let find ((plist field-plist))
                        (cond ((null? plist)
                               (error "make-condition: no value for field" name type))
                              ((eq? (car plist) name) (cadr plist))
                              (else (find (cddr plist))))))
                    names))))

    (define (condition-has-type? c type)
      (check-type 'condition-has-type? type)
      (and ((r6:condition-predicate type) c) #t))

    ;; The value of field NAME in simple condition S, or FAIL if S's type
    ;; has no such field.
    (define (simple-ref s name fail)
      (let loop ((rtd (r6:record-rtd s)))
        (if (not rtd)
            fail
            (let search ((fields (vector->list (r6:record-type-field-names rtd)))
                         (k 0))
              (cond ((null? fields) (loop (r6:record-type-parent rtd)))
                    ((eq? (car fields) name) ((r6:record-accessor rtd k) s))
                    (else (search (cdr fields) (+ k 1))))))))

    (define (condition-ref c name)
      (let ((fail (list 'fail)))
        (let loop ((ss (r6:simple-conditions c)))
          (if (null? ss)
              (error "condition-ref: no such field in condition" c name)
              (let ((v (simple-ref (car ss) name fail)))
                (if (eq? v fail) (loop (cdr ss)) v))))))

    (define (make-compound-condition c . cs)
      (apply r6:condition c cs))

    (define (extract-condition c type)
      (check-type 'extract-condition type)
      (let loop ((ss (r6:simple-conditions c)))
        (cond ((null? ss)
               (error "extract-condition: condition has no component of type" c type))
              ((eq? (r6:record-rtd (car ss)) type) (car ss))
              ((descends? (r6:record-rtd (car ss)) type)
               (let ((s (car ss)))
                 (apply make-condition type
                        (let collect ((names (all-field-names type)))
                          (if (null? names)
                              '()
                              (cons (car names)
                                    (cons (simple-ref s (car names) #f)
                                          (collect (cdr names)))))))))
              (else (loop (cdr ss))))))

    (define-syntax define-condition-type
      (syntax-rules ()
        ((define-condition-type type supertype predicate (field accessor) ...)
         (begin
           (define type (make-condition-type 'type supertype '(field ...)))
           (define (predicate thing)
             (and (condition? thing) (condition-has-type? thing type)))
           (define (accessor c)
             (condition-ref (extract-condition c type) 'field))
           ...))))

    (define-syntax condition
      (syntax-rules ()
        ((condition (type (field value) ...) ...)
         (make-compound-condition
          (apply make-condition type (append (list 'field value) ...))
          ...))))))
