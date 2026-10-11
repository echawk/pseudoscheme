;;; -*- Mode: Scheme -*-
;;;; (chezscheme): Chez Scheme's older records -- define-record,
;;;; type-descriptor and make-record-type -- on R6RS's records.  Part of
;;;; the library's body (src/chez/chez.lisp).
;;;;
;;;;   (define-record name [parent] (field ...) [((field init) ...)] [(option ...)])
;;;;
;;;; A field is name, (name), (mutable|immutable name), (type name) or
;;;; (mutable|immutable type name); fields are mutable unless said
;;;; immutable.  The second list's fields aren't constructor arguments:
;;;; each init is evaluated when a record is made, in the scope of the
;;;; first list's fields.  Options: (prefix "string"), (predicate id),
;;;; (constructor id).  define-record defines make-name, name?, name-field
;;;; and, for each mutable field, set-name-field!; name is the record type
;;;; descriptor ((type-descriptor name)).  A parent's constructor
;;;; arguments come first.

;; How a define-record type's records are made: per type, its own
;; constructor arguments' count and a procedure of them giving the
;; values of all its own fields (those with inits too).
(define record-makers (make-eq-hashtable))

;; (parent-args ... own-args ...) => every field's value, root first
(define (record-all-fields rtd args)
  (define (own-count rtd)
    (let ((maker (hashtable-ref record-makers rtd #f)))
      (if maker (car maker) (vector-length (record-type-field-names rtd)))))
  (define (count rtd)
    (if rtd (+ (own-count rtd) (count (record-type-parent rtd))) 0))
  (unless (= (length args) (count rtd))
    (assertion-violation (record-type-name rtd) "incorrect number of arguments" args))
  (let loop ((rtd rtd) (args args))
    (let* ((parent (record-type-parent rtd))
           (parent-n (count parent))
           (maker (hashtable-ref record-makers rtd #f))
           (own-args (list-tail args parent-n)))
      (append (if parent (loop parent (list-head args parent-n)) '())
              (if maker (apply (cdr maker) own-args) own-args)))))

(define (record-maker rtd)
  (let ((make (%chez:record-constructor rtd)))
    (lambda args (apply make (record-all-fields rtd args)))))

(define csv7:record-field-accessor %chez:csv7-record-field-accessor)
(define csv7:record-field-mutator %chez:csv7-record-field-mutator)
(define csv7:record-field-accessible? %chez:csv7-record-field-accessible?)
(define csv7:record-field-mutable? %chez:csv7-record-field-mutable?)
(define csv7:record-type-field-names %chez:csv7-record-type-field-names)
(define csv7:record-type-field-decls %chez:csv7-record-type-field-decls)

(define-syntax define-record
  (lambda (x)
    (define (field-parts f)
      ;; => (name mutable? type)
      (syntax-case f (mutable immutable)
        (id (identifier? #'id) (list #'id #t #f))
        ((id) (identifier? #'id) (list #'id #t #f))
        ((mutable id) (identifier? #'id) (list #'id #t #f))
        ((immutable id) (identifier? #'id) (list #'id #f #f))
        ((mutable type id) (identifier? #'id) (list #'id #t (syntax->datum #'type)))
        ((immutable type id) (identifier? #'id) (list #'id #f (syntax->datum #'type)))
        ((type id) (identifier? #'id) (list #'id #t (syntax->datum #'type)))))
    (define (init-parts f)
      ;; (field-spec init) => (name mutable? type init)
      (syntax-case f ()
        ((spec init) (append (field-parts #'spec) (list #'init)))))
    (define (id-of template . parts)
      (datum->syntax template
        (string->symbol
          (apply string-append
                 (map (lambda (p) (if (string? p) p (symbol->string (syntax->datum p)))) parts)))))
    (define (parse name parent fields inits options)
      (let* ((fs (map field-parts fields))
             (is (map init-parts inits))
             (all (append fs (map (lambda (i) (list (car i) (cadr i) (caddr i))) is)))
             (opts (map (lambda (o)
                          (syntax-case o ()
                            ((k v) (identifier? #'k) (cons (syntax->datum #'k) #'v))))
                        options))
             (prefix (let ((p (assq 'prefix opts)))
                       (if p
                           (syntax->datum (cdr p))
                           (string-append (symbol->string (syntax->datum name)) "-"))))
             (maker (let ((c (assq 'constructor opts))) (if c (cdr c) (id-of name "make-" name))))
             (pred (let ((p (assq 'predicate opts))) (if p (cdr p) (id-of name name "?")))))
        (with-syntax ((name name)
                      (parent-rtd (or parent #'#f))
                      (maker maker)
                      (pred pred)
                      ((field ...) (map car fs))
                      ((init-field ...) (map car is))
                      ((init ...) (map cadddr is))
                      ((spec ...) (map (lambda (f) (list (if (cadr f) 'mutable 'immutable)
                                                         (syntax->datum (car f))))
                                       all))
                      ((type ...) (map caddr all))
                      (((accessor fname) ...)
                       (map (lambda (f) (list (id-of name prefix (car f)) (car f))) all))
                      (((mutator mname) ...)
                       (map (lambda (f) (list (id-of name "set-" prefix (car f) "!") (car f)))
                            (filter cadr all))))
          #'(begin
              (define name
                (let ((rtd (make-record-type-descriptor 'name parent-rtd #f #f #f '#(spec ...))))
                  (%chez:set-record-field-types! rtd '(type ...))
                  (hashtable-set! record-makers rtd
                    (cons (length '(field ...))
                          (lambda (field ...)
                            (let* ((init-field init) ...) (list field ... init-field ...)))))
                  rtd))
              (define maker (record-maker name))
              (define pred (record-predicate name))
              (define accessor (%chez:csv7-record-field-accessor name 'fname)) ...
              (define mutator (%chez:csv7-record-field-mutator name 'mname)) ...))))
    (syntax-case x ()
      ((_ name (field ...))
       (identifier? #'name)
       (parse #'name #f #'(field ...) '() '()))
      ((_ name (field ...) (init ...))
       (identifier? #'name)
       (parse #'name #f #'(field ...) #'(init ...) '()))
      ((_ name (field ...) (init ...) (option ...))
       (identifier? #'name)
       (parse #'name #f #'(field ...) #'(init ...) #'(option ...)))
      ((_ name parent (field ...))
       (and (identifier? #'name) (identifier? #'parent))
       (parse #'name #'parent #'(field ...) '() '()))
      ((_ name parent (field ...) (init ...))
       (and (identifier? #'name) (identifier? #'parent))
       (parse #'name #'parent #'(field ...) #'(init ...) '()))
      ((_ name parent (field ...) (init ...) (option ...))
       (and (identifier? #'name) (identifier? #'parent))
       (parse #'name #'parent #'(field ...) #'(init ...) #'(option ...))))))

(define-syntax type-descriptor
  (syntax-rules ()
    ((_ name)
     (let ((rtd name))
       (if (record-type-descriptor? rtd)
           rtd
           (assertion-violation 'type-descriptor "not a record type" 'name))))))

;; (make-record-type [parent] name fields): a record type descriptor,
;; the fields as define-record's: a symbol or (mutable|immutable [type] name)
(define make-record-type
  (case-lambda
    ((name fields) (make-record-type #f name fields))
    ((parent name fields)
     (define (bad) (assertion-violation 'make-record-type "invalid field list" fields))
     (unless (list? fields) (bad))
     (let* ((specs (map (lambda (f)
                          ;; => (mutable? type name)
                          (cond ((symbol? f) (list #t #f f))
                                ((not (and (list? f) (andmap symbol? f))) (bad))
                                ((and (= (length f) 2) (memq (car f) '(mutable immutable)))
                                 (list (eq? (car f) 'mutable) #f (cadr f)))
                                ((and (= (length f) 3) (memq (car f) '(mutable immutable)))
                                 (list (eq? (car f) 'mutable) (cadr f) (caddr f)))
                                ((= (length f) 2) (list #t (car f) (cadr f)))
                                ((= (length f) 1) (list #t #f (car f)))
                                (else (bad))))
                        fields))
            (rtd (make-record-type-descriptor
                   (if (string? name) (string->symbol name) name)
                   parent #f #f #f
                   (list->vector
                     (map (lambda (s) (list (if (car s) 'mutable 'immutable) (caddr s))) specs)))))
       (%chez:set-record-field-types! rtd (map cadr specs))
       rtd))))

;; equal? and equal-hash on records: host.lisp
(define record-type-equal-procedure %chez:record-type-equal-procedure)
(define record-type-hash-procedure %chez:record-type-hash-procedure)
(define record-equal-procedure %chez:record-equal-procedure)
(define record-hash-procedure %chez:record-hash-procedure)
(define (procedure-or-false who)
  (lambda (x)
    (unless (or (not x) (procedure? x))
      (assertion-violation who "not a procedure or #f" x))
    x))
(define default-record-equal-procedure
  (make-parameter #f (procedure-or-false 'default-record-equal-procedure)))
(define default-record-hash-procedure
  (make-parameter #f (procedure-or-false 'default-record-hash-procedure)))
(%chez:set-record-procedure-defaults! default-record-equal-procedure default-record-hash-procedure)

(define record-constructor-descriptor? %chez:record-constructor-descriptor?)
;; R6RS's own: of a constructor descriptor only, and record? of one argument
(define (r6rs:record-constructor rcd)
  (unless (record-constructor-descriptor? rcd)
    (assertion-violation 'record-constructor "not a record constructor descriptor" rcd))
  (record-constructor rcd))
(define (r6rs:record? x) (record? x))

;; #[name field ...]: host.lisp
(define record-reader %chez:record-reader)
