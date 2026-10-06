;;; SRFI 150: hygienic ERR5RS record syntax (reduced).  Written for
;;; Pseudoscheme with syntax-case, on R6RS procedural records, following
;;; the design of Marc Nieper-Wißkirchen's sample implementation
;;; (reference/srfi-150/150.scm, kept for reference; MIT licence in
;;; reference/srfi-150/LICENSE).  The sample implementation isn't used:
;;; it needs SRFI 147 (custom macro transformers) and SRFI 148 (eager
;;; syntax-rules), which Pseudoscheme doesn't have.
;;;
;;; As in the sample implementation, the <type name> is bound to a macro
;;; that carries the type's fields for subtypes' definitions.  Used as an
;;; expression it evaluates to the record type's R6RS record-type
;;; descriptor, so (rnrs records inspection) works on SRFI 150 types and
;;; records.  Field names are matched as the SRFI specifies: a
;;; constructor's field name is looked up among the definition's field
;;; names, then its accessor names (bound-identifier=?), then the
;;; ancestors' field names and accessor names (free-identifier=?);
;;; constants (strings, numbers, characters, booleans) compare with
;;; equal?.  Fields a constructor doesn't initialize hold #f.
;;;
;;; define-record-type is a different binding from (scheme base)'s, so
;;; import (except (scheme base) define-record-type) with this library.
(define-library (srfi 150)
  (export define-record-type)
  (import (except (scheme base) define-record-type)
          (scheme cxr)
          (rnrs syntax-case)
          (only (rnrs records procedural) make-record-type-descriptor
                make-record-constructor-descriptor record-constructor
                record-predicate record-accessor record-mutator))
  (begin
    ;; The keyword a type name's macro answers queries with.
    (define-syntax :secret (syntax-rules ()))

    (define-syntax define-record-type
      (lambda (x)
        (syntax-case x ()
          ((_ (type-name #f) . rest)
           #'(%define-record-type type-name #f () . rest))
          ((_ (type-name parent) . rest)
           #'(parent :secret %with-parent type-name . rest))
          ((_ type-name . rest)
           #'(%define-record-type type-name #f () . rest)))))

    ;; The answer to a parent type name's query.
    (define-syntax %with-parent
      (syntax-rules ()
        ((_ parent-rtd parent-fields type-name . rest)
         (%define-record-type type-name parent-rtd parent-fields . rest))))

    ;; (%define-record-type type-name parent-rtd parent-fields
    ;;    constructor-spec predicate-spec field-spec ...)
    ;; where parent-fields is ((name accessor) ...) for all the
    ;; ancestors' fields, in index order.
    (define-syntax %define-record-type
      (lambda (x)

        (define (syntax->list s)
          (syntax-case s ()
            ((a . b) (cons #'a (syntax->list #'b)))
            (() '())))

        (define (complain message . form)
          (syntax-violation 'define-record-type message x
                            (if (pair? form) (car form) #f)))

        ;; Field-name equality, within a definition and across.
        (define (same-name? a b compare)
          (cond ((and (identifier? a) (identifier? b)) (compare a b))
                ((or (identifier? a) (identifier? b)) #f)
                (else (equal? (syntax->datum a) (syntax->datum b)))))

        (define (index-of name names compare)
          (let loop ((names names) (i 0))
            (cond ((null? names) #f)
                  ((same-name? name (car names) compare) i)
                  (else (loop (cdr names) (+ i 1))))))

        ;; A symbol to name the field in the R6RS record type.
        (define (field-symbol name i)
          (let ((d (syntax->datum name)))
            (string->symbol
             (string-append
              (cond ((symbol? d) (symbol->string d))
                    ((string? d) d)
                    ((number? d) (number->string d))
                    ((char? d) (string d))
                    (else "field"))
              (if (symbol? d) "" (string-append "-" (number->string i)))))))

        (syntax-case x ()
          ((_ type-name parent-rtd ((pname paccessor) ...)
              constructor-spec predicate-spec field-spec ...)
           (let* ((specs (map syntax->list #'(field-spec ...)))
                  (_ (for-each (lambda (spec)
                                 (unless (and (memv (length spec) '(2 3))
                                              (identifier? (cadr spec))
                                              (or (null? (cddr spec))
                                                  (identifier? (caddr spec))))
                                   (complain "bad field spec")))
                               specs))
                  (names (map car specs))
                  (accessors (map cadr specs))
                  (mutators (map (lambda (spec)
                                   (if (null? (cddr spec)) #f (caddr spec)))
                                 specs))
                  (pnames #'(pname ...))
                  (paccessors #'(paccessor ...))
                  (np (length pnames))
                  (n (+ np (length names)))
                  (indexes (let loop ((i np) (l names))
                             (if (null? l) '() (cons i (loop (+ i 1) (cdr l))))))
                  ;; The index of the field a constructor argument names.
                  (lookup
                   (lambda (f)
                     (let ((own (or (index-of f names bound-identifier=?)
                                    (index-of f accessors bound-identifier=?))))
                       (if own
                           (+ np own)
                           (or (index-of f pnames free-identifier=?)
                               (index-of f paccessors free-identifier=?)
                               (complain "record field not found" f))))))
                  (rtd (if (identifier? #'type-name)
                           (car (generate-temporaries '(rtd)))
                           (complain "bad type name" #'type-name)))
                  (maker (car (generate-temporaries '(maker)))))
             (with-syntax
                 ((rtd rtd)
                  (maker maker)
                  ((field-symbol ...)
                   (map field-symbol names indexes))
                  ((accessor-def ...)
                   (map (lambda (acc i)
                          #`(define #,acc (record-accessor #,rtd #,(- i np))))
                        accessors indexes))
                  ((mutator-def ...)
                   (let loop ((ms mutators) (is indexes))
                     (cond ((null? ms) '())
                           ((car ms)
                            (cons #`(define #,(car ms)
                                      (record-mutator #,rtd #,(- (car is) np)))
                                  (loop (cdr ms) (cdr is))))
                           (else (loop (cdr ms) (cdr is))))))
                  (predicate-def
                   (syntax-case #'predicate-spec ()
                     (#f #'(begin))
                     (pred (identifier? #'pred)
                      #`(define pred (record-predicate #,rtd)))))
                  (constructor-def
                   (syntax-case #'constructor-spec ()
                     (#f #'(begin))
                     (name (identifier? #'name)
                      #`(define name #,maker))
                     ((name f ...)
                      (identifier? #'name)
                      (let* ((fs #'(f ...))
                             (is (map lookup fs))
                             (temps (generate-temporaries fs)))
                        (let check ((is is))
                          (when (pair? is)
                            (when (memv (car is) (cdr is))
                              (complain "field initialized twice"
                                        #'constructor-spec))
                            (check (cdr is))))
                        (with-syntax
                            (((temp ...) temps)
                             ((arg ...)
                              (let loop ((i 0))
                                (if (= i n)
                                    '()
                                    (cons (let find ((is is) (ts temps))
                                            (cond ((null? is) #f)
                                                  ((= (car is) i) (car ts))
                                                  (else (find (cdr is) (cdr ts)))))
                                          (loop (+ i 1)))))))
                          #`(define (name temp ...) (#,maker arg ...)))))
                     (_ (complain "bad constructor spec"))))
                  (((name accessor) ...)
                   (map list names accessors)))
               #'(begin
                   (define rtd
                     (make-record-type-descriptor
                      'type-name parent-rtd #f #f #f
                      (vector '(mutable field-symbol) ...)))
                   (define maker
                     (record-constructor
                      (make-record-constructor-descriptor rtd #f #f)))
                   predicate-def
                   constructor-def
                   accessor-def ...
                   mutator-def ...
                   (define-syntax type-name
                     (lambda (y)
                       (syntax-case y (:secret)
                         ((_ :secret k arg (... ...))
                          #'(k rtd ((pname paccessor) ... (name accessor) ...)
                               arg (... ...)))
                         (id (identifier? #'id) #'rtd)
                         (_ (syntax-violation
                             #f "invalid use of a record type name" y))))))))))))))
