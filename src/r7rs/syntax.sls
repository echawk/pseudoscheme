;;; -*- Mode: Scheme -*-
;;;; R7RS-small syntax that R6RS lacks or spells differently, for psyntax.
;;;;
;;;; The (scheme ...) libraries that src/r7rs/front.lisp generates take
;;;; these from here and everything else from (rnrs ...) or the host.
;;;; (pseudoscheme host) is the library of host procedures front.lisp
;;;; installs at boot; it is imported with a % prefix.

(library (pseudoscheme r7rs syntax)
  (export define-record-type parameterize define-values case cond-expand
          syntax-error delay-force include include-ci)
  (import (except (rnrs) define-record-type case)
          (prefix (pseudoscheme host) %))

  ;; (define-record-type <name> (<constructor> <field> ...) <predicate>
  ;;   (<field> <accessor> [<modifier>]) ...)       R7RS 5.5
  ;; <constructor> may also be a bare identifier (all fields, in order)
  ;; or #f (none), as SRFI 131/136 and most implementations allow.
  (define-syntax define-record-type
    (lambda (x)
      (define (field-index field fields)
        (let loop ((fs fields) (i 0))
          (cond ((null? fs) (syntax-violation 'define-record-type "unknown field" field))
                ((bound-identifier=? field (car fs)) i)
                (else (loop (cdr fs) (+ i 1))))))
      (syntax-case x ()
        ((_ type ctor-spec pred (field . accessors) ...)
         (let* ((fields #'(field ...))
                (ctor-spec #'ctor-spec))
           (with-syntax (((rtd) (generate-temporaries '(rtd))))
           (with-syntax
               (((field-spec ...) (map (lambda (f) #`(mutable #,f)) fields))
                (ctor-def
                 (syntax-case ctor-spec ()
                   (#f #'(begin))
                   ((ctor cfield ...)
                    (let ((cfields #'(cfield ...)))
                      (for-each (lambda (c) (field-index c fields)) cfields)
                      (with-syntax
                          (((arg ...)
                            ;; for each field, in order: its constructor
                            ;; argument, or #f if the constructor omits it
                            (map (lambda (f)
                                   (let loop ((cs cfields))
                                     (cond ((null? cs) #f)
                                           ((bound-identifier=? (car cs) f) (car cs))
                                           (else (loop (cdr cs))))))
                                 fields)))
                        #'(define ctor
                            (record-constructor
                             (make-record-constructor-descriptor
                              rtd #f
                              (lambda (p) (lambda (cfield ...) (p arg ...)))))))))
                   (ctor
                    (identifier? #'ctor)
                    #'(define ctor
                        (record-constructor
                         (make-record-constructor-descriptor rtd #f #f))))))
                ((accessor-def ...)
                 (apply append
                        (map (lambda (f acc*)
                               (let ((i (field-index f fields)))
                                 (syntax-case acc* ()
                                   (() '())
                                   ((acc) (list #`(define acc (record-accessor rtd #,i))))
                                   ((acc mod)
                                    (list #`(define acc (record-accessor rtd #,i))
                                          #`(define mod (record-mutator rtd #,i)))))))
                             fields
                             #'(accessors ...)))))
             #'(begin
                 (define rtd
                   ;; (vector ...), not '#(field-spec ...): psyntax 2007
                   ;; doesn't expand ellipses in vector templates.
                   (make-record-type-descriptor 'type #f #f #f #f (vector 'field-spec ...)))
                 (define type rtd)
                 ctor-def
                 (define pred (record-predicate rtd))
                 accessor-def ...))))))))

  ;; (parameterize ((param value) ...) body ...)   R7RS 4.2.6; parameter
  ;; objects are the host's (make-parameter), which apply converters.
  (define-syntax parameterize
    (syntax-rules ()
      ((_ () body1 body2 ...)
       (let () body1 body2 ...))
      ((_ ((param value) ...) body1 body2 ...)
       (%%parameterize (list param ...) (list value ...)
                       (lambda () body1 body2 ...)))))

  ;; (define-values <formals> <expression>)   R7RS 5.3.3
  (define-syntax define-values
    (lambda (x)
      (define (formals->ids f)
        (syntax-case f ()
          (() '())
          ((a . d) (cons #'a (formals->ids #'d)))
          (r (list #'r))))
      (define (rebuild f tmps)
        (syntax-case f ()
          (() '())
          ((a . d) (cons (car tmps) (rebuild #'d (cdr tmps))))
          (r (car tmps))))
      (syntax-case x ()
        ((_ formals expr)
         (let* ((ids (formals->ids #'formals))
                (tmps (generate-temporaries ids)))
           (with-syntax (((id ...) ids)
                         ((tmp ...) tmps)
                         (tformals (rebuild #'formals tmps))
                         ((dummy) (generate-temporaries '(dummy))))
             #'(begin
                 (define id #f) ...
                 (define dummy
                   (call-with-values (lambda () expr)
                     (lambda tformals (set! id tmp) ... #f))))))))))

  ;; case with => clauses (R7RS 4.2.1)
  (define-syntax case
    (lambda (x)
      (syntax-case x (else =>)
        ((_ key clause ...)
         (with-syntax (((k) (generate-temporaries '(k))))
           (with-syntax
               ((body
                 (let loop ((cs #'(clause ...)))
                   (if (null? cs)
                       #'(if #f #f)
                       (syntax-case (car cs) (else =>)
                         ((else => proc) #'(proc k))
                         ((else e1 e2 ...) #'(begin e1 e2 ...))
                         (((d ...) => proc)
                          (with-syntax ((rest (loop (cdr cs))))
                            #'(if (memv k '(d ...)) (proc k) rest)))
                         (((d ...) e1 e2 ...)
                          (with-syntax ((rest (loop (cdr cs))))
                            #'(if (memv k '(d ...)) (begin e1 e2 ...) rest))))))))
             #'(let ((k key)) body)))))))

  ;; cond-expand (R7RS 4.2.1): requirements are checked at expansion time
  ;; against the host's feature list and available libraries.
  (define-syntax cond-expand
    (lambda (x)
      (syntax-case x ()
        ((_ clause ...)
         (let loop ((cs #'(clause ...)))
           (if (null? cs)
               (syntax-violation 'cond-expand "no clause applies" (syntax->datum x))
               (syntax-case (car cs) ()
                 ((req body ...)
                  (if (%cond-expand-satisfied? (syntax->datum #'req))
                      #'(begin body ...)
                      (loop (cdr cs)))))))))))

  ;; (syntax-error message arg ...): fail at expansion (R7RS 4.3.3)
  (define-syntax syntax-error
    (lambda (x)
      (syntax-case x ()
        ((_ message arg ...)
         (syntax-violation #f (syntax->datum #'message) (syntax->datum #'(arg ...)))))))

  (define-syntax delay-force
    (syntax-rules ()
      ((_ expression) (%$delay-force (lambda () expression)))))

  ;; include / include-ci (R7RS 4.1.7): the forms of the files, read
  ;; now, in the context of the include form.
  (define-syntax include
    (lambda (x)
      (syntax-case x ()
        ((k file ...)
         (with-syntax (((form ...)
                        (datum->syntax #'k
                          (apply append
                                 (map (lambda (f) (%read-file-forms f #f))
                                      (syntax->datum #'(file ...)))))))
           #'(begin form ...))))))

  (define-syntax include-ci
    (lambda (x)
      (syntax-case x ()
        ((k file ...)
         (with-syntax (((form ...)
                        (datum->syntax #'k
                          (apply append
                                 (map (lambda (f) (%read-file-forms f #t))
                                      (syntax->datum #'(file ...)))))))
           #'(begin form ...)))))))
