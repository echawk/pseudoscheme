;;; The body of the library (srfi :240 define-record-type), from Marc
;;; Nieper-Wißkirchen's sample implementation of SRFI 240
;;; (define-record-type.sls here), unmodified.

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
