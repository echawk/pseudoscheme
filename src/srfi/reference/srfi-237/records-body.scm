;;; The body of the library (srfi :237 records) from Marc
;;; Nieper-Wißkirchen's sample implementation of SRFI 237 (records.sls
;;; here), with the changes marked PSEUDOSCHEME:
;;;   - no record-types.scm file: the uid table is in memory;
;;;   - no SRFI 213 identifier properties (define-property and the
;;;     lookup procedure), which Pseudoscheme's psyntax lacks;
;;;   - two bugs fixed: make-record-type-descriptor with a #f parent,
;;;     and record-mutator, which returned an accessor.
;;; Its *end* variable, which marked the end of record-types.scm, is gone.

  ;; PSEUDOSCHEME: the table of record types by uid is kept in memory only.
  ;; The sample implementation loads it from, and appends each new
  ;; record type with a uid to, a file record-types.scm in the current
  ;; directory, which it requires to exist.
  (define *rtd-table* (make-hashtable symbol-hash symbol=?))

  (define register-rtd!
    (lambda (rtd)
      (define record-type-fields
        (lambda (rtd)
          (define names (record-type-field-names rtd))
          (define n (vector-length names))
          (do ([fields (make-vector n)]
               [i 0 (fx+ i 1)])
              ((fx=? i n)
               fields)
            (vector-set! fields
                         i
                         (list (if (record-field-mutable? rtd i)
                                   'mutable
                                   'immutable)
                               (vector-ref names i))))))
      (assert (rnrs:record-type-descriptor? rtd))
      (let ([uid (rnrs:record-type-uid rtd)])  ; PSEUDOSCHEME: no file
        (when (and uid (not (hashtable-ref *rtd-table* uid #f)))
          (hashtable-set! *rtd-table* uid rtd)))
      rtd))

  (define record-uid->rtd
    (lambda (uid)
      (define who 'record-uid->rtd)
      (unless (symbol? uid)
        (assertion-violation who "not a symbol" uid))
      (hashtable-ref *rtd-table* uid #f)))

  (define-syntax generative
    (lambda (stx)
      (syntax-violation 'generative "invalid use of auxiliary syntax" stx)))

  (rnrs:define-record-type rd
    (nongenerative) (opaque #t) (sealed #t)
    (fields rtd cd parent))

  (define record-type-descriptor?
    (lambda (obj)
      (or (rnrs:record-type-descriptor? obj)
	  (rd? obj))))

  (define record-descriptor?
    (lambda (obj)
      (rd? obj)))

  (define record-constructor-descriptor?
    (lambda (obj)
      (record-descriptor? obj)))

  (define rnrs:rtd
    (lambda (rtd)
      (unless (record-type-descriptor? rtd)
	(assertion-violation #f "not a record-type descriptor" rtd))
      (if (rnrs:record-type-descriptor? rtd)
	  rtd
	  (rd-rtd rtd))))

  (define rnrs:rtd*
    (lambda (rtd)
      (and rtd (rnrs:rtd rtd))))

  (define rnrs:cd
    (lambda (rd)
      (and rd
	   (begin
	     (unless (record-descriptor? rd)
	       (assertion-violation #f "not a record descriptor" rd))
	     (rd-cd rd)))))

  (define make-record-type-descriptor
    (lambda (name parent uid sealed? opaque? fields)
      (register-rtd!
        ;; PSEUDOSCHEME: was (rnrs:rtd parent), which rejects a #f parent.
        (rnrs:make-record-type-descriptor name (rnrs:rtd* parent) uid sealed? opaque? fields))))

  (define make-record-descriptor
    (case-lambda
     [(rtd parent-descriptor protocol)
      (make-rd (rnrs:rtd rtd)
	       (rnrs:make-record-constructor-descriptor (rnrs:rtd rtd) (rnrs:cd parent-descriptor) protocol)
	       parent-descriptor)]
     [(name parent uid sealed? opaque? fields protocol)
      (make-record-descriptor (make-record-type-descriptor name parent uid sealed? opaque? fields)
			      parent protocol)]))

  (define make-record-constructor-descriptor
    (lambda (rtd parent-descriptor protocol)
      (make-record-descriptor rtd parent-descriptor protocol)))

  (define record-descriptor-rtd
    (lambda (rd)
      (define who 'record-descriptor-rtd)
      (unless (record-descriptor? rd)
	(assertion-violation who "not a record descriptor" rd))
      (rd-rtd rd)))

  (define record-descriptor-parent
    (lambda (rd)
      (define who 'record-descriptor-rtd)
      (unless (record-descriptor? rd)
	(assertion-violation who "not a record descriptor" rd))
      (rd-parent rd)))

  (define record-constructor
    (lambda (rd)
      (define who 'record-constructor)
      (unless (record-descriptor? rd)
	(assertion-violation who "not a record descriptor" rd))
      (rnrs:record-constructor (rd-cd rd))))

  (define record-predicate
    (lambda (rtd)
      (rnrs:record-predicate (rnrs:rtd rtd))))

  (define record-accessor
    (lambda (rtd k)
      (rnrs:record-accessor (rnrs:rtd rtd) k)))

  (define record-mutator
    (lambda (rtd k)
      ;; PSEUDOSCHEME: was rnrs:record-accessor.
      (rnrs:record-mutator (rnrs:rtd rtd) k)))

  (define record?
    (lambda (obj)
      (rnrs:record? obj)))

  (define record-rtd
    (lambda (record)
      (rnrs:record-rtd record)))

  (define record-type-name
    (lambda (rtd)
      (rnrs:record-type-name (rnrs:rtd rtd))))

  (define record-type-parent
    (lambda (rtd)
      (rnrs:record-type-parent (rnrs:rtd rtd))))

  (define record-type-uid
    (lambda (rtd)
      (rnrs:record-type-uid (rnrs:rtd rtd))))

  (define record-type-generative?
    (lambda (rtd)
      (rnrs:record-type-generative? (rnrs:rtd rtd))))

  (define record-type-sealed?
    (lambda (rtd)
      (rnrs:record-type-sealed? (rnrs:rtd rtd))))

  (define record-type-opaque?
    (lambda (rtd)
      (rnrs:record-type-opaque? (rnrs:rtd rtd))))

  (define record-type-field-names
    (lambda (rtd)
      (rnrs:record-type-field-names (rnrs:rtd rtd))))

  (define record-field-mutable?
    (lambda (rtd k)
      (rnrs:record-field-mutable? (rnrs:rtd rtd) k)))

  (define assert-protocol
    (lambda (obj)
      (unless (procedure? obj)
        (assertion-violation #f "protocol must be a procedure" obj))
      obj))

  (define-syntax define-record-name
    (lambda (stx)
      (define who 'define-record-name)
      (define parse-constructor-clauses
        (lambda (record-name clauses)
          (let f ([clauses clauses] [parent* '()] [protocol* '()])
            (if (null? clauses)
                (list (if (pair? parent*)
                          (car parent*)
			  (with-syntax ([record-name record-name])
			    #'(record-descriptor-parent record-name)))
                      (if (pair? protocol*)
                          (with-syntax ([expr (car protocol*)])
                            #'(assert-protocol expr))
                          #f))
                (let ([clause (car clauses)] [clauses (cdr clauses)])
                  (syntax-case clause (parent protocol)
                    [(parent name)
                     (identifier? #'name)
                     (if (null? parent*)
                         (f clauses (list #'name) protocol*)
                         (syntax-violation "duplicate parent clause" stx clause))]
                    [(protocol expr)
                     (if (null? protocol*)
                         (f clauses parent* (list #'expr))
                         (syntax-violation "duplicate protocol clause" stx clause))]
                    [_ (syntax-violation who "invalid record clause" stx clause)]))))))
      (syntax-case stx ()
	[(k (record-name record-type) record-clause ...)
	 (and (identifier? #'record-name)
	      (identifier? #'record-type))
	 (with-syntax ([constructor-name
			(datum->syntax #'k (string->symbol (string-append "make-" (symbol->string (syntax->datum #'record-name)))))])
	   #'(define-record-name (record-name record-type constructor-name) record-clause ...))]
        [(_ (record-name record-type constructor-name) record-clause ...)
         (and (identifier? #'record-name)
	      (identifier? #'record-type)
              (identifier? #'constructor-name))
         (with-syntax ([(parent protocol)
                        (parse-constructor-clauses #'record-type #'(record-clause ...))])
           #'(begin
	       (define rd
                 (make-record-descriptor record-type parent protocol))
               (define-rn record-name rd)
	       (define constructor-name (record-constructor record-name))))]
        [_
         (syntax-violation who "invalid record name definition" stx)])))

  (define-syntax define-record-type
    (lambda (stx)
      (define who 'define-record-type)
      (define update-name-spec
	(lambda (spec)
	  (syntax-case spec ()
	    [name
	     (identifier? #'name)
	     (let ([name (syntax->datum #'name)])
	       (list spec
                     spec
		     (list (datum->syntax #'* name)
			   (datum->syntax spec (string->symbol (string-append "make-" (symbol->string name))))
			   (datum->syntax spec (string->symbol (string-append (symbol->string name) "?"))))))]
            [(rtd-name record-name)
	     (and (identifier? #'rtd-name)
                  (identifier? #'record-name))
	     (let ([name (syntax->datum #'rtd-name)])
	       (list #'rtd-name
                     #'record-name
		     (list (datum->syntax #'* name)
			   (datum->syntax #'rtd-name (string->symbol (string-append "make-" (symbol->string name))))
			   (datum->syntax #'rtd-name (string->symbol (string-append (symbol->string name) "?"))))))]
	    [(name constructor-name predicate-name)
	     (and (identifier? #'name)
		  (identifier? #'constructor-name)
		  (identifier? #'predicate-name))
	     (list #'name
                   #'name
		   (list (datum->syntax #'* (syntax->datum #'name))
			 #'constructor-name
			 #'predicate-name))]
            [(rtd-name record-name constructor-name predicate-name)
             (and (identifier? #'rtd-name)
                  (identifier? #'record-name)
                  (identifier? #'constructor-name)
                  (identifier? #'predicate-name))
             (list #'rtd-name
                   #'record-name
                   (list (datum->syntax #'* (syntax->datum #'rtd-name))
                         #'constructor-name
                         #'predicate-name))]
	    [_
	     (syntax-violation who "invalid name spec" stx spec)])))
      ;; PSEUDOSCHEME: was (lambda (lookup) ...), a SRFI 213 transformer;
      ;; Pseudoscheme has no identifier properties, so lookup finds none,
      ;; and a parent clause always becomes a parent-rtd clause.
      (let ([lookup (lambda (id key) #f)])
	(define update-record-clauses
	  (lambda (k prefix clauses)
	    (define update-field-spec
	      (lambda (field-spec)
		(syntax-case field-spec (immutable mutable)
		  [(immutable field-name)
		   (identifier? #'field-name)
		   (let ([name (symbol->string (syntax->datum #'field-name))])
		     (with-syntax ([accessor-name
				    (datum->syntax k (string->symbol (string-append prefix name)))])
		       #'(immutable field-name accessor-name)))]
		  [(mutable field-name)
		   (identifier? #'field-name)
		   (let ([name (symbol->string (syntax->datum #'field-name))])
		     (with-syntax ([accessor-name
				    (datum->syntax k (string->symbol (string-append prefix "-" name)))]
				   [mutator-name
				    (datum->syntax k (string->symbol (string-append prefix "-" name "-set!")))])
		       #'(mutable field-name accessor-name mutator-name)))]
		  [field-name
		   (identifier? #'field-name)
		   (let ([name (symbol->string (syntax->datum #'field-name))])
		     (with-syntax ([accessor-name
				    (datum->syntax k (string->symbol (string-append prefix "-" name)))])
		       #'(immutable field-name accessor-name)))]
		  [_ field-spec])))
            (let f ([clauses clauses] [transformed '()] [gen #f])
              (if (null? clauses)
                  transformed
                  (let ([clause (car clauses)] [clauses (cdr clauses)])
                    (define g
                      (lambda (clause)
                        (f clauses (cons clause transformed) gen)))
                    (syntax-case clause (fields parent parent-rtd nongenerative generative)
	              [(fields field-spec ...)
	               (with-syntax ([(field-spec ...) (map update-field-spec #'(field-spec ...))])
		         (g #'((fields field-spec ...))))]
	              [(parent name)
	               (identifier? #'name)
	               (g (cond
		           [(lookup #'name #'rnrs:record-name) =>
		            (lambda (record-name)
		              (with-syntax ([record-name record-name])
		                #'((define parent-rd name)
			           (parent record-name))))]
		           [else
		            #'((define parent-rd name)
		               (parent-rtd (rnrs:rtd name) (rnrs:cd parent-rd)))]))]
	              [(parent-rtd rtd-expr cd-expr)
	               (g #'((define parent-rd cd-expr)
		             (parent-rtd (rnrs:rtd* rtd-expr) (rnrs:cd parent-rd))))]
                      [(nongenerative . args)
                       (if (eq? gen 'generative)
                           (syntax-violation who "nongenerative and generative clauses are mutally exclusive" stx clause)
                           (f clauses (cons (list clause) transformed) 'nongenerative))]
                      [(generative)
                       (case gen
                         [(generative)
                          (syntax-violation who "duplicate generative clause" stx clause)]
                         [(nongenerative)
                          (syntax-violation who "nongenerative and generative clauses are mutally exclusive" stx clause)]
                         [else
                          (f clauses transformed 'generative)])]
	              [clause
	               (g #'(clause))]))))))
	(syntax-case stx ()
          [(_ name-spec record-clause ...)
	   (with-syntax ([(k record-name name-spec) (update-name-spec #'name-spec)])
	     (define prefix (symbol->string (syntax->datum #'record-name)))
	     (with-syntax ([((def ... record-clause) ...)
                            (update-record-clauses #'k prefix #'(record-clause ...))])
	       #'(define-record-type-aux record-name (def ... ...) parent-rd name-spec record-clause ...)))]
          [_
           (syntax-violation who "invalid record-type definition" stx)]))))

  (define rnrs:record-name)

  (define-syntax define-rn
    (syntax-rules ()
      [(define-rn name rd)
       (define-syntax name
	 (lambda (stx)
	   (syntax-case stx ()
	     [k
	      (identifier? #'k)
	      #'rd]
	     [_
	      (syntax-violation 'record-name "invalid use of record name" stx)])))]))

  (define-syntax define-record-type-aux
    (syntax-rules ()
      [(_ record-name () parent-rd (tmp-name constructor-name predicate-name) record-clause ...)
       (define-record-type-aux record-name ((define parent-rd #f)) parent-rd (tmp-name constructor-name predicate-name) record-clause ...)]
      [(_ record-name (def ...) parent-rd (tmp-name constructor-name predicate-name) record-clause ...)
       (begin
	 def ...
	 (rnrs:define-record-type (tmp-name constructor-name predicate-name) record-clause ...)
	 (define rtd (register-rtd! (rnrs:record-type-descriptor tmp-name)))
	 (define cd (rnrs:record-constructor-descriptor tmp-name))
	 (define rd (make-rd rtd cd parent-rd))
         (define-rn record-name rd))]))  ; PSEUDOSCHEME: no define-property

  (define-syntax record-type-descriptor
    (lambda (stx)
      (define who 'record-type-descriptor)
      (syntax-case stx ()
	[(_ record-name)
	 (identifier? #'record-name)
	 #'(record-descriptor-rtd record-name)]
	[_
	 (syntax-violation who "invalid syntax" stx)])))

  (define-syntax record-constructor-descriptor
    (lambda (stx)
      (define who 'record-constructor-descriptor)
      (syntax-case stx ()
	[(_ record-name)
	 (identifier? #'record-name)
	 #'(values record-name)]
	[_
	 (syntax-violation who "invalid syntax" stx)])))
