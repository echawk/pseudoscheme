;;; A small stand-in for Racket CS's rumble layer (and the bits of its
;;; io and thread layers a linklet calls): enough of Racket's primitives,
;;; with Racket's behavior, to run linklets schemify produced for Chez
;;; Scheme on Pseudoscheme's Chez mode.  Written for Pseudoscheme (not
;;; derived from Racket's source).

(library (racket-lite)
  (export |#%name| |#%app| |#%nongenerative-uid|
          make-record-type-descriptor make-struct-type-install-properties
          make-struct-type make-struct-field-accessor make-struct-field-mutator
          make-struct-type-property struct-type-property-accessor
          prop:procedure prop:authentic prop:equal+hash prop:custom-write
          prop:object-name prop:checked-procedure prop:impersonator-of
          current-inspector
          hash hasheqv hasheq make-hash make-hasheq make-hasheqv make-weak-hash make-weak-hasheq
          hash? hash-ref hash-set hash-set! hash-remove hash-remove! hash-count
          hash-iterate-first hash-iterate-next hash-iterate-key hash-iterate-value
          hash-for-each hash-map hash->list
          bytes? bytes make-bytes bytes-length bytes-ref bytes-set! subbytes bytes-append
          bytes->list list->bytes bytes-copy bytes->immutable-bytes byte?
          bytes->string/utf-8 string->bytes/utf-8 bytes->string/latin-1 string->bytes/latin-1
          bytes-utf-8-length string-utf-8-length bytes=?
          unsafe-bytes-ref unsafe-bytes-set! unsafe-bytes-length unsafe-bytes-copy!
          unsafe-car unsafe-cdr unsafe-vector-ref unsafe-vector-set! unsafe-vector-length
          unsafe-fx+ unsafe-fx- unsafe-fx* unsafe-fx< unsafe-fx> unsafe-fx<= unsafe-fx>= unsafe-fx=
          unsafe-undefined unsafe-make-place-local unsafe-place-local-ref unsafe-place-local-set!
          raise-argument-error raise-argument-error* raise-arguments-error raise-arguments-error*
          raise-range-error raise-range-error* raise-result-error error-print-width
          error-value->string-handler
          arithmetic-shift exact-nonnegative-integer? exact-integer? list* vector-copy!
          null void keyword? keyword->string arity-at-least arity-at-least? arity-at-least-value
          procedure-arity-includes? procedure-arity object-name
          make-continuation-prompt-tag default-continuation-prompt-tag
          call-with-continuation-prompt abort-current-continuation
          unquoted-printing-string string->immutable-string symbol->immutable-string
          char-grapheme-step path? path->string path->bytes
          eof input-port? output-port? progress-evt? peek-byte read-byte
          peek-bytes-avail! peek-bytes-avail!* read-bytes! write-bytes write-string
          make-input-port open-input-bytes
          format make-parameter apply-string-append apply-bytes-append begin0
          char-general-category
          structure-type-lookup-prefab-uid struct-type? $value impersonator? impersonator-val
          string->keyword keyword<? 
          syntax? syntax-e syntax-property syntax-source syntax-line syntax-column
          syntax-position syntax-span datum->syntax syntax->datum primitive-table
          with-continuation-mark* with-continuation-mark continuation-mark-set-first
          exception-handler-key parameterization-key push-authentic extend-parameterization
          current-continuation-marks continuation-mark-set->list
          unsafe-fxrshift unsafe-fxlshift unsafe-fxand unsafe-fxior unsafe-fxxor unsafe-fxnot unsafe-fxquotient unsafe-fxremainder unsafe-fxmodulo unsafe-fxmin unsafe-fxmax unsafe-fxabs unsafe-fx->fl unsafe-fl+ unsafe-fl- unsafe-fl* unsafe-fl/ unsafe-fl< unsafe-fl> unsafe-fl= unsafe-fl<= unsafe-fl>= unsafe-string-ref unsafe-string-length unsafe-string-set! unsafe-unbox unsafe-set-box! unsafe-unbox* unsafe-set-box*! unsafe-vector*-ref unsafe-vector*-set! unsafe-vector*-length unsafe-char=? unsafe-char<? unsafe-char->integer unsafe-cons-list unsafe-list-ref unsafe-list-tail unsafe-fxquotient/remainder
          unsafe-struct-ref unsafe-struct-set! unsafe-struct*-ref unsafe-struct*-set!
          symbol-interned?
          hash-iterate-key+value hash-iterate-pair hash-keys hash-values hash-empty? hash-update
          make-internal-variable variable-ref/no-check variable-ref variable-set! variable-set!/define call-with-module-prompt make-instance-variable variable? raise-binding-result-arity-error |#%app/no-return| |#%app/value|
          instance-variable-value)
  (import (except (chezscheme) make-record-type-descriptor format make-parameter void
                  char-general-category datum->syntax syntax->datum
                  with-continuation-mark current-continuation-marks continuation-mark-set-first
                  continuation-mark-set->list
                  vector-copy! string->immutable-string input-port? output-port?)
          (rename (only (chezscheme) make-record-type-descriptor format make-parameter char-general-category
                        input-port? output-port?)
                  (make-record-type-descriptor chez:make-record-type-descriptor)
                  (char-general-category chez:char-general-category)
                  (format chez:format)
                  (make-parameter chez:make-parameter)
                  (input-port? chez:input-port?)
                  (output-port? chez:output-port?)))

  ;; (#%name name expr) names a procedure; (#%app f arg ...) applies F,
  ;; which may be a structure with prop:procedure
  (define-syntax |#%name|
    (syntax-rules () [(_ name e) e]))
  (define-syntax |#%app|
    (syntax-rules () [(_ f arg ...) (apply-any f arg ...)]))
  (define-syntax |#%nongenerative-uid|
    (syntax-rules () [(_ name) 'name]))

  (define (void . args) (if #f #f))
  (define null '())
  (define unsafe-undefined (string->symbol "#<unsafe-undefined>"))

  ;; ---------------------------------------------------------------
  ;; Record types, as Chez 10's make-record-type-descriptor takes them
  ;; too: fields as a count and a mask of the mutable ones, (n . mask)

  (define (field-specs fields)
    (if (pair? fields)
        (let ([n (car fields)] [mask (cdr fields)])
          (let loop ([i (- n 1)] [acc '()])
            (if (< i 0)
                (list->vector acc)
                (loop (- i 1)
                      (cons (list (if (bitwise-bit-set? mask i) 'mutable 'immutable)
                                  (string->symbol (chez:format "f~a" i)))
                            acc)))))
        fields))

  (define make-record-type-descriptor
    (case-lambda
      [(name parent uid sealed? opaque? fields)
       (chez:make-record-type-descriptor name parent uid sealed? opaque? (field-specs fields))]
      [(name parent uid sealed? opaque? fields who)
       (chez:make-record-type-descriptor name parent uid sealed? opaque? (field-specs fields))]))

  ;; ---------------------------------------------------------------
  ;; Structure type properties: a property's values for a record type
  ;; are kept in a table, inherited by subtypes

  (define-record-type struct-property (fields name guard supers))
  (define property-values (make-eq-hashtable))   ; rtd -> ((prop . value) ...)

  (define (rtd-property rtd prop)
    (let loop ([rtd rtd])
      (and rtd
           (let ([p (assq prop (hashtable-ref property-values rtd '()))])
             (if p p (loop (record-type-parent rtd)))))))

  (define make-struct-type-property
    (case-lambda
      [(name) (make-struct-type-property name #f '())]
      [(name guard) (make-struct-type-property name guard '())]
      [(name guard supers . rest)
       (let ([p (make-struct-property name guard supers)])
         (values p
                 (lambda (x)
                   (or (and (record? x) (rtd-property (record-rtd x) p) #t)
                       (and (record-type-descriptor? x) (rtd-property x p) #t)))
                 (lambda (x . default)
                   (let ([e (and (record? x) (rtd-property (record-rtd x) p))]
                         [e2 (and (record-type-descriptor? x) (rtd-property x p))])
                     (cond [(or e e2) => cdr]
                           [(pair? default) (let ([d (car default)]) (if (procedure? d) (d) d))]
                           [else (raise-argument-error name (chez:format "~a?" name) x)])))))]))

  (define (struct-type-property-accessor p) (lambda (x) (cdr (rtd-property (record-rtd x) p))))

  (define-values (prop:procedure procedure-property? procedure-property-ref)
    (make-struct-type-property 'prop:procedure))
  (define-values (prop:authentic authentic? authentic-ref) (make-struct-type-property 'prop:authentic))
  (define-values (prop:equal+hash equal+hash? equal+hash-ref) (make-struct-type-property 'prop:equal+hash))
  (define-values (prop:custom-write custom-write? custom-write-ref) (make-struct-type-property 'prop:custom-write))
  (define-values (prop:object-name object-name? object-name-ref) (make-struct-type-property 'prop:object-name))
  (define-values (prop:checked-procedure checked-procedure? checked-procedure-ref)
    (make-struct-type-property 'prop:checked-procedure))
  (define-values (prop:impersonator-of impersonator-of? impersonator-of-ref)
    (make-struct-type-property 'prop:impersonator-of))

  (define (install-property! rtd prop value)
    (let ([guard (struct-property-guard prop)])
      (let ([value (if guard (guard value (list (record-type-name rtd))) value)])
        (hashtable-update! property-values rtd (lambda (l) (cons (cons prop value) l)) '())
        (for-each (lambda (super)
                    (install-property! rtd (car super) ((cdr super) value)))
                  (struct-property-supers prop)))))

  ;; (make-struct-type-install-properties '(name) fields auto-fields
  ;; super props inspector proc-spec immutables guard constructor-name):
  ;; a procedure that gives a record type those properties
  (define (make-struct-type-install-properties name-spec field-count auto-count super props
                                               insp proc-spec immutables guard ctor-name)
    (lambda (rtd)
      (for-each (lambda (p) (install-property! rtd (car p) (cdr p))) props)
      (when proc-spec
        (install-property! rtd prop:procedure proc-spec))))

  ;; Applying anything Racket can apply
  (define (apply-any f . args)
    (cond
      [(procedure? f) (apply f args)]
      [(and (record? f) (rtd-property (record-rtd f) prop:procedure))
       => (lambda (p)
            (let ([v (cdr p)])
              (if (procedure? v)
                  (apply v f args)
                  (apply (apply-any-field f v) args))))]
      [else (raise-argument-error 'apply "procedure?" f)]))
  (define (apply-any-field f i)
    ((record-accessor (record-rtd f) i) f))

  ;; Racket's make-struct-type: five values
  (define (make-struct-type name super init-count auto-count . rest)
    (let* ([auto-v (if (pair? rest) (car rest) #f)]
           [props (if (and (pair? rest) (pair? (cdr rest))) (cadr rest) '())]
           [proc-spec (if (>= (length rest) 4) (list-ref rest 3) #f)]
           [n (+ init-count auto-count)]
           [parent-n (if super (record-type-field-count super) 0)]
           [rtd (make-record-type-descriptor name super #f #f #f
                                             (cons n (- (expt 2 n) 1)))])
      ((make-struct-type-install-properties (list name) init-count auto-count super props #f proc-spec '() #f #f) rtd)
      (let ([raw (record-constructor (make-record-constructor-descriptor rtd #f #f))])
        (values rtd
                (lambda args (apply raw (append args (make-list auto-count auto-v))))
                (record-predicate rtd)
                (lambda (s i) ((record-accessor rtd i) s))
                (lambda (s i v) ((record-mutator rtd i) s v))))))

  (define (record-type-field-count rtd)
    (let loop ([rtd rtd] [n 0])
      (if rtd (loop (record-type-parent rtd) (+ n (vector-length (record-type-field-names rtd)))) n)))

  (define make-struct-field-accessor
    (case-lambda
      [(ref i) (lambda (s) (ref s i))]
      [(ref i name . _) (lambda (s) (ref s i))]))
  (define make-struct-field-mutator
    (case-lambda
      [(set i) (lambda (s v) (set s i v))]
      [(set i name . _) (lambda (s v) (set s i v))]))

  (define current-inspector (chez:make-parameter 'inspector))

  ;; ---------------------------------------------------------------
  ;; Hash tables.  Mutable ones are R6RS hashtables; immutable ones are
  ;; association lists in a record, by equal?, eqv? or eq?

  (define-record-type ihash (fields kind alist))
  (define (kind-same kind) (case kind [(eq) eq?] [(eqv) eqv?] [else equal?]))
  (define (ilookup h k)
    (let ([same (kind-same (ihash-kind h))])
      (let loop ([l (ihash-alist h)])
        (cond [(null? l) #f] [(same (caar l) k) (car l)] [else (loop (cdr l))]))))
  (define (ihash-set h k v)
    (let ([same (kind-same (ihash-kind h))])
      (make-ihash (ihash-kind h)
                  (cons (cons k v) (remp (lambda (p) (same (car p) k)) (ihash-alist h))))))
  (define (pairs->ihash kind kvs)
    (let loop ([h (make-ihash kind '())] [kvs kvs])
      (if (null? kvs) h (loop (ihash-set h (car kvs) (cadr kvs)) (cddr kvs)))))
  (define (hash . kvs) (pairs->ihash 'equal kvs))
  (define (hasheqv . kvs) (pairs->ihash 'eqv kvs))
  (define (hasheq . kvs) (pairs->ihash 'eq kvs))
  (define (make-hash . _) (make-hashtable equal-hash equal?))
  (define (make-hasheq . _) (make-eq-hashtable))
  (define (make-hasheqv . _) (make-eqv-hashtable))
  (define (make-weak-hash . _) (make-hashtable equal-hash equal?))
  (define (make-weak-hasheq . _) (make-weak-eq-hashtable))
  (define (hash? x) (or (ihash? x) (hashtable? x)))
  (define (failure-result who k fail)
    (cond [(procedure? fail) (fail)]
          [(eq? fail no-default) (raise-arguments-error who "no value found for key" "key" k)]
          [else fail]))
  (define no-default (list 'no-default))
  (define hash-ref
    (case-lambda
      [(h k) (hash-ref h k no-default)]
      [(h k fail)
       (if (ihash? h)
           (let ([p (ilookup h k)]) (if p (cdr p) (failure-result 'hash-ref k fail)))
           (let ([v (hashtable-ref h k no-default)])
             (if (eq? v no-default) (failure-result 'hash-ref k fail) v)))]))
  (define (hash-set h k v) (ihash-set h k v))
  (define (hash-set! h k v) (hashtable-set! h k v))
  (define (hash-remove h k)
    (let ([same (kind-same (ihash-kind h))])
      (make-ihash (ihash-kind h) (remp (lambda (p) (same (car p) k)) (ihash-alist h)))))
  (define (hash-remove! h k) (hashtable-delete! h k))
  (define (hash-count h) (if (ihash? h) (length (ihash-alist h)) (hashtable-size h)))
  (define (hash->list h)
    (if (ihash? h)
        (ihash-alist h)
        (let-values ([(ks vs) (hashtable-entries h)]) (map cons (vector->list ks) (vector->list vs)))))
  (define (hash-for-each h f) (for-each (lambda (p) (f (car p) (cdr p))) (hash->list h)))
  (define (hash-map h f) (map (lambda (p) (f (car p) (cdr p))) (hash->list h)))
  ;; iteration positions are indices into hash->list
  (define (hash-iterate-first h) (if (zero? (hash-count h)) #f 0))
  (define (hash-iterate-next h i) (if (< (+ i 1) (hash-count h)) (+ i 1) #f))
  (define (hash-iterate-key h i . _) (car (list-ref (hash->list h) i)))
  (define (hash-iterate-value h i . _) (cdr (list-ref (hash->list h) i)))

  ;; ---------------------------------------------------------------
  ;; Byte strings are bytevectors

  (define (bytes? x) (bytevector? x))
  (define (bytes . bs) (u8-list->bytevector bs))
  (define make-bytes (case-lambda [(n) (make-bytevector n 0)] [(n b) (make-bytevector n b)]))
  (define (bytes-length b) (bytevector-length b))
  (define (bytes-ref b i) (bytevector-u8-ref b i))
  (define (bytes-set! b i v) (bytevector-u8-set! b i v))
  (define subbytes
    (case-lambda
      [(b start) (subbytes b start (bytevector-length b))]
      [(b start end)
       (let ([r (make-bytevector (- end start))])
         (bytevector-copy! b start r 0 (- end start))
         r)]))
  (define (bytes-append . bs)
    (let ([r (make-bytevector (apply + (map bytevector-length bs)))])
      (let loop ([bs bs] [i 0])
        (if (null? bs) r
            (begin (bytevector-copy! (car bs) 0 r i (bytevector-length (car bs)))
                   (loop (cdr bs) (+ i (bytevector-length (car bs)))))))))
  (define (bytes->list b) (bytevector->u8-list b))
  (define (list->bytes l) (u8-list->bytevector l))
  (define (bytes-copy b) (bytevector-copy b))
  (define (bytes->immutable-bytes b) b)
  (define (bytes=? a b) (bytevector=? a b))
  (define (byte? x) (and (fixnum? x) (<= 0 x 255)))
  (define (range-of who len start end)
    (values (or start 0) (or end len)))
  (define bytes->string/utf-8
    (case-lambda
      [(b) (utf8->string b)]
      [(b err) (utf8->string b)]
      [(b err start) (utf8->string (subbytes b start))]
      [(b err start end) (utf8->string (subbytes b start end))]))
  (define string->bytes/utf-8
    (case-lambda
      [(s) (string->utf8 s)]
      [(s err) (string->utf8 s)]
      [(s err start) (string->utf8 (substring s start (string-length s)))]
      [(s err start end) (string->utf8 (substring s start end))]))
  (define (bytes->string/latin-1 b . _)
    (list->string (map integer->char (bytevector->u8-list b))))
  (define (string->bytes/latin-1 s . _)
    (u8-list->bytevector (map char->integer (string->list s))))
  (define bytes-utf-8-length
    (case-lambda
      [(b) (string-length (utf8->string b))]
      [(b err) (string-length (utf8->string b))]
      [(b err start) (string-length (utf8->string (subbytes b start)))]
      [(b err start end) (string-length (utf8->string (subbytes b start end)))]))
  (define string-utf-8-length
    (case-lambda
      [(s) (bytevector-length (string->utf8 s))]
      [(s start) (bytevector-length (string->utf8 (substring s start (string-length s))))]
      [(s start end) (bytevector-length (string->utf8 (substring s start end)))]))
  (define (unsafe-bytes-ref b i) (bytevector-u8-ref b i))
  (define (unsafe-bytes-set! b i v) (bytevector-u8-set! b i v))
  (define (unsafe-bytes-length b) (bytevector-length b))
  (define unsafe-bytes-copy!
    (case-lambda
      [(dst dst-start src) (unsafe-bytes-copy! dst dst-start src 0 (bytevector-length src))]
      [(dst dst-start src src-start) (unsafe-bytes-copy! dst dst-start src src-start (bytevector-length src))]
      [(dst dst-start src src-start src-end)
       (bytevector-copy! src src-start dst dst-start (- src-end src-start))]))

  ;; ---------------------------------------------------------------
  ;; Unsafe operations are the safe ones

  (define unsafe-car car) (define unsafe-cdr cdr)
  (define unsafe-vector-ref vector-ref) (define unsafe-vector-set! vector-set!)
  (define unsafe-vector-length vector-length)
  (define unsafe-fx+ fx+) (define unsafe-fx- fx-) (define unsafe-fx* fx*)
  (define unsafe-fx< fx<) (define unsafe-fx> fx>) (define unsafe-fx<= fx<=)
  (define unsafe-fx>= fx>=) (define unsafe-fx= fx=)
  (define (unsafe-make-place-local v) (box v))
  (define (unsafe-place-local-ref b) (unbox b))
  (define (unsafe-place-local-set! b v) (set-box! b v))

  ;; ---------------------------------------------------------------
  ;; Racket's errors, raised as Chez conditions with Racket's messages

  (define error-print-width (chez:make-parameter 256))
  (define error-value->string-handler
    (chez:make-parameter (lambda (v n) (chez:format "~s" v))))
  (define (raise-argument-error who expected v . rest)
    (error who (chez:format "contract violation\n  expected: ~a\n  given: ~s" expected v)))
  (define (raise-argument-error* who realm expected v . rest)
    (apply raise-argument-error who expected v rest))
  (define (raise-arguments-error who msg . fields)
    (error who (let loop ([f fields] [s msg])
                 (if (or (null? f) (null? (cdr f))) s
                     (loop (cddr f) (chez:format "~a\n  ~a: ~s" s (car f) (cadr f)))))))
  (define (raise-arguments-error* who realm msg . fields) (apply raise-arguments-error who msg fields))
  (define (raise-range-error who type-desc index-prefix index v lower upper . _)
    (error who (chez:format "~aindex is out of range\n  ~aindex: ~s" index-prefix index-prefix index)))
  (define (raise-range-error* who realm . rest) (apply raise-range-error who rest))
  (define (raise-result-error who expected v)
    (error who (chez:format "result contract violation\n  expected: ~a\n  received: ~s" expected v)))

  ;; ---------------------------------------------------------------
  ;; Numbers, lists, vectors, procedures

  (define (arithmetic-shift n k) (bitwise-arithmetic-shift n k))
  (define (exact-nonnegative-integer? x) (and (integer? x) (exact? x) (>= x 0)))
  (define (exact-integer? x) (and (integer? x) (exact? x)))
  (define (list* x . rest)
    (if (null? rest) x (cons x (apply list* rest))))
  (define vector-copy!
    (case-lambda
      [(dst dst-start src) (vector-copy! dst dst-start src 0 (vector-length src))]
      [(dst dst-start src src-start) (vector-copy! dst dst-start src src-start (vector-length src))]
      [(dst dst-start src src-start src-end)
       (if (and (eq? dst src) (> dst-start src-start))
           (do ([i (- src-end 1) (- i 1)]) ((< i src-start))
             (vector-set! dst (+ dst-start (- i src-start)) (vector-ref src i)))
           (do ([i src-start (+ i 1)]) ((= i src-end))
             (vector-set! dst (+ dst-start (- i src-start)) (vector-ref src i))))]))
  ;; Keywords, interned by name
  (define-record-type keyword (fields name))
  (define keywords (make-hashtable string-hash string=?))
  (define (string->keyword s)
    (or (hashtable-ref keywords s #f)
        (let ([k (make-keyword (string-copy s))]) (hashtable-set! keywords s k) k)))
  (define (keyword->string k) (keyword-name k))
  (define (keyword<? a b . more) (string<? (keyword-name a) (keyword-name b)))
  (define-record-type arity-at-least (fields value))
  (define (procedure-arity-includes? f n . _)
    (or (not (procedure? f))
        (logbit? n (procedure-arity-mask f))))
  (define (procedure-arity f)
    (let ([mask (procedure-arity-mask f)])
      (if (< mask 0)
          (make-arity-at-least (let loop ([i 0]) (if (logbit? i mask) i (loop (+ i 1)))))
          (let loop ([i 0] [acc '()])
            (if (> (ash 1 i) mask)
                (let ([l (reverse acc)]) (if (= (length l) 1) (car l) l))
                (loop (+ i 1) (if (logbit? i mask) (cons i acc) acc)))))))
  (define (object-name x)
    (cond [(and (record? x) (rtd-property (record-rtd x) prop:object-name)) => cdr]
          [(record? x) (record-type-name (record-rtd x))]
          [else #f]))

  ;; ---------------------------------------------------------------
  ;; Prompts: a prompt is an escape continuation on a stack of them,
  ;; by tag; aborting to one calls its handler with the values

  (define-record-type prompt-tag (fields name))
  (define make-continuation-prompt-tag
    (case-lambda [() (make-prompt-tag #f)] [(name) (make-prompt-tag name)]))
  (define the-default-tag (make-prompt-tag 'default))
  (define (default-continuation-prompt-tag) the-default-tag)
  (define prompts (chez:make-parameter '()))   ; ((tag . escape) ...)
  (define call-with-continuation-prompt
    (case-lambda
      [(proc) (call-with-continuation-prompt proc the-default-tag #f)]
      [(proc tag) (call-with-continuation-prompt proc tag #f)]
      [(proc tag handler . args)
       (let ([r (call/1cc
                  (lambda (k)
                    (parameterize ([prompts (cons (cons tag k) (prompts))])
                      (call-with-values (lambda () (apply proc args))
                        (lambda vals (cons 'done vals))))))])
         (if (eq? (car r) 'done)
             (apply values (cdr r))
             (if handler
                 (apply handler (cdr r))
                 (apply (car (cdr r)) '()))))]))
  (define (abort-current-continuation tag . vals)
    (let ([p (assq tag (prompts))])
      (unless p (error 'abort-current-continuation "no such prompt" tag))
      ((cdr p) (cons 'abort vals))))

  ;; ---------------------------------------------------------------
  ;; Strings, characters, paths

  (define (unquoted-printing-string s) s)
  (define (string->immutable-string s) s)
  (define (symbol->immutable-string s) (symbol->string s))
  ;; one character at a time: a grapheme cluster per character
  (define (char-grapheme-step c state) (values #t 0))
  (define (path? x) #f)
  (define (path->string p) p)
  (define (path->bytes p) (string->utf8 p))

  ;; ---------------------------------------------------------------
  ;; Ports: Chez's binary ports, with Racket's names; a string port is
  ;; read as bytes

  (define eof (eof-object))
  ;; Racket's make-input-port: a port of a read and a peek procedure
  ;; (each filling a byte string, giving a count or eof)
  (define-record-type rport (fields name read-in peek close))
  (define (make-input-port name read-in peek close . _) (make-rport name read-in peek close))
  (define (input-port? x) (or (rport? x) (chez:input-port? x)))
  (define (output-port? x) (chez:output-port? x))
  (define (progress-evt? x . _) #f)
  (define (binary p) (if (binary-port? p) p (error 'regexp "textual ports aren't supported here" p)))
  (define (rport-fill p bs start end skip peek?)
    ;; into BS[START, END): a count or eof
    (let ([tmp (make-bytevector (- end start))])
      (let ([n (if peek? ((rport-peek p) tmp skip #f) ((rport-read-in p) tmp))])
        (unless (eof-object? n) (bytevector-copy! tmp 0 bs start n))
        n)))
  (define peek-byte
    (case-lambda
      [(p) (peek-byte p 0)]
      [(p skip)
       (cond
         [(rport? p)
          (let* ([b (make-bytevector 1)] [n (rport-fill p b 0 1 skip #t)])
            (if (eof-object? n) n (bytevector-u8-ref b 0)))]
         [(zero? skip) (lookahead-u8 (binary p))]
         [else
          (let ([pos (port-position p)])
            (set-port-position! p (+ pos skip))
            (let ([b (lookahead-u8 p)]) (set-port-position! p pos) b))])]))
  (define (read-byte p)
    (if (rport? p)
        (let* ([b (make-bytevector 1)] [n (rport-fill p b 0 1 0 #f)])
          (if (eof-object? n) n (bytevector-u8-ref b 0)))
        (get-u8 (binary p))))
  (define (peek-bytes-avail! bs skip evt p . range)
    (let ([start (if (pair? range) (car range) 0)]
          [end (if (and (pair? range) (pair? (cdr range))) (cadr range) (bytevector-length bs))])
      (if (rport? p)
          (rport-fill p bs start end skip #t)
          (let ([pos (port-position p)])
            (set-port-position! p (+ pos skip))
            (let ([n (get-bytevector-n! p bs start (- end start))])
              (set-port-position! p pos)
              n)))))
  (define peek-bytes-avail!* peek-bytes-avail!)
  (define (read-bytes! bs p . range)
    (let ([start (if (pair? range) (car range) 0)]
          [end (if (and (pair? range) (pair? (cdr range))) (cadr range) (bytevector-length bs))])
      (if (rport? p)
          (let loop ([i start])
            (if (= i end)
                (- end start)
                (let ([n (rport-fill p bs i end 0 #f)])
                  (if (eof-object? n)
                      (if (= i start) n (- i start))
                      (loop (+ i n))))))
          (get-bytevector-n! (binary p) bs start (- end start)))))
  (define (write-bytes bs p . range)
    (let ([start (if (pair? range) (car range) 0)]
          [end (if (and (pair? range) (pair? (cdr range))) (cadr range) (bytevector-length bs))])
      (if (binary-port? p)
          (put-bytevector p bs start (- end start))
          (put-string p (utf8->string (subbytes bs start end))))
      (- end start)))
  (define (write-string s p . range)
    (let ([start (if (pair? range) (car range) 0)]
          [end (if (and (pair? range) (pair? (cdr range))) (cadr range) (string-length s))])
      (put-string p s start (- end start))
      (- end start)))
  (define (open-input-bytes b . _) (open-bytevector-input-port b))

  ;; Racket's format directives are Chez's for what a linklet uses
  (define (format fmt . args) (apply chez:format fmt args))
  (define make-parameter
    (case-lambda
      [(v) (chez:make-parameter v)]
      [(v guard) (chez:make-parameter v (or guard (lambda (x) x)))]
      [(v guard name) (chez:make-parameter v (or guard (lambda (x) x)))]))

  ;; (apply string-append a ... l) as schemify writes it: the arguments
  ;; as one list (PRE-N of them before L's, for error messages)
  (define (apply-string-append pre-n l) (apply string-append l))
  (define (apply-bytes-append pre-n l) (apply bytes-append l))
  (define-syntax begin0
    (syntax-rules ()
      [(_ e0 e ...) (call-with-values (lambda () e0) (lambda vals e ... (apply values vals)))]))
  ;; Racket's categories are lowercase: 'lu, not 'Lu
  (define (char-general-category c)
    (string->symbol (string-downcase (symbol->string (chez:char-general-category c)))))

  ;; Prefab structure types: one uid per name, parent and field count
  (define (structure-type-lookup-prefab-uid name parent init-count auto-count auto-v immutables)
    (string->symbol
      (chez:format "prefab:~a:~a:~a" name (if parent (record-type-uid parent) "")
                   (+ init-count auto-count))))
  (define (struct-type? x) (record-type-descriptor? x))
  (define-syntax $value (syntax-rules () [(_ e) e]))
  (define (impersonator? x) #f)
  (define (impersonator-val x) x)

  ;; Correlated objects: what syntax objects are to schemify, a datum
  ;; with a source location and properties
  (define-record-type correlated (fields e source line column position span props))
  (define (syntax? x) (correlated? x))
  (define (syntax-e x) (correlated-e x))
  (define syntax-property
    (case-lambda
      [(x k) (let ([p (assq k (correlated-props x))]) (and p (cdr p)))]
      [(x k v) (make-correlated (correlated-e x) (correlated-source x) (correlated-line x)
                                (correlated-column x) (correlated-position x) (correlated-span x)
                                (cons (cons k v) (correlated-props x)))]))
  (define (syntax-source x) (correlated-source x))
  (define (syntax-line x) (correlated-line x))
  (define (syntax-column x) (correlated-column x))
  (define (syntax-position x) (correlated-position x))
  (define (syntax-span x) (correlated-span x))
  (define datum->syntax
    (case-lambda
      [(ctx v) (datum->syntax ctx v #f)]
      [(ctx v srcloc . _)
       (if (correlated? v)
           v
           (let ([loc (cond [(correlated? srcloc)
                             (list (correlated-source srcloc) (correlated-line srcloc) (correlated-column srcloc)
                                   (correlated-position srcloc) (correlated-span srcloc))]
                            [(vector? srcloc) (vector->list srcloc)]
                            [else (list #f #f #f #f #f)])])
             (apply make-correlated v (append loc (list '())))))]))
  (define (syntax->datum x)
    (cond [(correlated? x) (syntax->datum (correlated-e x))]
          [(pair? x) (cons (syntax->datum (car x)) (syntax->datum (cdr x)))]
          [else x]))
  (define (primitive-table name)
    (and (eq? name '|#%kernel|)
         (hash 'syntax? syntax? 'syntax-e syntax-e 'syntax-property syntax-property
               'datum->syntax datum->syntax 'syntax->datum syntax->datum
               'syntax-source syntax-source 'syntax-line syntax-line 'syntax-column syntax-column
               'syntax-position syntax-position 'syntax-span syntax-span
               'syntax-property-symbol-keys
               (lambda (x) (filter symbol? (map car (correlated-props x)))))))

  ;; Continuation marks: a dynamic list of (key . value).  A mark on
  ;; exception-handler-key is a handler; one on parameterization-key
  ;; binds parameters.  (A mark in tail position adds to the list rather
  ;; than replacing the frame's.)
  (define exception-handler-key (list 'exception-handler-key))
  (define parameterization-key (list 'parameterization-key))
  (define push-authentic 'push-authentic)
  (define current-marks (chez:make-parameter '()))
  (define-record-type parameterization (fields bindings))
  (define (extend-parameterization pz . pvs)
    (let loop ([pvs pvs] [b (if (parameterization? pz) (parameterization-bindings pz) '())])
      (if (null? pvs) (make-parameterization b) (loop (cddr pvs) (cons (cons (car pvs) (cadr pvs)) b)))))
  (define (call-with-mark k v thunk)
    (parameterize ([current-marks (cons (cons k v) (current-marks))])
      (cond
        [(eq? k exception-handler-key)
         (with-exception-handler (lambda (c) (v c)) thunk)]
        [(and (eq? k parameterization-key) (parameterization? v))
         (let loop ([b (reverse (parameterization-bindings v))])
           (if (null? b)
               (thunk)
               (let ([p (caar b)] [val (cdar b)])
                 (let ([old #f])
                   (dynamic-wind
                     (lambda () (set! old (p)) (p val))
                     (lambda () (loop (cdr b)))
                     (lambda () (set! val (p)) (p old)))))))]
        [else (thunk)])))
  (define-syntax with-continuation-mark*
    (syntax-rules () [(_ mode k v body) (call-with-mark k v (lambda () body))]))
  (define-syntax with-continuation-mark
    (syntax-rules () [(_ k v body) (call-with-mark k v (lambda () body))]))
  (define (continuation-mark-set-first set k . default)
    (let ([p (assq k (current-marks))])
      (cond [p (cdr p)] [(pair? default) (car default)] [else #f])))
  (define (current-continuation-marks . _) (current-marks))
  (define (continuation-mark-set->list set k . _)
    (map cdr (filter (lambda (p) (eq? (car p) k)) (if (list? set) set (current-marks)))))

  ;; more unsafe operations: the safe ones
  (define unsafe-fxrshift fxsra)
  (define unsafe-fxlshift fxsll)
  (define unsafe-fxand fxand)
  (define unsafe-fxior fxior)
  (define unsafe-fxxor fxxor)
  (define unsafe-fxnot fxnot)
  (define unsafe-fxquotient fxquotient)
  (define unsafe-fxremainder fxremainder)
  (define unsafe-fxmodulo fxmodulo)
  (define unsafe-fxmin fxmin)
  (define unsafe-fxmax fxmax)
  (define unsafe-fxabs fxabs)
  (define unsafe-fx->fl fixnum->flonum)
  (define unsafe-fl+ fl+)
  (define unsafe-fl- fl-)
  (define unsafe-fl* fl*)
  (define unsafe-fl/ fl/)
  (define unsafe-fl< fl<)
  (define unsafe-fl> fl>)
  (define unsafe-fl= fl=)
  (define unsafe-fl<= fl<=)
  (define unsafe-fl>= fl>=)
  (define unsafe-string-ref string-ref)
  (define unsafe-string-length string-length)
  (define unsafe-string-set! string-set!)
  (define unsafe-unbox unbox)
  (define unsafe-set-box! set-box!)
  (define unsafe-unbox* unbox)
  (define unsafe-set-box*! set-box!)
  (define unsafe-vector*-ref vector-ref)
  (define unsafe-vector*-set! vector-set!)
  (define unsafe-vector*-length vector-length)
  (define unsafe-char=? char=?)
  (define unsafe-char<? char<?)
  (define unsafe-char->integer char->integer)
  (define unsafe-cons-list cons)
  (define unsafe-list-ref list-ref)
  (define unsafe-list-tail list-tail)
  (define unsafe-fxquotient/remainder fxdiv-and-mod)
  (define (unsafe-struct-ref s i) ((record-accessor-any s) s i))
  (define (unsafe-struct-set! s i v) ((record-mutator-any s) s i v))
  (define unsafe-struct*-ref unsafe-struct-ref)
  (define unsafe-struct*-set! unsafe-struct-set!)
  ;; the Ith field of a record, counting its parents' fields first
  (define (record-field-rtd s i)
    (let loop ([rtd (record-rtd s)])
      (let ([base (if (record-type-parent rtd) (record-type-field-count (record-type-parent rtd)) 0)])
        (if (>= i base) (values rtd (- i base)) (loop (record-type-parent rtd))))))
  (define (record-accessor-any s)
    (lambda (s i) (let-values ([(rtd j) (record-field-rtd s i)]) ((record-accessor rtd j) s))))
  (define (record-mutator-any s)
    (lambda (s i v) (let-values ([(rtd j) (record-field-rtd s i)]) ((record-mutator rtd j) s v))))
  (define (symbol-interned? s) (not (gensym? s)))
  (define (hash-iterate-key+value h i . _) (let ([p (list-ref (hash->list h) i)]) (values (car p) (cdr p))))
  (define (hash-iterate-pair h i . _) (let ([p (list-ref (hash->list h) i)]) (cons (car p) (cdr p))))
  (define (hash-keys h . _) (map car (hash->list h)))
  (define (hash-values h) (map cdr (hash->list h)))
  (define (hash-empty? h) (zero? (hash-count h)))
  (define (hash-update h k f . d) (hash-set h k (f (apply hash-ref h k d))))

  ;; ---------------------------------------------------------------
  ;; The linklet runtime a schemified linklet's lambda expects: its
  ;; variables, which hold a definition's value, and the prompt around a
  ;; definition's right-hand side (which here only stores the values)

  (define-record-type variable (fields name (mutable value) (mutable constance)))
  (define (make-internal-variable name) (make-variable name unsafe-undefined #f))
  (define (make-instance-variable name) (make-internal-variable name))
  (define (instance-variable-value v) (variable-value v))
  (define (variable-ref/no-check v) (variable-value v))
  (define (variable-ref v)
    (let ([x (variable-value v)])
      (if (eq? x unsafe-undefined) (error (variable-name v) "undefined") x)))
  (define (variable-set! v x . _) (variable-value-set! v x))
  (define (variable-set!/define v x . _) (variable-value-set! v x))
  ;; (call-with-module-prompt proc [names constances var ...])
  (define (call-with-module-prompt proc . names+vars)
    (call-with-values proc
      (lambda vals
        (let loop ([vals vals] [vars (if (and (pair? names+vars) (pair? (cddr names+vars))) (cddr names+vars) '())])
          (unless (or (null? vals) (null? vars))
            (variable-value-set! (car vars) (car vals))
            (loop (cdr vals) (cdr vars))))
        (apply values vals))))
  (define (raise-binding-result-arity-error n args)
    (error 'let-values "wrong number of values" n args))
  (define-syntax |#%app/no-return| (syntax-rules () [(_ f a ...) (|#%app| f a ...)]))
  (define-syntax |#%app/value| (syntax-rules () [(_ f a ...) (|#%app| f a ...)])))
