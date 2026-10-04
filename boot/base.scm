;;; boot/base.scm -- utilities, tables, records and a model of Common
;;; Lisp packages, for running Pseudoscheme's translator in a Scheme
;;; that isn't Pseudoscheme.
;;;
;;; Everything in boot/ outside hosts/ is written in plain R5RS, using
;;; only these from its host adapter (boot/hosts/<name>.scm):
;;;
;;;   (boot:eval form)               evaluate at the top level
;;;   boot:host-name                 e.g. "Guile 3.0.10"
;;;   (boot:make-record rtd serial fields)   an opaque object: not a
;;;   (boot:record? x)               pair, vector, procedure, symbol ...
;;;   (boot:record-rtd r) (boot:record-serial r) (boot:record-fields r)
;;;
;;; All of boot/'s own names start with "boot:", so that the
;;; translator's sources, which are evaluated in the same top-level
;;; environment, can't collide with them.

(define boot:host-error error)

(define (boot:die msg . irritants)
  (apply boot:host-error (string-append "bootstrap: " msg) irritants))

;;; Strings and lists

(define (boot:string-index s c)
  (let loop ((i 0))
    (cond ((= i (string-length s)) #f)
	  ((char=? (string-ref s i) c) i)
	  (else (loop (+ i 1))))))

(define (boot:string-map f s)
  (let* ((n (string-length s)) (new (make-string n)))
    (do ((i 0 (+ i 1))) ((= i n) new)
      (string-set! new i (f (string-ref s i))))))

(define (boot:string-upcase s) (boot:string-map char-upcase s))
(define (boot:string-downcase s) (boot:string-map char-downcase s))

(define (boot:string-every? pred s)
  (let loop ((i 0))
    (or (= i (string-length s))
	(and (pred (string-ref s i)) (loop (+ i 1))))))

(define (boot:has-lower-case? s)
  (not (boot:string-every? (lambda (c) (not (char-lower-case? c))) s)))

(define (boot:string-join strings)
  (apply string-append strings))

(define (boot:filter pred l)
  (cond ((null? l) '())
	((pred (car l)) (cons (car l) (boot:filter pred (cdr l))))
	(else (boot:filter pred (cdr l)))))

(define (boot:for-each-index f n)
  (do ((i 0 (+ i 1))) ((= i n)) (f i)))

;;; Hash tables: KIND is 'eqv (keys compared with EQV?, like CL's
;;; default EQL tables) or 'string.  Symbols and records hash well;
;;; other EQV? keys (the translator's nodes are vectors) share a bucket.

(define (boot:string-hash s)
  (let loop ((i 0) (h 0))
    (if (= i (string-length s))
	h
	(loop (+ i 1)
	      (modulo (+ (* h 31) (char->integer (string-ref s i)))
		      16777213)))))

(define (boot:make-table kind)
  (vector kind 0 (make-vector 64 '())))

(define (boot:table-hash table key)
  (if (eq? (vector-ref table 0) 'string)
      (boot:string-hash key)
      (cond ((symbol? key) (boot:string-hash (symbol->string key)))
	    ((boot:record? key) (boot:record-serial key))
	    ((and (integer? key) (exact? key)) (abs key))
	    ((char? key) (char->integer key))
	    ((string? key) (string-length key))
	    (else 0))))

(define (boot:table-bucket-index table key)
  (modulo (boot:table-hash table key) (vector-length (vector-ref table 2))))

(define (boot:table-assoc table key bucket)
  (if (eq? (vector-ref table 0) 'string)
      (assoc key bucket)
      (assv key bucket)))

(define (boot:table-ref table key default)
  (let ((entry (boot:table-assoc
		table key
		(vector-ref (vector-ref table 2)
			    (boot:table-bucket-index table key)))))
    (if entry (cdr entry) default)))

(define (boot:table-set! table key value)
  (let* ((buckets (vector-ref table 2))
	 (i (boot:table-bucket-index table key))
	 (entry (boot:table-assoc table key (vector-ref buckets i))))
    (if entry
	(set-cdr! entry value)
	(begin
	  (vector-set! buckets i (cons (cons key value) (vector-ref buckets i)))
	  (vector-set! table 1 (+ (vector-ref table 1) 1))
	  (if (> (vector-ref table 1) (* 2 (vector-length buckets)))
	      (boot:table-grow! table))))))

(define (boot:table-grow! table)
  (let* ((old (vector-ref table 2))
	 (new (make-vector (* 2 (vector-length old)) '())))
    (vector-set! table 2 new)
    (boot:for-each-index
     (lambda (i)
       (for-each (lambda (entry)
		   (let ((j (boot:table-bucket-index table (car entry))))
		     (vector-set! new j (cons entry (vector-ref new j)))))
		 (vector-ref old i)))
     (vector-length old))))

(define (boot:table-walk table proc)
  (let ((buckets (vector-ref table 2)))
    (boot:for-each-index
     (lambda (i)
       (for-each (lambda (entry) (proc (car entry) (cdr entry)))
		 (vector-ref buckets i)))
     (vector-length buckets))))

;;; Records (the interface of the translator's p-record.scm)

(define boot:record-serial-counter 0)

(define (boot:new-record rtd fields)
  (set! boot:record-serial-counter (+ boot:record-serial-counter 1))
  (boot:make-record rtd boot:record-serial-counter fields))

;; A record type descriptor is itself a record, of type boot:rtd-rtd.
;; Its fields: #(type-id field-names discloser)
(define boot:rtd-rtd 'record-type-descriptor)

(define (boot:rtd? x)
  (and (boot:record? x) (eq? (boot:record-rtd x) boot:rtd-rtd)))

(define (boot:make-rtd type-id field-names)
  (boot:new-record boot:rtd-rtd (vector type-id field-names #f)))

(define (boot:rtd-id rtd) (vector-ref (boot:record-fields rtd) 0))
(define (boot:rtd-field-names rtd) (vector-ref (boot:record-fields rtd) 1))

(define (boot:rtd-field-index rtd name)
  (let loop ((l (boot:rtd-field-names rtd)) (i 0))
    (cond ((null? l)
	   (boot:die "not a field name" name (boot:rtd-id rtd)))
	  ((eq? (car l) name) i)
	  (else (loop (cdr l) (+ i 1))))))

(define (boot:instance? rtd x)
  (and (boot:record? x) (eq? (boot:record-rtd x) rtd)))

(define (boot:check-instance rtd x)
  (if (not (boot:instance? rtd x))
      (boot:die "wrong record type" (boot:rtd-id rtd) x)))

;;; Common Lisp packages and symbols
;;;
;;; The translator works with CL symbols: it reads Scheme source as
;;; symbols in the SCHEME package, and emits code made of symbols in
;;; PS (the Pseudoscheme package, which exports the CL symbols the
;;; translator uses), in the target packages it creates (e.g.
;;; SCHEME-TRANSLATOR), and keywords.  Here every CL symbol is a host
;;; symbol, so that EQ?, MEMQ, ASSQ and CASE in the translator work
;;; unchanged; the symbol's package and name are recorded in
;;; BOOT:SYMBOL-INFO.  Host spellings:
;;;
;;;   SCHEME::FOO   foo         (an ordinary Scheme symbol)
;;;   PS:CAR        ps:car      (also COMMON-LISP:CAR, PS-LISP:CAR ...)
;;;   :TEST         :test
;;;   P::FOO        p::foo      (any other package P)
;;;
;;; with the name's case inverted, as Pseudoscheme does (core.lisp).

;; A package's fields: #(name nicknames use-list present externals)
(define boot:package-rtd 'package)

(define boot:all-packages '())

(define (boot:package? x) (boot:instance? boot:package-rtd x))
(define (boot:package-name p) (vector-ref (boot:record-fields p) 0))
(define (boot:package-nicknames p) (vector-ref (boot:record-fields p) 1))
(define (boot:package-use-list p) (vector-ref (boot:record-fields p) 2))
(define (boot:package-present p) (vector-ref (boot:record-fields p) 3))
(define (boot:package-externals p) (vector-ref (boot:record-fields p) 4))

(define (boot:set-package-use-list! p l)
  (vector-set! (boot:record-fields p) 2 l))

(define (boot:make-package name nicknames use-list)
  (let ((p (boot:new-record boot:package-rtd
			    (vector name nicknames use-list
				    (boot:make-table 'string)
				    (boot:make-table 'string)))))
    (set! boot:all-packages (cons p boot:all-packages))
    p))

(define (boot:find-package name)
  (let loop ((l boot:all-packages))
    (cond ((null? l) #f)
	  ((or (string=? name (boot:package-name (car l)))
	       (member name (boot:package-nicknames (car l))))
	   (car l))
	  (else (loop (cdr l))))))

(define boot:scheme-package (boot:make-package "SCHEME" '() '()))
(define boot:keyword-package (boot:make-package "KEYWORD" '() '()))
(define boot:ps-package
  (boot:make-package "PS" '("PS-LISP" "PSEUDOSCHEME") '()))
(define boot:cl-package
  (boot:make-package "COMMON-LISP" '("CL") '()))

;; host symbol -> (package . name)
(define boot:symbol-info (boot:make-table 'eqv))

(define (boot:symbol-package sym)
  (car (boot:info sym)))

(define (boot:symbol-name sym)
  (cdr (boot:info sym)))

(define (boot:info sym)
  (or (boot:table-ref boot:symbol-info sym #f)
      ;; A symbol the reader didn't make: one written in boot/'s own
      ;; code, or by SYMBOL->STRING / STRING->SYMBOL.
      (boot:parse-host-symbol sym)))

(define (boot:parse-host-symbol sym)
  (let* ((s (symbol->string sym))
	 (i (boot:string-index s #\:))
	 (info
	  (cond ((not i) (cons boot:scheme-package (boot:invert-case s)))
		((= i 0) (cons boot:keyword-package
			       (boot:invert-case (substring s 1 (string-length s)))))
		(else
		 (let ((p (boot:find-package
			   (boot:string-upcase (substring s 0 i))))
		       (j (if (and (< (+ i 1) (string-length s))
				   (char=? (string-ref s (+ i 1)) #\:))
			      (+ i 2)
			      (+ i 1))))
		   (if p
		       (cons p (boot:invert-case
				(substring s j (string-length s))))
		       (cons boot:scheme-package (boot:invert-case s))))))))
    (boot:table-set! boot:symbol-info sym info)
    (boot:table-set! (boot:package-present (car info)) (cdr info) sym)
    info))

;; Pseudoscheme's SYMBOL->STRING and STRING->SYMBOL invert case
;; (core.lisp), and so do these spellings: then the host's own
;; SYMBOL->STRING and STRING->SYMBOL, which the translator uses, agree
;; with Pseudoscheme's.
(define (boot:invert-case s)
  (let ((upper? (not (boot:string-every? (lambda (c) (not (char-upper-case? c))) s)))
	(lower? (boot:has-lower-case? s)))
    (cond ((and upper? lower?) s)
	  (upper? (boot:string-downcase s))
	  (lower? (boot:string-upcase s))
	  (else s))))

(define (boot:host-spelling name package)
  (let ((n (boot:invert-case name)))
    (cond ((eq? package boot:scheme-package)
	   (if (boot:string-index n #\:) (string-append "scheme::" n) n))
	  ((eq? package boot:keyword-package) (string-append ":" n))
	  ((eq? package boot:ps-package) (string-append "ps:" n))
	  ((eq? package boot:cl-package) (string-append "common-lisp:" n))
	  (else (string-append (boot:string-downcase (boot:package-name package))
			       "::" n)))))

;; Returns (symbol . status), status one of internal external
;; inherited; or #f.
(define (boot:find-symbol name package)
  (let ((present (boot:table-ref (boot:package-present package) name #f)))
    (if present
	(cons present
	      (if (boot:table-ref (boot:package-externals package) name #f)
		  'external
		  'internal))
	(let loop ((l (if (eq? package boot:cl-package)
			  ;; We don't know CL's symbols, only those PS
			  ;; exports; treat those as CL's.
			  (list boot:ps-package)
			  (boot:package-use-list package))))
	  (cond ((null? l) #f)
		((boot:table-ref (boot:package-externals (car l)) name #f)
		 (cons (boot:table-ref (boot:package-present (car l)) name #f)
		       'inherited))
		(else (loop (cdr l))))))))

(define (boot:intern name package)
  (let ((found (boot:find-symbol name package)))
    (if found
	(car found)
	(let ((sym (string->symbol (boot:host-spelling name package))))
	  (if (boot:table-ref boot:symbol-info sym #f)
	      (boot:die "host spelling collision" sym))
	  (boot:table-set! boot:symbol-info sym (cons package name))
	  (boot:table-set! (boot:package-present package) name sym)
	  sym))))

(define (boot:import sym package)
  (let ((name (boot:symbol-name sym)))
    (boot:table-set! (boot:package-present package) name sym)))

(define (boot:export sym package)
  (let ((name (boot:symbol-name sym)))
    (if (not (boot:table-ref (boot:package-present package) name #f))
	(boot:import sym package))
    (boot:table-set! (boot:package-externals package) name #t)))

(define (boot:keyword name) (boot:intern name boot:keyword-package))
(define (boot:ps-symbol name) (boot:intern name boot:ps-package))
