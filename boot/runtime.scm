;;; boot/runtime.scm -- what the translator needs from its host.
;;;
;;; Under Common Lisp, p-record.scm and p-utils.scm supply the
;;; translator's records, tables, fluids, packages and pretty printer,
;;; written with CL primitives.  These definitions take their place;
;;; the rest of translator.files is loaded unchanged.

;;; Order of evaluation.  The translator runs, and is checked against
;;; its output, under Common Lisp, where arguments, LET bindings and
;;; MAP (MAPCAR) are evaluated left to right.  Some of its side effects
;;; show in its output in that order (e.g. the names in a SPECIAL
;;; declaration).  R5RS leaves the order unspecified, and hosts differ,
;;; so BOOT:HOST-FORM makes it explicit, and has the translator's MAPs
;;; call this one.

(define (boot:map f l . ls)
  (define (map1 l)
    (if (pair? l)
	(let ((x (f (car l))))
	  (cons x (map1 (cdr l))))
	'()))
  (define (map-n ls)
    (if (let loop ((ls ls))
	  (or (null? ls) (and (pair? (car ls)) (loop (cdr ls)))))
	(let ((x (apply f (boot:map-car ls))))
	  (cons x (map-n (boot:map-cdr ls))))
	'()))
  (if (null? ls) (map1 l) (map-n (cons l ls))))

(define (boot:map-car ls)
  (if (null? ls) '() (cons (caar ls) (boot:map-car (cdr ls)))))

(define (boot:map-cdr ls)
  (if (null? ls) '() (cons (cdar ls) (boot:map-cdr (cdr ls)))))

;;; p-record.scm

(define (make-record-type type-id field-names)
  (boot:make-rtd type-id field-names))

(define (record-constructor rtd . init-names-option)
  (let* ((n (length (boot:rtd-field-names rtd)))
	 (names (if (null? init-names-option)
		    (boot:rtd-field-names rtd)
		    (car init-names-option)))
	 (indices (map (lambda (name) (boot:rtd-field-index rtd name)) names)))
    (lambda args
      (if (not (= (length args) (length indices)))
	  (boot:die "wrong number of arguments to record constructor"
		    (boot:rtd-id rtd) args))
      ;; An uninitialized DEFSTRUCT slot is NIL, i.e. ().
      (let ((fields (make-vector n '())))
	(for-each (lambda (i x) (vector-set! fields i x)) indices args)
	(boot:new-record rtd fields)))))

(define (record-predicate rtd)
  (lambda (x) (boot:instance? rtd x)))

(define (record-accessor rtd name)
  (let ((i (boot:rtd-field-index rtd name)))
    (lambda (r)
      (boot:check-instance rtd r)
      (vector-ref (boot:record-fields r) i))))

(define (record-modifier rtd name)
  (let ((i (boot:rtd-field-index rtd name)))
    (lambda (r x)
      (boot:check-instance rtd r)
      (vector-set! (boot:record-fields r) i x))))

(define (record-type r)
  (and (boot:record? r) (boot:record-rtd r)))

(define (define-record-discloser rtd proc)
  (vector-set! (boot:record-fields rtd) 2 proc))

(define (disclose-record r)
  (let ((proc (vector-ref (boot:record-fields (boot:record-rtd r)) 2)))
    (if proc (proc r) (list (boot:rtd-id (boot:record-rtd r))))))

;;; p-utils.scm

(define (last-pair x)
  (if (pair? (cdr x)) (last-pair (cdr x)) x))

(define (vector-posq thing v)
  (let loop ((i 0))
    (cond ((= i (vector-length v)) #f)
	  ((eq? (vector-ref v i) thing) i)
	  (else (loop (+ i 1))))))

(define (string-posq c s)
  (boot:string-index s c))

;; Fluids

(define (make-fluid top-level-value)
  (list top-level-value))

(define (fluid f) (car f))

(define (set-fluid! f val) (set-car! f val))

(define (let-fluid f val thunk)
  (let ((swap (lambda ()
		(let ((old (car f)))
		  (set-car! f val)
		  (set! val old)))))
    (dynamic-wind swap thunk swap)))

;; Tables

(define (make-table) (boot:make-table 'eqv))

(define (table-set! table key val) (boot:table-set! table key val))

(define (table-ref table key) (boot:table-ref table key #f))

;; Printing

(define (write-pretty form port package)
  (boot:write-pretty form port package))

;; Packages

(define lisp-package boot:ps-package)

(define scheme-package boot:scheme-package)

(define (intern-renaming-perhaps string package)
  (boot:intern (if (eq? package scheme-package)
		   string
		   (perhaps-rename string))
	       package))

(define (perhaps-rename string)		;Cf. defune in rts.lisp
  (if (or (let ((probe (boot:find-symbol string lisp-package)))
	    (and probe (eq? (cdr probe) 'external)))
	  (and (> (string-length string) 0)
	       (char=? (string-ref string 0) #\&)))
      (string-append "." string)
      string))

(define (qualified-symbol? sym)
  (and (symbol? sym)
       (not (eq? (boot:symbol-package sym) scheme-package))))

(define (boot:use-packages! uses package)
  (for-each (lambda (use)
	      (if (not (memq use (boot:package-use-list package)))
		  (boot:set-package-use-list!
		   package
		   (append (boot:package-use-list package) (list use)))))
	    uses))

(define (make-package-using id use-list)
  (let* ((name (boot:symbol-name id))
	 (probe (boot:find-package name))
	 (package
	  (if probe
	      (begin
		(boot:set-package-use-list!
		 probe
		 (boot:filter (lambda (use)
				(or (eq? use lisp-package) (memq use use-list)))
			      (boot:package-use-list probe)))
		probe)
	      (boot:make-package name '() '()))))
    (boot:use-packages! (if (eq? id 'scheme)
			    use-list	;Kludge
			    (cons lisp-package use-list))
			package)
    package))

(define (make-package-exporting id syms)
  (let* ((name (boot:symbol-name id))
	 (new (or (boot:find-package name)
		  (boot:make-package name '() '()))))
    (for-each (lambda (sym) (boot:import sym new)) syms)
    (for-each (lambda (sym) (boot:export sym new)) syms)
    new))

(define (scheme-implementation-version) boot:host-name)

(define (defined-as-cl-macro? cl-sym) #f)

(define boot:output-file-name #f)

(define (true-name file)
  (if (string? file) file boot:output-file-name))

(define (package-name package) (boot:package-name package))

(define (intern string package) (boot:intern string package))

;;; Elsewhere in Pseudoscheme's run-time system

(define (ps:symbol-name sym) (boot:symbol-name sym))

(define (ps:scheme-error msg . irritants)
  (apply boot:host-error msg irritants))

(define (warn msg . irritants)
  (display "Warning: ")
  (display msg)
  (for-each (lambda (x) (display " ") (write x)) irritants)
  (newline))

;; reify.scm's MOVE-VALUE-OR-DENOTATION works on CL symbols' value
;; cells, for MAKE-SCHEME-USER-ENVIRONMENT; nothing here calls it.
(define (boot:unavailable name)
  (lambda args (boot:die "not available while bootstrapping" name)))
(define ps:boundp (boot:unavailable 'boundp))
(define ps:symbol-value (boot:unavailable 'symbol-value))
(define ps:setf (boot:unavailable 'setf))
(define ps:set-function-from-value (boot:unavailable 'set-function-from-value))
(define ps:eval (boot:unavailable 'eval))

;;; Translated files.  These replace the translator's own
;;; COMPILING-TO-FILE (translate.scm), which leans on CL's FRESH-LINE to
;;; separate forms; BOOT:WRITE-PRETTY ends each form with a newline
;;; instead.

(define (boot:compiling-to-file outfile package write-message proc)
  (let-fluid @translating-to-file? #t
    (lambda ()
      (with-target-package package
	(lambda ()
	  (set! boot:output-file-name outfile)
	  (display "Writing ")
	  (display outfile)
	  (newline)
	  (call-with-output-file outfile
	    (lambda (port)
	      (display "; -*- Mode: Lisp; Syntax: Common-Lisp; Package: " port)
	      (display (package-name package) port)
	      (display "; -*-" port)
	      (newline port)
	      (newline port)
	      (display "; This file was generated by " port)
	      (display (translator-version) port)
	      (newline port)
	      (display ";  running in " port)
	      (display (scheme-implementation-version) port)
	      (newline port)
	      (write-message port)
	      (newline port)
	      (display "(ps:in-package " port)
	      (display (boot:string->cl-string (package-name package)) port)
	      (display ")" port)
	      (newline port)
	      (write-form (list (boot:ps-symbol "BEGIN-TRANSLATED-FILE")) port)
	      (proc port)))
	  outfile)))))

(define (boot:write-defpackages struct-list filename)
  (display "Writing ")
  (display filename)
  (newline)
  (call-with-output-file filename
    (lambda (port)
      (display "; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER; -*-" port)
      (newline port)
      (newline port)
      (display "; This file was generated by " port)
      (display (translator-version) port)
      (newline port)
      (display ";  running in " port)
      (display (scheme-implementation-version) port)
      (newline port)
      (newline port)
      (for-each (lambda (struct)
		  (with-target-package lisp-package
		    (lambda ()
		      (write-form (generate-structure-defpackage struct) port))))
		struct-list))))

;;; Loading the translator's sources into the host.  BOOT:HOST-FORM
;;; rewrites a form of the translator's into one for the host:
;;;
;;;  - A few CL forms occur as code (not as data to be emitted) outside
;;;    p-record.scm and p-utils.scm: #'f and (ps-lisp:if ...).
;;;  - Calls and LETs evaluate their operands left to right, and MAP is
;;;    BOOT:MAP (see above).
;;;
;;; Quoted and quasiquoted data are left alone: that's where the
;;; translator keeps the CL code it emits.

(define (boot:host-form x)
  (cond ((eq? x 'map) 'boot:map)
	((not (pair? x)) x)
	((not (symbol? (car x))) (boot:host-call x))
	(else
	 (case (car x)
	   ((quote) x)
	   ((quasiquote) (boot:host-form (boot:expand-quasiquote (cadr x) 1)))
	   ((lambda) (boot:cons* 'lambda (cadr x) (boot:host-forms (cddr x))))
	   ((define) (boot:cons* 'define (cadr x) (boot:host-forms (cddr x))))
	   ((set!) (list 'set! (cadr x) (boot:host-form (caddr x))))
	   ((if begin and or delay) (cons (car x) (boot:host-forms (cdr x))))
	   ((let)
	    (if (symbol? (cadr x))
		(boot:host-let (cadr x) (caddr x) (cdddr x))
		(boot:host-let #f (cadr x) (cddr x))))
	   ((let* letrec)
	    (boot:cons* (car x) (boot:host-bindings (cadr x)) (boot:host-forms (cddr x))))
	   ((cond)
	    (cons 'cond (map (lambda (clause) (boot:host-forms clause)) (cdr x))))
	   ((case)
	    (boot:cons* 'case (boot:host-form (cadr x))
		   (map (lambda (clause) (cons (car clause) (boot:host-forms (cdr clause))))
			(cddr x))))
	   ((do)
	    (boot:cons* 'do
		   (map (lambda (spec) (cons (car spec) (boot:host-forms (cdr spec))))
			(cadr x))
		   (boot:host-forms (caddr x))
		   (boot:host-forms (cdddr x))))
	   (else
	    (cond ((and (eq? (car x) (boot:ps-symbol "FUNCTION"))
			(pair? (cdr x)) (null? (cddr x)) (symbol? (cadr x)))
		   ;; #'f: f's value, which these host definitions provide
		   (boot:host-form (cadr x)))
		  ((and (eq? (car x) (boot:ps-symbol "IF"))
			(boot:proper-list? x)
			(<= 3 (length x) 4))
		   (boot:host-form (cons 'if (cdr x))))
		  (else (boot:host-call x))))))))

(define (boot:cons* x . rest)
  (if (null? rest) x (cons x (apply boot:cons* rest))))

(define (boot:host-forms l)
  (if (pair? l)
      (cons (boot:host-form (car l)) (boot:host-forms (cdr l)))
      l))

(define (boot:host-bindings bindings)
  (map (lambda (b) (cons (car b) (boot:host-forms (cdr b)))) bindings))

;; Operands to be evaluated in order: if more than one of them might
;; have a side effect (i.e. is a combination), evaluate each into a
;; temporary first.  Returns (values specs operands).
(define (boot:sequence-operands operands)
  (if (< (length (boot:filter pair? operands)) 2)
      (cons '() operands)
      (let loop ((l operands) (i 0) (specs '()) (temps '()))
	(if (null? l)
	    (cons (reverse specs) (reverse temps))
	    (let ((temp (string->symbol
			 (string-append "boot:operand-" (number->string i)))))
	      (loop (cdr l) (+ i 1)
		    (cons (list temp (car l)) specs)
		    (cons temp temps)))))))

(define (boot:host-call x)
  (let* ((x (boot:host-forms x))
	 (seq (boot:sequence-operands (cdr x))))
    (if (null? (car seq))
	x
	(list 'let* (car seq) (cons (car x) (cdr seq))))))

(define (boot:host-let name bindings body)
  (let* ((bindings (boot:host-bindings bindings))
	 (body (boot:host-forms body))
	 (seq (boot:sequence-operands (map cadr bindings)))
	 (bindings (map (lambda (b init) (list (car b) init)) bindings (cdr seq)))
	 (let-form (if name
		       (boot:cons* 'let name bindings body)
		       (boot:cons* 'let bindings body))))
    (if (null? (car seq))
	let-form
	(list 'let* (car seq) let-form))))

;; Quasiquote, expanded into calls (so that, like any calls, they
;; evaluate left to right).
(define (boot:expand-quasiquote x level)
  (cond ((not (boot:unquotes? x level)) (list 'quote x))
	((vector? x)
	 (list 'list->vector (boot:expand-quasiquote (vector->list x) level)))
	((eq? (car x) 'unquote)
	 (if (= level 1)
	     (cadr x)
	     (list 'list ''unquote (boot:expand-quasiquote (cadr x) (- level 1)))))
	((eq? (car x) 'quasiquote)
	 (list 'list ''quasiquote (boot:expand-quasiquote (cadr x) (+ level 1))))
	((and (pair? (car x)) (eq? (caar x) 'unquote-splicing) (= level 1))
	 (list 'append (cadar x) (boot:expand-quasiquote (cdr x) level)))
	(else
	 (list 'cons
	       (boot:expand-quasiquote (car x) level)
	       (boot:expand-quasiquote (cdr x) level)))))

;; Whether X, inside LEVEL quasiquotes, has anything to evaluate.
(define (boot:unquotes? x level)
  (cond ((vector? x) (boot:unquotes? (vector->list x) level))
	((not (pair? x)) #f)
	((memq (car x) '(unquote unquote-splicing))
	 (or (= level 1) (boot:unquotes? (cdr x) (- level 1))))
	((eq? (car x) 'quasiquote) (boot:unquotes? (cdr x) (+ level 1)))
	(else (or (boot:unquotes? (car x) level) (boot:unquotes? (cdr x) level)))))
