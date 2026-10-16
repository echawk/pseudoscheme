;;; -*- Mode: Scheme -*-
;;;; (chezscheme): ftypes, Chez's way to describe foreign memory --
;;;; define-ftype, make-ftype-pointer, ftype-ref, ftype-set!, ftype-&ref
;;;; and the others of CSUG's Foreign Interface.  Part of the library's
;;;; body (src/chez/chez.lisp).
;;;;
;;;;   (define-ftype name ftype [(nongenerative uid)])
;;;;   (define-ftype (name ftype [(nongenerative uid)]) ...)
;;;;   ftype: name | (struct [field ftype] ...) | (union [field ftype] ...)
;;;;        | (array length ftype) | (* ftype) | (bits [field signed|unsigned width] ...)
;;;;        | (function conv ... (arg ...) result)
;;;;        | (packed ftype) | (unpacked ftype) | (endian big|little|native ftype)
;;;;   (ftype-ref ftype (accessor ...) fptr [index])
;;;;   (ftype-set! ftype (accessor ...) fptr [index] value)
;;;;
;;;; Everything is in (pseudoscheme chez ftype-helpers), src/chez/ftype-helpers.sls,
;;;; which these macros call when they expand (a transformer can't use
;;;; definitions of the library it is defined in), and in the host
;;;; procedures chez:ftype-... (src/chez/ftype.lisp).  An ftype name is a
;;;; macro that, used as (name k arg ...), expands into (k number arg ...),
;;;; the number of the ftype's descriptor, so that each macro of the
;;;; operations finds it by being used twice (ft-cps).

;; define-ftype: the names the types use are found (ft-resolve), and then
;; ft-define-1 makes the descriptors and defines the names
(define-syntax define-ftype
  (lambda (x) (ft-define-start x #'ft-resolve #'ft-define-1)))

(define-syntax ft-resolve
  (syntax-rules ()
    ((_ () acc k form) (k acc form))
    ((_ (n1 n2 ...) (a ...) k form)
     (n1 ft-resolve-step n1 (n2 ...) (a ...) k form))))

(define-syntax ft-resolve-step
  (syntax-rules ()
    ((_ idv n1 rest (a ...) k form)
     (ft-resolve rest (a ... (n1 idv)) k form))))

(define-syntax ft-define-1
  (lambda (x) (ft-define-finish x #'ft-name)))

;; (ft-name name number): name is the ftype of that number
(define-syntax ft-name
  (syntax-rules ()
    ((_ name idv)
     (define-syntax name
       (lambda (x)
         (syntax-case x ()
           ((_ k . rest) #'(k idv . rest))))))))

;; The operations on an ftype, with the ftype's name
(define-syntax ftype-sizeof (lambda (x) (ft-cps x #'ftype-sizeof-1)))
(define-syntax ftype-sizeof-1 (lambda (x) (ft-sizeof-expand x)))

(define-syntax ftype-ref (lambda (x) (ft-cps x #'ftype-ref-1)))
(define-syntax ftype-ref-1 (lambda (x) (ft-expand 'ref x)))

(define-syntax ftype-set! (lambda (x) (ft-cps x #'ftype-set!-1)))
(define-syntax ftype-set!-1 (lambda (x) (ft-expand 'set x)))

(define-syntax ftype-&ref (lambda (x) (ft-cps x #'ftype-&ref-1)))
(define-syntax ftype-&ref-1 (lambda (x) (ft-expand 'ref& x)))

(define-syntax ftype-any-ref (lambda (x) (ft-cps x #'ftype-any-ref-1)))
(define-syntax ftype-any-ref-1 (lambda (x) (ft-expand 'any-ref x)))

(define-syntax ftype-any-set! (lambda (x) (ft-cps x #'ftype-any-set!-1)))
(define-syntax ftype-any-set!-1 (lambda (x) (ft-expand 'any-set x)))

(define-syntax ftype-locked-incr! (lambda (x) (ft-cps x #'ftype-locked-incr!-1)))
(define-syntax ftype-locked-incr!-1 (lambda (x) (ft-expand 'incr x)))

(define-syntax ftype-locked-decr! (lambda (x) (ft-cps x #'ftype-locked-decr!-1)))
(define-syntax ftype-locked-decr!-1 (lambda (x) (ft-expand 'decr x)))

(define-syntax ftype-init-lock! (lambda (x) (ft-cps x #'ftype-init-lock!-1)))
(define-syntax ftype-init-lock!-1 (lambda (x) (ft-expand 'init-lock x)))

(define-syntax ftype-lock! (lambda (x) (ft-cps x #'ftype-lock!-1)))
(define-syntax ftype-lock!-1 (lambda (x) (ft-expand 'lock x)))

(define-syntax ftype-spin-lock! (lambda (x) (ft-cps x #'ftype-spin-lock!-1)))
(define-syntax ftype-spin-lock!-1 (lambda (x) (ft-expand 'spin-lock x)))

(define-syntax ftype-unlock! (lambda (x) (ft-cps x #'ftype-unlock!-1)))
(define-syntax ftype-unlock!-1 (lambda (x) (ft-expand 'unlock x)))

;; (make-ftype-pointer ftype address); for a function ftype, a procedure
;; may be given instead, which is made a foreign callable
(define-syntax make-ftype-pointer (lambda (x) (ft-cps x #'make-ftype-pointer-1)))
(define-syntax make-ftype-pointer-1
  (syntax-rules ()
    ((_ idv e) (ft-make-pointer idv e))))

;; (ftype-pointer? obj) and (ftype-pointer? ftype obj)
(define-syntax ftype-pointer?
  (lambda (x)
    (syntax-case x ()
      ((_ e) #'(ft-pointer? e))
      ((_ n e) (identifier? #'n) (ft-cps x #'ftype-pointer?-1))
      (id (identifier? #'id) #'ft-pointer?))))
(define-syntax ftype-pointer?-1
  (syntax-rules ()
    ((_ idv e) (ft-pointer-of? idv e))))

;; (make-ftype-scheme-object-pointer object [offset [ftype]])
(define-syntax make-ftype-scheme-object-pointer
  (lambda (x)
    (syntax-case x ()
      ((_ obj) #'(ft-make-sop obj 0 #f))
      ((_ obj off) #'(ft-make-sop obj off #f))
      ((_ obj off n) (identifier? #'n) #'(ft-sop-front n obj off)))))
(define-syntax ft-sop-front (lambda (x) (ft-cps x #'ft-sop-1)))
(define-syntax ft-sop-1
  (syntax-rules ()
    ((_ idv obj off) (ft-make-sop obj off idv))))

;; (ftype-guardian ftype): a guardian for the pointers of that ftype
(define-syntax ftype-guardian (lambda (x) (ft-cps x #'ftype-guardian-1)))
(define-syntax ftype-guardian-1
  (syntax-rules ()
    ((_ idv) (ft-make-guardian idv))))

(define (ft-make-guardian id)
  (let ((g (make-guardian)))
    (case-lambda
      (() (g))
      ((p) (ft-check-fptr 'ftype-guardian p id) (g p))
      ((p rep) (ft-check-fptr 'ftype-guardian p id) (g p rep)))))

(define ftype-pointer-address ft-pointer-address)
(define ftype-pointer-null? ft-pointer-null?)
(define ftype-pointer=? ft-pointer=?)
(define ftype-pointer-ftype ft-pointer-ftype)
(define ftype-pointer->sexpr ft-pointer->sexpr)
(define ftype-scheme-object-pointer? ft-sop?)
(define ftype-scheme-object-pointer-object ft-sop-object)
(define ftype-scheme-object-pointer-offset ft-sop-offset)
