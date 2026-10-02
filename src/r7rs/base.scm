;;; -*- Mode: Scheme; Syntax: Scheme -*-
;;;; R7RS-small: derived syntax and the procedures that are simplest to
;;;; write in Scheme itself.
;;;;
;;;; Loaded (with the dedicated reader) into the R7RS implementation
;;;; environment, which starts out as a copy of the R5RS bindings plus
;;;; the Lisp primitives of rts.lisp, so redefining e.g. VECTOR-FILL!
;;;; here to take a range does not disturb the R5RS one.
;;;;
;;;; Written against Pseudoscheme's own (R4RS-style) SYNTAX-RULES, so no
;;;; patterns after an ellipsis, no custom ellipsis, no (... ...).  The
;;;; same definitions will be fine under syntax-case's SYNTAX-RULES.

;;; ------------------------------------------------------------------
;;; 4.2 Derived expression types

(define-syntax when
  (syntax-rules ()
    ((_ test result1 result2 ...)
     (if test (begin result1 result2 ...)))))

(define-syntax unless
  (syntax-rules ()
    ((_ test result1 result2 ...)
     (if (not test) (begin result1 result2 ...)))))

(define-syntax letrec*
  (syntax-rules ()
    ((_ ((var init) ...) body1 body2 ...)
     (let ()
       (define var init) ...
       (let () body1 body2 ...)))))

;;; let-values / let*-values / define-values (4.2.2, 5.3.3), after the
;;; report's own sample implementation.

(define-syntax let-values
  (syntax-rules ()
    ((_ (binding ...) body0 body1 ...)
     (let-values "bind" (binding ...) () (begin body0 body1 ...)))
    ((_ "bind" () tmps body)
     (let tmps body))
    ((_ "bind" ((b0 e0) binding ...) tmps body)
     (let-values "mktmp" b0 e0 () (binding ...) tmps body))
    ((_ "mktmp" () e0 args bindings tmps body)
     (call-with-values (lambda () e0)
       (lambda args (let-values "bind" bindings tmps body))))
    ((_ "mktmp" (a . b) e0 (arg ...) bindings (tmp ...) body)
     (let-values "mktmp" b e0 (arg ... x) bindings (tmp ... (a x)) body))
    ((_ "mktmp" a e0 (arg ...) bindings (tmp ...) body)
     (call-with-values (lambda () e0)
       (lambda (arg ... . x)
         (let-values "bind" bindings (tmp ... (a x)) body))))))

(define-syntax let*-values
  (syntax-rules ()
    ((_ () body0 body1 ...)
     (let () body0 body1 ...))
    ((_ (binding0 binding1 ...) body0 body1 ...)
     (let-values (binding0)
       (let*-values (binding1 ...) body0 body1 ...)))))

;; The report's definition needs patterns after an ellipsis; this walks
;; the formals one variable at a time instead, pairing each with a
;; fresh temporary.
(define-syntax define-values
  (syntax-rules ()
    ((_ formals expr)
     (%define-values formals () expr))))

(define-syntax %define-values
  (syntax-rules ()
    ((_ () ((var tmp) ...) expr)
     (begin
       (define var #f) ...
       (call-with-values (lambda () expr)
         (lambda (tmp ...) (set! var tmp) ... (if #f #f)))))
    ((_ (a . rest) (pair ...) expr)
     (%define-values rest (pair ... (a tmp)) expr))
    ((_ r (pair ...) expr)
     (%define-values-rest r (pair ...) expr))))

;; A dotted tail variable: the last temporary is the rest list.
(define-syntax %define-values-rest
  (syntax-rules ()
    ((_ r ((var tmp) ...) expr)
     (begin
       (define var #f) ...
       (define r #f)
       (call-with-values (lambda () expr)
         (lambda (tmp ... . rtmp) (set! var tmp) ... (set! r rtmp) (if #f #f)))))))

;;; parameterize (4.2.6); the dynamic extent is managed by %PARAMETERIZE
;;; (rts.lisp), which also applies the parameters' converters.

(define-syntax parameterize
  (syntax-rules ()
    ((_ () body1 body2 ...)
     (let () body1 body2 ...))
    ((_ ((param value) ...) body1 body2 ...)
     (%parameterize (list param ...) (list value ...)
                    (lambda () body1 body2 ...)))))

;;; case-lambda (4.2.9).  The report's version tests arity with patterns;
;;; here the formals are quoted and examined at run time, which gets the
;;; same behavior from simpler SYNTAX-RULES.

(define (%formals-accept? formals n)
  (let loop ((f formals) (n n))
    (cond ((pair? f) (and (> n 0) (loop (cdr f) (- n 1))))
          ((null? f) (= n 0))
          (else #t))))                  ; a rest variable takes anything left

(define-syntax %case-lambda-dispatch
  (syntax-rules ()
    ((_ args n)
     (error "case-lambda: no clause matches this many arguments" n))
    ((_ args n (formals body1 body2 ...) clause ...)
     (if (%formals-accept? 'formals n)
         (apply (lambda formals body1 body2 ...) args)
         (%case-lambda-dispatch args n clause ...)))))

(define-syntax case-lambda
  (syntax-rules ()
    ((_ clause ...)
     (lambda args
       (%case-lambda-dispatch args (length args) clause ...)))))

;;; define-record-type (5.5) over the primitives in rts.lisp.

(define-syntax define-record-type
  (syntax-rules ()
    ((_ type (ctor cfield ...) pred (fname . fclauses) ...)
     (begin
       (define type (%make-record-type 'type '(fname ...)))
       (define ctor (%record-constructor type '(cfield ...)))
       (define pred (%record-predicate type))
       (%define-record-field type fname . fclauses) ...))
    ((_ type ctor pred (fname . fclauses) ...)
     (begin
       (define type (%make-record-type 'type '(fname ...)))
       (define ctor (%record-constructor type '(fname ...)))
       (define pred (%record-predicate type))
       (%define-record-field type fname . fclauses) ...))))

(define-syntax %define-record-field
  (syntax-rules ()
    ((_ type field)
     (begin))
    ((_ type field accessor)
     (define accessor (%record-accessor type 'field)))
    ((_ type field accessor modifier)
     (begin
       (define accessor (%record-accessor type 'field))
       (define modifier (%record-modifier type 'field))))))

;;; guard (4.2.7).  The report's sample implementation re-enters the
;;; raise's continuation when no clause matches; that needs full
;;; re-entrant continuations, which CALL/CC here (an escape-only
;;; BLOCK/RETURN-FROM) cannot give.  This one escapes to the guard first
;;; and re-raises from there with RAISE-CONTINUABLE, so the only
;;; observable difference is for a RAISE-CONTINUABLE whose handler
;;; return value would have come back through a non-matching guard.

(define-syntax guard
  (syntax-rules ()
    ((_ (var clause ...) e1 e2 ...)
     ((call-with-current-continuation
       (lambda (guard-k)
         (with-exception-handler
          (lambda (condition)
            (guard-k
             (lambda ()
               (let ((var condition))
                 (%guard-aux (raise-continuable condition) clause ...)))))
          (lambda ()
            (call-with-values
             (lambda () e1 e2 ...)
             (lambda args
               (guard-k (lambda () (apply values args)))))))))))))

(define-syntax %guard-aux
  (syntax-rules (else =>)
    ((_ reraise (else result1 result2 ...))
     (begin result1 result2 ...))
    ((_ reraise (test => result))
     (let ((temp test)) (if temp (result temp) reraise)))
    ((_ reraise (test => result) clause1 clause2 ...)
     (let ((temp test))
       (if temp (result temp) (%guard-aux reraise clause1 clause2 ...))))
    ((_ reraise (test))
     (or test reraise))
    ((_ reraise (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp temp (%guard-aux reraise clause1 clause2 ...))))
    ((_ reraise (test result1 result2 ...))
     (if test (begin result1 result2 ...) reraise))
    ((_ reraise (test result1 result2 ...) clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (%guard-aux reraise clause1 clause2 ...)))))

;;; (scheme lazy), 4.2.5.  Promises keep a box (done? . value-or-thunk);
;;; FORCE is the report's iterative algorithm, so long DELAY-FORCE
;;; chains run in constant space.

(define-record-type promise
  (%make-promise box)
  promise?
  (box %promise-box %set-promise-box!))

(define (make-promise obj)
  (if (promise? obj)
      obj
      (%make-promise (cons #t obj))))

(define-syntax delay-force
  (syntax-rules ()
    ((_ expression)
     (%make-promise (cons #f (lambda () expression))))))

(define-syntax delay
  (syntax-rules ()
    ((_ expression)
     (delay-force (make-promise expression)))))

(define (force promise)
  (if (not (promise? promise))
      promise
      (let loop ()
        (let ((box (%promise-box promise)))
          (if (car box)
              (cdr box)
              (let ((promise* ((cdr box))))
                (let ((box (%promise-box promise)))
                  (if (not (car box))
                      (let ((box* (%promise-box promise*)))
                        (set-car! box (car box*))
                        (set-cdr! box (cdr box*))
                        (%set-promise-box! promise* box))))
                (loop)))))))

;;; ------------------------------------------------------------------
;;; Numbers, booleans, symbols

(define exact inexact->exact)
(define inexact exact->inexact)
(define (square z) (* z z))

;;; ------------------------------------------------------------------
;;; Equivalence-style list operations with the optional predicate
;;; argument R7RS added.

(define (member x lst . compare)
  (let ((same? (if (pair? compare) (car compare) equal?)))
    (let loop ((l lst))
      (cond ((null? l) #f)
            ((same? x (car l)) l)
            (else (loop (cdr l)))))))

(define (assoc x alist . compare)
  (let ((same? (if (pair? compare) (car compare) equal?)))
    (let loop ((l alist))
      (cond ((null? l) #f)
            ((same? x (car (car l))) (car l))
            (else (loop (cdr l)))))))

(define (make-list k . fill)
  (let ((x (if (pair? fill) (car fill) #f)))
    (let loop ((i 0) (acc '()))
      (if (>= i k) acc (loop (+ i 1) (cons x acc))))))

(define (list-copy obj)
  (if (pair? obj)
      (cons (car obj) (list-copy (cdr obj)))
      obj))

(define (list-set! lst k obj)
  (set-car! (list-tail lst k) obj))

;;; ------------------------------------------------------------------
;;; Strings and vectors: optional start/end ranges, multi-sequence maps.

(define (%start r) (if (pair? r) (car r) 0))
(define (%end r default)
  (if (and (pair? r) (pair? (cdr r))) (car (cdr r)) default))

(define (string-copy s . r)
  (substring s (%start r) (%end r (string-length s))))

(define (string-copy! to at from . r)
  (let ((start (%start r)) (end (%end r (string-length from))))
    (if (and (eq? to from) (> at start))
        (do ((i (- end 1) (- i 1)))               ; overlapping: copy backwards
            ((< i start))
          (string-set! to (+ at (- i start)) (string-ref from i)))
        (do ((i start (+ i 1)))
            ((>= i end))
          (string-set! to (+ at (- i start)) (string-ref from i))))))

(define (string-fill! s c . r)
  (do ((i (%start r) (+ i 1)))
      ((>= i (%end r (string-length s))))
    (string-set! s i c)))

(define (string->list s . r)
  (let ((start (%start r)))
    (let loop ((i (- (%end r (string-length s)) 1)) (acc '()))
      (if (< i start) acc (loop (- i 1) (cons (string-ref s i) acc))))))

(define (string-map proc s . more)
  (list->string (apply map proc (string->list s) (map string->list more))))

(define (string-for-each proc s . more)
  (apply for-each proc (string->list s) (map string->list more)))

(define (vector->list v . r)
  (let ((start (%start r)))
    (let loop ((i (- (%end r (vector-length v)) 1)) (acc '()))
      (if (< i start) acc (loop (- i 1) (cons (vector-ref v i) acc))))))

(define (string->vector s . r)
  (list->vector (apply string->list s r)))

(define (vector->string v . r)
  (list->string (apply vector->list v r)))

(define (vector-copy v . r)
  (let* ((start (%start r)) (end (%end r (vector-length v)))
         (out (make-vector (- end start) #f)))
    (do ((i start (+ i 1)))
        ((>= i end) out)
      (vector-set! out (- i start) (vector-ref v i)))))

(define (vector-copy! to at from . r)
  (let ((start (%start r)) (end (%end r (vector-length from))))
    (if (and (eq? to from) (> at start))
        (do ((i (- end 1) (- i 1)))
            ((< i start))
          (vector-set! to (+ at (- i start)) (vector-ref from i)))
        (do ((i start (+ i 1)))
            ((>= i end))
          (vector-set! to (+ at (- i start)) (vector-ref from i))))))

(define (vector-fill! v x . r)
  (do ((i (%start r) (+ i 1)))
      ((>= i (%end r (vector-length v))))
    (vector-set! v i x)))

(define (vector-append . vs)
  (list->vector (apply append (map vector->list vs))))

(define (vector-map proc v . more)
  (list->vector (apply map proc (vector->list v) (map vector->list more))))

(define (vector-for-each proc v . more)
  (apply for-each proc (vector->list v) (map vector->list more)))

;;; ------------------------------------------------------------------
;;; n-ary comparisons (R7RS 6.6, 6.7) -- the R5RS ones take exactly two.

(define (%chain binary)
  (letrec ((chain
            (lambda (a b . rest)
              (and (binary a b)
                   (or (null? rest)
                       (apply chain b rest))))))
    chain))

(define string=? (%chain string=?))
(define string<? (%chain string<?))
(define string>? (%chain string>?))
(define string<=? (%chain string<=?))
(define string>=? (%chain string>=?))
(define string-ci=? (%chain string-ci=?))
(define string-ci<? (%chain string-ci<?))
(define string-ci>? (%chain string-ci>?))
(define string-ci<=? (%chain string-ci<=?))
(define string-ci>=? (%chain string-ci>=?))
(define char=? (%chain char=?))
(define char<? (%chain char<?))
(define char>? (%chain char>?))
(define char<=? (%chain char<=?))
(define char>=? (%chain char>=?))
(define char-ci=? (%chain char-ci=?))
(define char-ci<? (%chain char-ci<?))
(define char-ci>? (%chain char-ci>?))
(define char-ci<=? (%chain char-ci<=?))
(define char-ci>=? (%chain char-ci>=?))

;;; ------------------------------------------------------------------
;;; Integral inexact numbers are integers too (R7RS 6.2.6): lift the
;;; exact-only R5RS operations through EXACT / INEXACT.

(define (%lift-exact proc)
  (lambda args
    (if (every-exact? args)
        (apply proc args)
        (inexact (apply proc (map exact args))))))

(define (every-exact? args)
  (or (null? args)
      (and (exact? (car args)) (every-exact? (cdr args)))))

(define numerator (%lift-exact numerator))
(define denominator (%lift-exact denominator))
(define gcd (%lift-exact gcd))
(define lcm (%lift-exact lcm))

;;; ------------------------------------------------------------------
;;; Aliases and small things

(define call/cc call-with-current-continuation)
(define write-simple write)
(define write-shared write)
