;;; -*- Mode: Scheme -*-
;;;; What the older Revised Reports have that R5RS lacks or spells
;;;; differently (docs/reports.md).  src/reports/reports.lisp builds each
;;;; report's library out of R5RS's bindings, these, and its own name list.

;;; The empty list counts as false in the Revised Report, R2RS and R3RS
;;; (R4RS and IEEE 1178 say it counts as true, and are plain R5RS there).
;;; The conditionals, and not, that treat it so.  What the report leaves
;;; unspecified (a one-armed if whose test fails, a cond with no clause
;;; chosen, a do with no result expressions) is '(), which is NIL in the
;;; Revised Report.
(library (pseudoscheme reports truth)
  (export if cond and or do not)
  (import (except (rnrs) if cond and or do not)
          (prefix (only (rnrs) if cond and or do not) r6:))

  (define (not x) (r6:or (eq? x #f) (null? x)))

  (define-syntax truth
    (syntax-rules ()
      ((_ e) (let ((t e)) (r6:if (null? t) #f t)))))

  (define-syntax if
    (syntax-rules ()
      ((_ c a) (r6:if (truth c) a '()))
      ((_ c a b) (r6:if (truth c) a b))))

  ;; (cond (test => f) ...) is in all of them; a report that doesn't have
  ;; it doesn't export =>, so that its user's => is another identifier.
  (define-syntax cond
    (syntax-rules (else =>)
      ((_) '())
      ((_ (else e1 e2 ...)) (begin e1 e2 ...))
      ((_ (test => f) c ...) (let ((t test)) (if t (f t) (cond c ...))))
      ((_ (test) c ...) (or test (cond c ...)))
      ((_ (test e1 e2 ...) c ...) (if test (begin e1 e2 ...) (cond c ...)))))

  (define-syntax and
    (syntax-rules ()
      ((_) #t)
      ((_ e) e)
      ((_ e1 e2 ...) (if e1 (and e2 ...) #f))))

  (define-syntax or
    (syntax-rules ()
      ((_) #f)
      ((_ e) e)
      ((_ e1 e2 ...) (let ((t e1)) (if t t (or e2 ...))))))

  ;; (do ((var init step) ...) (test expr ...) stmt ...), where a step may
  ;; be left out, and then the init too (the variable starts out as '()).
  (define-syntax do
    (syntax-rules ()
      ((_ (spec ...) clause stmt ...)
       (do-normalize (spec ...) () clause stmt ...))))

  (define-syntax do-normalize
    (syntax-rules ()
      ((_ () (done ...) clause stmt ...)
       (do-loop (done ...) clause stmt ...))
      ((_ ((v) spec ...) (done ...) clause stmt ...)
       (do-normalize (spec ...) (done ... (v '() v)) clause stmt ...))
      ((_ ((v x) spec ...) (done ...) clause stmt ...)
       (do-normalize (spec ...) (done ... (v x v)) clause stmt ...))
      ((_ ((v x s) spec ...) (done ...) clause stmt ...)
       (do-normalize (spec ...) (done ... (v x s)) clause stmt ...))))

  (define-syntax do-loop
    (syntax-rules ()
      ((_ ((v x s) ...) (test e ...) stmt ...)
       (let loop ((v x) ...)
         (if test
             (begin '() e ...)
             (begin stmt ... (loop s ...))))))))

;;; The names the reports have that R5RS doesn't (or has differently),
;;; and the extra procedures of their optional features.
(library (pseudoscheme reports extras)
  (export define named-lambda rec sequence
          =? <? >? <=? >=? 1+ -1+ append! last-pair string-null?
          substring-fill! substring-move-left! substring-move-right!
          object-hash object-unhash transcript-on transcript-off
          number->string string->number rationalize)
  (import (except (rnrs) define number->string string->number rationalize)
          (prefix (only (rnrs) define number->string string->number rationalize) r6:)
          (rnrs mutable-strings) (rnrs mutable-pairs))

  ;; (define ((f a) b) ...) is (define (f a) (lambda (b) ...)) (R2RS 1.1)
  (define-syntax define
    (syntax-rules ()
      ((_ ((head . f1) . f2) body1 body2 ...)
       (define (head . f1) (lambda f2 body1 body2 ...)))
      ((_ (name . formals) body1 body2 ...)
       (r6:define (name . formals) body1 body2 ...))
      ((_ name expr) (r6:define name expr))
      ((_ name) (r6:define name))))

  (define-syntax named-lambda
    (syntax-rules ()
      ((_ (name . formals) body1 body2 ...)
       (letrec ((name (lambda formals body1 body2 ...))) name))))

  (define-syntax rec
    (syntax-rules ()
      ((_ var expr) (letrec ((var expr)) var))))

  (define-syntax sequence
    (syntax-rules ()
      ((_ e1 e2 ...) (begin e1 e2 ...))))

  (define =? =)
  (define <? <)
  (define >? >)
  (define <=? <=)
  (define >=? >=)
  (define (1+ n) (+ n 1))
  (define (-1+ n) (- n 1))
  (define (append! . lists) (apply append lists))
  (define (last-pair x)
    (if (pair? (cdr x)) (last-pair (cdr x)) x))
  (define (string-null? s) (= (string-length s) 0))

  (define (substring-fill! s start end c)
    (do ((i start (+ i 1))) ((>= i end))
      (string-set! s i c)))
  (define (substring-move-left! s1 m1 n1 s2 m2)
    (do ((i m1 (+ i 1)) (j m2 (+ j 1))) ((>= i n1))
      (string-set! s2 j (string-ref s1 i))))
  (define (substring-move-right! s1 m1 n1 s2 m2)
    (do ((i (- n1 1) (- i 1)) (j (+ m2 (- n1 m1 1)) (- j 1))) ((< i m1))
      (string-set! s2 j (string-ref s1 i))))

  ;; the object table (R2RS II.10), strongly held
  (define object-table '())
  (define (object-hash obj)
    (let ((entry (let loop ((t object-table))
                   (cond ((null? t) #f)
                         ((eq? (cdr (car t)) obj) (car t))
                         (else (loop (cdr t)))))))
      (if entry
          (car entry)
          (let ((n (+ (length object-table) 1)))
            (set! object-table (cons (cons n obj) object-table))
            n))))
  (define (object-unhash n)
    (let ((entry (assv n object-table)))
      (if entry (cdr entry) #f)))

  ;; transcripts are accepted, and not made
  (define (transcript-on filename) (if #f #f))
  (define (transcript-off) (if #f #f))

  ;; R2RS and R3RS take a format, (number->string n '(fix 2)), and
  ;; string->number an exactness and a radix, both as symbols (E or EXACT,
  ;; B, O, D, X, or BINARY ...); R4RS a radix.  Formats are not made.
  (define (number->string n . rest)
    (if (and (pair? rest) (number? (car rest)))
        (r6:number->string n (car rest))
        (r6:number->string n)))

  (define (string->number s . options)
    (let loop ((options options) (radix 10) (exactness #f))
      (if (null? options)
          (let ((n (r6:string->number s radix)))
            (cond ((not n) #f)
                  ((eq? exactness 'exact) (exact n))
                  ((eq? exactness 'inexact) (inexact n))
                  (else n)))
          (let ((o (car options)))
            (cond ((number? o) (loop (cdr options) o exactness))
                  ((symbol? o)
                   (let ((name (symbol->string o)))
                     (cond ((member name '("e" "exact")) (loop (cdr options) radix 'exact))
                           ((member name '("i" "inexact")) (loop (cdr options) radix 'inexact))
                           ((member name '("b" "binary")) (loop (cdr options) 2 exactness))
                           ((member name '("o" "octal")) (loop (cdr options) 8 exactness))
                           ((member name '("d" "decimal")) (loop (cdr options) 10 exactness))
                           ((member name '("x" "hexadecimal")) (loop (cdr options) 16 exactness))
                           (else (error 'string->number "bad option" o)))))
                  (else (error 'string->number "bad option" o)))))))

  ;; one argument: the best approximation that keeps all the precision
  (define (rationalize x . y)
    (if (null? y)
        (exact x)
        (r6:rationalize x (car y)))))
