;;; microKanren (Hemann and Friedman, 2013) with miniKanren's surface
;;; syntax as syntax-rules macros: fresh, conde, run, run*.  Used from
;;; Common Lisp by tests/programs/kanren.lisp.

(define-library (kanren micro)
  (export var var? == call/fresh disj conj zzz conj+ disj+
          fresh conde run run* succeed fail reify-name)
  (import (scheme base))
  (begin
    (define-record-type logic-variable (var index) var? (index var-index))

    (define (walk u s)
      (let ((binding (and (var? u) (assp-var u s))))
        (if binding (walk (cdr binding) s) u)))

    (define (assp-var v s)
      (cond ((null? s) #f)
            ((= (var-index (caar s)) (var-index v)) (car s))
            (else (assp-var v (cdr s)))))

    (define (unify u v s)
      (let ((u (walk u s)) (v (walk v s)))
        (cond ((and (var? u) (var? v) (= (var-index u) (var-index v))) s)
              ((var? u) (cons (cons u v) s))
              ((var? v) (cons (cons v u) s))
              ((and (pair? u) (pair? v))
               (let ((s (unify (car u) (car v) s)))
                 (and s (unify (cdr u) (cdr v) s))))
              (else (and (equal? u v) s)))))

    ;; A state is (substitution . next variable index); a stream is (),
    ;; a pair of a state and a stream, or a thunk (immature).
    (define (== u v)
      (lambda (state)
        (let ((s (unify u v (car state))))
          (if s (list (cons s (cdr state))) '()))))

    (define (call/fresh f)
      (lambda (state)
        (let ((c (cdr state)))
          ((f (var c)) (cons (car state) (+ c 1))))))

    (define (mplus a b)
      (cond ((null? a) b)
            ((procedure? a) (lambda () (mplus b (a))))
            (else (cons (car a) (mplus (cdr a) b)))))

    (define (bind stream g)
      (cond ((null? stream) '())
            ((procedure? stream) (lambda () (bind (stream) g)))
            (else (mplus (g (car stream)) (bind (cdr stream) g)))))

    (define (disj g1 g2) (lambda (state) (mplus (g1 state) (g2 state))))
    (define (conj g1 g2) (lambda (state) (bind (g1 state) g2)))

    (define succeed (lambda (state) (list state)))
    (define fail (lambda (state) '()))

    ;; Inverse-eta delay: a goal's expression isn't evaluated until the
    ;; goal is run, so recursive relations terminate.  s/c is a name a
    ;; user might well have too, which hygiene keeps apart.
    (define-syntax zzz
      (syntax-rules ()
        ((_ g) (lambda (s/c) (lambda () (g s/c))))))

    (define-syntax conj+
      (syntax-rules ()
        ((_ g) (zzz g))
        ((_ g0 g ...) (conj (zzz g0) (conj+ g ...)))))

    (define-syntax disj+
      (syntax-rules ()
        ((_ g) (zzz g))
        ((_ g0 g ...) (disj (zzz g0) (disj+ g ...)))))

    (define-syntax conde
      (syntax-rules ()
        ((_ (g0 g ...) ...) (disj+ (conj+ g0 g ...) ...))))

    (define-syntax fresh
      (syntax-rules ()
        ((_ () g0 g ...) (conj+ g0 g ...))
        ((_ (x0 x ...) g0 g ...)
         (call/fresh (lambda (x0) (fresh (x ...) g0 g ...))))))

    (define (pull stream) (if (procedure? stream) (pull (stream)) stream))

    (define (take-all stream)
      (let ((stream (pull stream)))
        (if (null? stream) '() (cons (car stream) (take-all (cdr stream))))))

    (define (take n stream)
      (if (= n 0)
          '()
          (let ((stream (pull stream)))
            (if (null? stream) '() (cons (car stream) (take (- n 1) (cdr stream)))))))

    (define (walk* v s)
      (let ((v (walk v s)))
        (if (pair? v) (cons (walk* (car v) s) (walk* (cdr v) s)) v)))

    (define (reify-name n)
      (string->symbol (string-append "_." (number->string n))))

    (define (reify-s v s)
      (let ((v (walk v s)))
        (cond ((var? v) (cons (cons v (reify-name (length s))) s))
              ((pair? v) (reify-s (cdr v) (reify-s (car v) s)))
              (else s))))

    (define (reify-first state)
      (let ((v (walk* (var 0) (car state))))
        (walk* v (reify-s v '()))))

    (define empty-state '(() . 0))

    (define-syntax run
      (syntax-rules ()
        ((_ n (x) g0 g ...)
         (map reify-first (take n ((fresh (x) g0 g ...) empty-state))))))

    (define-syntax run*
      (syntax-rules ()
        ((_ (x) g0 g ...)
         (map reify-first (take-all ((fresh (x) g0 g ...) empty-state))))))))
