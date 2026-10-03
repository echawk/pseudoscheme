;;; (srfi private shim): what the old SRFI reference implementations
;;; assume of their host -- Scheme48's CHECK-ARG, :OPTIONAL and
;;; LET-OPTIONALS -- so they can be included unmodified.

(define-library (srfi private shim)
  (export check-arg :optional let-optionals let-optionals* receive %char->latin1 %latin1->char)
  (import (scheme base))
  (begin
    ;; (check-arg pred val caller): VAL if (PRED VAL), else an error.
    (define (check-arg pred val caller)
      (if (pred val)
          val
          (error "bad argument" val caller)))

    ;; (:optional rest default [arg-test])
    (define-syntax :optional
      (syntax-rules ()
        ((_ rest default)
         (let ((r rest)) (if (pair? r) (car r) default)))
        ((_ rest default test)
         (let ((r rest))
           (if (pair? r)
               (let ((v (car r)))
                 (if (test v) v (error "optional argument failed its test" v)))
               default)))))

    ;; (let-optionals* rest ((var default [test]) ... [rest-var]) body ...)
    (define-syntax let-optionals*
      (syntax-rules ()
        ((_ rest () body1 body2 ...)
         (let () body1 body2 ...))
        ;; scsh: ((var ...) parser) -- (parser rest) returns the
        ;; leftover list and then the variables' values
        ((_ rest (((var1 var2 ...) parser) . more) body1 body2 ...)
         (call-with-values (lambda () (parser rest))
           (lambda (r var1 var2 ...) (let-optionals* r more body1 body2 ...))))
        ((_ rest ((var default test) . more) body1 body2 ...)
         (let* ((r rest)
                (var (if (pair? r) (car r) default)))
           (if (not test) (error "optional argument failed its test" 'var))
           (let-optionals* (if (pair? r) (cdr r) '()) more body1 body2 ...)))
        ((_ rest ((var default) . more) body1 body2 ...)
         (let* ((r rest)
                (var (if (pair? r) (car r) default)))
           (let-optionals* (if (pair? r) (cdr r) '()) more body1 body2 ...)))
        ;; scsh: a rest variable after the specs, ((var default) ... rest)
        ((_ rest (rest-var) body1 body2 ...)
         (let ((rest-var rest)) body1 body2 ...))
        ((_ rest rest-var body1 body2 ...)
         (let ((rest-var rest)) body1 body2 ...))))

    ;; let-optionals: the same, as the reference implementations use it.
    (define-syntax let-optionals
      (syntax-rules ()
        ((_ rest specs body1 body2 ...)
         (let-optionals* rest specs body1 body2 ...))))

    (define-syntax receive
      (syntax-rules ()
        ((_ formals expr body1 body2 ...)
         (call-with-values (lambda () expr) (lambda formals body1 body2 ...)))))

    (define (%char->latin1 c) (char->integer c))
    (define (%latin1->char i) (integer->char i))))
