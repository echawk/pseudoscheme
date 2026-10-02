;;; macros.ss -- R6RS (rnrs base) syntax that the vendored syntax-case's
;;; own macro-defs.ss (R5RS-level derived forms) doesn't provide.
;;; Evaluated through the syntax-case expander, so these land in its
;;; global macro table.

;; Sequential initialization, unlike the LETREC that the expander turns
;; internal definitions into.
(define-syntax letrec*
  (syntax-rules ()
    ((_ ((var init) ...) body1 body2 ...)
     (let ((var #f) ...)
       (set! var init) ...
       (let () body1 body2 ...)))))

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

;; (assert expr) returns the value of expr, or raises an assertion
;; violation.
(define-syntax assert
  (syntax-rules ()
    ((_ expr)
     (or expr (assertion-violation #f "assertion failed" 'expr)))))

;; guard (rnrs exceptions); see the note on GUARD in ../r7rs/base.scm
;; about re-raising from the guard rather than from the raise point.
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
                 (guard-aux (raise-continuable condition) clause ...)))))
          (lambda ()
            (call-with-values
             (lambda () e1 e2 ...)
             (lambda args
               (guard-k (lambda () (apply values args)))))))))))))

(define-syntax guard-aux
  (syntax-rules (else =>)
    ((_ reraise (else result1 result2 ...))
     (begin result1 result2 ...))
    ((_ reraise (test => result))
     (let ((temp test)) (if temp (result temp) reraise)))
    ((_ reraise (test => result) clause1 clause2 ...)
     (let ((temp test))
       (if temp (result temp) (guard-aux reraise clause1 clause2 ...))))
    ((_ reraise (test))
     (or test reraise))
    ((_ reraise (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp temp (guard-aux reraise clause1 clause2 ...))))
    ((_ reraise (test result1 result2 ...))
     (if test (begin result1 result2 ...) reraise))
    ((_ reraise (test result1 result2 ...) clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (guard-aux reraise clause1 clause2 ...)))))

;; identifier-syntax is NOT provided: the 1992 expander rejects a macro
;; keyword used as a plain identifier ("invalid context for
;; identifier"), which identifier macros need.  See ROADMAP.md.
