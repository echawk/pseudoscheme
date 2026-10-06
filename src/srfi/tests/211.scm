;;; Tests for SRFI 211, from the SRFI document's examples and its
;;; specification of each library.
(import (scheme base) (scheme cxr) (scheme process-context) (srfi 64)
        (srfi 211 syntax-case)
        (prefix (srfi 211 explicit-renaming) er:)
        (srfi 211 implicit-renaming)
        (srfi 211 identifier-syntax)
        (srfi 211 variable-transformer)
        (prefix (srfi 211 low-level) ll:)
        (srfi 211 presyntax)
        (srfi 211 define-macro)
        (prefix (srfi 211 syntactic-closures) sc:))

(test-begin "srfi-211")

;; The skip example, both ways.  (The SRFI's syntax-case version calls
;; (list-tail n list); the arguments are swapped here.)
(define-syntax skip-sc
  (lambda (x)
    (syntax-case x ()
      ((_ n b ...)
       (with-syntax (((b ...)
                      (list-tail #'(b ...) (syntax->datum #'n))))
         #'(letrec* () b ...))))))
(define-syntax skip-er
  (er:er-macro-transformer
   (lambda (e r c)
     `(,(r 'letrec*) () ,@(list-tail (cddr e) (syntax->datum (cadr e)))))))
(test-equal 3 (skip-sc 2 1 2 3))
(test-equal 3 (skip-er 2 1 2 3))

;; syntax-case accepts ... and _ as literals
(define-syntax lit
  (lambda (x)
    (syntax-case x (... _)
      ((k ...) #''dots)
      ((k _) #''underscore)
      ((k a) #''other))))
(test-equal '(dots underscore other) (list (lit ...) (lit _) (lit 1)))

;; Explicit renaming: hygiene of renamed identifiers
(define-syntax my-or2
  (er:er-macro-transformer
   (lambda (x r c)
     (let ((a (cadr x)) (b (caddr x)))
       `(,(r 'let) ((,(r 'tmp) ,a))
          (,(r 'if) ,(r 'tmp) ,(r 'tmp) ,b))))))
(test-equal 5 (let ((tmp 5)) (my-or2 #f tmp)))
(test-equal 1 (let ((if list)) (my-or2 1 2)))
;; compare
(define-syntax which
  (er:er-macro-transformer
   (lambda (x r c)
     (if (c (cadr x) (r 'else)) ''else ''not-else))))
(test-equal 'else (which else))
(test-equal 'not-else (which foo))
(test-equal 'not-else (let ((else 1)) (which else)))
;; raw symbols in the output are injected: an unhygienic loop with exit
(define-syntax loop
  (er:er-macro-transformer
   (lambda (x r c)
     (let ((body (cdr x)))
       `(,(r 'call-with-current-continuation)
         (,(r 'lambda) (exit)
          (,(r 'let) ,(r 'f) () ,@body (,(r 'f)))))))))
(test-equal 42 (let ((n 0)) (loop (set! n (+ n 1)) (if (= n 42) (exit n)))))
(test-assert (er:identifier? 'foo))
(test-assert (er:identifier? #'foo))
(test-assert (not (er:identifier? 1)))

;; Implicit renaming
(define-syntax while
  (ir-macro-transformer
   (lambda (x inject compare)
     (let ((test (cadr x)) (body (cddr x)))
       `(call-with-current-continuation
         (lambda (,(inject 'break))
           (let lp ()
             (when ,test ,@body (lp)))))))))
(test-equal 10
  (let ((i 0))
    (while #t (set! i (+ i 1)) (if (= i 10) (break i)))))
(define-syntax swap!
  (ir-macro-transformer
   (lambda (x inject compare)
     `(let ((tmp ,(cadr x)))
        (set! ,(cadr x) ,(caddr x))
        (set! ,(caddr x) tmp)))))
(test-equal '(2 1) (let ((tmp 1) (y 2)) (swap! tmp y) (list tmp y)))

;; Identifier syntax and variable transformers
(define counter 0)
(define-syntax count (identifier-syntax (begin (set! counter (+ counter 1)) counter)))
(test-equal '(1 2) (list count count))
(define store 0)
(define-syntax box-var
  (make-variable-transformer
   (lambda (x)
     (syntax-case x (set!)
       ((set! _ v) #'(set! store (* 2 v)))
       (id (identifier? #'id) #'store)))))
(set! box-var 21)
(test-equal 42 box-var)
(define-syntax er-var
  (make-variable-transformer
   (er:er-macro-transformer
    (lambda (x r c)
      (if (pair? x) `(,(r 'quote) set) `(,(r 'quote) ref))))))
(test-equal 'ref er-var)

;; Low level
(test-assert (ll:identifier? (ll:syntax x)))
(test-equal 'x (ll:identifier->symbol (ll:syntax x)))
(test-assert (ll:free-identifier=? (ll:syntax car) (ll:syntax car)))
(test-assert (ll:identifier? (ll:generate-identifier)))
(test-assert (symbol? (ll:identifier->symbol (ll:generate-identifier 'foo))))
(test-assert (not (ll:bound-identifier=? (ll:generate-identifier 'a)
                                         (ll:generate-identifier 'a))))
(test-assert (ll:bound-identifier=? (ll:construct-identifier (ll:syntax q) 'z)
                                    (ll:construct-identifier (ll:syntax q) 'z)))
(test-assert (pair? (ll:unwrap-syntax (ll:syntax (a b)))))
(define-syntax ll-first
  (lambda (x)
    (let ((form (ll:unwrap-syntax x)))
      (car (ll:unwrap-syntax (cdr form))))))
(test-equal 7 (ll-first 7 8))

;; Presyntax
(test-assert (preidentifier? 'a))
(test-assert (preidentifier? #'a))
(test-assert (not (preidentifier? "a")))
(test-equal '(a b #(c 1)) (presyntax->datum (list 'a #'b (vector #'c 1))))
(test-equal 'a (preidentifier->symbol 'a))
(test-equal 'b (preidentifier->symbol #'b))
(test-equal 'c (unwrap-presyntax 'c))

;; define-macro and lisp-transformer
(define-macro (my-when test . body) `(if ,test (begin ,@body) #f))
(test-equal 'yes (my-when #t 'yes))
(test-equal #f (my-when #f 'yes))
(define-macro twice (lambda (form) `(list ,(cadr form) ,(cadr form))))
(test-equal '(3 3) (twice 3))

;; Syntactic closures
(define-syntax push!
  (sc:sc-macro-transformer
   (lambda (exp env)
     (let ((item (sc:make-syntactic-closure env '() (cadr exp)))
           (lst (sc:close-syntax (caddr exp) env)))
       `(set! ,lst (cons ,item ,lst))))))
(test-equal '(1 2) (let ((l '(2)) (cons list)) (push! 1 l) l))
(define-syntax rsc-if
  (sc:rsc-macro-transformer
   (lambda (exp env)
     `(,(sc:close-syntax 'if env) ,@(cdr exp)))))
(test-equal 'b (let ((if list)) (rsc-if #f 'a 'b)))
(define-syntax sc-loop-until
  (sc:sc-macro-transformer
   (lambda (exp env)
     (let ((id (cadr exp))
           (init (sc:close-syntax (caddr exp) env))
           (done (sc:make-syntactic-closure env (list (cadr exp)) (cadddr exp))))
       `(let lp ((,id ,init))
          (if ,done ,id (lp (+ ,id 1))))))))
(test-equal 5 (sc-loop-until i 0 (= i 5)))
(define-syntax sc-test-env
  (sc:sc-macro-transformer
   (lambda (exp env)
     (if (sc:identifier=? env (cadr exp) env 'else) ''else ''other))))
(test-equal 'else (sc-test-env else))
(test-equal 'other (sc-test-env x))
(test-assert (sc:identifier? (sc:make-synthetic-identifier 'a)))
(define-syntax capture-test
  (sc:sc-macro-transformer
   (lambda (exp env)
     (sc:capture-syntactic-environment
      (lambda (env2) ''captured)))))
(test-equal 'captured (capture-test))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-211")
  (exit (if (zero? failures) 0 1)))
