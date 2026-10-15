;;; -*- Mode: Scheme -*-
;;;; (chezscheme): more of Chez's syntactic forms -- record-case,
;;;; meta-cond and extend-syntax.  Part of the library's body
;;;; (src/chez/chez.lisp).  (define-structure, datum and with-implicit are
;;;; in chezscheme.scm and top-level.scm.)
;;;;
;;;;   (record-case exp [key formals body ...] ... [else body ...])
;;;;   (meta-cond [test body ...] ... [else body ...])
;;;;   (extend-syntax (name key ...) [pattern [fender] template] ...)

;;; record-case: EXP, a pair, is dispatched on its car, compared with
;;; eqv? to KEY (or to each of a list of keys); the clause's formals are
;;; bound to the cdr's elements.  No clause matching and no else: void.

(define-syntax record-case
  (syntax-rules ()
    ((_ exp clause ...)
     (let ((r exp)) (record-case-clauses r clause ...)))))

(define-syntax record-case-clauses
  (syntax-rules (else)
    ((_ r) (void))
    ((_ r (else e1 e2 ...)) (let () e1 e2 ...))
    ((_ r ((key ...) formals e1 e2 ...) clause ...)
     (if (memv (car r) '(key ...))
         (apply (lambda formals e1 e2 ...) (cdr r))
         (record-case-clauses r clause ...)))
    ((_ r (key formals e1 e2 ...) clause ...)
     (if (eqv? (car r) 'key)
         (apply (lambda formals e1 e2 ...) (cdr r))
         (record-case-clauses r clause ...)))))

;;; meta-cond: the first clause whose test is true when the form is
;;; expanded (the test is evaluated then, as a transformer's code is) is
;;; what the form expands into: its body, spliced.  No true test and no
;;; else: (void).  Each test is the transformer of a local macro, so that
;;; it may use syntax objects (#'id, free-identifier=?).

(define-syntax meta-cond
  (syntax-rules (else)
    ((_) (void))
    ((_ (else e ...)) (begin e ...))
    ((_ (test e ...) clause ...)
     (let-syntax ((m (lambda (x)
                       (syntax-case x ()
                         ((_ a b) (if test #'a #'b))))))
       (m (begin e ...) (meta-cond clause ...))))))

;;; extend-syntax, Chez's old macro facility (CSUG's, TSPL's appendix):
;;;
;;;   (extend-syntax (name key ...) [pattern [fender] template] ...)
;;;
;;; defines the macro NAME.  A pattern is (name . p), p made of
;;; identifiers (pattern variables, or keys, which match themselves),
;;; constants, pairs and `x ...', which must end a list.  A clause is
;;; used if its pattern matches and its fender, an expression, is true.
;;; The template is substituted for the form: pattern variables are
;;; replaced, and `x ...' is repeated for the variables it holds.  The
;;; expansion's identifiers are those of the use (it isn't hygienic, as
;;; the old facility wasn't).  Anywhere in a template,
;;;
;;;   (with ([p e] ...) template)
;;;
;;; binds the pattern p (as a pattern's) to the value of the expression
;;; e, in turn, and is replaced by the template; in e, a quoted datum
;;; 'x is a template of its own giving a datum (so '(x ...) is the list
;;; of x's values).  The form is matched, and the expansion made, as
;;; plain data, with environments of (variable depth . value).

;; The helpers are in (pseudoscheme chez extend-syntax),
;; src/chez/extend-syntax.sls: a transformer can't use definitions of
;; the library it is defined in.

(define-syntax extend-syntax
  (lambda (x)
    (define (with-form? t)
      (and (pair? t) (eq? (car t) 'with)
           (pair? (cdr t)) (list? (cadr t))
           (pair? (cddr t)) (null? (cdddr t))))
    (define (has-with? t)
      (or (with-form? t)
          (and (pair? t) (or (has-with? (car t)) (has-with? (cdr t))))))
    ;; the code, over the variable es$env, making template T's datum
    (define (gen t)
      (cond ((not (has-with? t)) `(es$subst ',t es$env))
            ((with-form? t) (gen-with t))
            ((pair? t)
             (if (and (pair? (cdr t)) (eq? (cadr t) '...))
                 `(append (es$map ',(car t) es$env (lambda (es$env) ,(gen (car t))))
                          ,(gen (cddr t)))
                 `(cons ,(gen (car t)) ,(gen (cdr t)))))
            (else `',t)))
    (define (gen-with t)
      (let loop ((bs (cadr t)))
        (cond ((null? bs) (gen (caddr t)))
              ((and (pair? bs) (pair? (car bs)) (pair? (cdar bs)) (null? (cddar bs)))
               `(let ((es$env (es$bind ',(caar bs) ,(rw (cadar bs)) es$env)))
                  ,(loop (cdr bs))))
              (else (syntax-violation 'extend-syntax "invalid with" t)))))
    ;; the expression E, a quoted datum in it being a template's datum
    (define (rw e)
      (cond ((and (pair? e) (eq? (car e) 'quote) (pair? (cdr e)) (null? (cddr e)))
             (gen (cadr e)))
            ((pair? e) (cons (rw (car e)) (rw (cdr e))))
            (else e)))
    (define (clause-code name keys c next)
      (unless (and (pair? c) (pair? (car c)) (pair? (cdr c))
                   (or (null? (cddr c)) (and (pair? (cddr c)) (null? (cdddr c)))))
        (syntax-violation 'extend-syntax "invalid clause" x c))
      (let ((pat (car c))
            (fender (and (pair? (cddr c)) (cadr c)))
            (template (if (pair? (cddr c)) (caddr c) (cadr c))))
        (es-check-pattern (cdr pat) keys x)
        `(let ((es$env (and (pair? es$f) (es$match ',(cdr pat) (cdr es$f) ',keys '()))))
           (if ,(if fender `(and es$env ,(rw fender)) 'es$env)
               (es$wrap es$x ,(gen template))
               ,next))))
    (syntax-case x ()
      ((k (name key ...) clause ...)
       (and (identifier? #'name) (for-all identifier? #'(key ...)))
       (let ((keys (syntax->datum #'(key ...)))
             (clauses (syntax->datum #'(clause ...))))
         (when (memq '... keys)
           (syntax-violation 'extend-syntax "invalid keyword list" x))
         (with-syntax (((es$x es$f es$env es$match es$subst es$map es$bind es$wrap)
                        (map (lambda (s) (datum->syntax #'k s))
                             '(es$x es$f es$env es$match es$subst es$map es$bind es$wrap)))
                       (code (datum->syntax #'k
                               (let loop ((cs clauses))
                                 (if (null? cs)
                                     `(syntax-violation ',(syntax->datum #'name) "invalid syntax" es$x)
                                     (clause-code (syntax->datum #'name) keys (car cs)
                                                  (loop (cdr cs))))))))
           #'(define-syntax name
               (lambda (es$x)
                 (let ((es$match es-match) (es$subst es-subst) (es$map es-map)
                       (es$bind es-bind) (es$wrap es-wrap)
                       (es$f (syntax->datum es$x)))
                   code)))))))))

;; (windows?): whether the machine is Windows, for meta-cond tests
(define (windows?) #f)
