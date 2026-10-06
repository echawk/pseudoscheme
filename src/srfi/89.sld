;;; SRFI 89: optional positional and named parameters.  After Marc
;;; Feeley's implementation in the SRFI document
;;; (reference/srfi-89/srfi-89.scm; MIT licence, per the SRFI document,
;;; in reference/srfi-89/LICENSE).  Its run-time procedures are
;;; included unmodified (reference/srfi-89/srfi-89-runtime.scm).  Its
;;; expansion code is written with define-macro, so it is adapted here
;;; to syntax-case, below: parse-formals and expand-lambda* are the
;;; document's, working on syntax objects instead of symbols (variables
;;; are identifiers, and the introduced $args, $req, $opt and
;;; $key-values are hygienic), and they report errors with
;;; syntax-violation.  make-perfect-hash-table is in
;;; (srfi private srfi-89-keywords), so the transformer can call it.
;;; (The Martin Becze implementation in the SRFI repository's contrib/
;;; is GPL, and isn't used.)
;;;
;;; Keywords: Pseudoscheme has no SRFI 88, so keyword objects are the
;;; self-evaluating #:name keywords (docs/interop.md 3.3).  In a
;;; parameter list a keyword may be written #:name or name: (as in the
;;; SRFI); both mean #:name.  Calls pass #:name:
;;;   (define* (g a (b a) (key: k (* a b))) (list a b k))
;;;   (g 3 4 #:key 5)  =>  (3 4 5)
(define-library (srfi 89)
  (export define* lambda*)
  (import (scheme base)
          (scheme cxr)
          (only (rnrs syntax-case) syntax-case syntax quasisyntax unsyntax
                unsyntax-splicing identifier? bound-identifier=?
                syntax->datum syntax-violation)
          (srfi private srfi-89-keywords))
  (begin
    (define-syntax define*
      (syntax-rules ()
        ((_ (name . formals) . body)
         (define name (lambda* formals . body)))
        ((_ name . body)
         (define name . body))))

    (define-syntax lambda*
      (lambda (form)

        (define (syntax->list* x)  ; a possibly improper list of syntax
          (syntax-case x ()
            ((a . b) (cons #'a (syntax->list* #'b)))
            (() '())
            (_ x)))

        (define (fail message)
          (syntax-violation 'lambda* message form))

        (define (parse-formals formals)

          (define (variable? x)
            (and (identifier? x)
                 (not (syntax-keyword? (syntax->datum x)))))

          (define (keyword-syntax? x)
            (syntax-keyword? (syntax->datum x)))

          (define (required-positional? x)
            (variable? x))

          (define (optional-positional? x)
            (and (pair? x)
                 (pair? (cdr x))
                 (null? (cddr x))
                 (variable? (car x))))

          (define (required-named? x)
            (and (pair? x)
                 (pair? (cdr x))
                 (null? (cddr x))
                 (keyword-syntax? (car x))
                 (variable? (cadr x))))

          (define (optional-named? x)
            (and (pair? x)
                 (pair? (cdr x))
                 (pair? (cddr x))
                 (null? (cdddr x))
                 (keyword-syntax? (car x))
                 (variable? (cadr x))))

          (define (named? x)
            (or (required-named? x)
                (optional-named? x)))

          (define (duplicates? lst same?)
            (cond ((null? lst)
                   #f)
                  ((let loop ((l (cdr lst)))
                     (and (pair? l)
                          (or (same? (car lst) (car l)) (loop (cdr l)))))
                   #t)
                  (else
                   (duplicates? (cdr lst) same?))))

          (define (parse-positional-section lst cont)
            (let loop1 ((lst lst) (rev-reqs '()))
              (if (and (pair? lst)
                       (required-positional? (car lst)))
                  (loop1 (cdr lst) (cons (car lst) rev-reqs))
                  (let loop2 ((lst lst) (rev-opts '()))
                    (if (and (pair? lst)
                             (optional-positional? (syntax->list* (car lst))))
                        (loop2 (cdr lst) (cons (syntax->list* (car lst)) rev-opts))
                        (cont lst (cons (reverse rev-reqs) (reverse rev-opts))))))))

          (define (parse-named-section lst cont)
            (let loop ((lst lst) (rev-named '()))
              (if (and (pair? lst)
                       (named? (syntax->list* (car lst))))
                  (loop (cdr lst) (cons (syntax->list* (car lst)) rev-named))
                  (cont lst (reverse rev-named)))))

          (define (parse-rest lst
                              positional-before-named?
                              positional-reqs/opts
                              named)
            (if (null? lst)
                (parse-end positional-before-named?
                           positional-reqs/opts
                           named
                           #f)
                (if (variable? lst)
                    (parse-end positional-before-named?
                               positional-reqs/opts
                               named
                               lst)
                    (fail "syntax error in formal parameter list"))))

          (define (parse-end positional-before-named?
                             positional-reqs/opts
                             named
                             rest)
            (let ((positional-reqs (car positional-reqs/opts))
                  (positional-opts (cdr positional-reqs/opts)))
              (let ((vars
                     (append positional-reqs
                             (map car positional-opts)
                             (map cadr named)
                             (if rest (list rest) '())))
                    (keys
                     (map (lambda (x) (syntax-keyword (syntax->datum (car x))))
                          named)))
                (cond ((duplicates? vars bound-identifier=?)
                       (fail "duplicate variable in formal parameter list"))
                      ((duplicates? keys eq?)
                       (fail "duplicate keyword in formal parameter list"))
                      (else
                       (list positional-before-named?
                             positional-reqs
                             positional-opts
                             named
                             rest))))))

          (define (parse lst)
            (if (and (pair? lst)
                     (named? (syntax->list* (car lst))))
                (parse-named-section
                 lst
                 (lambda (lst named)
                   (parse-positional-section
                    lst
                    (lambda (lst positional-reqs/opts)
                      (parse-rest lst
                                  #f
                                  positional-reqs/opts
                                  named)))))
                (parse-positional-section
                 lst
                 (lambda (lst positional-reqs/opts)
                   (parse-named-section
                    lst
                    (lambda (lst named)
                      (parse-rest lst
                                  #t
                                  positional-reqs/opts
                                  named)))))))

          (parse (syntax->list* formals)))

        (define (expand-lambda* formals body)

          (define (range lo hi)
            (if (< lo hi)
                (cons lo (range (+ lo 1) hi))
                '()))

          (define (expand positional-before-named?
                          positional-reqs
                          positional-opts
                          named
                          rest)
            (if (and (null? positional-opts) (null? named)) ; direct R5RS equivalent

                #`(lambda #,(append positional-reqs (or rest '())) #,@body)

                (let ()

                  (define utility-fns
                    `(,@(if (or positional-before-named?
                                (null? positional-reqs))
                            `()
                            (list
                             #'($req
                                (lambda ()
                                  (if (pair? $args)
                                      (let ((arg (car $args)))
                                        (set! $args (cdr $args))
                                        arg)
                                      (error "too few actual parameters"))))))
                      ,@(if (null? positional-opts)
                            `()
                            (list
                             #'($opt
                                (lambda (default)
                                  (if (pair? $args)
                                      (let ((arg (car $args)))
                                        (set! $args (cdr $args))
                                        arg)
                                      (default))))))))

                  (define positional-bindings
                    `(,@(if positional-before-named?
                            `()
                            (map (lambda (x)
                                   #`(#,x ($req)))
                                 positional-reqs))
                      ,@(map (lambda (x)
                               #`(#,(car x) ($opt (lambda () #,(cadr x)))))
                             positional-opts)))

                  (define named-bindings
                    (if (null? named)
                        `()
                        (cons
                         #`($key-values
                             (vector #,@(map (lambda (x) #'$undefined)
                                             named)))
                         (cons
                          #`($args
                             ($process-keys
                              $args
                              '#,(make-perfect-hash-table
                                  (map (lambda (x i)
                                         (cons (syntax-keyword
                                                (syntax->datum (car x)))
                                               i))
                                       named
                                       (range 0 (length named))))
                              $key-values))
                          (map (lambda (x i)
                                   #`(#,(cadr x)
                                      #,(if (null? (cddr x))
                                            #`($req-key $key-values #,i)
                                            #`($opt-key $key-values #,i
                                                        (lambda ()
                                                          #,(caddr x))))))
                                 named
                                 (range 0 (length named)))))))

                  (define rest-binding
                    (if (not rest)
                        (list #'($args (or (null? $args)
                                           (error "too many actual parameters"))))
                        (list #`(#,rest $args))))

                  (let ((bindings
                         (append (if positional-before-named?
                                     (append utility-fns
                                             positional-bindings
                                             named-bindings)
                                     (append named-bindings
                                             utility-fns
                                             positional-bindings))
                                 rest-binding)))
                    #`(lambda #,(append (if positional-before-named?
                                            positional-reqs
                                            '())
                                        #'$args)
                        (let* #,bindings
                          #,@body))))))

          (apply expand (parse-formals formals)))

        (syntax-case form ()
          ((_ formals body1 body2 ...)
           (expand-lambda* #'formals (syntax->list* #'(body1 body2 ...))))))))

  (include "reference/srfi-89/srfi-89-runtime.scm"))
