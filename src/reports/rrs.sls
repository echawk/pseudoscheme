;;; -*- Mode: Scheme -*-
;;;; The Revised Report on Scheme (AI Memo 452, 1978): its magic forms
;;;; and syntactic extensions, and the primitives it names.  docs/reports.md.

;;; The procedures: what the report's primitives are (it says "all the
;;; usual MacLISP SUBRs"; these are the ones its examples use, and the
;;; ones it names), and the processes.
(library (pseudoscheme reports rrs runtime)
  (export fluid-ref fluid-set! call-with-fluids
          register-expander! lookup-expander
          amapcar amaplist amapc nreverse
          atom null eq equal rplaca rplacd list-of-all
          mapcar mapc maplist minus zerop minusp plusp
          +$ -$ *$ //$ gensym print terpri error
          procp enclose
          create!process start!process stop!process terminate **process**)
  (import (rnrs) (rnrs mutable-pairs) (rnrs eval)
          (prefix (only (pseudoscheme host) gensym) %)
          (prefix (only (rnrs) error) r6:))

  ;; dynamic (fluid) variables: an association list, shadowed by
  ;; FLUIDBIND for as long as its body runs
  (define fluid-env '())
  (define (fluid-ref name)
    (let ((p (assq name fluid-env)))
      (if p (cdr p) (r6:error 'fluid "unbound fluid variable" name))))
  (define (fluid-set! name value)
    (let ((p (assq name fluid-env)))
      (if p (set-cdr! p value) (set! fluid-env (cons (cons name value) fluid-env)))))
  (define (call-with-fluids names values thunk)
    (let ((outer fluid-env) (inner (map cons names values)))
      (dynamic-wind
       (lambda () (set! outer fluid-env) (set! fluid-env (append inner outer)))
       thunk
       (lambda () (set! fluid-env outer)))))

  ;; the expanders MACRO defines: the procedure from (macro name pattern
  ;; body) is kept here, and what the keyword name expands to calls it
  (define expanders '())
  (define (register-expander! name procedure)
    (let ((p (assq name expanders)))
      (if p (set-cdr! p procedure) (set! expanders (cons (cons name procedure) expanders)))))
  (define (lookup-expander name)
    (let ((p (assq name expanders)))
      (if p (cdr p) (r6:error 'macro "no expander" name))))

  (define (any-null? lists)
    (and (pair? lists) (or (null? (car lists)) (any-null? (cdr lists)))))

  (define (amapcar f . lists)
    (let loop ((lists lists) (result '()))
      (if (any-null? lists)
          (reverse result)
          (loop (map cdr lists) (cons (apply f (map car lists)) result)))))
  (define (amaplist f . lists)
    (let loop ((lists lists) (result '()))
      (if (any-null? lists)
          (reverse result)
          (loop (map cdr lists) (cons (apply f lists) result)))))
  (define (amapc f . lists)
    (let loop ((lists lists))
      (if (any-null? lists)
          '()
          (begin (apply f (map car lists)) (loop (map cdr lists))))))
  (define mapcar amapcar)
  (define mapc amapc)
  (define maplist amaplist)

  (define (nreverse l) (reverse l))
  (define (atom x) (not (pair? x)))
  (define (null x) (null? x))
  (define (eq x y) (eq? x y))
  (define (equal x y) (equal? x y))
  (define (rplaca p x) (set-car! p x) p)
  (define (rplacd p x) (set-cdr! p x) p)
  (define (list-of-all . x) x)
  (define (minus x) (- x))
  (define (zerop x) (zero? x))
  (define (minusp x) (negative? x))
  (define (plusp x) (positive? x))
  (define (+$ . x) (apply + x))
  (define (-$ . x) (apply - x))
  (define (*$ . x) (apply * x))
  (define (//$ . x) (apply / x))
  (define (gensym) (%gensym))
  (define (print x) (write x) (newline) x)
  (define (terpri) (newline))
  (define (error message . objects)
    (apply r6:error 'scheme (if (string? message) message (call-with-string-output-port (lambda (p) (write message p)))) objects))

  ;; PROCP: true of procedures; for a Scheme procedure the report has it
  ;; return the number of arguments, "if the number of arguments cannot
  ;; be computed ... T"
  (define (procp x) (procedure? x))

  ;; (enclose '(lambda (x) ...) '((var . value) ...)): the lambda
  ;; expression closed in the global environment extended by the a-list
  (define enclose-environment #f)
  (define (enclose lambda-expression alist)
    (unless enclose-environment
      (set! enclose-environment (environment '(pseudoscheme reports rrs))))
    (eval `((lambda ,(map car alist) ,lambda-expression)
            ,@(map (lambda (binding) (list 'quote (cdr binding))) alist))
          enclose-environment))

  ;; Processes, as coroutines: START!PROCESS runs the process, until it
  ;; stops, terminates, or starts another, and the process that started
  ;; it is runnable meanwhile; TERMINATE and STOP!PROCESS of the running
  ;; process pass control to a runnable one, or, if none is, back to the
  ;; top level.  (There is no preemption.)
  (define **process** 0)
  (define processes '())               ; (id state continuation-or-thunk) vectors
  (define next-id 1)
  (define run-queue '())               ; ids of runnable processes
  (define top-level-k #f)

  (define (process-ref id)
    (let loop ((ps processes))
      (cond ((null? ps) (r6:error 'process "no such process" id))
            ((eqv? (vector-ref (car ps) 0) id) (car ps))
            (else (loop (cdr ps))))))

  (define (create!process thunk)
    (let ((id next-id))
      (set! next-id (+ id 1))
      (set! processes (cons (vector id 'created thunk) processes))
      id))

  ;; run the next runnable process, if any, else go back to the top level
  (define (dispatch)
    (if (null? run-queue)
        (if top-level-k (top-level-k #f) (r6:error 'process "no process to run"))
        (let ((id (car run-queue)))
          (set! run-queue (cdr run-queue))
          (resume id))))

  (define (resume id)
    (let* ((p (process-ref id)) (what (vector-ref p 2)))
      (set! **process** id)
      (if (eq? (vector-ref p 1) 'created)
          (begin
            (vector-set! p 1 'running)
            (what)
            (terminate))
          (begin (vector-set! p 1 'running) (what #f)))))

  (define (suspend-current-and-run id queue-self?)
    (let ((me **process**))
      (call-with-current-continuation
       (lambda (k)
         (when (and (not (eqv? me 0)) (eq? (vector-ref (process-ref me) 1) 'running))
           (vector-set! (process-ref me) 2 k)
           (vector-set! (process-ref me) 1 (if queue-self? 'runnable 'stopped))
           (when queue-self? (set! run-queue (append run-queue (list me)))))
         (when (eqv? me 0)
           (set! top-level-k k))
         (resume id)))
      (set! **process** me)))

  (define (start!process id)
    (let ((p (process-ref id)))
      (cond ((eqv? id **process**) #f)
            ((eq? (vector-ref p 1) 'dead) #f)
            (else (set! run-queue (remq-id id run-queue))
                  (suspend-current-and-run id #t)))))

  (define (remq-id id l)
    (cond ((null? l) '()) ((eqv? (car l) id) (cdr l)) (else (cons (car l) (remq-id id (cdr l))))))

  (define (stop!process id)
    (let ((p (process-ref id)))
      (set! run-queue (remq-id id run-queue))
      (if (eqv? id **process**)
          (call-with-current-continuation
           (lambda (k)
             (vector-set! p 2 k)
             (vector-set! p 1 'stopped)
             (dispatch)))
          (when (memq (vector-ref p 1) '(runnable running))
            (vector-set! p 1 'stopped)))))

  (define (terminate)
    (let ((me **process**))
      (unless (eqv? me 0)
        (vector-set! (process-ref me) 1 'dead))
      (set! **process** 0)
      (dispatch))))

(library (pseudoscheme reports rrs)
  (export lambda if quote labels define aset fluidbind fluid fluidset static catch
          block let do iterate test cond => or and amapcar amaplist amapc prog
          schmac macro
          procp enclose create!process start!process stop!process terminate **process**
          t nil not
          atom null eq equal rplaca rplacd
          car cdr cons caar cadr cdar cddr caddr cdddr cadddr
          list append reverse nreverse length memq assq mapcar mapc maplist
          + - * // = < > <= >= abs minus 1+ -1+ zerop minusp plusp max min sqrt
          +$ -$ *$ //$ gensym print terpri error read gcd)
  (import (except (rnrs) if cond and or do not define error)
          (prefix (only (rnrs) define) r6:)
          (pseudoscheme reports truth)
          (only (pseudoscheme reports extras) 1+ -1+)
          (pseudoscheme reports rrs runtime))

  (r6:define t #t)
  (r6:define nil '())
  (r6:define // /)

  ;; (define (name arg ...) form ...), (define name (arg ...) form ...), and
  ;; (define name (lambda ...))
  (define-syntax define
    (syntax-rules ()
      ((_ (name . formals) form1 form2 ...)
       (r6:define (name . formals) form1 form2 ...))
      ((_ name (formal ...) form1 form2 ...)
       (r6:define (name formal ...) form1 form2 ...))
      ((_ name expr) (r6:define name expr))))

  (define-syntax labels
    (syntax-rules ()
      ((_ ((name lambda-expression) ...) body1 body2 ...)
       (letrec ((name lambda-expression) ...) body1 body2 ...))))

  ;; (aset' x e) is read as (aset (quote x) e): the reader takes ' as a
  ;; quote of the next datum, as MacLISP's does
  (define-syntax aset
    (syntax-rules (quote)
      ((_ (quote v) e) (set! v e))
      ((_ v e) (set! v e))))

  (define-syntax fluidbind
    (syntax-rules ()
      ((_ ((v e) ...) form1 form2 ...)
       (call-with-fluids '(v ...) (list e ...) (lambda () form1 form2 ...)))))
  (define-syntax fluid
    (syntax-rules ()
      ((_ v) (fluid-ref 'v))))
  (define-syntax fluidset
    (syntax-rules (quote)
      ((_ (quote v) e) (fluid-set! 'v e))
      ((_ v e) (fluid-set! 'v e))))

  (define-syntax static
    (syntax-rules ()
      ((_ v) v)))

  (define-syntax catch
    (syntax-rules ()
      ((_ id form) (call-with-current-continuation (lambda (id) form)))))

  (define-syntax block
    (syntax-rules ()
      ((_ form1 form2 ...) (begin form1 form2 ...))))

  (define-syntax iterate
    (syntax-rules ()
      ((_ name ((v e) ...) body1 body2 ...)
       ((letrec ((name (lambda (v ...) body1 body2 ...))) name) e ...))))

  (define-syntax test
    (syntax-rules ()
      ((_ pred fn alt)
       (let ((p pred) (f (lambda () fn)) (a (lambda () alt)))
         (if p ((f) p) (a))))))

  ;; (macro name pattern body): pattern is a list of variables, as for
  ;; a lambda, that what follows the keyword is matched against; body
  ;; (Scheme) returns the form to evaluate instead.  schmac is the
  ;; same, its body being Scheme too.
  (define-syntax macro
    (lambda (x)
      (syntax-case x ()
        ((_ name pattern body)
         #'(begin
             (register-expander! 'name (lambda pattern body))
             (define-syntax name
               (lambda (use)
                 (syntax-case use ()
                   ((keyword . rest)
                    (datum->syntax #'keyword
                                   (apply (lookup-expander 'name) (syntax->datum #'rest))))))))))))
  (define-syntax schmac
    (syntax-rules ()
      ((_ name pattern body) (macro name pattern body))))

  ;; (prog (var ...) statement ...): the variables start out as NIL; a
  ;; statement that is an atom is a tag; (go tag) goes to the statement
  ;; after it, (return value) leaves the prog, and falling off its end
  ;; returns NIL.  go and return are bound in the prog, to the user's names.
  (define-syntax prog
    (lambda (x)
      (define (unwrap s)
        (syntax-case s ()
          (() '())
          ((a . d) (cons #'a (unwrap #'d)))))
      ;; statements -> ((tag . statements) ...), the first segment's tag #f
      (define (split statements)
        (let loop ((l statements) (tag #f) (current '()) (segments '()))
          (cond ((null? l) (reverse (cons (cons tag (reverse current)) segments)))
                ((identifier? (car l))
                 (loop (cdr l) (car l) '() (cons (cons tag (reverse current)) segments)))
                (else (loop (cdr l) tag (cons (car l) current) segments)))))
      (syntax-case x ()
        ((k (var ...) statement ...)
         (let* ((segments (split (unwrap #'(statement ...))))
                (names (generate-temporaries segments))
                (bodies (let loop ((segs segments) (names names))
                          (if (null? segs)
                              '()
                              (cons (let ((stmts (cdr (car segs))))
                                      (if (null? (cdr names))
                                          #`(lambda () #,@stmts '())
                                          #`(lambda () #,@stmts (#,(cadr names)))))
                                    (loop (cdr segs) (cdr names))))))
                (assignments (map (lambda (name body) #`(set! #,name #,body)) names bodies)))
           (with-syntax ((go (datum->syntax #'k 'go))
                         (return (datum->syntax #'k 'return))
                         ((escape) (generate-temporaries '(escape)))
                         (first (car names))
                         ((assignment ...) assignments)
                         ((name ...) names)
                         ((tag ...) (map car (cdr segments)))
                         ((tag-name ...) (cdr names)))
             #'(let ((var '()) ...)
                 (call-with-current-continuation
                  (lambda (escape)
                    (let ((name #f) ...)
                      (let-syntax ((go (syntax-rules (tag ...)
                                         ((_ tag) (tag-name)) ...))
                                   (return (syntax-rules ()
                                             ((_ v) (escape v)))))
                        assignment ...
                        (first)))))))))))))
