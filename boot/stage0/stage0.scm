;;; boot/stage0/stage0.scm -- run psyntax on a Scheme without R6RS.
;;;
;;; psyntax is written as R6RS libraries that use syntax-case, so the
;;; first image of it has to come from somewhere that can run those
;;; sources.  boot/psyntax.sh normally gets it from Chez Scheme, which
;;; has R6RS libraries natively.  This file gets it from any R7RS-small
;;; Scheme instead: it reads psyntax's libraries and build script,
;;; flattens them into plain top-level definitions, expands the few
;;; macros they define (syntax-rules macros, and four procedural ones
;;; rewritten here by hand), and evaluates the result in the host.  Then
;;; psyntax, running in the host, runs its own build script, which
;;; expands psyntax's sources and writes psyntax-pseudoscheme.pp: a seed
;;; image.  Pseudoscheme rebuilds from the seed until the image
;;; reproduces itself, so nothing of this stage survives into the result;
;;; boot/psyntax.sh checks that it comes out the same as from Chez's seed.
;;;
;;; This is not a general expander.  It is just enough for psyntax's own
;;; sources, which define few macros:
;;;
;;;  - syntax-rules macros (builders.ss, config.ss, the build script's
;;;    define-prims, two in expander.ss), expanded with renaming: every
;;;    symbol a template introduces becomes a fresh alias that resolves
;;;    where the macro was defined;
;;;  - parameterize and define-record (compat.ss), stx-error and
;;;    syntax-match (expander.ss), written with syntax-case there and
;;;    rewritten as transformers here; and no-source, an identifier
;;;    macro for #f.
;;;
;;; Every local variable gets a fresh name, so nothing the expansion
;;; introduces can be captured.  Each library's definitions are renamed
;;; library:name.  R6RS procedures an R7RS host lacks (or has with other
;;; arguments, like error) are defined here as r6:name.
;;;
;;; A host adapter (boot/stage0/hosts/*.scm) defines, before loading
;;; this file:
;;;   (s0:host-eval form)          evaluate FORM at top level
;;;   (s0:make-table)              a mutable table keyed by eq?
;;;   (s0:table-ref table key default)
;;;   (s0:table-set! table key value)
;;;   (s0:table-delete! table key)
;;;   (s0:table-keys table)        a list of the keys
;;; and calls (s0:build) in the directory holding psyntax's sources
;;; (psyntax/*.ss and psyntax-buildscript.ss), which writes
;;; psyntax-pseudoscheme.pp there.

;;; ------------------------------------------------------------------
;;; Utilities

(define (s0:die msg . irritants)
  (display "stage0: " (current-error-port))
  (display msg (current-error-port))
  (for-each (lambda (x) (display " " (current-error-port)) (write x (current-error-port)))
            irritants)
  (newline (current-error-port))
  (error "stage0 failed" msg))

(define s0:counter 0)

(define (s0:fresh base)
  ;; A new symbol, base%N.
  (set! s0:counter (+ s0:counter 1))
  (string->symbol (string-append (symbol->string base) "%" (number->string s0:counter))))

(define (s0:proper-list? x)
  (or (null? x) (and (pair? x) (s0:proper-list? (cdr x)))))

(define (s0:read-file name)
  (call-with-input-file name
    (lambda (port)
      (let loop ((forms '()))
        (let ((x (read port)))
          (if (eof-object? x) (reverse forms) (loop (cons x forms))))))))

;;; ------------------------------------------------------------------
;;; Aliases: symbols a macro expansion introduced.  An alias resolves
;;; as its original symbol does in the environment and library where the
;;; macro was defined, unless the expansion bound the alias itself.

(define s0:aliases (s0:make-table))

(define (s0:make-alias original env lib)
  (let ((a (s0:fresh (s0:strip original))))
    (s0:table-set! s0:aliases a (list original env lib))
    a))

(define (s0:alias? x) (and (symbol? x) (s0:table-ref s0:aliases x #f) #t))
(define (s0:alias-info x) (s0:table-ref s0:aliases x #f))

(define (s0:strip x)
  ;; X with its aliases replaced by the symbols they stand for: for
  ;; quoted data, and to compare names.
  (cond ((s0:alias? x) (s0:strip (car (s0:alias-info x))))
        ((pair? x)
         (let ((a (s0:strip (car x))) (d (s0:strip (cdr x))))
           (if (and (eq? a (car x)) (eq? d (cdr x))) x (cons a d))))
        ((vector? x) (list->vector (map s0:strip (vector->list x))))
        (else x)))

(define (s0:same-name? x name)
  (and (symbol? x) (eq? (s0:strip x) name)))

;;; ------------------------------------------------------------------
;;; Libraries
;;;
;;; A library is a vector: name, prefix, exports, imports, bindings.
;;; BINDINGS is a table from a symbol to its binding:
;;;   (global . host-symbol)   a variable
;;;   (macro . transformer)    TRANSFORMER: (lambda (form env lib) form)
;;;   (keyword . name)         core syntax

(define (s0:make-library name prefix exports imports)
  (vector name prefix exports imports (s0:make-table)))
(define (s0:lib-name l) (vector-ref l 0))
(define (s0:lib-prefix l) (vector-ref l 1))
(define (s0:lib-exports l) (vector-ref l 2))
(define (s0:lib-imports l) (vector-ref l 3))
(define (s0:lib-bindings l) (vector-ref l 4))

(define s0:libraries '())		; (name . library)

(define (s0:find-library name)
  (let ((p (assoc name s0:libraries)))
    (and p (cdr p))))

(define s0:keywords
  '(quote quasiquote unquote unquote-splicing lambda case-lambda define
    define-syntax set! if begin let let* letrec letrec* let-values
    let*-values do cond case and or when unless else => assert
    syntax-rules delay))

;;; psyntax's host primitives (compat.ss imports them from (psyntax
;;; system $bootstrap)), defined below as s0:name.
(define s0:bootstrap-names
  '(void gensym eval-core symbol-value set-symbol-value! pretty-print
    lisp-keyword? host-literal?))

;;; R6RS procedures defined below as r6:name.
(define s0:r6rs-names
  '(error assertion-violation for-all exists find filter partition remq
    remv remove remp memp assp fold-left fold-right cons* list-sort
    vector-sort make-eq-hashtable make-eqv-hashtable make-hashtable
    hashtable-ref hashtable-set! hashtable-update! hashtable-delete!
    hashtable-contains? hashtable-keys hashtable-size hashtable?
    open-string-output-port call-with-string-output-port
    string-hash equal-hash symbol-hash div mod
    syntax-violation condition? file-exists? delete-file))

(define (s0:import-allows? spec name)
  ;; Whether import SPEC, e.g. (psyntax compat) or (only (psyntax x) a b),
  ;; brings in NAME (for the libraries we model).
  (cond ((and (pair? spec) (eq? (car spec) 'only))
         (memq name (cddr spec)))
        ((and (pair? spec) (eq? (car spec) 'except))
         (not (memq name (cddr spec))))
        (else #t)))

(define (s0:import-library spec)
  (cond ((and (pair? spec) (memq (car spec) '(only except prefix rename)))
         (s0:import-library (cadr spec)))
        (else spec)))

(define (s0:export-binding lib name)
  (and (memq name (s0:lib-exports lib))
       (s0:resolve-free name lib)))

(define (s0:resolve-free s lib)
  (or (s0:table-ref (s0:lib-bindings lib) s #f)
      (let loop ((imports (s0:lib-imports lib)))
        (and (pair? imports)
             (let* ((spec (car imports))
                    (name (s0:import-library spec))
                    (other (s0:find-library name)))
               (or (and other (s0:import-allows? spec s) (s0:export-binding other s))
                   (and (equal? name '(psyntax system $bootstrap))
                        (s0:import-allows? spec s)
                        (memq s s0:bootstrap-names)
                        (cons 'global (string->symbol
                                       (string-append "s0:" (symbol->string s)))))
                   (loop (cdr imports))))))
      (and (memq s s0:keywords) (cons 'keyword s))
      (and (memq s s0:r6rs-names)
           (cons 'global (string->symbol (string-append "r6:" (symbol->string s)))))
      (cons 'global s)))

;;; ------------------------------------------------------------------
;;; Environments: a list of frames, each a table from symbols to
;;; bindings: (local . host-symbol) or (macro . transformer).

(define (s0:new-frame) (s0:make-table))

(define (s0:lookup s env)
  (and (pair? env)
       (or (s0:table-ref (car env) s #f)
           (s0:lookup s (cdr env)))))

(define (s0:resolve s env lib)
  (or (s0:lookup s env)
      (if (s0:alias? s)
          (let ((info (s0:alias-info s)))
            (s0:resolve (car info) (cadr info) (caddr info)))
          (s0:resolve-free s lib))))

(define (s0:bind-local! frame s)
  ;; Bind S (a symbol or alias) in FRAME to a fresh host variable.
  (let ((host (s0:fresh (s0:strip s))))
    (s0:table-set! frame s (cons 'local host))
    host))

(define (s0:keyword-of x env lib)
  ;; The core keyword X names, if it does.
  (and (symbol? x)
       (let ((b (s0:resolve x env lib)))
         (and (eq? (car b) 'keyword) (cdr b)))))

(define (s0:macro-of x env lib)
  (and (symbol? x)
       (let ((b (s0:resolve x env lib)))
         (and (eq? (car b) 'macro) (cdr b)))))

;;; ------------------------------------------------------------------
;;; The expander: core-ish Scheme in, host Scheme out.

(define (s0:expand x env lib)
  (cond ((symbol? x)
         (let ((b (s0:resolve x env lib)))
           (case (car b)
             ((local global) (cdr b))
             ((macro) (s0:expand ((cdr b) x env lib) env lib))
             (else (s0:die "keyword used as a variable" (s0:strip x))))))
        ((pair? x)
         (let ((head (car x)))
           (cond ((s0:macro-of head env lib)
                  => (lambda (m) (s0:expand (m x env lib) env lib)))
                 ((s0:keyword-of head env lib)
                  => (lambda (k) (s0:expand-keyword k x env lib)))
                 (else (map (lambda (y) (s0:expand y env lib)) x)))))
        ((vector? x) (list 'quote (s0:strip x)))
        ((null? x) (s0:die "empty combination"))
        (else x)))

(define (s0:expand* xs env lib)
  (map (lambda (y) (s0:expand y env lib)) xs))

(define (s0:bind-formals! frame formals)
  ;; Bind the variables of lambda FORMALS; the host's formals.
  (cond ((null? formals) '())
        ((symbol? formals) (s0:bind-local! frame formals))
        (else (let* ((a (s0:bind-local! frame (car formals)))
                     (d (s0:bind-formals! frame (cdr formals))))
                (cons a d)))))

(define (s0:expand-lambda formals body env lib)
  (let* ((frame (s0:new-frame))
         (host (s0:bind-formals! frame formals)))
    (list 'lambda host (s0:expand-body body (cons frame env) lib))))

(define (s0:unspecified) '(if #f #f))

(define (s0:expand-keyword k x env lib)
  (case k
    ((quote) (list 'quote (s0:strip (cadr x))))
    ((quasiquote) (list 'quasiquote (s0:expand-quasi (cadr x) 1 env lib)))
    ((lambda) (s0:expand-lambda (cadr x) (cddr x) env lib))
    ((case-lambda)
     (cons 'case-lambda
           (map (lambda (clause)
                  (cdr (s0:expand-lambda (car clause) (cdr clause) env lib)))
                (cdr x))))
    ((set!)
     (let ((b (s0:resolve (cadr x) env lib)))
       (unless (memq (car b) '(local global))
         (s0:die "set! of a non-variable" (s0:strip (cadr x))))
       (list 'set! (cdr b) (s0:expand (caddr x) env lib))))
    ((if) (cons 'if (s0:expand* (cdr x) env lib)))
    ((begin)
     (if (null? (cdr x)) (s0:unspecified) (cons 'begin (s0:expand* (cdr x) env lib))))
    ((let)
     (if (and (pair? (cdr x)) (symbol? (cadr x)) (not (null? (cadr x))))
         ;; named let: (letrec ((name (lambda vars body))) (name inits))
         (let* ((name (cadr x)) (bindings (caddr x)) (body (cdddr x))
                (frame (s0:new-frame))
                (host (s0:bind-local! frame name)))
           (cons (list 'letrec
                       (list (list host (s0:expand-lambda (map car bindings) body
                                                          (cons frame env) lib)))
                       host)
                 ;; the inits are outside the loop's scope
                 (s0:expand* (map cadr bindings) env lib)))
         (let* ((bindings (cadr x))
                (inits (s0:expand* (map cadr bindings) env lib))
                (frame (s0:new-frame))
                (vars (map (lambda (b) (s0:bind-local! frame (car b))) bindings)))
           (list 'let (map list vars inits)
                 (s0:expand-body (cddr x) (cons frame env) lib)))))
    ((let*)
     (if (null? (cadr x))
         (s0:expand-keyword 'let x env lib)
         (s0:expand (list (s0:keyword-alias 'let env lib) (list (car (cadr x)))
                          (cons (s0:keyword-alias 'let* env lib)
                                (cons (cdr (cadr x)) (cddr x))))
                    env lib)))
    ((letrec letrec*)
     (let* ((bindings (cadr x))
            (frame (s0:new-frame))
            (vars (map (lambda (b) (s0:bind-local! frame (car b))) bindings))
            (env2 (cons frame env)))
       (list 'letrec* (map (lambda (v b) (list v (s0:expand (cadr b) env2 lib))) vars bindings)
             (s0:expand-body (cddr x) env2 lib))))
    ((let-values let*-values)
     ;; as nested let-values, one binding each (let*-values's scoping,
     ;; which let-values's code here doesn't rely on differing from)
     (let loop ((bindings (cadr x)) (env env))
       (if (null? bindings)
           (s0:expand-body (cddr x) env lib)
           (let* ((init (s0:expand (cadr (car bindings)) env lib))
                  (frame (s0:new-frame))
                  (formals (s0:bind-formals! frame (car (car bindings)))))
             (list 'let-values (list (list formals init))
                   (loop (cdr bindings) (cons frame env)))))))
    ((do)
     (let* ((specs (cadr x))
            (inits (s0:expand* (map cadr specs) env lib))
            (frame (s0:new-frame))
            (vars (map (lambda (s) (s0:bind-local! frame (car s))) specs))
            (env2 (cons frame env)))
       (list 'do (map (lambda (v s i)
                        (if (pair? (cddr s))
                            (list v i (s0:expand (caddr s) env2 lib))
                            (list v i)))
                      vars specs inits)
             (s0:expand* (caddr x) env2 lib)
             (s0:expand-body-or-unspecified (cdddr x) env2 lib))))
    ((cond)
     (cons 'cond
           (map (lambda (clause)
                  (cond ((s0:keyword-of (car clause) env lib)
                         => (lambda (k)
                              (if (eq? k 'else)
                                  (cons 'else (s0:expand* (cdr clause) env lib))
                                  (s0:expand* clause env lib))))
                        ((and (pair? (cdr clause)) (eq? (s0:keyword-of (cadr clause) env lib) '=>))
                         (list (s0:expand (car clause) env lib) '=> (s0:expand (caddr clause) env lib)))
                        (else (s0:expand* clause env lib))))
                (cdr x))))
    ((case)
     (cons 'case
           (cons (s0:expand (cadr x) env lib)
                 (map (lambda (clause)
                        (cons (if (eq? (s0:keyword-of (car clause) env lib) 'else)
                                  'else
                                  (s0:strip (car clause)))
                              (if (and (pair? (cdr clause))
                                       (eq? (s0:keyword-of (cadr clause) env lib) '=>))
                                  (list '=> (s0:expand (caddr clause) env lib))
                                  (s0:expand* (cdr clause) env lib))))
                      (cddr x)))))
    ((and or when unless delay) (cons k (s0:expand* (cdr x) env lib)))
    ((assert)
     (list 'if (s0:expand (cadr x) env lib) (s0:unspecified)
           (list 'r6:assertion-violation ''assert "assertion failed"
                 (list 'quote (s0:strip (cadr x))))))
    ((define define-syntax)
     (s0:die "definition in expression context" (s0:strip x)))
    ((syntax-rules)
     (s0:die "syntax-rules outside define-syntax"))
    (else (s0:die "unexpected keyword" k))))

(define (s0:keyword-alias k env lib)
  ;; An alias that resolves to keyword K whatever the use site binds.
  (s0:make-alias k '() (s0:keyword-library)))

(define s0:the-keyword-library #f)
(define (s0:keyword-library)
  (or s0:the-keyword-library
      (begin (set! s0:the-keyword-library (s0:make-library '(stage0 keywords) "kw" '() '()))
             s0:the-keyword-library)))

(define (s0:expand-quasi x depth env lib)
  (cond ((and (pair? x) (s0:same-name? (car x) 'unquote))
         (if (= depth 1)
             (list 'unquote (s0:expand (cadr x) env lib))
             (list 'unquote (s0:expand-quasi (cadr x) (- depth 1) env lib))))
        ((and (pair? x) (pair? (car x)) (s0:same-name? (caar x) 'unquote-splicing))
         (cons (if (= depth 1)
                   (list 'unquote-splicing (s0:expand (cadar x) env lib))
                   (list 'unquote-splicing (s0:expand-quasi (cadar x) (- depth 1) env lib)))
               (s0:expand-quasi (cdr x) depth env lib)))
        ((and (pair? x) (s0:same-name? (car x) 'quasiquote))
         (list 'quasiquote (s0:expand-quasi (cadr x) (+ depth 1) env lib)))
        ((pair? x) (cons (s0:expand-quasi (car x) depth env lib)
                         (s0:expand-quasi (cdr x) depth env lib)))
        ((vector? x) (list->vector (s0:expand-quasi (vector->list x) depth env lib)))
        (else (s0:strip x))))

;;; Bodies: definitions (internal define and define-syntax, also from
;;; macros and begin) then expressions, as letrec*.

(define (s0:head-expand x env lib)
  ;; Expand macro uses at the head of X until it isn't one.
  (let ((m (and (pair? x) (s0:macro-of (car x) env lib))))
    (if m (s0:head-expand (m x env lib) env lib) x)))

(define (s0:define-parts x)
  ;; (define name expr) or (define (name . formals) body ...): name and
  ;; the expression.
  (let ((target (cadr x)))
    (if (pair? target)
        (values (car target)
                (cons (s0:keyword-alias 'lambda '() #f) (cons (cdr target) (cddr x))))
        (values target (if (pair? (cddr x)) (caddr x) '(if #f #f))))))

(define (s0:expand-body-or-unspecified body env lib)
  (if (null? body) '() (list (s0:expand-body body env lib))))

(define (s0:expand-body body env lib)
  (let ((frame (s0:new-frame)))
    (let loop ((forms body) (defs '()) (exprs '()))
      (if (null? forms)
          (let ((env2 (cons frame env)))
            (let ((bindings (map (lambda (d) (list (car d) (s0:expand (cdr d) env2 lib)))
                                 (reverse defs)))
                  (exprs (let ((e (s0:expand* (reverse exprs) env2 lib)))
                           (if (null? e) (list (s0:unspecified)) e))))
              (cond ((pair? bindings) (cons 'letrec* (cons bindings exprs)))
                    ((null? (cdr exprs)) (car exprs))
                    (else (cons 'begin exprs)))))
          (let* ((env2 (cons frame env))
                 (x (s0:head-expand (car forms) env2 lib))
                 (k (and (pair? x) (s0:keyword-of (car x) env2 lib))))
            (case k
              ((begin) (loop (append (cdr x) (cdr forms)) defs exprs))
              ((define)
               (let-values (((name expr) (s0:define-parts x)))
                 (let ((host (s0:bind-local! frame name)))
                   (loop (cdr forms) (cons (cons host expr) defs) exprs))))
              ((define-syntax)
               (s0:table-set! frame (cadr x)
                              (cons 'macro (s0:make-transformer (cadr x) (caddr x) env2 lib)))
               (loop (cdr forms) defs exprs))
              (else (loop (cdr forms) defs (cons x exprs)))))))))

;;; ------------------------------------------------------------------
;;; Transformers

(define (s0:make-transformer name rhs env lib)
  (let ((k (and (pair? rhs) (s0:keyword-of (car rhs) env lib))))
    (cond ((eq? k 'syntax-rules)
           (s0:syntax-rules-transformer (cadr rhs) (cddr rhs) env lib))
          ((assq (s0:strip name) s0:procedural-macros)
           => (lambda (p) ((cdr p) lib)))
          (else (s0:die "a procedural macro stage0 doesn't know" (s0:strip name))))))

;;; syntax-rules

(define (s0:ellipsis? x) (s0:same-name? x '...))

(define (s0:syntax-rules-transformer literals rules def-env def-lib)
  (let ((literals (map s0:strip literals)))
    (lambda (form env lib)
      (let loop ((rules rules))
        (if (null? rules)
            (s0:die "no syntax-rules rule matches" (s0:strip form))
            (let* ((pattern (car (car rules)))
                   (b (s0:match (cdr pattern) (cdr form) literals)))
              (if b
                  (s0:instantiate (cadr (car rules)) b (s0:make-table) def-env def-lib)
                  (loop (cdr rules)))))))))

(define (s0:match p x literals)
  ;; An alist of (pattern-variable . value), (pattern-variable ellipsis
  ;; . values) for one under an ellipsis, or #f.
  (call-with-current-continuation
   (lambda (fail)
     (let m ((p p) (x x))
       (cond ((symbol? p)
              (cond ((s0:same-name? p '_) '())
                    ((memq (s0:strip p) literals)
                     (if (s0:same-name? x (s0:strip p)) '() (fail #f)))
                    (else (list (cons p x)))))
             ((and (pair? p) (pair? (cdr p)) (s0:ellipsis? (cadr p)))
              (let* ((tail (cddr p))
                     (min (s0:pattern-length tail))
                     (xs (s0:list-prefix x min fail)))
                (let ((items (car xs)) (rest (cdr xs)))
                  (let ((matches (map (lambda (item) (m (car p) item)) items))
                        (vars (s0:pattern-vars (car p) literals)))
                    (append (map (lambda (v)
                                   (cons v (cons '... (map (lambda (mm) (cdr (assq v mm))) matches))))
                                 vars)
                            (m tail rest))))))
             ((pair? p)
              (if (pair? x)
                  (append (m (car p) (car x)) (m (cdr p) (cdr x)))
                  (fail #f)))
             ((null? p) (if (null? x) '() (fail #f)))
             ((vector? p)
              (if (vector? x) (m (vector->list p) (vector->list x)) (fail #f)))
             (else (if (equal? p (s0:strip x)) '() (fail #f))))))))

(define (s0:pattern-length p)
  (let loop ((p p) (n 0)) (if (pair? p) (loop (cdr p) (+ n 1)) n)))

(define (s0:list-prefix x keep fail)
  ;; Split list X so that KEEP elements (and any improper tail) are left.
  (let ((n (let loop ((x x) (n 0)) (if (pair? x) (loop (cdr x) (+ n 1)) n))))
    (when (< n keep) (fail #f))
    (let loop ((x x) (i (- n keep)) (acc '()))
      (if (= i 0) (cons (reverse acc) x) (loop (cdr x) (- i 1) (cons (car x) acc))))))

(define (s0:pattern-vars p literals)
  (cond ((symbol? p)
         (if (or (s0:same-name? p '_) (s0:ellipsis? p) (memq (s0:strip p) literals)) '() (list p)))
        ((pair? p) (append (s0:pattern-vars (car p) literals) (s0:pattern-vars (cdr p) literals)))
        ((vector? p) (s0:pattern-vars (vector->list p) literals))
        (else '())))

(define (s0:instantiate t b renames def-env def-lib)
  ;; Template T with B's bindings substituted, its other symbols renamed
  ;; (consistently, via RENAMES) to aliases.
  (cond ((symbol? t)
         (let ((p (assq t b)))
           (cond (p (if (and (pair? (cdr p)) (eq? (cadr p) '...))
                        (s0:die "pattern variable used without its ellipsis" (s0:strip t))
                        (cdr p)))
                 ((s0:table-ref renames t #f))
                 (else (let ((a (s0:make-alias t def-env def-lib)))
                         (s0:table-set! renames t a)
                         a)))))
        ((and (pair? t) (s0:ellipsis? (car t)) (pair? (cdr t)))
         ;; (... template): template with ... taken literally
         (s0:instantiate-escaped (cadr t)))
        ((and (pair? t) (pair? (cdr t)) (s0:ellipsis? (cadr t)))
         (let* ((vars (filter-vars (s0:template-vars (car t)) b))
                (series (map (lambda (v) (cddr (assq v b))) vars)))
           (when (null? vars) (s0:die "ellipsis with no pattern variable" (s0:strip t)))
           (let loop ((series series) (acc '()))
             (if (null? (car series))
                 (append (reverse acc) (s0:instantiate (cddr t) b renames def-env def-lib))
                 (loop (map cdr series)
                       (cons (s0:instantiate (car t)
                                             (append (map (lambda (v s) (cons v (car s))) vars series) b)
                                             renames def-env def-lib)
                             acc))))))
        ((pair? t) (cons (s0:instantiate (car t) b renames def-env def-lib)
                         (s0:instantiate (cdr t) b renames def-env def-lib)))
        ((vector? t) (list->vector (s0:instantiate (vector->list t) b renames def-env def-lib)))
        (else t)))

(define (s0:instantiate-escaped t) t)

(define (filter-vars vars b)
  ;; The VARS bound under an ellipsis in B.
  (let loop ((vars vars) (acc '()))
    (cond ((null? vars) (reverse acc))
          ((let ((p (assq (car vars) b))) (and p (pair? (cdr p)) (eq? (cadr p) '...)))
           (loop (cdr vars) (if (memq (car vars) acc) acc (cons (car vars) acc))))
          (else (loop (cdr vars) acc)))))

(define (s0:template-vars t)
  (cond ((symbol? t) (list t))
        ((pair? t) (append (s0:template-vars (car t)) (s0:template-vars (cdr t))))
        ((vector? t) (s0:template-vars (vector->list t)))
        (else '())))

;;; The procedural macros, rewritten.  Each takes the library it's
;;; defined in and returns a transformer.  (r 'name) is an alias for name
;;; as that library sees it; each call makes a new one, so a temporary
;;; must be one (r ...) used for both its binding and its references.

(define (s0:renamer lib)
  (lambda (name) (s0:make-alias name '() lib)))

(define s0:procedural-macros
  (list
   ;; (define-syntax no-source (lambda (x) #f)): an identifier macro
   (cons 'no-source (lambda (lib) (lambda (form env use-lib) #f)))

   ;; compat.ss: (parameterize ((p v) ...) body ...) with psyntax's
   ;; parameters (procedures of zero or one argument), swapped with
   ;; dynamic-wind
   (cons 'parameterize
         (lambda (lib)
           (let ((r (s0:renamer lib)))
             (lambda (form env use-lib)
               (let ((bindings (cadr form)) (body (cddr form)))
                 (if (null? bindings)
                     (cons (r 'let) (cons '() body))
                     (let ((lhs* (map (lambda (b) (r 'lhs)) bindings))
                           (rhs* (map (lambda (b) (r 'rhs)) bindings))
                           (t (r 't)) (swap (r 'swap)))
                       `(,(r 'let) (,@(map (lambda (l b) (list l (car b))) lhs* bindings)
                                    ,@(map (lambda (v b) (list v (cadr b))) rhs* bindings))
                         (,(r 'let) ((,swap (,(r 'lambda) ()
                                             ,@(map (lambda (l v)
                                                      `(,(r 'let) ((,t (,l)))
                                                        (,l ,v)
                                                        (,(r 'set!) ,v ,t)))
                                                    lhs* rhs*))))
                          (,(r 'dynamic-wind) ,swap (,(r 'lambda) () ,@body) ,swap))))))))))

   ;; compat.ss: (define-record name (field ...) [printer]): records as
   ;; vectors tagged with a symbol of their own
   (cons 'define-record
         (lambda (lib)
           (let ((r (s0:renamer lib)))
             (lambda (form env use-lib)
               (let* ((name (s0:strip (cadr form)))
                      (fields (map s0:strip (caddr form)))
                      (sname (symbol->string name))
                      (tag (s0:fresh (string->symbol (string-append "rtd:" sname))))
                      (n (length fields))
                      (id (lambda (s) (string->symbol s)))
                      (pred (id (string-append sname "?")))
                      (x (r 'x)) (v (r 'v)))
                 `(,(r 'begin)
                   (,(r 'define) ,(id (string-append "make-" sname))
                    (,(r 'lambda) ,fields (,(r 'vector) (,(r 'quote) ,tag) ,@fields)))
                   (,(r 'define) ,pred
                    (,(r 'lambda) (,x)
                     (,(r 'and) (,(r 'vector?) ,x)
                      (,(r '=) (,(r 'vector-length) ,x) ,(+ n 1))
                      (,(r 'eq?) (,(r 'vector-ref) ,x 0) (,(r 'quote) ,tag)))))
                   ,@(let loop ((fs fields) (i 1) (acc '()))
                       (if (null? fs)
                           (reverse acc)
                           (let ((get (id (string-append sname "-" (symbol->string (car fs)))))
                                 (set (id (string-append "set-" sname "-" (symbol->string (car fs)) "!"))))
                             (loop (cdr fs) (+ i 1)
                                   (cons `(,(r 'define) ,set
                                           (,(r 'lambda) (,x ,v)
                                            (,(r 'if) (,pred ,x)
                                             (,(r 'vector-set!) ,x ,i ,v)
                                             (,(r 'error) (,(r 'quote) ,set) "~s is not of type ~s" ,x (,(r 'quote) ,name)))))
                                         (cons `(,(r 'define) ,get
                                                 (,(r 'lambda) (,x)
                                                  (,(r 'if) (,pred ,x)
                                                   (,(r 'vector-ref) ,x ,i)
                                                   (,(r 'error) (,(r 'quote) ,get) "~s is not of type ~s" ,x (,(r 'quote) ,name)))))
                                               acc))))))))))))

   ;; expander.ss: (stx-error stx [msg])
   (cons 'stx-error
         (lambda (lib)
           (let ((r (s0:renamer lib)))
             (lambda (form env use-lib)
               (if (null? (cddr form))
                   `(,(r 'error) (,(r 'quote) expander) "invalid syntax" (,(r 'stx->datum) ,(cadr form)))
                   `(,(r 'error) (,(r 'quote) expander) ,(caddr form) (,(r 'strip) ,(cadr form) (,(r 'quote) ()))))))))

   ;; expander.ss: (syntax-match expr (literal ...) clause ...), clause
   ;; being (pattern body) or (pattern guard body): psyntax's own matcher
   ;; for syntax objects
   (cons 'syntax-match
         (lambda (lib)
           (lambda (form env use-lib)
             (s0:syntax-match form lib))))))

(define (s0:syntax-match form lib)
  (let* ((r (s0:renamer lib))
         (expr (cadr form))
         (lits (map s0:strip (caddr form)))
         (clauses (cdddr form)))
    (define (q x) (list (r 'quote) x))
    (define (parse-pat pat)
      ;; (values pattern-variables deconstructor), as syntax-match's own
      ;; parse-pat: the deconstructor maps a syntax object to the list of
      ;; the variables' values, or #f
      (cond ((symbol? pat)
             (let ((name (s0:strip pat)) (x (r 'x)))
               (cond ((memq name lits)
                      (values '() `(,(r 'lambda) (,x)
                                    (,(r 'and) (,(r 'id?) ,x)
                                     (,(r 'free-id=?) ,x (,(r 'scheme-stx) ,(q name)))
                                     ,(q '())))))
                     ((eq? name '_) (values '() `(,(r 'lambda) (,x) ,(q '()))))
                     (else (values (list pat) `(,(r 'lambda) (,x) (,(r 'list) ,x)))))))
            ((and (pair? pat) (pair? (cdr pat)) (s0:ellipsis? (cadr pat)) (null? (cddr pat)))
             (let-values (((pvars decon) (parse-pat (car pat))))
               (let ((f (r 'f)) (x (r 'x)) (cars (r 'cars)) (cdrs (r 'cdrs)))
                 (values pvars
                         `(,(r 'letrec)
                           ((,f (,(r 'lambda) (,x)
                                 (,(r 'cond)
                                  ((,(r 'syntax-pair?) ,x)
                                   (,(r 'let) ((,cars (,decon (,(r 'syntax-car) ,x))))
                                    (,(r 'and) ,cars
                                     (,(r 'let) ((,cdrs (,f (,(r 'syntax-cdr) ,x))))
                                      (,(r 'and) ,cdrs (,(r 'map) ,(r 'cons) ,cars ,cdrs))))))
                                  ((,(r 'syntax-null?) ,x)
                                   (,(r 'list) ,@(map (lambda (v) (q '())) pvars)))
                                  (,(r 'else) #f)))))
                           ,f)))))
            ((and (pair? pat) (pair? (cdr pat)) (s0:ellipsis? (cadr pat)))
             (let-values (((p1 d1) (parse-pat (car pat)))
                          ((p2 d2) (parse-pat (cddr pat))))
               (let ((f (r 'f)) (x (r 'x)) (cars (r 'cars)) (df (r 'df)) (d (r 'd)) (y (r 'y)))
                 (values (append p1 p2)
                         `(,(r 'letrec)
                           ((,f (,(r 'lambda) (,x)
                                 (,(r 'cond)
                                  ((,(r 'syntax-pair?) ,x)
                                   (,(r 'let) ((,cars (,d1 (,(r 'syntax-car) ,x))))
                                    (,(r 'and) ,cars
                                     (,(r 'let) ((,df (,f (,(r 'syntax-cdr) ,x))))
                                      (,(r 'and) ,df
                                       (,(r 'cons) (,(r 'map) ,(r 'cons) ,cars (,(r 'car) ,df))
                                        (,(r 'cdr) ,df)))))))
                                  (,(r 'else)
                                   (,(r 'let) ((,d (,d2 ,x)))
                                    (,(r 'and) ,d
                                     (,(r 'cons) (,(r 'list) ,@(map (lambda (v) (q '())) p1))
                                      ,d))))))))
                           (,(r 'lambda) (,y)
                            (,(r 'let) ((,y (,f ,y)))
                             (,(r 'and) ,y (,(r 'append) (,(r 'car) ,y) (,(r 'cdr) ,y))))))))))
            ((pair? pat)
             (let-values (((p1 d1) (parse-pat (car pat)))
                          ((p2 d2) (parse-pat (cdr pat))))
               (let ((x (r 'x)) (a (r 'a)) (b (r 'b)))
                 (values (append p1 p2)
                         `(,(r 'lambda) (,x)
                           (,(r 'and) (,(r 'syntax-pair?) ,x)
                            (,(r 'let) ((,a (,d1 (,(r 'syntax-car) ,x))))
                             (,(r 'and) ,a
                              (,(r 'let) ((,b (,d2 (,(r 'syntax-cdr) ,x))))
                               (,(r 'and) ,b (,(r 'append) ,a ,b)))))))))))
            ((vector? pat)
             (let-values (((pvars d) (parse-pat (vector->list pat))))
               (let ((x (r 'x)))
                 (values pvars
                         `(,(r 'lambda) (,x)
                           (,(r 'and) (,(r 'syntax-vector?) ,x)
                            (,d (,(r 'syntax-vector->list) ,x))))))))
            (else
             (let ((x (r 'x)))
               (values '()
                       `(,(r 'lambda) (,x)
                         (,(r 'and) (,(r 'equal?) (,(r 'stx->datum) ,x) ,(q (s0:strip pat))) ,(q '()))))))))
    (if (null? clauses)
        `(,(r 'stx-error) ,expr "invalid syntax")
        (let* ((clause (car clauses))
               (pat (car clause))
               (guard (if (null? (cddr clause)) #t (cadr clause)))
               (body (if (null? (cddr clause)) (cadr clause) (caddr clause))))
          (let-values (((pvars decon) (parse-pat pat)))
            (let ((t (r 't)) (ls (r 'ls)))
              `(,(r 'let) ((,t ,expr))
                (,(r 'let) ((,ls (,decon ,t)))
                 (,(r 'if) (,(r 'and) ,ls (,(r 'apply) (,(r 'lambda) ,pvars ,guard) ,ls))
                  (,(r 'apply) (,(r 'lambda) ,pvars ,body) ,ls)
                  (,(r 'syntax-match) ,t ,(caddr form) ,@(cdr clauses)))))))))))

;;; ------------------------------------------------------------------
;;; Loading a library or the program: flattened into host definitions

(define (s0:library-prefix name)
  (symbol->string (car (reverse name))))

(define (s0:global-name lib name)
  (string->symbol (string-append (s0:lib-prefix lib) ":" (symbol->string (s0:strip name)))))

(define (s0:load-body! lib body)
  ;; First the definitions, so that every global is known; then each
  ;; definition and expression is expanded and evaluated in order.
  (let loop ((forms body) (items '()))
    (if (pair? forms)
        (let* ((x (s0:head-expand (car forms) '() lib))
               (k (and (pair? x) (s0:keyword-of (car x) '() lib))))
          (case k
            ((begin) (loop (append (cdr x) (cdr forms)) items))
            ((define)
             (let-values (((name expr) (s0:define-parts x)))
               (let ((host (s0:global-name lib name)))
                 (s0:table-set! (s0:lib-bindings lib) (s0:strip name) (cons 'global host))
                 (loop (cdr forms) (cons (list 'define host expr) items)))))
            ((define-syntax)
             (s0:table-set! (s0:lib-bindings lib) (s0:strip (cadr x))
                            (cons 'macro (s0:make-transformer (cadr x) (caddr x) '() lib)))
             (loop (cdr forms) items))
            (else (loop (cdr forms) (cons (list 'expr x) items)))))
        (for-each (lambda (item)
                    (s0:host-eval
                     (if (eq? (car item) 'define)
                         (list 'define (cadr item) (s0:expand (caddr item) '() lib))
                         (s0:expand (cadr item) '() lib))))
                  (reverse items)))))

(define (s0:load-library! form)
  ;; (library name (export ...) (import ...) body ...)
  (let* ((name (cadr form))
         (exports (cdr (caddr form)))
         (imports (cdr (cadddr form)))
         (lib (s0:make-library name (s0:library-prefix name) exports imports)))
    (set! s0:libraries (cons (cons name lib) s0:libraries))
    (s0:load-body! lib (cddddr form))))

(define (s0:load-program! forms)
  ;; (import ...) body ...
  (let ((lib (s0:make-library '(program) "program" '() (cdr (car forms)))))
    (s0:load-body! lib (cdr forms))))

;;; ------------------------------------------------------------------
;;; R6RS procedures an R7RS-small host lacks (or has differently)

(define (r6:error who msg . irritants)
  (apply error
         (if who
             (string-append (if (symbol? who) (symbol->string who) "") ": "
                            (if (string? msg) msg (s0:->string msg)))
             (if (string? msg) msg (s0:->string msg)))
         irritants))

(define (s0:->string x)
  (let ((p (open-output-string))) (write x p) (get-output-string p)))

(define (r6:assertion-violation who msg . irritants)
  (apply r6:error who msg irritants))
(define (r6:syntax-violation who msg form . sub)
  (r6:error who msg form))
(define (r6:condition? x) #f)

(define (r6:for-all f l . ls)
  (if (null? ls)
      (let loop ((l l)) (or (null? l) (and (f (car l)) (loop (cdr l)))))
      (let loop ((ls (cons l ls)))
        (or (null? (car ls))
            (and (apply f (map car ls)) (loop (map cdr ls)))))))

(define (r6:exists f l . ls)
  (if (null? ls)
      (let loop ((l l)) (and (pair? l) (or (f (car l)) (loop (cdr l)))))
      (let loop ((ls (cons l ls)))
        (and (pair? (car ls))
             (or (apply f (map car ls)) (loop (map cdr ls)))))))

(define (r6:find f l)
  (let loop ((l l)) (cond ((null? l) #f) ((f (car l)) (car l)) (else (loop (cdr l))))))
(define (r6:filter f l)
  (let loop ((l l) (acc '()))
    (cond ((null? l) (reverse acc)) ((f (car l)) (loop (cdr l) (cons (car l) acc))) (else (loop (cdr l) acc)))))
(define (r6:remp f l) (r6:filter (lambda (x) (not (f x))) l))
(define (r6:partition f l) (values (r6:filter f l) (r6:remp f l)))
(define (r6:remove x l) (r6:remp (lambda (y) (equal? x y)) l))
(define (r6:remv x l) (r6:remp (lambda (y) (eqv? x y)) l))
(define (r6:remq x l) (r6:remp (lambda (y) (eq? x y)) l))
(define (r6:memp f l)
  (let loop ((l l)) (cond ((null? l) #f) ((f (car l)) l) (else (loop (cdr l))))))
(define (r6:assp f l)
  (let loop ((l l)) (cond ((null? l) #f) ((f (car (car l))) (car l)) (else (loop (cdr l))))))
(define (r6:fold-left f init l . ls)
  (if (null? ls)
      (let loop ((acc init) (l l)) (if (null? l) acc (loop (f acc (car l)) (cdr l))))
      (let loop ((acc init) (ls (cons l ls)))
        (if (null? (car ls)) acc (loop (apply f acc (map car ls)) (map cdr ls))))))
(define (r6:fold-right f init l . ls)
  (if (null? ls)
      (let loop ((l (reverse l)) (acc init)) (if (null? l) acc (loop (cdr l) (f (car l) acc))))
      (let loop ((ls (map reverse (cons l ls))) (acc init))
        (if (null? (car ls)) acc (loop (map cdr ls) (apply f (append (map car ls) (list acc))))))))
(define (r6:cons* x . rest)
  (if (null? rest) x (cons x (apply r6:cons* rest))))
(define (r6:list-sort less? l)
  ;; merge sort
  (define (merge a b)
    (cond ((null? a) b) ((null? b) a)
          ((less? (car b) (car a)) (cons (car b) (merge a (cdr b))))
          (else (cons (car a) (merge (cdr a) b)))))
  (define (sort l n)
    (if (<= n 1)
        (if (= n 0) '() (list (car l)))
        (let ((h (quotient n 2)))
          (merge (sort l h) (sort (list-tail l h) (- n h))))))
  (sort l (length l)))
(define (r6:vector-sort less? v) (list->vector (r6:list-sort less? (vector->list v))))
(define (r6:div x y) (floor-quotient x y))
(define (r6:mod x y) (floor-remainder x y))
(define (r6:file-exists? name)
  (guard (e (#t #f)) (call-with-input-file name (lambda (p) #t))))
(define (r6:delete-file name) (s0:host-delete-file name))

;;; Hashtables: the host's eq tables, wrapped so that hashtable? works.
(define r6:hashtable-tag (list 'hashtable))
(define (r6:make-eq-hashtable . size) (vector r6:hashtable-tag (s0:make-table)))
(define r6:make-eqv-hashtable r6:make-eq-hashtable)
(define (r6:make-hashtable hash equiv . size) (r6:make-eq-hashtable))
(define (r6:hashtable? x) (and (vector? x) (= (vector-length x) 2) (eq? (vector-ref x 0) r6:hashtable-tag)))
(define (r6:hashtable-ref h k default) (s0:table-ref (vector-ref h 1) k default))
(define (r6:hashtable-set! h k v) (s0:table-set! (vector-ref h 1) k v))
(define (r6:hashtable-delete! h k) (s0:table-delete! (vector-ref h 1) k))
(define (r6:hashtable-contains? h k)
  (let ((missing (list 'missing)))
    (not (eq? (s0:table-ref (vector-ref h 1) k missing) missing))))
(define (r6:hashtable-update! h k f default)
  (r6:hashtable-set! h k (f (r6:hashtable-ref h k default))))
(define (r6:hashtable-keys h) (list->vector (s0:table-keys (vector-ref h 1))))
(define (r6:hashtable-size h) (length (s0:table-keys (vector-ref h 1))))
(define (r6:string-hash s) (string-length s))
(define (r6:equal-hash x) 0)
(define (r6:symbol-hash s) (string-length (symbol->string s)))

(define (r6:open-string-output-port)
  (let ((p (open-output-string)))
    (values p (lambda ()
                (let ((s (get-output-string p)))
                  ;; R6RS: the extractor resets the port
                  s)))))
(define (r6:call-with-string-output-port f)
  (let ((p (open-output-string))) (f p) (get-output-string p)))

;;; ------------------------------------------------------------------
;;; psyntax's host primitives: (psyntax system $bootstrap)

(define (s0:void . ignore) (if #f #f))

(define s0:gensym-count 0)
(define (s0:gensym . ignore)
  ;; Interned, so the expanded code they end up in can be written out
  ;; and read back.
  (set! s0:gensym-count (+ s0:gensym-count 1))
  (string->symbol (string-append "g$s0$" (number->string s0:gensym-count))))

(define (s0:lisp-keyword? x) #f)
(define (s0:host-literal? x) #f)

(define (s0:pretty-print x . port)
  (let ((p (if (null? port) (current-output-port) (car port))))
    (write x p)
    (newline p)))

;;; Globals of the code psyntax evaluates: the locations it makes for
;;; library variables and for the primitives (the build script maps each
;;; primitive to a location), in a table.
(define s0:globals (s0:make-table))
(define s0:unbound (list 'unbound))

(define (s0:symbol-value s)
  (let ((v (s0:table-ref s0:globals s s0:unbound)))
    (if (eq? v s0:unbound) (s0:die "unbound global" s) v)))
(define (s0:set-symbol-value! s v) (s0:table-set! s0:globals s v))

(define (s0:eval-core x) (s0:host-eval (s0:core->host x '())))

(define (s0:core->host x bound)
  ;; psyntax's core forms (lambda, if, set!, define, begin, letrec, quote
  ;; and calls) to host Scheme, its free variables globals.
  (cond ((symbol? x)
         (if (memq x bound) x (list 's0:symbol-value (list 'quote x))))
        ((not (pair? x)) (list 'quote x))
        (else
         (case (car x)
           ((quote) x)
           ((lambda)
            (let ((vars (let loop ((f (cadr x)))
                          (cond ((null? f) '()) ((symbol? f) (list f)) (else (cons (car f) (loop (cdr f))))))))
              (list 'lambda (cadr x) (s0:core->host (caddr x) (append vars bound)))))
           ((case-lambda)
            (cons 'case-lambda
                  (map (lambda (c)
                         (let ((vars (let loop ((f (car c)))
                                       (cond ((null? f) '()) ((symbol? f) (list f)) (else (cons (car f) (loop (cdr f))))))))
                           (list (car c) (s0:core->host (cadr c) (append vars bound)))))
                       (cdr x))))
           ((if begin) (cons (car x) (map (lambda (y) (s0:core->host y bound)) (cdr x))))
           ((set! define)
            (if (memq (cadr x) bound)
                (list 'set! (cadr x) (s0:core->host (caddr x) bound))
                (list 's0:set-symbol-value! (list 'quote (cadr x)) (s0:core->host (caddr x) bound))))
           ((letrec letrec*)
            (let ((bound (append (map car (cadr x)) bound)))
              (list 'letrec* (map (lambda (b) (list (car b) (s0:core->host (cadr b) bound))) (cadr x))
                    (s0:core->host (caddr x) bound))))
           ((primitive) (s0:die "a primitive with no location" (cadr x)))
           (else (map (lambda (y) (s0:core->host y bound)) x))))))

;;; ------------------------------------------------------------------
;;; The build

(define s0:library-files
  ;; psyntax's libraries that its build script imports, in dependency order
  '("psyntax/config.ss" "psyntax/compat.ss" "psyntax/internal.ss"
    "psyntax/builders.ss" "psyntax/library-manager.ss" "psyntax/expander.ss"))

(define (s0:build)
  (for-each (lambda (file)
              (display "stage0: loading ") (display file) (newline)
              (for-each s0:load-library! (s0:read-file file)))
            s0:library-files)
  (display "stage0: running psyntax-buildscript.ss") (newline)
  (s0:load-program! (s0:read-file "psyntax-buildscript.ss"))
  (display "stage0: done") (newline))
