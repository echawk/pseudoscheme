;;; -*- Mode: Scheme -*-
;;;; (chezscheme): the top level and the system, as Chez Scheme has them:
;;;; eval in the interaction environment, top-level values, load, the
;;;; library path, the compiler's entry points and parameters, and
;;;; Chez's extra syntax.  Part of the library's body
;;;; (src/chez/chez.lisp), whose host procedures run at Chez's top level
;;;; (pseudoscheme --chez).

;;; eval: in the interaction environment by default, the top level a
;;; Chez script or the REPL runs at

(define (interaction-environment) (%r7rs:interaction-environment))

(define (top-level-environment? env) (eq? env (interaction-environment)))

;; eval: (rnrs eval)'s, which with one argument, or the interaction
;; environment, evaluates at the top level running (src/chez/chez.lisp,
;; install-chez-extensions); so does dynamic-wind take Chez's critical?

(define interpret eval)
(define compile eval)

;; The expanded form: here, the form itself (Chez's is its internal
;; language)
(define (expand x . env) x)
(define (sc-expand x . env) x)

(define (scheme-environment) (environment '(chezscheme)))

;; Libraries: their exports, and (approximately) what they import
(define (library-exports name) (%chez:library-exports name))
(define (library-requirements name . options) '())
(define-syntax library-requirements-options
  (syntax-rules () ((_ option ...) '(option ...))))
(define (copy-environment env . rest) env)
(define (environment-symbols env)
  (if (top-level-environment? env)
      (%chez:top-level-symbols)
      (%psyntax:environment-symbols env)))

;;; Top-level values: what eval at the top level sees

(define (top-level-value s . env)
  (eval s (if (pair? env) (car env) (interaction-environment))))

(define (set-top-level-value! s v . env)
  (%chez:set-top-level-value! s v))

(define (define-top-level-value s v . env)
  (%chez:define-top-level-value! s v))

(define (top-level-bound? s . env)
  (%chez:top-level-bound? s))

(define (top-level-mutable? s . env) (top-level-bound? s))

;;; Loading and compiling.  Compiling is what loading does here, so the
;;; compile- procedures load or do nothing; libraries are compiled when
;;; imported, into the compiled-library cache.

(define load
  (case-lambda
    ((path) (%chez:load path))
    ((path eval-proc) (%chez:load path))))

(define (load-program path) (%chez:load path))
(define (load-library path) (%chez:load path))
(define (visit path) (%chez:load path))
(define (revisit path) (%chez:load path))
(define (compile-file . args) (if #f #f))
(define (compile-library . args) (if #f #f))
(define (compile-program . args) (if #f #f))
(define (compile-script . args) (if #f #f))
(define (compile-to-file . args) (if #f #f))
(define (maybe-compile-file . args) (if #f #f))
(define (maybe-compile-library . args) (if #f #f))
(define (maybe-compile-program . args) (if #f #f))

;; (library-directories): the search path as Chez gives it, a list of
;; (source-directory . object-directory) pairs; set with a list of
;; those, of directories, or a string of them separated by colons
(define library-directories
  (case-lambda
    (() (%chez:library-directories))
    ((dirs) (%chez:set-library-directories! dirs))))

(define library-extensions
  (make-parameter '((".chezscheme.sls" . ".chezscheme.so") (".ss" . ".so")
                    (".sls" . ".so") (".scm" . ".so") (".sch" . ".so"))))

;; The compiler's and the system's parameters: accepted, and mostly
;; without effect here (SBCL compiles everything with its own policy)
(define compile-imported-libraries (make-parameter #f))
(define compile-file-message (make-parameter #t))
(define compile-interpret-simple (make-parameter #t))
(define optimize-level (make-parameter 0))
(define debug-level (make-parameter 1))
(define commonization-level (make-parameter 0))
(define generate-inspector-information (make-parameter #t))
(define generate-procedure-source-information (make-parameter #f))
(define generate-interrupt-trap (make-parameter #t))
(define enable-object-counts (make-parameter #f))
(define undefined-variable-warnings (make-parameter #f))
(define import-notify (make-parameter #f))
(define library-timestamp-mode (make-parameter 'modification-time))
(define current-expand (make-parameter expand))
(define current-eval (make-parameter eval))
(define collect-request-handler (make-parameter (lambda () (collect))))
(define collect-trip-bytes (make-parameter (* 8 1024 1024)))
(define (collect-rendezvous) (collect))

;; The printer's parameters; print-length and print-level are honoured
;; only as far as the writer does (not yet)
(define print-graph (make-parameter #f))
(define print-length (make-parameter #f))
(define print-level (make-parameter #f))
(define print-radix (make-parameter 10))
(define print-gensym (make-parameter #t))
(define print-brackets (make-parameter #t))
(define print-char-name (make-parameter #f))
(define print-vector-length (make-parameter #f))
(define print-precision (make-parameter #f))
(define print-extended-identifiers (make-parameter #f))
(define print-unicode (make-parameter #t))
(define pretty-line-length (make-parameter 75))
(define pretty-one-line-limit (make-parameter 60))
(define pretty-initial-indent (make-parameter 0))
(define pretty-standard-indent (make-parameter 1))
(define pretty-maximum-lines (make-parameter #f))
(define case-sensitive (make-parameter #t))

;;; The system

(define (command-line-arguments)
  (let ((cl (command-line)))
    (if (pair? cl) (cdr cl) '())))

(define get-process-id %chez:get-process-id)
(define char-ready? %char-ready?)
(define putenv %chez:putenv)
(define (scheme-version) "Pseudoscheme (as Chez Scheme Version 10.4.1)")
(define (scheme-version-number) (values 10 4 1))
(define (petite?) #f)
(define (interactive?) #f)
(define abort-handler (make-parameter (lambda args (exit 1))))
(define (abort . x) (apply (abort-handler) x))
(define exit-handler (make-parameter exit))
(define reset-handler (make-parameter (lambda () (exit 1))))
(define (reset) ((reset-handler)))
(define (cd . dir) (apply current-directory dir))

;;; Interrupts, timers and engines.  Nothing interrupts Scheme code
;;; here, so disabling interrupts does nothing; engines, which need a
;;; timer interrupt, aren't supported (docs/chez.md).

(define interrupt-level 0)
(define (disable-interrupts) (set! interrupt-level (+ interrupt-level 1)) interrupt-level)
(define (enable-interrupts)
  (when (> interrupt-level 0) (set! interrupt-level (- interrupt-level 1)))
  interrupt-level)
(define-syntax with-interrupts-disabled
  (syntax-rules () ((_ body1 body2 ...) (let () body1 body2 ...))))
(define-syntax critical-section
  (syntax-rules () ((_ body1 body2 ...) (let () body1 body2 ...))))
(define keyboard-interrupt-handler (make-parameter (lambda () (exit 130))))
(define timer-interrupt-handler (make-parameter (lambda () (if #f #f))))
(define (set-timer n) 0)
(define (register-signal-handler sig proc) (if #f #f))
(define (make-engine thunk)
  (error 'make-engine "engines are not supported (they need a timer interrupt)"))

;;; Syntax

;; (datum template): (syntax->datum (syntax template))
(define-syntax datum
  (lambda (x)
    (syntax-case x ()
      ((_ t) #'(syntax->datum (syntax t))))))

;; (rec var expr): a recursive expression
(define-syntax rec
  (syntax-rules ()
    ((_ var expr) (letrec ((var expr)) var))))

;; (eval-when (situation ...) form ...): here everything is evaluated as
;; it's expanded or run, so the forms are simply spliced in
(define-syntax eval-when
  (syntax-rules ()
    ((_ (situation ...) form ...) (begin form ...))))

;; #%car reads as ($primitive car): the system's car
(define-syntax $primitive
  (syntax-rules ()
    ((_ name) name)
    ((_ level name) name)))

(define (syntax->list x)
  (syntax-case x ()
    ((a ...) #'(a ...))))

(define (syntax->vector x)
  (syntax-case x ()
    (#(a ...) #'#(a ...))))

;; Inspection and debugging: not here
(define (inspect x) (if #f #f))
(define (inspect/object x) (if #f #f))
(define (debug) (if #f #f))
(define (break . args) (if #f #f))
(define procedure-arity-mask %chez:procedure-arity-mask)

;;; Ports beyond R6RS

(define port-closed? %chez:port-closed?)
(define (fresh-line . port) (if #f #f))
(define get-bytevector-some! %chez:get-bytevector-some!)
(define (put-bytevector-some port bv . range)
  (apply put-bytevector port bv range)
  (if (null? range) (bytevector-length bv) (cadr range)))
(define (truncate-file . args) (if #f #f))
(define (set-port-length! port n) (if #f #f))

;;; Source objects and annotations: the reader keeps no source
;;; positions, so there are no annotations; get-datum/annotations reads a
;;; datum and gives the position after it

(define (annotation? x) #f)
(define (annotation-expression a) a)
(define (annotation-stripped a) a)
(define (annotation-source a) #f)
(define (source-object-bfp s) 0)
(define (source-object-efp s) 0)
(define (source-file-descriptor path checksum) (cons path checksum))
(define (get-datum/annotations port sfd bfp)
  (let ((d (get-datum port)))
    (values d (+ bfp (port-position port)))))
(define (read-token . args)
  (error 'read-token "not supported"))

(define compile-library-handler (make-parameter (lambda args (if #f #f))))
(define (engine-block) (if #f #f))
