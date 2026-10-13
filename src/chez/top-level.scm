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
    ;; each form read is passed to eval-proc
    ((path eval-proc)
     (call-with-input-file path
       (lambda (p)
         (let loop ()
           (let ((x (read p)))
             (unless (eof-object? x)
               (eval-proc x)
               (loop)))))))))

(define (load-program path) (%chez:load path))
(define (load-library path) (%chez:load path))
(define (visit path) (%chez:load path))
(define (revisit path) (%chez:load path))
;; Compiling a program: its object file is an SBCL fasl
;; (src/chez/compile.lisp).  Compiling another file: its "object file"
;; is a copy of its source, which load and --script run as they do the
;; source (and compile, as they go, with SBCL); a library's is compiled
;; into the compiled-library cache when it's first imported.  So
;; programs that drive Chez's compiler, as Idris 2's build does, find
;; the files they expect.
(define (object-file-name in)
  (let loop ((i (- (string-length in) 1)))
    (cond ((< i 0) (string-append in ".so"))
          ((char=? (string-ref in i) #\.) (string-append (substring in 0 i) ".so"))
          ((char=? (string-ref in i) #\/) (string-append in ".so"))
          (else (loop (- i 1))))))
(define (copy-to-object-file who in out)
  (unless (string? in) (assertion-violation who "not a string" in))
  (let ((out (or out (object-file-name in))))
    (when (compile-file-message)
      (printf "compiling ~a with output to ~a\n" in out))
    (let ((text (call-with-input-file in get-string-all)))
      (when (file-exists? out) (delete-file out))
      (call-with-output-file out (lambda (p) (unless (eof-object? text) (put-string p text))))
      ;; as Chez's: a program with a #! line can be run
      (chmod out (get-mode in)))
    (if #f #f)))
(define (compile-file in . out) (copy-to-object-file 'compile-file in (and (pair? out) (car out))))
(define (compile-library in . out) (copy-to-object-file 'compile-library in (and (pair? out) (car out))))
(define (compile-program in . out)
  (unless (string? in) (assertion-violation 'compile-program "not a string" in))
  (let ((out (if (pair? out) (car out) (object-file-name in))))
    (when (compile-file-message)
      (printf "compiling ~a with output to ~a\n" in out))
    (%chez:compile-program in out)
    '()))
(define (compile-script in . out) (copy-to-object-file 'compile-script in (and (pair? out) (car out))))
;; Compiled code written to a file or a binary port is the forms' text,
;; which load and load-compiled-from-port evaluate
(define (write-forms forms p)
  (for-each (lambda (x) (write x p) (newline p)) forms))
(define (compile-to-file forms out . libs)
  (when (file-exists? out) (delete-file out))
  (call-with-output-file out (lambda (p) (write-forms forms p)))
  (if #f #f))
(define (compile-to-port forms bp . rest)
  (put-bytevector bp (string->utf8 (call-with-string-output-port (lambda (p) (write-forms forms p)))))
  (if #f #f))
(define (load-compiled-from-port bp)
  (let ([p (transcoded-port bp (make-transcoder (utf-8-codec)))])
    (let loop ([v (if #f #f)])
      (let ([x (read p)])
        (if (eof-object? x) v (loop (eval x)))))))
(define (maybe-compile-file in . out) (apply compile-file in out))
(define (maybe-compile-library in . out) (apply compile-library in out))
(define (maybe-compile-program in . out) (apply compile-program in out))

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
;; Chez's flag parameters take any value as a boolean
(define (->boolean x) (and x #t))
(define compile-imported-libraries (make-parameter #f ->boolean))
(define compile-file-message (make-parameter #t ->boolean))
(define compile-interpret-simple (make-parameter #t ->boolean))
(define optimize-level (make-parameter 0))
(define debug-level (make-parameter 1))
(define commonization-level (make-parameter 0))
(define generate-inspector-information (make-parameter #t ->boolean))
(define generate-procedure-source-information (make-parameter #f ->boolean))
(define generate-interrupt-trap (make-parameter #t ->boolean))
(define enable-object-counts (make-parameter #f ->boolean))
(define undefined-variable-warnings (make-parameter #f ->boolean))
(define import-notify (make-parameter #f ->boolean))
(define library-timestamp-mode (make-parameter 'modification-time))
(define current-expand (make-parameter expand))
(define current-eval (make-parameter eval))
(define collect-request-handler (make-parameter (lambda () (collect))))
(define collect-trip-bytes (make-parameter (* 8 1024 1024)))
(define (collect-rendezvous) (collect))

;; The printer's parameters; print-length and print-level are honoured
;; only as far as the writer does (not yet)
(define print-graph (make-parameter #f ->boolean))
(define print-length (make-parameter #f))
(define print-level (make-parameter #f))
(define print-radix (make-parameter 10))
(define print-gensym (make-parameter #t))
(define print-brackets (make-parameter #t ->boolean))
(define print-char-name (make-parameter #f ->boolean))
(define print-vector-length (make-parameter #f ->boolean))
(define print-precision (make-parameter #f))
(define print-extended-identifiers (make-parameter #f ->boolean))
(define print-unicode (make-parameter #t ->boolean))
(define pretty-line-length (make-parameter 75))
(define pretty-one-line-limit (make-parameter 60))
(define pretty-initial-indent (make-parameter 0))
(define pretty-standard-indent (make-parameter 1))
(define pretty-maximum-lines (make-parameter #f))
(define case-sensitive (make-parameter #t ->boolean))

;;; The system

(define command-line-arguments
  (case-lambda
    (() (let ((cl (command-line))) (if (pair? cl) (cdr cl) '())))
    ((args) (let ((cl (command-line)))
              (command-line (cons (if (pair? cl) (car cl) "") args))))))

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
(define make-engine %chez:make-engine)   ; engines run in threads (host.lisp)
(define engine-return %chez:engine-return)

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
(define make-wrapper-procedure %chez:make-wrapper-procedure)
(define make-arity-wrapper-procedure %chez:make-arity-wrapper-procedure)
(define wrapper-procedure? %chez:wrapper-procedure?)
(define wrapper-procedure-procedure %chez:wrapper-procedure-procedure)
(define wrapper-procedure-data %chez:wrapper-procedure-data)
(define set-wrapper-procedure-procedure! %chez:set-wrapper-procedure-procedure!)
(define set-wrapper-procedure-data! %chez:set-wrapper-procedure-data!)

;;; Ports beyond R6RS

(define port-closed? %chez:port-closed?)
(define (fresh-line . port) (apply %chez:fresh-line port))
(define get-bytevector-some! %chez:get-bytevector-some!)
(define (put-bytevector-some port bv . range)
  (apply put-bytevector port bv range)
  (if (null? range) (bytevector-length bv) (cadr range)))
(define (truncate-file . args) (if #f #f))
(define (set-port-length! port n) (if #f #f))

;;; Source objects and annotations.  An annotation (src/chez/host.lisp)
;;; stands for its expression wherever psyntax looks at syntax;
;;; get-datum/annotations reads a datum annotating each part with its
;;; source object.  load and the reader proper don't annotate.

(define (source-file-descriptor? x) (%chez:sfd? x))
(define (source-file-descriptor-path x)
  (unless (source-file-descriptor? x) (assertion-violation 'source-file-descriptor-path "not a source file descriptor" x))
  (%chez:sfd-path x))
(define (source-file-descriptor-checksum x)
  (unless (source-file-descriptor? x) (assertion-violation 'source-file-descriptor-checksum "not a source file descriptor" x))
  (%chez:sfd-checksum x))
(define (make-sfd path checksum) (%chez:make-sfd path checksum))
(define (source-file-descriptor path checksum)
  (unless (and (integer? checksum) (exact? checksum) (>= checksum 0))
    (assertion-violation 'source-file-descriptor "not an exact nonnegative integer" checksum))
  (make-sfd path checksum))
(define (bytes-checksum bv)
  (let loop ([i 0] [h 0])
    (if (= i (bytevector-length bv))
        h
        (loop (+ i 1) (bitwise-and (+ (* h 31) (bytevector-u8-ref bv i)) #xffffffff)))))
(define make-source-file-descriptor
  (case-lambda
    [(path bip) (make-source-file-descriptor path bip #f)]
    [(path bip reset?)
     (unless (and (input-port? bip) (binary-port? bip))
       (assertion-violation 'make-source-file-descriptor "not a binary input port" bip))
     (when (and reset? (not (and (port-has-port-position? bip) (port-has-set-port-position!? bip))))
       (assertion-violation 'make-source-file-descriptor "port does not support port-position and set-port-position!" bip))
     (let* ([start (and reset? (port-position bip))]
            [bv (get-bytevector-all bip)]
            [bv (if (eof-object? bv) (bytevector) bv)])
       (when reset? (set-port-position! bip start))
       (make-sfd path (bytes-checksum bv)))]))

(define (source-object? x) (%chez:source-object? x))
(define (check-so who x) (unless (source-object? x) (assertion-violation who "not a source object" x)))
(define (source-object-sfd x) (check-so 'source-object-sfd x) (%chez:source-object-sfd x))
(define (source-object-bfp x) (check-so 'source-object-bfp x) (%chez:source-object-bfp x))
(define (source-object-efp x) (check-so 'source-object-efp x) (%chez:source-object-efp x))
(define (source-object-line x) (check-so 'source-object-line x) (%chez:source-object-line x))
(define (source-object-column x) (check-so 'source-object-column x) (%chez:source-object-column x))
(define (%make-source-object sfd bfp efp line column) (%chez:make-source-object sfd bfp efp line column))
(define make-source-object
  (case-lambda
    [(sfd bfp efp) (check-source-object sfd bfp efp) (%make-source-object sfd bfp efp #f #f)]
    [(sfd bfp efp line column)
     (check-source-object sfd bfp efp)
     (unless (and (integer? line) (exact? line) (> line 0))
       (assertion-violation 'make-source-object "not an exact positive integer" line))
     (unless (and (integer? column) (exact? column) (> column 0))
       (assertion-violation 'make-source-object "not an exact positive integer" column))
     (%make-source-object sfd bfp efp line column)]))
(define (check-source-object sfd bfp efp)
  (unless (source-file-descriptor? sfd) (assertion-violation 'make-source-object "not a source file descriptor" sfd))
  (unless (and (integer? bfp) (exact? bfp)) (assertion-violation 'make-source-object "not an exact integer" bfp))
  (unless (and (integer? efp) (exact? efp)) (assertion-violation 'make-source-object "not an exact integer" efp))
  (unless (<= 0 bfp efp) (assertion-violation 'make-source-object "bfp is not between 0 and efp" bfp efp)))
(define current-make-source-object
  (make-parameter make-source-object
    (lambda (p) (unless (procedure? p) (assertion-violation 'current-make-source-object "not a procedure" p)) p)))

(define-enumeration annotation-option (debug profile) annotation-options)
(define make-annotation
  (case-lambda
    [(e source stripped) (make-annotation e source stripped (annotation-options debug profile))]
    [(e source stripped options)
     (unless (source-object? source) (assertion-violation 'make-annotation "not a source object" source))
     (%chez:make-annotation e source stripped options)]))
(define (annotation? x) (%chez:annotation? x))
(define (check-annotation who x) (unless (annotation? x) (assertion-violation who "not an annotation" x)))
(define (annotation-expression a) (check-annotation 'annotation-expression a) (%chez:annotation-expression a))
(define (annotation-source a) (check-annotation 'annotation-source a) (%chez:annotation-source a))
(define (annotation-stripped a) (check-annotation 'annotation-stripped a) (%chez:annotation-stripped a))
(define (annotation-option-set a) (check-annotation 'annotation-option-set a) (%chez:annotation-options a))
(define (syntax->annotation x) (%chez:syntax->annotation x))

;; where in its file a position is: (values path line column), or none
;; if the file isn't there (with the same checksum)
(define locate-source
  (case-lambda
    [(sfd pos) (locate-source sfd pos #f)]
    [(sfd pos use-cache?)
     (unless (source-file-descriptor? sfd) (assertion-violation 'locate-source "not a source file descriptor" sfd))
     (unless (and (integer? pos) (exact? pos) (>= pos 0))
       (assertion-violation 'locate-source "not an exact nonnegative integer" pos))
     (let ([ip (open-source-file sfd)])
       (if (not ip)
           (values)
           (let loop ([n 0] [line 1] [col 1])
             (let ([c (read-char ip)])
               (cond
                 [(or (= n pos) (eof-object? c))
                  (close-port ip)
                  (values (source-file-descriptor-path sfd) line col)]
                 [(char=? c #\newline) (loop (+ n 1) (+ line 1) 1)]
                 [else (loop (+ n 1) line (+ col 1))])))))]))
(define (locate-source-object-source src start? cache?)
  (unless (source-object? src) (assertion-violation 'locate-source-object-source "not a source object" src))
  (if (and start? (source-object-line src))
      (values (source-file-descriptor-path (source-object-sfd src)) (source-object-line src) (source-object-column src))
      (locate-source (source-object-sfd src) (if start? (source-object-bfp src) (source-object-efp src)) cache?)))
(define current-locate-source-object-source
  (make-parameter locate-source-object-source
    (lambda (p) (unless (procedure? p) (assertion-violation 'current-locate-source-object-source "not a procedure" p)) p)))
(define (open-source-file sfd)
  (let ([path (source-file-descriptor-path sfd)])
    (and (string? path) (file-exists? path)
         (let ([bv (call-with-port (open-file-input-port path) get-bytevector-all)])
           (and (= (bytes-checksum (if (eof-object? bv) (bytevector) bv)) (source-file-descriptor-checksum sfd))
                (open-input-file path))))))

;; get-datum/annotations: a reader that annotates each datum with its
;; source object (positions in characters from BFP)
(define (get-datum/annotations ip sfd bfp)
  (unless (and (input-port? ip) (textual-port? ip))
    (assertion-violation 'get-datum/annotations "not a textual input port" ip))
  (unless (source-file-descriptor? sfd)
    (assertion-violation 'get-datum/annotations "not a source file descriptor" sfd))
  (unless (and (integer? bfp) (exact? bfp) (>= bfp 0))
    (assertion-violation 'get-datum/annotations "not a valid file position" bfp))
  (let ([pos bfp])
    (define (peek) (peek-char ip))
    (define (next) (let ([c (read-char ip)]) (unless (eof-object? c) (set! pos (+ pos 1))) c))
    (define (annotate e start stripped)
      (make-annotation e (make-source-object sfd start pos) stripped))
    (define (delimiter? c)
      (or (eof-object? c) (char-whitespace? c) (memv c '(#\( #\) #\[ #\] #\" #\;))))
    (define (skip-atmosphere)
      (let ([c (peek)])
        (cond
          [(eof-object? c) #f]
          [(char-whitespace? c) (next) (skip-atmosphere)]
          [(char=? c #\;) (let loop () (let ([c (next)]) (unless (or (eof-object? c) (char=? c #\newline)) (loop)))) (skip-atmosphere)]
          [else #f])))
    (define (read-token)
      (let loop ([cs '()])
        (if (delimiter? (peek)) (list->string (reverse cs)) (loop (cons (next) cs)))))
    (define (read-list close start)
      (let loop ([items '()])
        (skip-atmosphere)
        (let ([c (peek)])
          (cond
            [(eof-object? c) (error 'get-datum/annotations "unexpected end of file")]
            [(memv c '(#\) #\])) (next)
             (let ([es (reverse items)])
               (annotate es start (map annotation-stripped es)))]
            [(and (char=? c #\.) (pair? items))
             (let ([dot-start pos] [tok (read-token)])
               (if (string=? tok ".")
                   (let ([tail (read-one)])
                     (skip-atmosphere) (next)
                     (let ([es (reverse items)])
                       (annotate (append es tail) start (append (map annotation-stripped es) (annotation-stripped tail)))))
                   (loop (cons (atom tok dot-start) items))))]
            [else (loop (cons (read-one) items))]))))
    (define (atom tok start)
      (let ([d (read (open-input-string tok))]) (annotate d start d)))
    (define (prefixed sym start)
      (let ([x (read-one)])
        (annotate (list (annotate sym start sym) x) start (list sym (annotation-stripped x)))))
    (define (read-one)
      (skip-atmosphere)
      (let ([start pos] [c (peek)])
        (cond
          [(eof-object? c) c]
          [(memv c '(#\( #\[)) (next) (read-list c start)]
          [(char=? c #\') (next) (prefixed 'quote start)]
          [(char=? c #\`) (next) (prefixed 'quasiquote start)]
          [(char=? c #\,) (next)
           (if (eqv? (peek) #\@) (begin (next) (prefixed 'unquote-splicing start)) (prefixed 'unquote start))]
          [(char=? c #\")
           (next)
           (let loop ([cs (list #\")])
             (let ([c (next)])
               (cond
                 [(char=? c #\\) (loop (cons (next) (cons c cs)))]
                 [(char=? c #\") (atom (list->string (reverse (cons c cs))) start)]
                 [else (loop (cons c cs))])))]
          [(char=? c #\#)
           (next)
           (let ([d (peek)])
             (cond
               [(eqv? d #\() (next)
                (let ([l (read-list #\( start)])
                  (annotate (list->vector (annotation-expression l)) start
                            (list->vector (annotation-stripped l))))]
               [(eqv? d #\') (next) (prefixed 'syntax start)]
               [(eqv? d #\`) (next) (prefixed 'quasisyntax start)]
               [(eqv? d #\,) (next)
                (if (eqv? (peek) #\@) (begin (next) (prefixed 'unsyntax-splicing start)) (prefixed 'unsyntax start))]
               [(eqv? d #\\) (next)
                (let ([first (next)])
                  (atom (string-append "#\\" (string first) (read-token)) start))]
               [else (atom (string-append "#" (read-token)) start)]))]
          [else (atom (read-token) start)])))
    (let ([x (read-one)])
      (values x pos))))
(define (read-token . args)
  (error 'read-token "not supported"))

(define compile-library-handler (make-parameter (lambda args (if #f #f))))
(define engine-block %chez:engine-block)
(define (r6rs:command-line) (command-line))
