;;; chez-mat.ss: a mat.ss for Pseudoscheme's --chez mode.
;;;
;;; Chez Scheme's mats (vendor/chez-mats/*.ms) are run by the mat macro
;;; of vendor/chez-mats/mat.ss, which counts nothing and writes its
;;; report to a file.  This replaces it: the same `mat` syntax, the same
;;; helper definitions the mats use (~=, nan, rm-rf, mkfile, ...), and
;;; one line per clause on stdout:
;;;
;;;   MAT <result> <mat name> <clause number> [message]
;;;
;;; where <result> is
;;;   pass     the clause was true (or an expected error was raised)
;;;   fail     it was false, or raised no error where one was expected
;;;   error    it raised an unexpected condition
;;;   unbound  it referred to a name not bound here (a missing export)
;;;   unsup    it needs something Pseudoscheme won't have: a `#%$`
;;;            system primitive, the compiler's output files, the
;;;            inspector, ... (decided from the clause's text, by
;;;            `unsupported-clause?`)
;;;
;;; Run it as tests/run-chez-mats.sh does: `pseudoscheme --chez
;;; tests/chez-mat.ss FILE.ms`.  The helper definitions below follow
;;; vendor/chez-mats/mat.ss (Copyright 1984-2017 Cisco Systems, Inc.,
;;; Apache License 2.0; see vendor/chez-mats/LICENSE).

(define-syntax mat
  (lambda (x)
    (syntax-case x (parameters)
      [(_ x (parameters [param val ...] ...) e ...)
       #'(let f ([p* (list param ...)] [v** (list (list val ...) ...)])
           (if (null? p*)
               (mat x e ...)
               (let ([p (car p*)])
                 (for-each
                   (lambda (v)
                     (parameterize ([p v])
                       (f (cdr p*) (cdr v**))))
                   (car v**)))))]
      [(_ x e ...)
       #'(mat-run 'x '(e) ...)])))

(define enable-cp0 (make-parameter #f))
(define mat-output (make-parameter (current-output-port)))

(define-syntax mat/cf
  (syntax-rules (testfile)
    [(_ (testfile ?path) expr ...)
     (error 'mat/cf "compile-file is not supported")]
    [(_ expr ...) (error 'mat/cf "compile-file is not supported")]))

(define (string-contains? s pat)
  (let ([n (string-length s)] [m (string-length pat)])
    (let loop ([i 0])
      (cond [(> (+ i m) n) #f]
            [(string=? (substring s i (+ i m)) pat) #t]
            [else (loop (+ i 1))]))))

;; clauses that exercise what Pseudoscheme doesn't implement: Chez's
;; compiler and its files, the inspector, `#%$` system primitives
(define unsupported-patterns
  '("#%$" "($" " $" "'$" "mat/cf" "compile-file" "compile-script" "compile-library"
    "compile-program" "expand/optimize" "inspect/object" "separate-eval"
    "separate-compile" "run-script" "test-cp0-expansion" "fasl-" "strip-fasl"
    "(compile " "interpret" "cp0" "generate-inspector" "generate-procedure-source"
    "enable-object-counts" "object-counts" "code-object" "(inspect " "profile-"
    "compile-profile" "source-table" "make-boot" "heap-check" "(exit" "(emergency-exit" "(abort" "foreign-callable"
    "foreign-procedure" "ftype" "load-shared-object" "collect-request-handler" "collect-trip-bytes"
    "collect-generation-radix" "collect-notify" "collect-rendezvous" "optimize-level"
    "generate-wpo" "wpo-" "compile-interpret-simple" "(expand " "expand-output"
    "debug-level" "enable-type-recovery" "check-loop-allocation"))

(define (unsupported-clause? clause)
  (let ([s (guard (c [#t ""])
             (call-with-string-output-port (lambda (p) (write clause p))))])
    (let loop ([ps unsupported-patterns])
      (cond [(null? ps) #f]
            [(string-contains? s (car ps)) #t]
            [else (loop (cdr ps))]))))

(define (condition-message-string c)
  (let ([s (call-with-string-output-port (lambda (p) (display-condition c p)))])
    (let loop ([i 0] [out '()])
      (if (= i (string-length s))
          (list->string (reverse out))
          (loop (+ i 1)
                (cons (let ([ch (string-ref s i)])
                        (if (or (char=? ch #\newline) (char>? ch #\~)) #\space ch))
                      out))))))

(define (unbound-message? m)
  (or (string-contains? m "is not bound")
      (string-contains? m "unbound variable")
      (string-contains? m "undefined variable")
      (string-contains? m "is undefined")
      (string-contains? m "attempt to reference unbound")))

;; => (pass) | (fail) | (error . message) | (warning . message)
(define (mat-one-exp expect th)
  (call/cc
    (lambda (k)
      (with-exception-handler
        (lambda (c)
          (cond
            [(and (warning? c) (not (eq? expect 'warning)))
             (raise-continuable c)]
            [else
             (k (cons (if (warning? c) 'warning 'error)
                      (guard (c2 [#t "?"]) (condition-message-string c))))]))
        (lambda ()
          (case (th)
            [(#t) '(true)]
            [(#f) '(false)]
            [else '(bogus)]))))))

(define mat-counts (list 0 0 0 0 0))   ; pass fail error unbound unsup

(define (dollar-unbound? m)
  ;; "variable $foo is not bound": a system primitive
  (string-contains? m "variable $"))

(define (mat-report result name count . msg)
  (printf "MAT ~a ~a ~a" result name count)
  (when (pair? msg) (printf " ~a" (car msg)))
  (newline)
  (flush-output-port (current-output-port)))

(define mat-skip
  ;; CHEZ_MAT_SKIP="name:n name:n": clauses that hung or killed an earlier
  ;; run of this file (tests/run-chez-mats.sh), reported as `crash`
  (let ([s (or (getenv "CHEZ_MAT_SKIP") "")])
    (let loop ([i 0] [start 0] [acc '()])
      (cond [(= i (string-length s))
             (reverse (if (> i start) (cons (substring s start i) acc) acc))]
            [(char=? (string-ref s i) #\space)
             (loop (+ i 1) (+ i 1) (if (> i start) (cons (substring s start i) acc) acc))]
            [else (loop (+ i 1) start acc)]))))

(define (mat-run name . clauses)
  (do ([clauses clauses (cdr clauses)]
       [count 1 (+ count 1)])
      ((null? clauses) 'done)
    (let ([clause (caar clauses)])
      (printf "MAT start ~a ~a\n" name count)
      (flush-output-port (current-output-port))
      (cond
        [(member (format "~a:~a" name count) mat-skip)
         (mat-report 'crash name count)]
        [(unsupported-clause? clause)
         (mat-report 'unsup name count)]
        [(and (list? clause) (= (length clause) 2)
              (memq (car clause) '(sanitized-error? error? warning?)))
         (let* ([expect (if (eq? (car clause) 'warning?) 'warning 'error)]
                [ans (mat-one-exp expect (lambda () (eval (cadr clause))))])
           (cond
             [(and (pair? ans) (eq? (car ans) expect))
              (mat-report 'pass name count)]
             [(and (pair? ans) (memq (car ans) '(error warning))
                   (unbound-message? (cdr ans)))
              (if (dollar-unbound? (cdr ans))
                  (mat-report 'unsup name count)
                  (mat-report 'unbound name count (cdr ans)))]
             [else (mat-report 'fail name count)]))]
        [else
         (let ([ans (mat-one-exp #f (lambda () (eval clause)))])
           (cond
             [(equal? ans '(true)) (mat-report 'pass name count)]
             [(equal? ans '(false)) (mat-report 'fail name count)]
             [(equal? ans '(bogus)) (mat-report 'fail name count "nonboolean")]
             [(and (unbound-message? (cdr ans)) (dollar-unbound? (cdr ans)))
              (mat-report 'unsup name count)]
             [(unbound-message? (cdr ans)) (mat-report 'unbound name count (cdr ans))]
             [else (mat-report 'error name count (cdr ans))]))]))))

(define equivalent-expansion?
  (lambda (x y) (equal? x y)))

(define *fuzz* 1e-14)

(define ~=
   (lambda (x y)
      (or (= x y)
          (and (fl~= (inexact (real-part x))
                     (inexact (real-part y)))
               (fl~= (inexact (imag-part x))
                     (inexact (imag-part y)))))))

(define fl~=
  (case-lambda
   [(x y fuzz)
    (cond
      [(and (fl>= (flabs x) 2.0) (fl>= (flabs y) 2.0))
       (fl~= (fl/ x 2.0) (fl/ y 2.0) fuzz)]
      [(and (fl< 0.0 (flabs x) 1.0) (fl< 0.0 (flabs y) 1.0))
       (fl~= (fl* x 2.0) (fl* y 2.0) fuzz)]
      [else (let ([d (flabs (fl- x y))])
              (or (fl<= d fuzz)
                  (begin (printf "fl~~=: ~s~%" d) #f)))])]
   [(x y) (fl~= x y *fuzz*)]))

(define cfl~=
   (lambda (x y)
      (and (fl~= (cfl-real-part x) (cfl-real-part y))
           (fl~= (cfl-imag-part x) (cfl-imag-part y)))))

(define == eqv?)

(define (nan) (/ 0.0 0.0))
(define pi (* (asin 1.0) 2))
(define +pi   3.14159265358979323846264)
(define +pi/2 1.57079632679489661923132)
(define +pi/4  .78539816339744830961566)
(define -pi (- +pi))
(define -pi/2 (- +pi/2))
(define -pi/4 (- +pi/4))

; smallest ieee flonum
(define +e 4.940656458412465e-324)
(define -e (- +e))

(define windows? (lambda () #f))
(define haiku? (lambda () #f))
(define embedded? (lambda () #f))
(define pb? (lambda () #f))

(define *scheme* "scheme")
(define *mats-dir* ".")
(define $cat_flush "./cat_flush")

(define rm-rf
  (lambda (path)
    (when (file-exists? path)
      (let f ([path path])
        (if (file-directory? path)
            (begin
              (for-each (lambda (x) (f (format "~a/~a" path x))) (directory-list path))
              (delete-directory path))
            (delete-file path))))))

(define mkfile
  (lambda (filename . expr*)
    (with-output-to-file filename
      (lambda () (for-each pretty-print expr*))
      'replace)))

(define-syntax retry-for-spurious
  (syntax-rules ()
    [(_ e) e]))

(define find-source
  (lambda (fn)
    (or (ormap (lambda (dir)
                 (let ([fn (path-build-normal dir fn)])
                   (and (file-exists? fn) fn)))
               (source-directories))
        fn)))

(define path-build-normal
  (lambda (dir fn)
    (cond
      [(path-absolute? fn) fn]
      [(equal? dir ".") fn]
      [else (path-build dir fn)])))

(define patch-exec-path (lambda (p) p))

(define touch
  (lambda (objfn srcfn)
    (let ([p (open-file-input/output-port srcfn (file-options no-fail no-truncate))])
      (put-u8 p (lookahead-u8 p))
      (close-port p))
    #t))

(define heap-check-interval (make-parameter 0))

;; the run: load each file named on the command line a form at a time (as
;; `load` does), so a form that fails to evaluate costs only itself
(define (run-mat-file file)
  (let ([port (open-input-file file)])
    (let loop ([n 0])
      (let ([x (guard (c [#t (printf "MAT loaderror ~a after form ~a: ~a\n" file n
                                     (condition-message-string c))
                             (eof-object)])
                 (read port))])
        (unless (eof-object? x)
          (guard (c [#t (printf "MAT loaderror ~a form ~a: ~a\n" file n
                                (condition-message-string c))])
            (eval x))
          (loop (+ n 1)))))
    (close-port port)))

(let ([args (command-line-arguments)])
  (for-each run-mat-file args)
  (printf "MATDONE\n"))
