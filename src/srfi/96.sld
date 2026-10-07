;;; SRFI 96: SLIB prerequisites.  Written for Pseudoscheme, after the
;;; specification (each SLIB .init file is an implementation for one
;;; Scheme, and the SRFI has no portable one).  SRFI 59's vicinities,
;;; which SRFI 96 requires, are re-exported from (srfi 59); with-load-
;;; pathname binds the pathname program-vicinity reads.
;;;
;;; How it maps onto Pseudoscheme:
;;;   - software-type is unix (ms-dos on Windows), scheme-implementation-
;;;     type pseudoscheme, scheme-implementation-version "3.0".
;;;     scheme-implementation-home-page is #f.
;;;   - slib:eval, macro:eval, defmacro:eval and the loaders evaluate at
;;;     the R7RS REPL's top level (Lisp's pseudoscheme-r7rs::eval-at-repl),
;;;     where definitions, define-syntax and import work.  The first use
;;;     imports (srfi 96) there, so loaded SLIB code sees these bindings.
;;;   - The loaders read and evaluate a file form by form (R7RS load, in
;;;     Pseudoscheme, runs a file as a program, which must begin with an
;;;     import).  slib:load-compiled is an error: there is no separately
;;;     loadable compiled code.
;;;   - defmacro defines a non-hygienic macro on syntax-case, and also
;;;     records its expander, so macroexpand and defmacro:expand* (and so
;;;     defmacro:eval) work on a form as data.  macro:expand returns #f, as
;;;     the SRFI allows.
;;;   - file-position uses R6RS port positions; it returns #f where a port
;;;     has none, and #t when it sets a position.  output-port-width and
;;;     output-port-height are 79 and 24.
;;;   - system runs a shell command and returns its exit status;
;;;     browse-url opens the URL with open (macOS) or xdg-open.
;;;   - make-exchanger's swap isn't atomic with respect to SRFI 18 threads.
;;;   - most-positive-fixnum is R6RS's greatest-fixnum (SBCL's fixnum
;;;     limit); char-code-limit is the host's, #x110000.
;;;   - The R7RS bindings that meet the specification are re-exported:
;;;     file-exists?, port?, close-port, current-error-port, and SRFI 1's
;;;     last-pair.  delete-file is SRFI 96's (it returns #f rather than
;;;     raising an error), so it clashes with (scheme file)'s.
(define-library (srfi 96)
  (export
   ;; Configuration
   software-type scheme-implementation-type scheme-implementation-version
   scheme-implementation-home-page scheme-file-suffix slib:features
   most-positive-fixnum char-code-limit
   ;; File system, and SRFI 59
   program-vicinity library-vicinity implementation-vicinity user-vicinity
   home-vicinity in-vicinity sub-vicinity make-vicinity pathname->vicinity
   vicinity:suffix?
   with-load-pathname tmpnam file-exists? delete-file
   ;; Input/output
   open-file port? close-port call-with-open-ports current-error-port
   force-output file-position output-port-width output-port-height
   ;; Defmacro
   defmacro gentemp defmacro:eval defmacro:load macroexpand defmacro:expand*
   ;; R5RS macros
   macro:expand macro:eval macro:load
   ;; System
   slib:load-source slib:load-compiled slib:load slib:eval slib:eval-load
   slib:warn slib:error slib:exit browse-url getenv system program-arguments
   ;; Miscellany
   identity slib:tab slib:form-feed make-exchanger t nil last-pair)
  (import (except (scheme base) delete-file)
          (scheme read)
          (rename (scheme file) (delete-file r7:delete-file))
          (scheme process-context)
          (scheme time)
          (rnrs syntax-case)
          (only (rnrs arithmetic fixnums) greatest-fixnum)
          (only (rnrs io ports) port-has-port-position? port-position
                port-has-set-port-position!? set-port-position!)
          (only (srfi 1) last-pair)
          (srfi 59)
          (srfi private srfi-59-support)
          (only (pseudoscheme chez host) chez:system)
          (only (pseudoscheme lisp) lisp-function)
          (prefix (only (cl common-lisp) char-code-limit) cl:))
  (begin
    ;; Configuration

    (define (scheme-implementation-type) 'pseudoscheme)
    (define (scheme-implementation-version) "3.0")
    (define (scheme-implementation-home-page) #f)
    (define (scheme-file-suffix) ".scm")

    (define slib:features
      '(source vicinity srfi-59 srfi-96 r5rs eval values dynamic-wind macro
        defmacro defmacroexpand delay multiarg-apply char-ready?
        rev4-optional-procedures full-continuation ieee-floating-point
        system getenv program-arguments))

    (define most-positive-fixnum (greatest-fixnum))
    (define char-code-limit cl:char-code-limit)

    ;; File system

    (define (temporary-directory)
      (let ((dir (or (get-environment-variable "TMPDIR") "/tmp/")))
        (if (vicinity:suffix? (string-ref dir (- (string-length dir) 1)))
            dir
            (string-append dir "/"))))

    (define tmpnam
      (let ((count 0))
        (lambda ()
          (let loop ()
            (set! count (+ count 1))
            (let ((name (string-append (temporary-directory) "slib"
                                       (number->string (current-jiffy) 36)
                                       "-" (number->string count))))
              (if (file-exists? name) (loop) name))))))

    (define (delete-file filename)
      (guard (e (#t #f))
        (r7:delete-file filename)
        #t))

    ;; Input/output

    (define (open-file filename modes)
      (case modes
        ((r) (open-input-file filename))
        ((rb) (open-binary-input-file filename))
        ((w) (open-output-file filename))
        ((wb) (open-binary-output-file filename))
        (else (slib:error "open-file: unknown mode" modes))))

    ;; (call-with-open-ports proc port ...) or (call-with-open-ports port ... proc)
    (define (call-with-open-ports . args)
      (let* ((proc-first? (procedure? (car args)))
             (proc (if proc-first? (car args) (car (last-pair args))))
             (ports (if proc-first?
                        (cdr args)
                        (let loop ((l args))
                          (if (null? (cdr l)) '() (cons (car l) (loop (cdr l))))))))
        (call-with-values (lambda () (apply proc ports))
          (lambda vals
            (for-each close-port ports)
            (apply values vals)))))

    (define (force-output . port)
      (flush-output-port (if (pair? port) (car port) (current-output-port))))

    (define (file-position port . k)
      (guard (e (#t #f))
        (if (null? k)
            (and (port-has-port-position? port) (port-position port))
            (and (port-has-set-port-position!? port)
                 (begin (set-port-position! port (car k)) #t)))))

    (define (output-port-width . port) 79)
    (define (output-port-height . port) 24)

    ;; System

    (define repl-eval (lisp-function "eval-at-repl" "pseudoscheme-r7rs"))
    (define repl-ready? #f)

    (define (slib:eval obj)
      (unless repl-ready?
        (repl-eval '(import (srfi 96)))
        (set! repl-ready? #t))
      (repl-eval obj))

    (define (slib:eval-load filename eval)
      (with-load-pathname filename
        (lambda ()
          (call-with-input-file filename
            (lambda (in)
              (let loop ()
                (let ((form (read in)))
                  (unless (eof-object? form)
                    (eval form)
                    (loop)))))))))

    (define (source-file name)
      (let ((with-suffix (string-append name (scheme-file-suffix))))
        (if (and (not (file-exists? name)) (file-exists? with-suffix))
            with-suffix
            name)))

    (define (slib:load-source name) (slib:eval-load (source-file name) slib:eval))
    (define (slib:load-compiled name)
      (slib:error "slib:load-compiled: Pseudoscheme has no separately loadable compiled code" name))
    (define (slib:load name) (slib:load-source name))

    (define (slib:exit . n)
      (exit (if (null? n) #t (car n))))

    (define (system command) (chez:system command))

    (define (shell-quote s)
      (let loop ((cs (string->list s)) (acc '(#\')))
        (cond ((null? cs) (list->string (reverse (cons #\' acc))))
              ((char=? (car cs) #\')
               (loop (cdr cs) (append (reverse (string->list "'\\''")) acc)))
              (else (loop (cdr cs) (cons (car cs) acc))))))

    (define (browse-url url)
      (let ((opener (cond-expand (darwin "open") (windows "start") (else "xdg-open"))))
        (zero? (system (string-append opener " " (shell-quote url))))))

    (define (program-arguments) (command-line))

    ;; R5RS macros

    (define (macro:expand sexpression) #f)
    (define (macro:eval sexpression) (slib:eval sexpression))
    (define (macro:load filename) (slib:eval-load filename macro:eval))

    ;; Defmacro

    (define gentemp
      (let ((count -1))
        (lambda ()
          (set! count (+ count 1))
          (string->symbol (string-append "slib:G" (number->string count))))))

    (define defmacros '())              ; ((name . expander) ...)

    (define (register-defmacro! name expander)
      (set! defmacros (cons (cons name expander) defmacros))
      name)

    (define (defmacro-expander form)
      (and (pair? form)
           (symbol? (car form))
           (let ((entry (assq (car form) defmacros)))
             (and entry (cdr entry)))))

    (define (macroexpand form)
      (let ((expander (defmacro-expander form)))
        (if expander
            (macroexpand (apply expander (cdr form)))
            form)))

    (define (defmacro:expand* e)
      (cond ((not (pair? e)) e)
            ((eq? (car e) 'quote) e)
            ((defmacro-expander e) (defmacro:expand* (macroexpand e)))
            (else
             (let loop ((e e))
               (cond ((pair? e) (cons (defmacro:expand* (car e)) (loop (cdr e))))
                     (else e))))))

    (define (defmacro:eval e) (slib:eval (defmacro:expand* e)))
    (define (defmacro:load filename) (slib:eval-load filename defmacro:eval))

    (define-syntax defmacro
      (lambda (x)
        (syntax-case x ()
          ((_ name lambda-list body1 body2 ...)
           (identifier? #'name)
           #'(begin
               (define-syntax name
                 (lambda (y)
                   (syntax-case y ()
                     ((k . args)
                      (datum->syntax
                       #'k
                       (apply (lambda lambda-list body1 body2 ...)
                              (syntax->datum #'args)))))))
               (define ignored
                 (register-defmacro! 'name (lambda lambda-list body1 body2 ...))))))))

    ;; Miscellany

    (define (identity x) x)
    (define slib:tab #\tab)
    (define slib:form-feed (integer->char 12))

    (define (make-exchanger obj)
      (lambda (new)
        (let ((old obj))
          (set! obj new)
          old)))

    (define t #t)
    (define nil #f)))
