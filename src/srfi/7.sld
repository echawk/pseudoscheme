;;; SRFI 7: feature-based program configuration language.  Richard
;;; Kelsey's PROCESS-PROGRAM, unmodified (reference/srfi-7/
;;; process-program.scm; MIT licence, in reference/srfi-7/LICENSE),
;;; turns a program into its Scheme forms for the features at hand
;;; (those of R7RS's (features)).  The SRFI leaves running them to the
;;; implementation.  Here they run as an R7RS program, through the R7RS
;;; front end (Lisp's pseudoscheme-r7rs::eval-forms), whose imports are:
;;;
;;;   - each library (srfi N) for a feature srfi-N named in a reached
;;;     REQUIRES clause or in the requirement of a chosen FEATURE-COND
;;;     clause, when Pseudoscheme has that library;
;;;   - all of R7RS-small, except for the names those SRFI libraries
;;;     export, so a SRFI's definition wins over R7RS's (SRFI 13's
;;;     string-map, SRFI 43's vector-map, ...).
;;;
;;; So (program (requires srfi-1) (code (display (iota 3)))) works with
;;; no import of (srfi 1).  Two required SRFI libraries that export
;;; different bindings under one name still conflict.
;;;
;;; Exports:
;;;   (program <program clause> ...)  syntax: run the program, in its own
;;;       top level (the code doesn't see the surrounding program's
;;;       bindings, and its definitions aren't visible after it).  The
;;;       value is that of the last form.  A FILES clause reads its files
;;;       when the program runs, relative to the current directory.
;;;   (load-program filename)  run the (program ...) form in a file.
;;;   (process-program program features)  the reference procedure.
;;;
;;; The reference implementation's second form, the PROGRAM macro on
;;; cond-expand (reference/srfi-7/program-macro.scm), splices the code
;;; into the surrounding program instead; it isn't used, since it can't
;;; import what a REQUIRES clause names.
(define-library (srfi 7)
  (export program process-program load-program)
  (import (scheme base)
          (scheme eval)
          (scheme read)
          (scheme file)
          (only (pseudoscheme host) psyntax:environment-symbols)
          (only (pseudoscheme lisp) lisp-function))
  (include "reference/srfi-7/process-program.scm")
  (begin
    (define r7rs-small-libraries
      '((scheme base) (scheme case-lambda) (scheme char) (scheme complex)
        (scheme cxr) (scheme eval) (scheme file) (scheme inexact)
        (scheme lazy) (scheme load) (scheme process-context) (scheme read)
        (scheme repl) (scheme time) (scheme write)))

    (define eval-forms (lisp-function "eval-forms" "pseudoscheme-r7rs"))

    ;; The features that PROCESS-PROGRAM's choices rest on: those in
    ;; reached REQUIRES clauses and in the requirements of the chosen
    ;; FEATURE-COND clauses (the same choices PROCESS-PROGRAM makes).
    (define (program-features program features)
      (define (satisfied? req)
        (cond ((symbol? req) (memq req features))
              ((eq? (car req) 'and) (all satisfied? (cdr req)))
              ((eq? (car req) 'or) (any satisfied? (cdr req)))
              ((eq? (car req) 'not) (not (satisfied? (cadr req))))
              (else #f)))
      (define (all p l) (or (null? l) (and (p (car l)) (all p (cdr l)))))
      (define (any p l) (and (pair? l) (or (p (car l)) (any p (cdr l)))))
      (define (identifiers req)               ; positive ones only
        (cond ((symbol? req) (list req))
              ((memq (car req) '(and or)) (apply append (map identifiers (cdr req))))
              (else '())))
      (define (clauses cs) (apply append (map clause cs)))
      (define (clause c)
        (case (car c)
          ((requires) (cdr c))
          ((feature-cond)
           (let loop ((cs (cdr c)))
             (cond ((null? cs) '())
                   ((and (eq? (caar cs) 'else) (null? (cdr cs)))
                    (clauses (cdar cs)))
                   ((satisfied? (caar cs))
                    (append (identifiers (caar cs)) (clauses (cdar cs))))
                   (else (loop (cdr cs))))))
          (else '())))
      (clauses (cdr program)))

    ;; srfi-N -> (srfi N), if that's a library here
    (define (feature-library feature)
      (let ((name (symbol->string feature)))
        (and (> (string-length name) 5)
             (string=? (substring name 0 5) "srfi-")
             (let ((n (string->number (substring name 5 (string-length name)))))
               (and n (exact-integer? n)
                    (let ((lib (list 'srfi n)))
                      (and (guard (e (#t #f)) (environment lib) #t)
                           lib)))))))

    (define (delete-duplicates l)
      (cond ((null? l) '())
            ((member (car l) (cdr l)) (delete-duplicates (cdr l)))
            (else (cons (car l) (delete-duplicates (cdr l))))))

    (define (program-imports program features)
      (let* ((srfis (delete-duplicates
                     (let loop ((fs (program-features program features)))
                       (cond ((null? fs) '())
                             ((feature-library (car fs))
                              => (lambda (lib) (cons lib (loop (cdr fs)))))
                             (else (loop (cdr fs)))))))
             (taken (apply append
                           (map (lambda (lib)
                                  (psyntax:environment-symbols (environment lib)))
                                srfis))))
        (append
         (map (lambda (lib)
                (let ((clash (let loop ((names (psyntax:environment-symbols
                                                (environment lib))))
                               (cond ((null? names) '())
                                     ((memq (car names) taken)
                                      (cons (car names) (loop (cdr names))))
                                     (else (loop (cdr names)))))))
                  (if (null? clash) lib `(except ,lib ,@clash))))
              r7rs-small-libraries)
         srfis)))

    (define (run-program program)
      (let* ((features (features))
             (forms (process-program program features)))
        (if (not forms)
            (error "program: this implementation lacks a feature the program requires"
                   program)
            (eval-forms (cons (cons 'import (program-imports program features))
                              forms)))))

    (define-syntax program
      (syntax-rules ()
        ((_ clause ...) (run-program '(program clause ...)))))

    (define (load-program filename)
      (let ((form (call-with-input-file filename read)))
        (if (and (pair? form) (eq? (car form) 'program))
            (run-program form)
            (error "load-program: not a (program ...) form" filename))))))
