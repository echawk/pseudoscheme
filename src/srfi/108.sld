;;; SRFI 108: Named quasi-literal constructors.  Written for Pseudoscheme.
;;; The reader reads &name[init ...]{text} (src/quasi.lisp) as
;;; ($construct$:name init ... $>>$ part ...) and &name{text} as
;;; ($construct$:name part ...), the parts as for SRFI 109's &{text}.
;;; define-simple-constructor defines $construct$:name to call a maker
;;; with the initial arguments and the text made a string ($string$, or
;;; another procedure).
(define-library (srfi 108)
  (export define-simple-constructor $string$ $<<$ $>>$)
  (import (scheme base) (rnrs syntax-case) (srfi 109))
  (begin
    ;; (%construct-split maker str-maker arg ...): the arguments up to
    ;; $>>$ are initial ones; with no $>>$, all are text.
    (define-syntax %construct-split
      (lambda (x)
        (syntax-case x ()
          ((_ maker str-maker arg ...)
           (let ((args #'(arg ...)))
             (let loop ((as args) (init '()))
               (cond ((null? as) #`(maker (str-maker #,@args)))
                     ((and (identifier? (car as)) (free-identifier=? (car as) #'$>>$))
                      #`(maker #,@(reverse init) (str-maker #,@(cdr as))))
                     (else (loop (cdr as) (cons (car as) init))))))))))

    (define-syntax define-simple-constructor
      (lambda (x)
        (syntax-case x ()
          ((_ cname maker) #'(define-simple-constructor cname maker $string$))
          ((_ cname maker str-maker)
           (with-syntax ((name (datum->syntax
                                #'cname
                                (string->symbol
                                 (string-append "$construct$:"
                                                (symbol->string (syntax->datum #'cname)))))))
             #'(define-syntax name
                 (syntax-rules ()
                   ((_ . args) (%construct-split maker str-maker . args)))))))))))
