;;; (srfi private srfi-147-syntax-rules): (scheme base)'s syntax-rules,
;;; with a workaround, for SRFI 147's implementation.  psyntax's
;;; syntax-rules mishandles a custom ellipsis together with ... as a
;;; literal, (syntax-rules ::: (...) ...): the literal ... never matches
;;; (it is renamed like an ordinary ... pattern variable).  SRFI 148's
;;; 148.scm has such rules.  Here, in that case, the rules' patterns and
;;; literals use %literal-ellipsis for ..., and the transformer maps the
;;; input's ... to it, and back in the output.  Otherwise this is
;;; (scheme base)'s syntax-rules.
(define-library (srfi private srfi-147-syntax-rules)
  (export syntax-rules)
  (import (rename (scheme base) (syntax-rules r7:syntax-rules))
          (only (rnrs syntax-case) syntax-case syntax identifier?
                free-identifier=? with-syntax))
  (begin
(define-syntax %literal-ellipsis (r7:syntax-rules ()))

(define-syntax syntax-rules
  (lambda (x)
    (define (memp pred l)
      (cond ((null? l) #f) ((pred (car l)) l) (else (memp pred (cdr l)))))
    (define (dots? id) (and (identifier? id) (free-identifier=? id (syntax (... ...)))))
    (define (replace-dots form)
      (syntax-case form ()
        ((a . b) (cons (replace-dots (syntax a)) (replace-dots (syntax b))))
        (#(a ...) (list->vector (map replace-dots (syntax (a ...)))))
        (id (dots? (syntax id)) (syntax %literal-ellipsis))
        (_ form)))
    (syntax-case x ()
      ((_ ell (lit ...) (pattern template) ...)
       (and (identifier? (syntax ell))
            (not (dots? (syntax ell)))
            (memp dots? (syntax (lit ...))))
       (with-syntax (((lit2 ...) (replace-dots (syntax (lit ...))))
                     ((pattern2 ...) (replace-dots (syntax (pattern ...)))))
         (syntax
          (let ((transformer (r7:syntax-rules ell (lit2 ...)
                               (pattern2 template) ...)))
            (lambda (form)
              (restore-dots (transformer (mark-dots form))))))))
      ((_ . rest) (syntax (r7:syntax-rules . rest))))))

(define (map-identifiers f form)
  (syntax-case form ()
    ((a . b) (cons (map-identifiers f (syntax a)) (map-identifiers f (syntax b))))
    (#(a ...) (list->vector (map (lambda (x) (map-identifiers f x)) (syntax (a ...)))))
    (id (identifier? (syntax id)) (f (syntax id)))
    (_ form)))

(define (mark-dots form)
  (map-identifiers (lambda (id)
                     (if (free-identifier=? id (syntax (... ...)))
                         (syntax %literal-ellipsis)
                         id))
                   form))

(define (restore-dots form)
  (map-identifiers (lambda (id)
                     (if (free-identifier=? id (syntax %literal-ellipsis))
                         (syntax (... ...))
                         id))
                   form))))
