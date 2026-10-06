;;; (srfi private srfi-242-define-property): enough of SRFI 213
;;; (identifier properties) for SRFI 242's reference implementation,
;;; which imports it as (srfi :213) and (srfi :213 define-property).
;;; SRFI 213's own implementation is Chez Scheme's define-property;
;;; psyntax has no identifier properties, so this emulates them.
;;; Written for Pseudoscheme.
;;;
;;;   (define-property id key expr)  a definition: attaches the value of
;;;       EXPR (evaluated at expansion time) to the binding of ID under
;;;       the binding of KEY.
;;;   define-syntax  R6RS's, except that when a transformer returns a
;;;       procedure (as SRFI 213 allows) the procedure is called with a
;;;       lookup procedure, (lookup id key), which returns the property,
;;;       or #f.  That is how SRFI 213's transformers see properties.
;;;   (capture-lookup proc)  PROC: such a transformer's result.
;;;   syntax  R6RS's, except that a list it makes ends in (), not a
;;;       wrapped () (see normalize-syntax).
;;;
;;; A property is registered when its definition is expanded, and when
;;; the library containing it is visited or invoked.  psyntax visits a
;;; library loaded from the cache only when one of its macros is used,
;;; so a property defined in a library is found only once that library
;;; has been visited or invoked (see 242.sld).
;;;
;;; Properties are kept in a list at expansion time and found with
;;; free-identifier=? on both the identifier and the key, so a property
;;; follows the binding (not the name), as in SRFI 213.  Unlike SRFI
;;; 213, a property attached in an inner scope is not dropped outside
;;; it, and looking up an unbound identifier is not an error.
(define-library (srfi private srfi-242-define-property)
  (export define-property capture-lookup define-syntax syntax)
  (import (rename (rnrs) (define-syntax r6:define-syntax) (syntax r6:syntax)))
  (begin
    (define properties '())             ; ((id key value) ...)

    (define (register-property! id key value)
      (set! properties (cons (list id key value) properties)))

    (define (lookup id key)
      (let loop ((ps properties))
        (cond ((null? ps) #f)
              ((and (free-identifier=? id (car (car ps)))
                    (free-identifier=? key (cadr (car ps))))
               (caddr (car ps)))
              (else (loop (cdr ps))))))

    (define (capture-lookup proc) proc)

    (define (wrap-transformer t)
      (if (procedure? t)
          (lambda (x)
            (let ((result (t x)))
              (if (procedure? result) (result lookup) result)))
          t))

    (r6:define-syntax define-syntax
      (syntax-rules ()
        ((_ name expr)
         (r6:define-syntax name (wrap-transformer expr)))))

    (r6:define-syntax define-property
      (lambda (x)
        (syntax-case x ()
          ((_ id key expr)
           (and (identifier? (r6:syntax id)) (identifier? (r6:syntax key)))
           ;; Registered when the definition is expanded or its library
           ;; visited, and also when its library is invoked, since a
           ;; library loaded compiled is visited only when one of its
           ;; macros is used.
           (with-syntax (((tmp tmp2) (generate-temporaries '(property property))))
             (r6:syntax
              (begin
                (r6:define-syntax tmp
                  (begin
                    (register-property! (r6:syntax id) (r6:syntax key) expr)
                    (lambda (y) (syntax-violation #f "invalid syntax" y))))
                (define tmp2
                  (register-property! (r6:syntax id) (r6:syntax key) expr)))))))))

    ;; psyntax's syntax can end a list it builds with a wrapped (), as
    ;; R6RS allows: (syntax (x)) for a pattern variable x isn't a list?.
    ;; The reference code, written for Chez, applies for-all and map to
    ;; such results, so this syntax makes their spines proper lists.
    (define (normalize-syntax x)
      (cond ((pair? x) (cons (car x) (normalize-syntax (cdr x))))
            ((and (not (identifier? x))
                  (syntax-case x () (() #t) (_ #f)))
             '())
            (else x)))

    (r6:define-syntax syntax
      (lambda (x)
        (syntax-case x ()
          ((_ template)
           (r6:syntax (normalize-syntax (r6:syntax template)))))))))
