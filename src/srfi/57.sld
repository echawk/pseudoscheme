;;; SRFI 57: records.  André van Tonder's reference implementation,
;;; from the SRFI document (reference/srfi-57/srfi-57.scm, unmodified;
;;; MIT licence, per the SRFI document, in reference/srfi-57/LICENSE).
;;; As it asks, R7RS's define-record-type serves as
;;; srfi-9:define-record-type, and case-lambda comes from
;;; (scheme case-lambda).  (scheme base)'s syntax-error is left out
;;; because the reference code defines its own.  The SRFI's examples
;;; are in reference/srfi-57/examples.scm.
;;;
;;; The included file, reference/srfi-57/srfi-57-core.scm, is the
;;; reference code with seven macro definitions deleted; they are
;;; defined below instead, taking the same arguments:
;;;   - The list utilities remove-duplicates, union, intersection,
;;;     remove-from and syntax-lookup compared labels with syntax-rules
;;;     tricks; at top level each comparison defined a new macro
;;;     (top:if-free=).  Here they are syntax-case macros on
;;;     (srfi private srfi-57-labels), comparing labels with
;;;     free-identifier=? (which is what if-free= and top:if-free=
;;;     test), and they ignore the comparison macro they are passed.
;;;   - make-generic and define-method, the generic functions behind
;;;     record schemes.  The reference's generic answers a zero-argument
;;;     call with a message dispatcher, so each define-method made three
;;;     calls at top level.  With full continuations, Pseudoscheme's
;;;     compile time grows steeply with the number of top-level calls to
;;;     procedures it doesn't know (see the report on this SRFI), and a
;;;     handful of record types took minutes to compile.  Here a
;;;     generic keeps its current method in a cell found through an eq
;;;     hashtable keyed on the generic, so define-method makes only
;;;     primitive calls (hashtable-ref, car, set-car!).  Methods chain
;;;     exactly as before: the newest method whose predicates all
;;;     accept the arguments wins, else the next one is tried.
;;;
;;; Only the names in the specification are exported.  define-record-type
;;; is a different binding from (scheme base)'s, so import
;;; (except (scheme base) define-record-type) with this library.  As the
;;; SRFI says, define-record-type and define-record-scheme are specified
;;; at top level only: the reference implementation emits definitions
;;; mixed with expressions, so use them in a program's or library's
;;; top-level body.  Unpopulated fields hold the symbol <undefined>.
(define-library (srfi 57)
  (export define-record-type define-record-scheme
          record-update record-update! record-compose)
  (import (except (scheme base) define-record-type syntax-error)
          (rename (only (scheme base) define-record-type)
                  (define-record-type srfi-9:define-record-type))
          (scheme case-lambda)
          (only (rnrs syntax-case) syntax-case syntax with-syntax)
          (only (rnrs hashtables) make-eq-hashtable hashtable-ref
                hashtable-set!)
          (srfi private srfi-57-labels))
  (begin
    ;; Generic procedure -> (list current-method).
    (define %generic-cells (make-eq-hashtable))

    (define-syntax make-generic
      (syntax-rules ()
        ((make-generic (arg arg+ ...) default-proc)
         (let* ((cell (list default-proc))
                (generic (lambda (arg arg+ ...) ((car cell) arg arg+ ...))))
           (hashtable-set! %generic-cells generic cell)
           generic))))

    (define-syntax define-method
      (syntax-rules ()
        ((define-method (generic (arg pred?) ...) . body)
         (define-method generic (pred? ...) (arg ...) (lambda (arg ...) . body)))
        ((define-method generic (pred? ...) (arg ...) procedure)
         (let* ((cell (hashtable-ref %generic-cells generic #f))
                (next (car cell))
                (proc procedure))
           (set-car! cell
                     (lambda (arg ...)
                       (if (and (pred? arg) ...)
                           (proc arg ...)
                           (next arg ...))))))))

    (define-syntax remove-duplicates
      (lambda (x)
        (syntax-case x ()
          ((_ lst compare? k)
           (with-syntax ((result (label-dedupe #'lst)))
             #'(syntax-apply k result))))))

    (define-syntax union
      (lambda (x)
        (syntax-case x ()
          ((_ (e ...) ... compare? k)
           (with-syntax ((result (label-dedupe #'(e ... ...))))
             #'(syntax-apply k result))))))

    (define-syntax intersection
      (lambda (x)
        (syntax-case x ()
          ((_ (a ...) (b ...) compare? k)
           (with-syntax ((result (label-filter
                                  (lambda (e) (label-member? e #'(b ...)))
                                  #'(a ...))))
             #'(syntax-apply k result))))))

    (define-syntax remove-from
      (lambda (x)
        (syntax-case x ()
          ((_ (a ...) (b ...) compare? k)
           (with-syntax ((result (label-filter
                                  (lambda (e) (not (label-member? e #'(b ...))))
                                  #'(a ...))))
             #'(syntax-apply k result))))))

    (define-syntax syntax-lookup
      (lambda (x)
        (syntax-case x ()
          ((_ label ((label* . value) ...) compare fail k)
           (with-syntax ((result (label-lookup #'label
                                               #'((label* . value) ...)
                                               #'fail)))
             #'(syntax-apply k result)))))))
  (include "reference/srfi-57/srfi-57-core.scm"))
