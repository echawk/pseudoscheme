;;; SRFI 100: define-lambda-object.  Joo ChurlSoo's implementation
;;; (reference/srfi-100/srfi-100.scm; MIT licence, in
;;; reference/srfi-100/LICENSE).  The included file,
;;; reference/srfi-100/define-lambda-object.scm, is its define-syntax
;;; part (the define-macro alternative is left out; of its two opt-key
;;; variants, the procedure is used, as shipped), with one change, marked
;;; PSEUDOSCHEME: the predicates need the host to tell lambda objects
;;; from other procedures (the implementation uses mzscheme's
;;; object-name), so each constructor registers the object it makes in
;;; a weak eq hashtable, and object-name, below, returns
;;; *%lambda-object%* for a registered object.
(define-library (srfi 100)
  (export define-lambda-object)
  (import (scheme base)
          (scheme case-lambda)
          (only (rnrs syntax-case) syntax-case syntax with-syntax
                syntax->datum datum->syntax)
          (only (srfi 126) make-eq-hashtable hashtable-ref hashtable-set!))
  (begin
    (define lambda-objects (make-eq-hashtable #f 'weak-key))

    (define (register-lambda-object! object)
      (hashtable-set! lambda-objects object #t)
      object)

    (define (object-name object)
      (and (procedure? object)
           (hashtable-ref lambda-objects object #f)
           '*%lambda-object%*)))
  (include "reference/srfi-100/define-lambda-object.scm"))
