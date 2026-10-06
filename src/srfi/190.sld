;;; SRFI 190: coroutine generators.  Written for Pseudoscheme after the
;;; sample implementation in the SRFI document (kept in
;;; reference/srfi-190/srfi-190.scm; MIT licence, in
;;; reference/srfi-190/LICENSE), on SRFI 158's make-coroutine-generator
;;; and so on Pseudoscheme's re-entrant continuations.
;;;
;;; The sample binds yield as a SRFI 139 syntax parameter, which
;;; Pseudoscheme does not have.  Here yield is identifier syntax for the
;;; value of a parameter object that coroutine-generator binds, with
;;; parameterize, around its body (the parameterization is re-entered
;;; whenever the generator resumes the body).  So yield, however it is
;;; reached (written in the body, introduced by a local macro, imported
;;; under another name), is the yielding procedure of the innermost
;;; coroutine generator whose body is running.  Evaluating yield outside
;;; any coroutine generator is an error at run time.  The difference from
;;; the sample: yield is scoped dynamically, not lexically, so a
;;; procedure defined elsewhere that refers to yield and is called from
;;; a generator's body yields from that generator, where SRFI 190 calls
;;; this an error.
(define-library (srfi 190)
  (export coroutine-generator define-coroutine-generator yield)
  (import (scheme base)
          (only (srfi 158) make-coroutine-generator)
          (only (rnrs base) identifier-syntax))
  (begin
    (define current-yield
      (make-parameter
       (lambda args
         (error "yield used outside coroutine generator"))))

    (define-syntax yield
      (identifier-syntax (current-yield)))

    (define-syntax coroutine-generator
      (syntax-rules ()
        ((_ . body)
         (make-coroutine-generator
          (lambda (%yield)
            (parameterize ((current-yield %yield))
              (let () . body)))))))

    (define-syntax define-coroutine-generator
      (syntax-rules ()
        ((_ (name . formals) . body)
         (define (name . formals) (coroutine-generator . body)))
        ((_ name . body)
         (define name (coroutine-generator . body)))))))
