;;; SRFI 17: generalized set!.  Lars Thomas Hansen's sample
;;; implementation for Twobit (reference/srfi-17/srfi-17-twobit.scm,
;;; kept unmodified there; MIT licence, per the SRFI document, in
;;; reference/srfi-17/LICENSE), with the SRFI document's sample
;;; getter-with-setter.  The code is copied into this file, because
;;; Twobit's (define-syntax set! let* ...) scope extension isn't R7RS.
;;; The changes:
;;;   - set!'s second clause expands into R7RS's set!, imported as
;;;     r7:set!, instead of reaching the outer set! through let* scoping;
;;;   - set-setter! returns no value instead of Twobit's (unspecified);
;;;   - the table also maps bytevector-u8-ref to bytevector-u8-set!.
;;;
;;; As in the SRFI's sample implementations, (srfi 17) exports its own
;;; set!, which is a different binding from (scheme base)'s; so import
;;; (except (scheme base) set!) with this library.  For a plain
;;; variable, (set! var value) behaves exactly as R7RS's set!.
(define-library (srfi 17)
  (export set! setter getter-with-setter)
  (import (except (scheme base) set!)
          (rename (only (scheme base) set!) (set! r7:set!)))
  (begin
    (define-syntax set!
      (syntax-rules ()
        ((set! (?e0 ?e1 ...) ?v)
         ((setter ?e0) ?e1 ... ?v))
        ((set! ?i ?v)
         (r7:set! ?i ?v))))

    (define setter
      (let ((setters (list (cons car  set-car!)
                           (cons cdr  set-cdr!)
                           (cons caar (lambda (p v) (set-car! (car p) v)))
                           (cons cadr (lambda (p v) (set-car! (cdr p) v)))
                           (cons cdar (lambda (p v) (set-cdr! (car p) v)))
                           (cons cddr (lambda (p v) (set-cdr! (cdr p) v)))
                           (cons vector-ref vector-set!)
                           (cons string-ref string-set!)
                           (cons bytevector-u8-ref bytevector-u8-set!))))
        (letrec ((setter
                  (lambda (proc)
                    (let ((probe (assv proc setters)))
                      (if probe
                          (cdr probe)
                          (error "No setter for " proc)))))
                 (set-setter!
                  (lambda (proc setter)
                    (r7:set! setters (cons (cons proc setter) setters))
                    (values))))
          (set-setter! setter set-setter!)
          setter)))

    (define (getter-with-setter get set)
      (let ((proc (lambda args (apply get args))))
        (set! (setter proc) set)
        proc))))
