;;; SRFI 263's (srfi 263 syntax).  The reference implementation's
;;; reference/srfi-263/srfi-263-syntax.impl.scm, copied with these
;;; changes: the SRFI names the method-definition form define-method
;;; (the code calls it set-method!, which is kept as well); (void),
;;; which R7RS lacks, is (if #f #f); copy-object's extra parents and
;;; slots work as derive-object's.
(define-library (srfi 263 syntax)
  (export define-method
          set-method!
          derive-object
          copy-object
          define-object)
  (import (scheme base)
          (srfi 263))
  (begin
    (define-syntax define-method
      (syntax-rules ()
        ((_ (obj message self resend args ...)
            body1 body ...)
         (obj 'set-method-slot! `message
              (lambda (self resend args ...)
                body1 body ...)))))

    (define-syntax set-method!
      (syntax-rules ()
        ((_ . rest) (define-method . rest))))

    (define-syntax derive-object
      (syntax-rules ()
        ((_ (creation-parent (parent-name parent-object) ...)
            slots ...)
         (let ((o (creation-parent 'derive)))
           (o 'set-parent-slot! 'parent-name parent-object)
           ...
           (derive-object/add-slots! o slots ...)
           o))))

    (define-syntax copy-object
      (syntax-rules ()
        ((_ (creation-parent (parent-name parent-object) ...)
            slots ...)
         (let ((o (creation-parent 'copy)))
           (o 'set-parent-slot! 'parent-name parent-object)
           ...
           (derive-object/add-slots! o slots ...)
           o))))

    (define-syntax derive-object/add-slots!
      (syntax-rules ()
        ((_ o)
         (if #f #f))
        ((_ o ((method-name . method-args) body ...)
            slots ...)
         (begin
           (o 'set-method-slot! `method-name (lambda method-args
                                               body ...))
           (derive-object/add-slots! o slots ...)))
        ((_ o (slot-getter slot-setter slot-value)
            slots ...)
         (begin
           (o 'set-value-slot! `slot-getter `slot-setter slot-value)
           (derive-object/add-slots! o slots ...)))
        ((_ o (slot-getter slot-value)
            slots ...)
         (begin
           (o 'set-value-slot! `slot-getter slot-value)
           (derive-object/add-slots! o slots ...)))))

    (define-syntax define-object
      (syntax-rules ()
        ((_ name body ...)
         (define name
           (derive-object body ...)))))))
