; SPDX-FileCopyrightText: 2026 Daniel Ziltener
;
; SPDX-License-Identifier: MIT

(import (scheme base)
        (srfi 263))

(define-syntax set-method!
  (syntax-rules ()
    ((_ (obj message self resend args ...)
        body1 body ...)
     (obj 'set-method-slot! `message
          (lambda (self resend args ...)
            body1 body ...)))))

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
     (void))
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
       (derive-object body ...)))))
