;;; SRFI 263: a prototype object system.  Daniel Ziltener's reference
;;; implementation (reference/srfi-263/srfi-263.impl.scm; MIT licence, in
;;; reference/srfi-263/LICENSE), written for and tested in Chicken.  Its
;;; code is copied here (not included) because it needed changes:
;;;
;;;   - its (import ...) line is this library's import declaration;
;;;   - recursive-lookup was defined twice (Chicken keeps the second
;;;     definition; a library can't): only the second is kept;
;;;   - the internal message '##srfi-263#obj-data, which Pseudoscheme's
;;;     reader rejects, is a private message (a fresh string), as the
;;;     SRFI's "Private Messages" suggests;
;;;   - a message no parent understands (or more than one does) is sent
;;;     to the receiver as message-not-understood (ambiguous-message-send)
;;;     with the message and its arguments, as the SRFI specifies; the
;;;     code applied the symbol;
;;;   - copy copies the receiver's own data (the code sent its mirror
;;;     messages mirrors don't have), and gives the copy a mirror slot
;;;     of its own (the copied one reflected the original);
;;;   - mirrors answer has-ancestor, which the SRFI specifies, and their
;;;     full-ancestor-list and full-slot-list are those of the mirrored
;;;     object (data-ancestors, data-slots): the code collected the
;;;     mirror's own ancestors, and compared slots with car.
(define-library (srfi 263)
  (export *the-root-object*
          slot?
          slot-getter
          slot-setter
          slot-type)
  (import (scheme base)
          (scheme case-lambda)
          (scheme cxr)
          (srfi 1))
  (begin
    (define obj-data-message (string-copy "srfi-263 object data"))

    ;;; Core system

    (define-record-type slot
      (make-slot getter setter type)
      slot?
      (getter slot-getter)
      (setter slot-setter)
      (type slot-type))

    (define (delete-slot! obj-data slot)
      (let* ((message-alist (get-message-alist obj-data))
             (slot-list (get-slot-list obj-data))
             (parent-list (get-parent-list obj-data))
             (setter-predicate (lambda (item) (eq? (slot-setter item) slot)))
             (is-setter? (find setter-predicate slot-list))
             (slot-predicate (lambda (item)
                               (or (eq? (slot-getter item) slot)
                                  (eq? (slot-setter item) slot))))
             (slots (filter slot-predicate slot-list)))
        (if (= 1 (length slots))
            (let ((slot (car slots)))
              (if (eq? 'parent (slot-type slot))
                  (set-parent-list! obj-data
                                    (delete
                                     ((cdr (assq (slot-getter slot) message-alist)) #f #f)
                                     parent-list)))
              (set-message-alist!
               obj-data
               (if is-setter?
                   (alist-delete (slot-setter slot) message-alist)
                   (alist-delete (slot-getter slot)
                                 (alist-delete (slot-setter slot) message-alist))))
              (set-slot-list!
               obj-data
               (if is-setter?
                   (map (lambda (item)
                          (if (setter-predicate item)
                              (make-slot (slot-getter item) #f (slot-type item))
                              item))
                        slot-list)
                   (remove slot-predicate slot-list)))))))

    (define (slot-add-message-name type)
      (case type
        ((value) 'set-value-slot!)
        ((method) 'set-method-slot!)
        ((parent) 'set-parent-slot!)))

    (define (gen-accessors type getter-name setter-name value)
      (values
       (case type
         ((value) (lambda (self resend) value))
         ((method) value)
         ((parent) (lambda (self resend) value)))
       (if setter-name
           (lambda (self resend value)
             (apply self (slot-add-message-name type) getter-name
                    (if setter-name (list setter-name value) value)))
           #f)))

    (define (set-object-data-slots! obj-data type getter-name getter setter-name setter)
      (let ((new-messages (if setter
                              `((,getter-name . ,getter)
                                (,setter-name . ,setter))
                              `((,getter-name . ,getter)))))
        (set-message-alist!
         obj-data (append new-messages (get-message-alist obj-data)))
        (set-slot-list!
         obj-data (cons (make-slot getter-name setter-name type) (get-slot-list obj-data)))))

    (define (set-slot! obj-data type getter-name . args)
      (let* ((setter? (< 1 (length args)))
             (setter-name (and setter? (car args)))
             (value (if setter? (cadr args) (car args))))
        (let-values (((getter setter)
                      (gen-accessors type getter-name setter-name value)))
          (delete-slot! obj-data getter-name)
          (set-object-data-slots! obj-data type getter-name getter setter-name setter)
          (when (eq? type 'parent)
            (set-parent-list!
             obj-data (cons value (get-parent-list obj-data)))))))

    (define (method-finder name message-alist)
      (letrec ((mfinder
                (lambda (self)
                  (cond ((or
                          (assq name message-alist)
                          (assq name (get-message-alist ((self 'mirror) obj-data-message))))
                         => cdr)
                        (else #f)))))
        mfinder))

    (define (recursive-lookup self checker skip?)
      (cond
       ((and (not skip?) (checker self))
        => (lambda (alist-entry)
             (values alist-entry #t)))
       (else
        (let ((obj-data ((self 'mirror) obj-data-message)))
          (let loop ((parents (get-parent-list obj-data))
                     (handlers '())
                     (handler #f)
                     (found #f))
            (cond
             ((not (null? parents))
              (let-values (((new-handler new-found)
                            (recursive-lookup (car parents) checker #f)))
                (loop (cdr parents)
                      (if new-found (lset-adjoin eq? handlers new-handler) handlers)
                      (if new-found new-handler handler)
                      (or new-found found))))
             (else
              (if handler
                  (if (= 1 (length handlers))
                      (values handler found)
                      (values 'ambiguous-message-send #f))
                  (values 'message-not-understood #f)))))))))

    (define (recursive-ancestor-collector self)
      (let ((parents (get-parent-list ((self 'mirror) obj-data-message))))
        (if (null? parents)
            (list self)
            (apply lset-union
                   eq?
                   parents
                   (map recursive-ancestor-collector parents)))))

    (define (recursive-slot-collector self)
      (let ((parents (recursive-ancestor-collector self)))
        (apply lset-union
               (lambda (a b)
                 (eq? (car a) (car b)))
               (list)
               (map (lambda (class)
                      (get-slot-list ((class 'mirror) obj-data-message)))
                    parents))))

    ;; PSEUDOSCHEME: the ancestors and slots of the object whose data is
    ;; OBJ-DATA, for its mirror (the collectors above work on the mirror).
    (define (object-data-of object) ((object 'mirror) obj-data-message))

    (define (data-ancestors obj-data)
      (let loop ((parents (get-parent-list obj-data)) (found '()))
        (if (null? parents)
            (reverse found)
            (let ((p (car parents)))
              (loop (cdr parents)
                    (if (memq p found)
                        found
                        (let inner ((more (data-ancestors (object-data-of p)))
                                    (found (cons p found)))
                          (cond ((null? more) found)
                                ((memq (car more) found) (inner (cdr more) found))
                                (else (inner (cdr more) (cons (car more) found)))))))))))

    (define (data-slots obj-data)
      (let loop ((datas (cons obj-data (map object-data-of (data-ancestors obj-data))))
                 (slots '()))
        (if (null? datas)
            (reverse slots)
            (loop (cdr datas)
                  (fold (lambda (slot slots)
                          (if (find (lambda (s) (eq? (slot-getter s) (slot-getter slot))) slots)
                              slots
                              (cons slot slots)))
                        slots
                        (get-slot-list (car datas)))))))

    ;;;; Method running

    (define (send-with-error-handling caller method-lookup method-name message-alist parents-only args)
      (let-values (((method found?)
                    (recursive-lookup
                     method-lookup
                     (method-finder method-name message-alist)
                     parents-only)))
        (if (and (not found?) (memq method '(message-not-understood ambiguous-message-send)))
            (caller method method-name args)
            (apply method caller (make-resender caller method-name) args))))

    (define (make-resender caller handler-name)
      (lambda (target-override . args)
        (let ((target (cond
                       ((eq? #f target-override)
                        caller)
                       (else target-override))))
          (send-with-error-handling caller target handler-name '() (eq? target-override #f) args))))

    ;;;; Root object

    (define-record-type object-data
      (make-object-data* message-alist slot-list parent-list)
      object-data?
      (message-alist get-message-alist set-message-alist!)
      (slot-list get-slot-list set-slot-list!)
      (parent-list get-parent-list set-parent-list!))

    (define (make-object-data)
      (make-object-data* '() '() '()))

    (define (*object* obj-data)
      (letrec
          ((obj-handler
            (lambda (message . args)
              (send-with-error-handling
               obj-handler obj-handler message (get-message-alist obj-data) #f args))))
        obj-handler))

    (define (set-method-slot! obj-data name . args)
      (apply set-slot! obj-data 'method name args))

    (define (derive-object obj mirror?)
      (let* ((obj-data (make-object-data))
             (derived-object (*object* obj-data)))
        (set-slot! obj-data 'parent 'parent obj)
        (set-method-slot!
         obj-data 'mirror
         (lambda (self resend)
           (let-values (((new-mirror new-mirror-data)
                         (derive-object (obj 'mirror) #t)))
             (populate-mirror new-mirror new-mirror-data obj-data))))
        (when mirror?
          (set-method-slot! obj-data 'derive
                            (lambda (self resend)
                              (let-values (((new-obj new-data)
                                            (derive-object self #t)))
                                new-obj))))
        (values derived-object obj-data)))

    (define (populate-mirror mirror mirror-data obj-data)
      (map
       (lambda (name proc)
         (set-method-slot! mirror-data name proc))
       (list obj-data-message 'immediate-message-alist
             'immediate-ancestor-list 'full-ancestor-list
             'immediate-slot-list 'full-slot-list)
       (list (lambda (self resend) obj-data)
             (lambda (self resend) (list-copy (get-message-alist obj-data)))
             (lambda (self resend) (list-copy (get-parent-list obj-data)))
             (lambda (self resend) (data-ancestors obj-data))
             (lambda (self resend) (list-copy (get-slot-list obj-data)))
             (lambda (self resend) (data-slots obj-data))))
      (set-method-slot! mirror-data 'has-ancestor
                        (lambda (self resend object)
                          (and (memq object (data-ancestors obj-data)) #t)))
      mirror)

    (define *the-root-object*
      (let* ((obj-data (make-object-data))
             (object (*object* obj-data)))
        (set-message-alist!
         obj-data
         (alist-cons 'set-method-slot!
                     (lambda (self resend name . args)
                       (apply set-method-slot! ((self 'mirror) obj-data-message)
                              name args))
                     (get-message-alist obj-data)))
        (set-slot-list!
         obj-data
         (append (list (make-slot set-method-slot! #f 'method)) (get-slot-list obj-data)))
        (set-method-slot!
         obj-data 'mirror
         (lambda (self resend)
           (let-values (((root-mirror mirror-data) (derive-object *the-root-object* #t)))
             (populate-mirror root-mirror mirror-data obj-data))))
        (set-method-slot!
         obj-data 'derive
         (lambda (self resend)
           (derive-object self #f)))
        (set-method-slot!
         obj-data 'copy
         (lambda (self resend)
           (let ((data ((self 'mirror) obj-data-message))
                 (obj-data (make-object-data)))
             (set-message-alist! obj-data (list-copy (get-message-alist data)))
             (set-slot-list! obj-data (list-copy (get-slot-list data)))
             (set-parent-list! obj-data (list-copy (get-parent-list data)))
             ;; the copied mirror slot would reflect the original
             (set-method-slot!
              obj-data 'mirror
              (lambda (self resend)
                (let-values (((new-mirror new-mirror-data)
                              (derive-object
                               (let ((parents (get-parent-list obj-data)))
                                 (if (null? parents)
                                     *the-root-object*
                                     ((car parents) 'mirror)))
                               #t)))
                  (populate-mirror new-mirror new-mirror-data obj-data))))
             (*object* obj-data))))
        (set-method-slot!
         obj-data 'delete-slot!
         (lambda (self resend name)
           (delete-slot! ((self 'mirror) obj-data-message) name)))
        (set-method-slot!
         obj-data 'set-value-slot!
         (lambda (self resend name . args)
           (apply set-slot! ((self 'mirror) obj-data-message) 'value name args)))
        (set-method-slot!
         obj-data 'set-parent-slot!
         (lambda (self resend name . args)
           (apply set-slot! ((self 'mirror) obj-data-message) 'parent name args)))
        (set-method-slot!
         obj-data 'message-not-understood
         (lambda (self resend message args)
           (error "Message not understood" self message args)))
        (set-method-slot!
         obj-data 'ambiguous-message-send
         (lambda (self resend message args)
           (error "Message ambiguous" self message args)))
        object))))
