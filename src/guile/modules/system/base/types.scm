;;; (system base types) for Pseudoscheme.  Guile's reads an object's
;;; memory as libguile lays it out, from the bits object-address gives,
;;; to make a copy of it, or an "inferior object" standing for it: for
;;; GDB, and for its test of the layout.  Objects here aren't laid out
;;; as libguile's, and object-address gives a number standing for an
;;; object, so scm->object looks the object up and copies it as Guile's
;;; would decode it (src/guile/foreign.lisp, %scm->object).  The memory
;;; backends read the process's memory as Guile's do.  Written for
;;; Pseudoscheme (not derived from Guile's source).

(define-module (system base types)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (system foreign)
  #:export (%word-size

            memory-backend
            memory-backend?
            %ffi-memory-backend
            dereference-word
            memory-port
            type-number->name

            inferior-object?
            inferior-object-kind
            inferior-object-sub-kind
            inferior-object-address

            inferior-struct?
            inferior-struct-name
            inferior-struct-fields

            scm->object))

(define %word-size (sizeof '*))

;;; Reading memory: PEEK an address's word, OPEN a port on its bytes,
;;; TYPE-NAME a SMOB or port type's name from its number.
(define-record-type <memory-backend>
  (memory-backend peek open type-name)
  memory-backend?
  (peek memory-backend-peek)
  (open memory-backend-open)
  (type-name memory-backend-type-name))

(define (read-word address)
  (bytevector-uint-ref (pointer->bytevector (make-pointer address) %word-size) 0
                       (native-endianness) %word-size))

(define %ffi-memory-backend
  (memory-backend read-word
                  (lambda (address size)
                    (open-bytevector-input-port
                     (bytevector-copy (pointer->bytevector (make-pointer address)
                                                           (or size %word-size)))))
                  (lambda (kind number) #f)))

(define (dereference-word backend address)
  ((memory-backend-peek backend) address))

(define* (memory-port backend address #:optional size)
  ((memory-backend-open backend) address size))

(define (type-number->name backend kind number)
  ((memory-backend-type-name backend) kind number))

;;; An object that can't be copied: its KIND (port, program, fluid, ...),
;;; maybe a SUB-KIND, and its address.
(define-record-type <inferior-object>
  (make-inferior-object kind sub-kind address)
  inferior-object?
  (kind inferior-object-kind)
  (sub-kind inferior-object-sub-kind)
  (address inferior-object-address))

(define inferior-object
  (case-lambda
    ((kind address) (make-inferior-object kind #f address))
    ((kind sub-kind address) (make-inferior-object kind sub-kind address))))

(set-record-type-printer! <inferior-object>
  (lambda (object port)
    (display "#<" port)
    (display (inferior-object-kind object) port)
    (when (inferior-object-sub-kind object)
      (display " " port)
      (display (inferior-object-sub-kind object) port))
    (display " " port)
    (display (number->string (inferior-object-address object) 16) port)
    (display ">" port)))

;;; A struct, by its vtable's name and its fields, which may refer to
;;; the inferior struct itself.
(define-record-type <inferior-struct>
  (inferior-struct name fields)
  inferior-struct?
  (name inferior-struct-name)
  (fields inferior-struct-fields set-inferior-struct-fields!))

;; The structs being printed, outermost last: a field that is the
;; struct is #0#, one that is a struct printing further out #-1#.
(define printing (make-parameter '()))

(set-record-type-printer! <inferior-struct>
  (lambda (struct port)
    (if (memq struct (printing))
        (display "#-1#" port)
        (parameterize ((printing (cons struct (printing))))
          (display "#<struct " port)
          (display (inferior-struct-name struct) port)
          (for-each (lambda (field)
                      (display " " port)
                      (if (eq? field struct)
                          (display "#0#" port)
                          (write field port)))
                    (inferior-struct-fields struct))
          (display " " port)
          (display (number->string (object-address struct) 16) port)
          (display ">" port)))))

(define* (scm->object bits #:optional (backend %ffi-memory-backend))
  "The object whose bits (as object-address gives them) are BITS."
  (%scm->object bits inferior-object inferior-struct set-inferior-struct-fields!))
