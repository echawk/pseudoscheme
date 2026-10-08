;;; -*- Mode: Scheme -*-
;;;; (chezscheme): the foreign-function interface, on CFFI (the host's
;;;; chez: procedures, src/chez/ffi.lisp).  Part of the library's body
;;;; (src/chez/chez.lisp).

(define load-shared-object %chez:load-shared-object)

;; (foreign-procedure conv ... entry (param-type ...) result-type): the
;; conventions (__collect_safe, (__varargs_after n), ...) are accepted
;; and ignored
(define-syntax foreign-procedure
  (syntax-rules ()
    ((_ conv ... entry (param ...) result)
     (%chez:foreign-procedure entry '(param ...) 'result))))

; (foreign-callable conv ... procedure (param-type ...) result-type): the
; code object is its entry point, an address, and is never freed
(define-syntax foreign-callable
  (syntax-rules ()
    ((_ conv ... proc (param ...) result)
     (%chez:foreign-callable proc '(param ...) 'result))))
(define (foreign-callable-entry-point code) code)
(define (foreign-callable-code-object address) address)

(define foreign-entry? %chez:foreign-entry?)
(define foreign-entry %chez:foreign-entry)
(define foreign-alloc %chez:foreign-alloc)
(define foreign-free %chez:foreign-free)
(define foreign-sizeof %chez:foreign-sizeof)
(define foreign-ref %chez:foreign-ref)
(define foreign-set! %chez:foreign-set!)

;; Ports on file descriptors (src/chez/host.lisp)
(define open-fd-input-port %chez:open-fd-input-port)
(define open-fd-output-port %chez:open-fd-output-port)
(define open-fd-input/output-port %chez:open-fd-input/output-port)
(define port-file-descriptor %chez:port-file-descriptor)
(define set-port-nonblocking! %chez:set-port-nonblocking!)
(define port-nonblocking? %chez:port-nonblocking?)

;; The collector doesn't move what C holds here (u8* arguments are pinned
;; for the call), so locking an object does nothing
(define (lock-object x) (if #f #f))
(define (unlock-object x) (if #f #f))
(define (locked-object? x) #f)
