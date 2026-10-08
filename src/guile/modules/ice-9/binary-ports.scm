;;; (ice-9 binary-ports) for Pseudoscheme: Guile's is written on Guile's
;;; C port buffers; this one is Pseudoscheme's R6RS binary ports, under
;;; Guile's interface.  Written for Pseudoscheme (not derived from
;;; Guile's source).

(define-module (ice-9 binary-ports)
  #:export (eof-object
            open-bytevector-input-port
            open-bytevector-output-port
            get-u8
            lookahead-u8
            get-bytevector-n
            get-bytevector-n!
            get-bytevector-some
            get-bytevector-some!
            get-bytevector-all
            get-string-n!
            put-u8
            put-bytevector
            unget-bytevector
            make-custom-binary-input-port
            make-custom-binary-output-port
            make-custom-binary-input/output-port
            call-with-input-bytevector
            call-with-output-bytevector))

(define-syntax-rule (host name ...)
  (begin (define name (%host-ref 'name)) ...))

(host eof-object open-bytevector-input-port open-bytevector-output-port
      get-u8 lookahead-u8 get-bytevector-n get-bytevector-n! get-bytevector-some
      get-bytevector-all get-string-n! put-u8 put-bytevector
      make-custom-binary-input-port make-custom-binary-output-port
      make-custom-binary-input/output-port)

(define (get-bytevector-some! port bv start count)
  (let ((got (get-bytevector-some port)))
    (if (eof-object? got)
        got
        (let ((n (min count (bytevector-length got))))
          (bytevector-copy! got 0 bv start n)
          n))))

(define* (unget-bytevector port bv #:optional (start 0) (count (- (bytevector-length bv) start)))
  (error "unget-bytevector isn't supported here"))

(define (call-with-input-bytevector bv proc)
  (proc (open-bytevector-input-port bv)))

(define (call-with-output-bytevector proc)
  (call-with-values open-bytevector-output-port
    (lambda (port get)
      (proc port)
      (get))))
