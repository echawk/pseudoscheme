;;; (ice-9 custom-ports) for Pseudoscheme: make-custom-port over
;;; Pseudoscheme's R6RS custom textual ports.  Guile's methods move bytes
;;; (read port bytevector start count), encoded as UTF-8 here.  Written
;;; for Pseudoscheme (not derived from Guile's source).

(define-module (ice-9 custom-ports)
  #:export (make-custom-port))

(define make-textual-input/output (%host-ref 'make-custom-textual-input/output-port))
(define make-textual-input (%host-ref 'make-custom-textual-input-port))
(define make-textual-output (%host-ref 'make-custom-textual-output-port))
(define host-utf8->string (%host-ref 'utf8->string))
(define host-string->utf8 (%host-ref 'string->utf8))
(define host-make-bytevector (%host-ref 'make-bytevector))
(define host-bytevector-copy (%host-ref 'bytevector-copy))
(define host-bytevector-length (%host-ref 'bytevector-length))

(define* (make-custom-port #:key read write input-waiting? read-wait-fd write-wait-fd
                           seek random-access? close get-natural-buffer-sizes
                           (id "custom-port") print truncate encoding
                           conversion-strategy close-on-gc?)
  (define port #f)
  (define pending "")                   ; decoded but not yet taken
  (define (read! string start count)
    (when (and (zero? (string-length pending)) read)
      (let* ((bv (host-make-bytevector (max count 4) 0))
             (n (read port bv 0 (max count 4))))
        (set! pending (host-utf8->string (host-bytevector-copy bv 0 n)))))
    (let ((n (min count (string-length pending))))
      (string-copy! string start pending 0 n)
      (set! pending (substring pending n (string-length pending)))
      n))
  (define (write! string start count)
    (let ((bv (host-string->utf8 (substring string start (+ start count)))))
      (let loop ((off 0))
        (when (< off (host-bytevector-length bv))
          (loop (+ off (write port bv off (- (host-bytevector-length bv) off))))))
      count))
  (define (closer) (when close (close port)))
  (set! port
        (cond ((and read write) (make-textual-input/output id read! write! #f #f closer))
              (read (make-textual-input id read! #f #f closer))
              (else (make-textual-output id write! #f #f closer))))
  port)
