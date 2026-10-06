;;; SRFI 192: port positioning.  Written for Pseudoscheme on its R6RS
;;; port positions (see 181.sld), not Shiro Kawai's sample
;;; implementation, which works only on that sample's custom ports.
;;; make-i/o-invalid-position-error and i/o-invalid-position-error? are
;;; (rnrs io ports)'s.  The others extend R6RS's:
;;; - port-has-port-position? and port-has-set-port-position!? are also
;;;   true of the ports R6RS's say no to but that support positions
;;;   anyway, such as R7RS string ports;
;;; - port-position gives the right position of a custom textual input
;;;   port after peek-char (see private/srfi-181-ports.sld);
;;; - set-port-position! flushes an output port first, and raises an
;;;   &i/o-invalid-position condition when the port can't take the
;;;   position (any error from the port's own positioning becomes one).
;;; Limitation: setting a string port's position past its end isn't
;;; detected; reading from it then returns an end of file.
;;; A binary port's position is its byte offset; a textual string or
;;; file port's is its character offset.
(define-library (srfi 192)
  (export port-has-port-position?
          port-position
          port-has-set-port-position!?
          set-port-position!
          i/o-invalid-position-error?
          make-i/o-invalid-position-error)
  (import (scheme base)
          (srfi private srfi-181-ports)
          (prefix (only (rnrs io ports)
                        port-has-port-position? port-has-set-port-position!?
                        port-position set-port-position!)
                  r6:)
          (only (rnrs io ports)
                i/o-invalid-position-error? make-i/o-invalid-position-error)
          (only (rnrs conditions)
                condition make-who-condition make-message-condition
                make-irritants-condition))
  (begin
    (define (port-has-port-position? port)
      (or (r6:port-has-port-position? port)
          (guard (e (#t #f))
            (exact-integer? (r6:port-position port)))))

    (define (port-position port)
      (custom-port-position port))

    (define (port-has-set-port-position!? port)
      (or (r6:port-has-set-port-position!? port)
          (guard (e (#t #f))
            (let ((pos (r6:port-position port)))
              (and (exact-integer? pos)
                   (begin (r6:set-port-position! port pos)
                          (eqv? pos (r6:port-position port))))))))

    (define (invalid-position port pos . irritants)
      (raise (condition (make-i/o-invalid-position-error pos)
                        (make-who-condition 'set-port-position!)
                        (make-message-condition "invalid port position")
                        (make-irritants-condition (cons port (cons pos irritants))))))

    (define (set-port-position! port pos)
      (unless (port-has-set-port-position!? port)
        (error "set-port-position!: port has no settable position" port))
      (when (output-port? port) (flush-output-port port))
      (guard (e ((i/o-invalid-position-error? e) (raise e))
                (#t (invalid-position port pos e)))
        (r6:set-port-position! port pos))
      ;; a port that can't take an integer position may ignore it
      (when (and (exact-integer? pos) (port-has-port-position? port)
                 (not (eqv? pos (r6:port-position port))))
        (invalid-position port pos)))))
