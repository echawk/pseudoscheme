;;; (ice-9 textual-ports) for Pseudoscheme: Guile's is written on Guile's
;;; C port buffers; this one is Pseudoscheme's R6RS textual ports, under
;;; Guile's interface.  Written for Pseudoscheme (not derived from
;;; Guile's source).

(define-module (ice-9 textual-ports)
  #:export (get-char
            unget-char
            unget-string
            lookahead-char
            get-string-n
            get-string-n!
            get-string-all
            get-line
            put-char
            put-string
            make-custom-textual-input-port
            make-custom-textual-output-port
            make-custom-textual-input/output-port))

(define-syntax-rule (host name ...)
  (begin (define name (%host-ref 'name)) ...))

(host get-char lookahead-char get-string-n get-string-n! get-string-all get-line
      put-char put-string make-custom-textual-input-port
      make-custom-textual-output-port make-custom-textual-input/output-port)

(define (unget-char port char) (unread-char char port))
(define (unget-string port string)
  (error "unget-string isn't supported here"))
