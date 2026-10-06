;;; SRFI 119: wisp, simpler indentation-sensitive Scheme.  The SRFI's
;;; implementation, wisp-scheme.scm by Arne Babenhauserheide (MIT;
;;; reference/srfi-119/), its definitions included from wisp-body.scm
;;; (without its Guile module header and closing examples, and with
;;; Guile's symbols '\_ and '\: written as R7RS writes them).  The Guile
;;; procedures it uses are defined here (source properties are not
;;; kept; unread-char pushes back onto a Gray stream wisp-read-form reads
;;; the port through), and match is Alex Shinn's (as in SRFI 201's).
;;;
;;; A file or port is read as wisp after #!wisp (the reader loads this
;;; library then; src/core.lisp, reader-directive).  wisp-read-form reads one
;;; form; wisp-scheme-read-chunk and the others are the implementation's.
(define-library (srfi 119)
  (export wisp-read-form wisp-scheme-read-chunk wisp-scheme-read-all
          wisp-scheme-read-file-chunk wisp-scheme-read-file
          wisp-scheme-read-string)
  (import (scheme base) (scheme char) (scheme cxr) (scheme read) (scheme write)
          (scheme file)
          (srfi 1) (only (srfi 48) format)
          (srfi private srfi-201-match)
          (only (pseudoscheme lisp) lisp-eval-string))
  (begin
    ;; Guile's
    (define (1+ n) (+ n 1))
    (define (source-properties x) '())
    (define (set-source-properties! x props) #f)
    (define (set-source-property! x key value) #f)
    (define (port-filename port) #f)
    (define (port-line port) 0)
    (define (call-with-input-string string proc)
      (proc (open-input-string string)))
    (define (throw key . args)
      (apply error (symbol->string key) args))
    (define (catch key thunk handler)
      (guard (e (#t (handler key e))) (thunk)))

    ;; Guile's unread-char: wisp reads through a port that keeps the
    ;; characters pushed back, a Gray stream over the port it reads
    (lisp-eval-string
     "(defpackage \"PSEUDOSCHEME-SRFI-119\" (:use \"COMMON-LISP\"))")
    (lisp-eval-string
     "(let ((*package* (find-package \"PSEUDOSCHEME-SRFI-119\")))
        (eval (read-from-string \"(progn
          (defclass pushback-port (trivial-gray-streams:fundamental-character-input-stream)
            ((source :initarg :source) (pushed :initform '())))
          (defmethod trivial-gray-streams:stream-read-char ((s pushback-port))
            (if (slot-value s 'pushed)
                (pop (slot-value s 'pushed))
                (read-char (slot-value s 'source) nil :eof)))
          (defmethod trivial-gray-streams:stream-unread-char ((s pushback-port) c)
            (push c (slot-value s 'pushed))
            nil)
          (defmethod trivial-gray-streams:stream-peek-char ((s pushback-port))
            (if (slot-value s 'pushed)
                (car (slot-value s 'pushed))
                (peek-char nil (slot-value s 'source) nil :eof)))
          (defmethod trivial-gray-streams:stream-read-char-no-hang ((s pushback-port))
            (trivial-gray-streams:stream-read-char s))
          (defmethod trivial-gray-streams:stream-listen ((s pushback-port))
            (or (and (slot-value s 'pushed) t) (listen (slot-value s 'source)))))\")))")
    (define make-pushback-port
      (lisp-eval-string
       "(lambda (port) (make-instance 'pseudoscheme-srfi-119::pushback-port :source port))"))
    (define unread-char
      (lisp-eval-string
       "(lambda (c port)
          (if (typep port 'pseudoscheme-srfi-119::pushback-port)
              (push c (slot-value port 'pseudoscheme-srfi-119::pushed))
              ;; another port, a string port say: what was just read
              ;; is unread, the latest first
              (file-position port (1- (file-position port))))
          ps:unspecific)")))
  (include "reference/srfi-119/wisp-body.scm")
  (begin
    ;; one form at a time, from the chunks wisp-scheme-read-chunk reads
    (define pending '())                ; ((port pushback-port form ...) ...)
    (define (wisp-read-form . port)
      (let* ((port (if (null? port) (current-input-port) (car port)))
             (entry (or (assq port pending)
                        (let ((e (list port (make-pushback-port port))))
                          (set! pending (cons e pending))
                          e)))
             (wrapper (cadr entry)))
        (cond ((pair? (cddr entry))
               (let ((form (car (cddr entry))))
                 (set-cdr! (cdr entry) (cdr (cddr entry)))
                 form))
              ((eof-object? (peek-char wrapper))
               (set! pending (remove (lambda (e) (eq? (car e) port)) pending))
               (eof-object))
              (else
               (set-cdr! (cdr entry) (wisp-scheme-read-chunk wrapper))
               (wisp-read-form port)))))))
