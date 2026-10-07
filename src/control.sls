;;; -*- Mode: Scheme -*-
;;;; (pseudoscheme control): delimited continuations, as Guile has them
;;;; (its core prompts and (ice-9 control)).  The prompts themselves are
;;;; host primitives over the continuation frames (src/continuations.lisp,
;;;; "Delimited continuations"); this is the rest, in Scheme.
;;;;
;;;;   (call-with-prompt tag thunk handler)  call THUNK under a prompt;
;;;;       an abort to TAG calls (HANDLER k value ...), K the composable
;;;;       continuation from the abort up to the prompt
;;;;   (abort-to-prompt tag value ...)
;;;;   (make-prompt-tag [stem]), (default-prompt-tag)
;;;;   (% expr), (% expr handler), (% tag expr handler)
;;;;   (reset body ...), (shift k body ...), reset*, shift*
;;;;   call-with-escape-continuation, call/ec, let/ec

(library (pseudoscheme control)
  (export call-with-prompt abort-to-prompt make-prompt-tag default-prompt-tag prompt-tag?
          % default-prompt-handler reset shift reset* shift*
          call-with-escape-continuation call/ec let/ec)
  (import (rnrs) (prefix (pseudoscheme host) host:))

  (define call-with-prompt host:call-with-prompt)
  (define abort-to-prompt host:abort-to-prompt)

  (define-record-type (prompt-tag %make-prompt-tag prompt-tag?)
    (fields stem))

  (define make-prompt-tag
    (case-lambda
      (() (%make-prompt-tag "prompt"))
      ((stem) (%make-prompt-tag stem))))

  (define the-default-prompt-tag (make-prompt-tag "default"))
  (define (default-prompt-tag) the-default-prompt-tag)

  ;; % with no handler: the handler takes a procedure from the abort and
  ;; calls it with the continuation, under the prompt again -- which is
  ;; what shift needs
  (define (default-prompt-handler k proc)
    (call-with-prompt (default-prompt-tag) (lambda () (proc k)) default-prompt-handler))

  (define-syntax %
    (syntax-rules ()
      ((_ expr) (call-with-prompt (default-prompt-tag) (lambda () expr) default-prompt-handler))
      ((_ expr handler) (call-with-prompt (default-prompt-tag) (lambda () expr) handler))
      ((_ tag expr handler) (call-with-prompt tag (lambda () expr) handler))))

  (define (reset* thunk) (% (thunk)))

  ;; the continuation shift's body gets is the one up to the reset, under
  ;; a reset of its own when called
  (define (shift* proc)
    (abort-to-prompt (default-prompt-tag)
                     (lambda (k)
                       (proc (lambda vals (reset* (lambda () (apply k vals))))))))

  (define-syntax reset
    (syntax-rules ()
      ((_ body1 body2 ...) (reset* (lambda () body1 body2 ...)))))

  (define-syntax shift
    (syntax-rules ()
      ((_ k body1 body2 ...) (shift* (lambda (k) body1 body2 ...)))))

  ;; Escape continuations: a prompt whose continuation is never called
  (define (call-with-escape-continuation proc)
    (let ((tag (make-prompt-tag "escape")))
      (call-with-prompt tag
        (lambda () (proc (lambda vals (apply abort-to-prompt tag vals))))
        (lambda (k . vals) (apply values vals)))))

  (define call/ec call-with-escape-continuation)

  (define-syntax let/ec
    (syntax-rules ()
      ((_ k body1 body2 ...) (call/ec (lambda (k) body1 body2 ...))))))
