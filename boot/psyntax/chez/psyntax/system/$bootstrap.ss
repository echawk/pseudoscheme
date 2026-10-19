;;; (psyntax system $bootstrap) for Chez Scheme: the host primitives
;;; psyntax's sources need (compat.ss), so that Chez can load them as
;;; native R6RS libraries and run psyntax-buildscript.ss.  See
;;; boot/README.md.

(library (psyntax system $bootstrap)
  (export void gensym eval-core symbol-value set-symbol-value!
          pretty-print lisp-keyword? host-literal?
          annotation? annotation-expression annotation-stripped strip-annotations)
  (import (except (chezscheme) gensym pretty-print
                  annotation? annotation-expression annotation-stripped))

  ;; Interned, so the expanded code they end up in can be written out
  ;; and read back.  Distinct from the names Quasischeme's own gensym
  ;; makes (src/psyntax.lisp).
  (define count 0)
  (define (gensym . ignore)
    (set! count (+ count 1))
    (string->symbol (string-append "g$chez$" (number->string count))))

  ;; Code psyntax evaluates while expanding (transformers, and the
  ;; libraries they use) calls psyntax's primitives by name: these must
  ;; be psyntax's own (identifier?, generate-temporaries, ...), not
  ;; Chez's, so it runs in an environment where they shadow Chez's.
  ;; Made on first use, when (psyntax expander) is loaded.
  (define env #f)
  (define (eval-env)
    (or env
        (begin
          (set! env
            (copy-environment
             (environment
              '(except (chezscheme)
                 gensym pretty-print void
                 identifier? environment environment? eval expand
                 generate-temporaries free-identifier=? bound-identifier=?
                 datum->syntax syntax-error syntax->datum
                 make-variable-transformer null-environment syntax->annotation
                 annotation? annotation-expression annotation-stripped)
              '(psyntax expander)
              '(psyntax system $bootstrap))
             #t))
          env)))

  (define (eval-core x) (eval x (eval-env)))
  (define (symbol-value x) (top-level-value x (eval-env)))
  (define (set-symbol-value! x v) (set-top-level-value! x v (eval-env)))

  ;; One form per line, as Quasischeme's writer does.
  (define (pretty-print x . port)
    (let ((p (if (null? port) (current-output-port) (car port))))
      (parameterize ((print-gensym #f) (print-graph #f))
        (write x p))
      (newline p)))

  (define (lisp-keyword? x) #f)
  (define (host-literal? x) #f)
  ;; source annotations: psyntax's sources have none here
  (define (annotation? x) #f)
  (define (annotation-expression x) x)
  (define (annotation-stripped x) x)
  (define (strip-annotations x) x))
