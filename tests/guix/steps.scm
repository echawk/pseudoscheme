;;; `guix build -d hello', step by step, each timed: for finding which
;;; step of it goes wrong or takes long on --guile.
;;;
;;;   pseudoscheme --guile tests/guix/steps.scm [PACKAGE]
;;;
;;; GUIX_TRACE_LOADS=1 also logs each module load (as tests/guix/guix.scm).

(when (getenv "GUIX_TRACE_LOADS")
  (let ((load-path-file primitive-load-path))
    (set! primitive-load-path
          (lambda (name . rest)
            (format (current-error-port) "~a load ~a~%" (current-time) name)
            (apply load-path-file name rest)))))

(define start (current-time))
(define (step name thunk)
  (format #t "~a: ...~%" name)
  (force-output)
  (let ((t0 (current-time))
        (result (thunk)))
    (format #t "~a: ~a (~as, ~as in all)~%" name result
            (- (current-time) t0) (- (current-time) start))
    (force-output)
    result))

(step "modules" (lambda () (use-modules (guix) (gnu packages)) 'loaded))

;;; What specification->package does first, piece by piece
(define (guix-ref module name) (module-ref (resolve-module module) name))
(define package-dir
  (string-append (car (filter (lambda (d) (file-exists? (string-append d "/gnu/packages")))
                              %load-path))
                 "/gnu/packages"))
(step "scandir*" (lambda () (length ((guix-ref '(guix build syscalls) 'scandir*) package-dir))))
(step "scheme-files"
      (lambda () (length ((guix-ref '(guix discovery) 'scheme-files) package-dir))))
(step "all-modules"
      (lambda () (length ((guix-ref '(guix discovery) 'all-modules)
                          ((guix-ref '(gnu packages) '%package-module-path))))))
(step "fold-packages"
      (lambda () ((guix-ref '(gnu packages) 'fold-packages) (lambda (p n) (+ n 1)) 0)))
(define name (if (> (length (command-line)) 1) (cadr (command-line)) "hello"))
(define package
  (step "find" (lambda () ((module-ref (resolve-interface '(gnu packages))
                                       'specification->package)
                           name))))
(step "derivation"
      (lambda ()
        (let ((with-store (module-ref (resolve-interface '(guix store)) 'open-connection))
              (package-derivation (module-ref (resolve-interface '(guix packages)) 'package-derivation))
              (derivation-file-name (module-ref (resolve-interface '(guix derivations)) 'derivation-file-name)))
          (derivation-file-name (package-derivation (with-store) package)))))
