#lang racket/base
;; Fixes from Racket's own suite's crashes: cyclic lists in memq and
;; assq, chaperones of immutable boxes, flvector/fxvector/stencil
;; equality, progress evts of closed ports, kill-thread and custodians,
;; result arities, variadic comparisons' arity errors, printing a
;; cyclic stencil vector (src/racket/native/).  The .expected file is
;; what Racket 9.3 prints.
(require racket/flonum racket/fixnum
         (only-in racket/unsafe/ops unsafe-set-immutable-cdr!))

(define-syntax-rule (show e)
  (with-handlers ([(lambda (x) #t)
                   (lambda (x)
                     (printf "~s: ~a: ~a\n" 'e
                             (cond [(exn:fail:contract:arity? x) "arity"]
                                   [(exn:fail:contract? x) "contract"]
                                   [(exn:fail? x) "fail"]
                                   [else "raise"])
                             (if (exn? x)
                                 (regexp-replace #rx"#<thread[^>]*>" (exn-message x) "#<thread>")
                                 x)))])
    (call-with-values (lambda () e)
                      (lambda vs (printf "~s => ~s\n" 'e vs)))))

(define cyc (read (open-input-string "#0=(1 . #0#)")))
(show (memq 'a cyc))
(show (memv 2 cyc))
(show (assq 'a cyc))
(show (memq 'a '(1 2 . 3)))
(show (memq 2 '(1 2 . 3)))
(show (assq 'a '(1)))
(show (assv 1 '((1 . 2))))
(show (member 'a cyc))
(show (chaperone? (chaperone-box (box-immutable 10) (lambda (b v) v) (lambda (b v) v))))
(show (unbox (chaperone-box (box-immutable 10) (lambda (b v) (+ v 1)) (lambda (b v) v))))
(show (impersonate-box (box-immutable 10) (lambda (b v) v) (lambda (b v) v)))
(show (immutable? (chaperone-box (box-immutable 10) (lambda (b v) v) (lambda (b v) v))))
(show (list (equal? (flvector +nan.0) (flvector +nan.0))
            (equal-always? (fxvector 1 2) (fxvector 1 2))
            (equal? (fxvector 1 2) (fxvector 1 2))
            (equal? (make-flvector 2 0.0) (make-flvector 2 -0.0))
            (equal? (stencil-vector 1 'a) (stencil-vector 1 'a))
            (equal-always? (stencil-vector 1 'a) (stencil-vector 1 'a))))
(show (= (equal-hash-code (fxvector 1 2)) (equal-hash-code (fxvector 1 2))))
(show (let-values ([(i o) (make-pipe)])
        (close-input-port i)
        (list (evt? (sync/timeout 0 (port-progress-evt i)))
              (peek-bytes-avail! (make-bytes 10) 0 (port-progress-evt i) i))))
(show (parameterize ([current-custodian (let ([c (make-custodian)]) (custodian-shutdown-all c) c)])
        (kill-thread (current-thread))))
(show (list (procedure? most-positive-fixnum) (fixnum? (most-positive-fixnum))))
(show (map procedure-result-arity (list car list values)))
(show (bytes<?))
(show (char-ci<?))
(show (string=?))
(show (bytes=? #"a" "a"))
(show (format "~s" (stencil-vector 1 (read (open-input-string "#0=(#0#)")))))
(show (let ([p (make-inspector)]) (parameterize ([current-inspector p]) (eq? (current-inspector) p))))
