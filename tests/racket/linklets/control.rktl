;;; Marks, prompts, raise and handlers, parameters, and host errors as
;;; Racket exceptions -- the way racket/private's with-handlers and
;;; parameterize use the primitives.
(linklet
 ()
 ()
 (define-values (key) 'k)
 (define-values (tag) (make-continuation-prompt-tag 'test))
 (define-values (catch)
   ;; with-handlers, cut down: the handler aborts to a prompt of our own
   (lambda (thunk)
     (call-with-continuation-prompt
      (lambda ()
        (with-continuation-mark exception-handler-key
          (lambda (e) (abort-current-continuation tag (lambda () (list 'caught (if (exn? e) (exn-message e) e)))))
          (thunk)))
      tag
      (lambda (h) (h)))))
 (define-values (p) (make-parameter 1))
 (define-values (q) (make-parameter 5 (lambda (v) (* v 2))))
 (list
  (with-continuation-mark key 1 (continuation-mark-set-first #f key))
  (with-continuation-mark key 1 (list (with-continuation-mark key 2 (continuation-mark-set->list (current-continuation-marks) key))))
  (with-continuation-mark key 1 (with-continuation-mark key 2 (continuation-mark-set->list (current-continuation-marks) key)))
  (call-with-continuation-prompt (lambda () (+ 1 (abort-current-continuation tag 10 20))) tag list)
  (catch (lambda () (raise 'oops)))
  (catch (lambda () (car 5)))
  (catch (lambda () (/ 1 0)))
  (catch (lambda () (error 'who "bad ~a" 42)))
  (catch (lambda () (catch (lambda () (raise 'inner)))))
  (list (p) (q))
  (with-continuation-mark parameterization-key (extend-parameterization (current-parameterization) p 2 q 3)
    (list (p) (q)))
  (list (p) (q))))
(linklet () () (raise 'uncaught) (list 'not-reached))
(linklet () () (list 'after))
