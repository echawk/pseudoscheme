;;; Composable continuations and prompts through the VM (tests/guile-vm/prompts.sh):
;;; each case compiled to bytecode at -O2, its result printed, to compare with
;;; the installed Guile.
(define (show . xs) (for-each display xs) (newline))
(define (try name src)
  (show name ": "
        (catch #t (lambda () (call-with-values
                                 (lambda () ((compile `(lambda () ,src) #:to 'value #:env (current-module) #:optimization-level 2)))
                               list))
          (lambda args (list 'error args)))))
(try "nested prompts"
  '(let ((k (call-with-prompt 'a
              (lambda ()
                (call-with-prompt 'b
                  (lambda () (abort-to-prompt 'a) (abort-to-prompt 'b #t))
                  (lambda (k x) x)))
              (lambda (k) k))))
     (k)))
(try "nested, values after"
  '(let ((k (call-with-prompt 'a
              (lambda ()
                (let ((r (call-with-prompt 'b
                           (lambda () (abort-to-prompt 'a) (abort-to-prompt 'b 10))
                           (lambda (k x) (+ x 1)))))
                  (values r (* r 2))))
              (lambda (k) k))))
     (call-with-values k list)))
(try "k called twice, nested"
  '(let ((k (call-with-prompt 'a
              (lambda ()
                (+ 1 (call-with-prompt 'b
                       (lambda () (+ 100 (abort-to-prompt 'a)))
                       (lambda (k x) x))))
              (lambda (k) k))))
     (list (k 1) (k 2))))
(try "dynamic-wind inside"
  '(let* ((log '())
          (k (call-with-prompt 'a
               (lambda ()
                 (dynamic-wind (lambda () (set! log (cons 'in log)))
                               (lambda () (* 2 (abort-to-prompt 'a)))
                               (lambda () (set! log (cons 'out log)))))
               (lambda (k) k))))
     (let ((r (k 21))) (list r (reverse log)))))
(try "fluid inside"
  '(let* ((f (make-fluid 1))
          (k (call-with-prompt 'a
               (lambda () (with-fluids ((f 2)) (+ (fluid-ref f) (abort-to-prompt 'a))))
               (lambda (k) k))))
     (list (k 10) (fluid-ref f))))
(try "generator"
  '(let ((gen (lambda (l)
                (let loop ((k (lambda () (for-each (lambda (x) (abort-to-prompt 'g x)) l) 'done)) (acc '()))
                  (call-with-prompt 'g k
                    (lambda (k2 x) (loop (lambda () (k2)) (cons x acc))))))))
     (call-with-prompt 'g (lambda () (gen '(1 2 3))) (lambda (k x) (list 'escaped x)))))
(try "abort inner after compose"
  '(let ((k (call-with-prompt 'a
              (lambda ()
                (call-with-prompt 'b
                  (lambda () (abort-to-prompt 'a) (call-with-prompt 'c (lambda () (abort-to-prompt 'b 5)) (lambda (k x) 'c)))
                  (lambda (k x) (list 'b x (k 7)))))
              (lambda (k) k))))
     (k)))(try "three deep"
  '(let ((k (call-with-prompt 'a
              (lambda ()
                (list 'x (call-with-prompt 'b
                  (lambda () (list 'y (call-with-prompt 'c
                    (lambda () (list 'z (abort-to-prompt 'a) (abort-to-prompt 'c 'cc)))
                    (lambda (k v) (list 'hc v)))))
                  (lambda (k v) (list 'hb v)))))
              (lambda (k) k))))
     (list (k 1) (k 2))))
(try "k inside another prompt"
  '(let ((k (call-with-prompt 'a
              (lambda () (call-with-prompt 'b (lambda () (+ 1 (abort-to-prompt 'a))) (lambda (k x) x)))
              (lambda (k) k))))
     (call-with-prompt 'a (lambda () (+ 100 (k 5))) (lambda (k2) 'aborted))))
(try "throw inside k"
  '(let ((k (call-with-prompt 'a
              (lambda () (call-with-prompt 'b (lambda () (car (abort-to-prompt 'a))) (lambda (k x) x)))
              (lambda (k) k))))
     (list (k '(9)) (catch #t (lambda () (k 5)) (lambda (key . args) key)))))
(try "escape from k"
  '(let ((k (call-with-prompt 'a
              (lambda () (call-with-prompt 'b (lambda () ((abort-to-prompt 'a))) (lambda (k x) x)))
              (lambda (k) k))))
     (list (call/cc (lambda (out) (k (lambda () (out 'escaped))))) (k (lambda () 'normal)))))
