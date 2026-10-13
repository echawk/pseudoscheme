;;; load-linklet.ss FILE EXPR ...: evaluate a schemified linklet's body
;;; (the forms after its export form) at this top level, with
;;; (racket-lite) imported, then each EXPR, printed with its value
(library-directories (cons "." (map car (library-directories))))
(import (racket-lite))
(define args (cdr (command-line)))
(define (load-linklet file)
  (call-with-input-file file
    (lambda (p)
      (let loop ([n 0] [errors 0])
        (let ([x (read p)])
          (cond
            [(eof-object? x) (printf ";; ~a: ~a forms, ~a failed\n" file n errors)]
            [(and (pair? x) (eq? (car x) 'export)) (loop n errors)]
            [else
             (let ([ok (guard (c [#t (when (< errors 20)
                                         (printf ";; form ~a failed: ~a\n" n
                                                 (if (message-condition? c) (condition-message c) c))
                                         (when (irritants-condition? c) (printf ";;   ~s\n" (condition-irritants c))))
                                       #f])
                         (eval x) #t)])
               (loop (+ n 1) (if ok errors (+ errors 1))))]))))))
(load-linklet (car args))
(for-each (lambda (s)
            (let ([e (read (open-input-string s))])
              (printf "~s\n  => ~s\n" e
                      (guard (c [#t (list 'error (if (message-condition? c) (condition-message c) c)
                                          (and (irritants-condition? c) (condition-irritants c)))])
                        (eval e)))))
          (cdr args))
