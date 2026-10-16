;;; -*- Mode: Scheme -*-
;;;; (chezscheme): conditions as Chez Scheme displays them.
;;;; Part of the library's body (src/chez/chez.lisp).

;;; format, printf and fprintf are Chez's own (format.scm).

;; errorf and friends format their message (a format condition, in
;; Chez); error's message is taken as it is
(define (raise-format-condition base who fmt args)
  (raise (apply condition base (make-format-condition)
                (append (if who (list (make-who-condition who)) '())
                        (list (make-message-condition (apply format #f fmt args))
                              (make-irritants-condition '()))))))
(define (errorf who fmt . args)
  (raise-format-condition (make-error) who fmt args))
(define (assertion-violationf who fmt . args)
  (raise-format-condition (make-assertion-violation) who fmt args))
(define (warningf who fmt . args)
  (display-warning who (apply format #f fmt args) '()))
(define (warning who message . irritants)
  (display-warning who message irritants))

(define (display-warning who message irritants)
  (let ((p (current-error-port)))
    (display (condition-text "Warning" who message irritants) p)
    (newline p)))

;;; Chez's wording: "Exception in car: 5 is not a pair",
;;; "Exception in foo: bad thing with irritant 42", "Exception: bad
;;; thing with irritants (1 2)", "Exception occurred with non-condition
;;; value 42".

(define (condition-text kind who message irritants)
  (call-with-string-output-port
   (lambda (p)
     (display kind p)
     (when who
       (display " in " p)
       (display (if (string? who) who (format #f "~s" who)) p))
     (display ": " p)
     (display message p)
     (cond ((null? irritants))
           ((null? (cdr irritants))
            (display " with irritant " p)
            (write (car irritants) p))
           (else
            (display " with irritants " p)
            (write irritants p))))))

(define (condition->chez-string c)
  (cond ((not (condition? c))
         (format #f "Exception occurred with non-condition value ~s" c))
        ;; a syntax violation: its message and then its form
        ((and (syntax-violation? c) (message-condition? c) (syntax-violation-form c))
         (string-append
          (condition-text "Exception" (and (who-condition? c) (condition-who c)) (condition-message c) '())
          " "
          (format #f "~s" (syntax->datum (syntax-violation-form c)))))
        ((message-condition? c)
         (condition-text (if (warning? c) "Warning" "Exception")
                         (and (who-condition? c) (condition-who c))
                         (condition-message c)
                         (if (irritants-condition? c) (condition-irritants c) '())))
        (else
         (format #f "Exception occurred with condition components:~{~%  ~s~}"
                 (simple-conditions c)))))

(define display-condition
  (case-lambda
    ((c) (display-condition c (current-output-port)))
    ((c port) (display (condition->chez-string c) port))))
