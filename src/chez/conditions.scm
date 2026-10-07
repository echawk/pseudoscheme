;;; -*- Mode: Scheme -*-
;;;; (chezscheme): format, and conditions as Chez Scheme displays them.
;;;; Part of the library's body (src/chez/chez.lisp).

;;; format, printf and fprintf: Common Lisp's format, which Chez's
;;; follows (~a ~s ~d ~x ~f ~e ~$ ~r ~{...~} ~^ ~% ...), with objects
;;; printed as Scheme prints them (%chez:format-to-string, host.lisp).
;;; (format #f fmt arg ...) and Chez's (format fmt arg ...) make a
;;; string; (format #t ...) writes to the current output port, (format
;;; port ...) to PORT.

(define (format dest . rest)
  (cond ((string? dest) (apply %chez:format-to-string dest rest))
        ((eq? dest #f) (apply %chez:format-to-string rest))
        ((eq? dest #t) (apply %chez:format-to-port (current-output-port) rest))
        (else (apply %chez:format-to-port dest rest))))

(define (printf fmt . args) (apply %chez:format-to-port (current-output-port) fmt args))
(define (fprintf port fmt . args) (apply %chez:format-to-port port fmt args))

;; errorf and friends format their message (a format condition, in
;; Chez); error's message is taken as it is
(define (errorf who fmt . args)
  (error who (apply format #f fmt args)))
(define (assertion-violationf who fmt . args)
  (assertion-violation who (apply format #f fmt args)))
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
