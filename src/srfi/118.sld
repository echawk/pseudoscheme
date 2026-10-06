;;; SRFI 118: simple adjustable-size strings.  Written for Pseudoscheme,
;;; and only an approximation: Pseudoscheme's strings are Common Lisp
;;; simple strings, which cannot change length in place (make-string and
;;; string-copy cannot return variable-size strings without a change to
;;; Pseudoscheme's string representation).
;;;
;;; So string-append! and string-replace! are macros, in the manner of
;;; SRFI 185's: when the string argument is a variable, the variable is
;;; set! to the resized string, which is a new string unless the length
;;; did not change.  Other references to the old string do not see the
;;; change.  When the string argument is not a variable, the operation is
;;; done in place if it keeps the length (string-replace! with a
;;; replacement of the same length, or string-append! of nothing) and is
;;; an error otherwise.  Being macros, they cannot be passed as
;;; procedures or applied.
(define-library (srfi 118)
  (export string-append! string-replace!)
  (import (scheme base) (scheme case-lambda)
          (only (rnrs syntax-case) syntax-case syntax identifier?))
  (begin
    ;; Both return DST itself, changed, when the length stays the same,
    ;; and otherwise a new string.
    (define (%string-append dst . values)
      (let ((tail (apply string-append
                         (map (lambda (x) (if (char? x) (string x) x))
                              values))))
        (if (zero? (string-length tail))
            dst
            (string-append dst tail))))

    (define %string-replace
      (case-lambda
        ((dst dst-start dst-end src)
         (%string-replace dst dst-start dst-end src 0 (string-length src)))
        ((dst dst-start dst-end src src-start)
         (%string-replace dst dst-start dst-end src src-start
                          (string-length src)))
        ((dst dst-start dst-end src src-start src-end)
         (unless (<= 0 dst-start dst-end (string-length dst))
           (error "string-replace!: bad destination range" dst-start dst-end))
         (unless (<= 0 src-start src-end (string-length src))
           (error "string-replace!: bad source range" src-start src-end))
         (if (= (- dst-end dst-start) (- src-end src-start))
             (begin
               ;; string-copy! copies as if through a temporary string
               (string-copy! dst dst-start src src-start src-end)
               dst)
             (string-append (substring dst 0 dst-start)
                            (substring src src-start src-end)
                            (substring dst dst-end (string-length dst)))))))

    (define (resized-in-place who dst result)
      (unless (eq? dst result)
        (error (string-append
                who ": cannot change the length of a string that is not "
                "in a variable (Pseudoscheme's strings are fixed-size)")
               dst)))

    (define-syntax string-append!
      (lambda (form)
        (syntax-case form ()
          ((_ place value ...)
           (identifier? #'place)
           #'(set! place (%string-append place value ...)))
          ((_ dst value ...)
           #'(let ((d dst))
               (resized-in-place "string-append!" d
                                 (%string-append d value ...)))))))

    (define-syntax string-replace!
      (lambda (form)
        (syntax-case form ()
          ((_ place arg ...)
           (identifier? #'place)
           #'(set! place (%string-replace place arg ...)))
          ((_ dst arg ...)
           #'(let ((d dst))
               (resized-in-place "string-replace!" d
                                 (%string-replace d arg ...)))))))))
