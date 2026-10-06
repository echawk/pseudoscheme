;;; SRFI 259: Tagged procedures with type safety.  Daphne Preston-Kendal's
;;; sample implementation from the SRFI document, on SRFI 229, unmodified
;;; but for this comment (MIT licence; reference/srfi-259/LICENSE).  As the
;;; SRFI notes, code that uses SRFI 229 directly can see these tags.
(define-library (srfi 259)
  (export define-procedure-tag)
  (import (scheme base)
          (srfi 229))
  (begin
    (define-record-type Tag
      (make-tag vals)
      tag?
      (vals tag-vals))
    (define-syntax define-procedure-tag
      (syntax-rules ()
        ((_ constructor predicate accessor)
         (begin
           (define key (cons 'constructor '()))
           (define (constructor tag underlying-proc)
             (if (and (procedure/tag? underlying-proc)
                      (tag? (procedure-tag underlying-proc)))
                 (lambda/tag (make-tag
                              (cons (cons key tag)
                                    (tag-vals
                                     (procedure-tag underlying-proc))))
                             args
                             (apply underlying-proc args))
                 (lambda/tag (make-tag (cons (cons key tag) '()))
                             args
                             (apply underlying-proc args))))
           (define (predicate obj)
             (and (procedure/tag? obj)
                  (tag? (procedure-tag obj))
                  (not (not (assq key (tag-vals (procedure-tag obj)))))))
           (define (accessor proc)
             (cond ((assq key (tag-vals (procedure-tag proc)))
                    => cdr)
                   (else (error "not tagged in this protocol" proc))))))))))
