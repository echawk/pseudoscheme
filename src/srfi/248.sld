;;; SRFI 248: Minimal delimited continuations.
;;;
;;; guard is the SRFI's sample implementation's (Marc Nieper-Wißkirchen,
;;; MIT; reference/srfi-248/guard.scm, unmodified).  The sample's
;;; with-unwind-handler is on Guile's prompts; here the prompts are
;;; written on full continuations:
;;;
;;; - call-with-prompt runs its thunk in a dynamic-wind that pushes the
;;;   prompt's entry, (tag return original), on the stack of prompts and
;;;   pops it, so that the stack is part of the dynamic state that
;;;   continuations restore.  The thunk's values go to the entry's
;;;   return continuation, at first the prompt's own.
;;; - abort-to-prompt captures the full continuation and jumps to the
;;;   return continuation of the nearest prompt with its tag, passing its
;;;   handler the delimited continuation.  Called, that makes the
;;;   prompt's entry return to its caller instead and reinstates the
;;;   captured continuation (which re-enters the prompt), so that its
;;;   values come back to its caller; an abort to the same tag inside it
;;;   goes on to the caller's nearest prompt with that tag.  Re-entering
;;;   a prompt otherwise, by an ordinary continuation, restores its
;;;   original return.
;;;
;;; One case differs from Guile's: an ordinary continuation captured
;;; inside a prompt and called while a reinstatement of the same prompt
;;; runs returns as the reinstatement does, not to the prompt as it was
;;; when captured (the prompt's dynamic-wind is common to both, so it
;;; isn't re-entered).  The stack is global, not per thread.  raise, raise-continuable and
;;; with-exception-handler are (scheme base)'s; guard replaces it.
(define-library (srfi 248)
  (export raise raise-continuable with-exception-handler with-unwind-handler guard
          => else)
  (import (rename (scheme base) (guard r7:guard)) (scheme case-lambda)
          (rnrs syntax-case))
  (begin
    (define prompts '())                ;entries, innermost first
    (define pending #f)                 ;the entry a delimited continuation re-enters

    (define (entry-tag e) (car e))
    (define (entry-return e) (cadr e))
    (define (set-entry-return! e k) (set-car! (cdr e) k))
    (define (entry-original e) (car (cddr e)))

    (define (remove-entry e es)
      (cond ((null? es) '())
            ((eq? (car es) e) (cdr es))
            (else (cons (car es) (remove-entry e (cdr es))))))

    (define (call-with-prompt tag thunk handler)
      (let ((r (call-with-current-continuation
                (lambda (k)
                  (let ((entry (list tag k k)))
                    (dynamic-wind
                     (lambda ()
                       (if (eq? pending entry)
                           (set! pending #f)
                           (set-entry-return! entry (entry-original entry)))
                       (set! prompts (cons entry prompts)))
                     (lambda ()
                       (call-with-values thunk
                         (lambda vals ((entry-return entry) (cons 'values vals)))))
                     (lambda () (set! prompts (remove-entry entry prompts)))))))))
        (if (eq? (car r) 'values)
            (apply values (cdr r))
            (handler (cadr r) (car (cddr r))))))

    (define (find-prompt tag)
      (let loop ((es prompts))
        (cond ((null? es) (error "abort-to-prompt: no prompt with this tag" tag))
              ((eq? (entry-tag (car es)) tag) (car es))
              (else (loop (cdr es))))))

    (define (abort-to-prompt tag obj)
      (call-with-values
          (lambda ()
            (call-with-current-continuation
             (lambda (c)
               (let ((entry (find-prompt tag)))
                 ((entry-return entry)
                  (list 'abort obj (delimited-continuation c entry)))))))
        (lambda vals
          (set! pending #f)
          (apply values vals))))

    (define (delimited-continuation c entry)
      (lambda vals
        (let ((r (call-with-current-continuation
                  (lambda (ret)
                    (set-entry-return! entry ret)
                    (set! pending entry)
                    (apply c vals)))))
          (if (eq? (car r) 'values)
              (apply values (cdr r))
              ((entry-return (find-prompt (entry-tag entry))) r)))))

    (define (with-unwind-handler handler thunk)
      (let ((tag (list 'handler)))
        (let f ((thunk thunk))
          (call-with-prompt tag
            (lambda ()
              (with-exception-handler
                  (lambda (obj) (abort-to-prompt tag obj))
                thunk))
            (lambda (obj k)
              (handler obj (lambda args (f (lambda () (apply k args))))))))))

    (include "reference/srfi-248/guard.scm")))
