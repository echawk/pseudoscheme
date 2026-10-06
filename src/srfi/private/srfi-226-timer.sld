;;; For the SRFI 226 sample implementation: Chez Scheme's timer
;;; interrupts, which preempt its threads.  There are none here: its
;;; threads switch when they yield, block or end.
(define-library (srfi private srfi-226-timer)
  (export %call-with-interrupt-handler %set-timer!)
  (import (scheme base))
  (begin
    (define (%call-with-interrupt-handler handler thunk) (thunk))
    (define (%set-timer! on?) (if #f #f))))
