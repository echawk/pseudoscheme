;;; SRFI 216: SICP prerequisites (portable).  Vladimir Nikishkin's sample
;;; implementation (reference/srfi-216/216.scm; MIT licence, per the SRFI
;;; document, in reference/srfi-216/LICENSE), on SRFI 18 and SRFI 27, with
;;; one change: runtime computed (* jiffies jiffies-per-second 1e6), which
;;; isn't microseconds; it is now jiffies * 1e6 / jiffies-per-second.  The
;;; library form is the sample's.  As in the sample, true, false and nil
;;; are #t, #f and '(), the-empty-stream is '() and stream-null? is null?.
;;; random is SRFI 216's, so it clashes with other libraries' random.
(define-library (srfi 216)
  (import (scheme base))
  (import (scheme time))
  (import (only (scheme write) display))
  (import (scheme lazy))
  (import (only (srfi 27) random-integer random-real))
  (import (only (srfi 18)
                thread-start!
                make-thread
                thread-join!
                make-mutex
                mutex-lock!
                mutex-unlock!))
  (export runtime random parallel-execute test-and-set!
          cons-stream stream-null? the-empty-stream
          true false nil)
  (include "reference/srfi-216/216.scm"))
