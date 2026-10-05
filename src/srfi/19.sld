;;; SRFI 19: time data types and procedures.  Will Fitzgerald's
;;; reference implementation (I/NET, Inc.; MIT license), in
;;; reference/srfi-19/srfi-19.scm.  It was written for MzScheme; the
;;; host procedures it expects (current-seconds, current-milliseconds,
;;; current-process-milliseconds, current-gc-milliseconds,
;;; seconds->date, date-time-zone-offset, exact->inexact, eof, ...) are
;;; supplied below, on top of R7RS (scheme time).
;;;
;;; Modifications to the reference code, each marked "Pseudoscheme:":
;;; - MzScheme's (define-struct time ...) and (define-struct date ...)
;;;   are written as define-record-type.  For date, the record's setters
;;;   are named tm:set-date-X! directly, since the original's
;;;   (define tm:set-date-X! set-date-X!) followed by a redefinition of
;;;   set-date-X! defines one name twice in a library body.
;;; - Current-time nanoseconds were computed as milliseconds * 10000;
;;;   they are milliseconds * 1000000 (and tm:current-time-ms-time
;;;   divided milliseconds by 10000 to get seconds; it divides by 1000).
;;; - date->julian-day divided by the negated zone offset rather than
;;;   subtracting it from the seconds: a division by zero for any UTC
;;;   date (a bug in the reference implementation).
;;; - tm:local-tz-offset takes an optional instant, and time->date
;;;   conversions use the local offset in effect at the time converted
;;;   (so a winter date gets standard time, a summer one daylight time),
;;;   instead of the offset in effect now.
;;; The reference string->date also calls an undefined time-error where
;;; it means tm:time-error; that is supplied below rather than edited.
;;;
;;; Time zones: the local offset comes from Common Lisp's
;;; decode-universal-time, which follows the host's time zone (TZ),
;;; daylight saving included.
(define-library (srfi 19)
  (export time-tai time-utc time-monotonic time-thread
          time-process time-duration current-time time-resolution
          make-time time? time-type time-second time-nanosecond
          set-time-type! set-time-second! set-time-nanosecond! copy-time
          time=? time<? time<=? time>? time>=?
          time-difference time-difference! add-duration add-duration!
          subtract-duration subtract-duration!
          make-date date? date-nanosecond date-second date-minute
          date-hour date-day date-month date-year date-zone-offset
          date-year-day date-week-day date-week-number current-date
          current-julian-day current-modified-julian-day
          date->julian-day date->modified-julian-day date->time-monotonic
          date->time-tai date->time-utc
          julian-day->date julian-day->time-monotonic
          julian-day->time-tai julian-day->time-utc
          modified-julian-day->date modified-julian-day->time-monotonic
          modified-julian-day->time-tai modified-julian-day->time-utc
          time-monotonic->date time-monotonic->julian-day
          time-monotonic->modified-julian-day
          time-monotonic->time-tai time-monotonic->time-tai!
          time-monotonic->time-utc time-monotonic->time-utc!
          time-utc->date time-utc->julian-day
          time-utc->modified-julian-day
          time-utc->time-monotonic time-utc->time-monotonic!
          time-utc->time-tai time-utc->time-tai!
          time-tai->date time-tai->julian-day
          time-tai->modified-julian-day
          time-tai->time-monotonic time-tai->time-monotonic!
          time-tai->time-utc time-tai->time-utc!
          date->string string->date)
  (import (scheme base) (scheme char) (scheme cxr) (scheme file) (scheme read) (scheme write)
          (scheme time)
          (only (prefix (cl common-lisp) cl:) cl:decode-universal-time))
  (begin
    ;; The host clock, as MzScheme's procedures.
    (define (current-seconds) (exact (floor (current-second))))
    (define (current-milliseconds) (exact (floor (* 1000 (current-second)))))
    ;; Process time: jiffies since some point in the process's life.
    (define (current-process-milliseconds)
      (quotient (* 1000 (current-jiffy)) (jiffies-per-second)))
    (define (current-gc-milliseconds) 0)
    ;; MzScheme's seconds->date and date-time-zone-offset, as far as
    ;; tm:local-tz-offset needs them: the "date" is the instant itself,
    ;; and its zone offset (seconds east of UTC) is the host's, by
    ;; decode-universal-time.  Lisp's zone is in hours west of UTC,
    ;; excluding daylight saving; DST, a Lisp generalized boolean, is
    ;; NIL (Scheme's '()) when daylight saving is not in effect.
    (define (seconds->date seconds) seconds)
    (define (date-time-zone-offset seconds)
      (call-with-values
          (lambda ()
            (cl:decode-universal-time (+ (exact (floor seconds)) 2208988800)))
        (lambda (s mi h d mo y dow dst zone)
          (exact (round (* -3600 (- zone (if (null? dst) 0 1))))))))
    (define (exact->inexact x) (inexact x))
    (define (inexact->exact x) (exact x))
    (define eof (eof-object))
    ;; The reference code's string->date calls time-error once where it
    ;; means tm:time-error (a bug in the reference implementation).
    (define (time-error . args) (apply tm:time-error args)))
  (include "reference/srfi-19/srfi-19.scm"))
