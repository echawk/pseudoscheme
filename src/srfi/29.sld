;;; SRFI 29: localization.  Scott G. Miller's reference implementation,
;;; from the SRFI document (reference/srfi-29/srfi-29.scm; MIT licence
;;; in reference/srfi-29/LICENSE).
;;;
;;; Changes, marked PSEUDOSCHEME in the reference file:
;;; - Its current-language and current-country, which it says "must be
;;;   rewritten for each Scheme system to default to the actual locale
;;;   of the session", are commented out; they are defined below, with
;;;   current-locale-details, which the reference omits.  They default
;;;   to the locale named by the environment variable LC_ALL,
;;;   LC_MESSAGES or LANG (the first that is set and not empty), e.g.
;;;   en_US.UTF-8 gives en, us and (utf-8); and to en, us and () when
;;;   none is set or it is C or POSIX.  The settings are global, not per
;;;   thread.
;;; - format's ~N@* took the Nth of the values not yet consumed; it now
;;;   takes the Nth value absolutely, as the SRFI specifies.
;;;
;;; The SRFI's text names store-bundle, its example and reference
;;; implementation store-bundle!; both names are exported.
;;; load-bundle! and store-bundle! always return #f: there is no
;;; external bundle store, as the SRFI permits.  format clashes with
;;; SRFI 28's (which this one extends).
(define-library (srfi 29)
  (export current-language current-country current-locale-details
          declare-bundle! store-bundle! (rename store-bundle! store-bundle)
          load-bundle! localized-template format)
  (import (scheme base)
          (scheme char)
          (scheme cxr)
          (scheme write)
          (scheme process-context))
  (begin
    ;; The session's locale, from the environment: (language country
    ;; detail ...) as symbols, e.g. "fr_CA.UTF-8@euro" gives
    ;; (fr ca utf-8 euro).
    (define (environment-locale)
      (define (setting)
        (let loop ((vars '("LC_ALL" "LC_MESSAGES" "LANG")))
          (if (null? vars)
              #f
              (let ((v (get-environment-variable (car vars))))
                (if (and v (not (string=? v "")))
                    v
                    (loop (cdr vars)))))))
      (define (split s)
        ;; break at _ . @ into lower-case symbols
        (let loop ((cs (string->list s)) (word '()) (words '()))
          (define (flush)
            (if (null? word)
                words
                (cons (string->symbol (list->string (reverse word))) words)))
          (cond ((null? cs) (reverse (flush)))
                ((memv (car cs) '(#\_ #\. #\@)) (loop (cdr cs) '() (flush)))
                (else (loop (cdr cs) (cons (char-downcase (car cs)) word) words)))))
      (let ((s (setting)))
        (if (or (not s) (string=? s "C") (string=? s "POSIX")
                (string=? s "C.UTF-8"))
            '(en us)
            (let ((parts (split s)))
              (cond ((null? parts) '(en us))
                    ((null? (cdr parts)) (list (car parts) 'us))
                    (else parts))))))

    (define locale (environment-locale))

    (define current-language-value (car locale))
    (define current-country-value (cadr locale))
    (define current-locale-details-value (cddr locale))

    (define (current-language . args)
      (if (null? args)
          current-language-value
          (set! current-language-value (car args))))

    (define (current-country . args)
      (if (null? args)
          current-country-value
          (set! current-country-value (car args))))

    (define (current-locale-details . args)
      (if (null? args)
          current-locale-details-value
          (set! current-locale-details-value (car args)))))
  (include "reference/srfi-29/srfi-29.scm"))
