;;; SRFI 36: I/O conditions.  Written for Pseudoscheme, on (srfi 35) and
;;; R6RS's I/O condition types (library report 8.1).
;;;
;;; Where R6RS has a type with the same place in the hierarchy and the
;;; same fields, the SRFI 36 type *is* the R6RS type, so a predicate here
;;; recognises the conditions the system raises:
;;;
;;;   &i/o-error                     = R6RS &i/o
;;;   &i/o-port-error                = R6RS &i/o-port      (field port)
;;;   &i/o-filename-error            = R6RS &i/o-filename  (field filename)
;;;   &i/o-file-protection-error     = R6RS &i/o-file-protection
;;;   &i/o-file-is-read-only-error   = R6RS &i/o-file-is-read-only
;;;   &i/o-file-already-exists-error = R6RS &i/o-file-already-exists
;;;   &i/o-no-such-file-error        = R6RS &i/o-file-does-not-exist
;;;
;;; and their predicates and accessors (i/o-error?, i/o-port-error?,
;;; i/o-error-port, i/o-filename-error?, i/o-error-filename, ...) are
;;; R6RS's, re-exported.  The rest are new types, defined with SRFI 35:
;;;
;;; - &i/o-read-error, &i/o-write-error and &i/o-closed-error are
;;;   subtypes of &i/o-port-error, as SRFI 36 says.  R6RS's &i/o-read and
;;;   &i/o-write descend from &i/o and have no port field, so they are
;;;   different types, and SRFI 36's i/o-read-error? and i/o-write-error?
;;;   clash with (rnrs io ports)'s.
;;; - &i/o-malformed-filename-error, under &i/o-filename-error.
;;; - &read-error, with fields line, column, position and span.  SRFI 36
;;;   puts it directly under &error; here its parent is R6RS's &i/o-read
;;;   (itself under &i/o and &error), so that R7RS's read-error? (true of
;;;   &i/o-read and &lexical conditions) recognises it.  read-error? is
;;;   then R7RS's own binding, re-exported, and does not clash with
;;;   (scheme base); it is also true of the &lexical conditions the
;;;   reader raises on a parse error, for which read-error-line and the
;;;   other accessors return #f (SRFI 36 lets every field be #f).  The
;;;   difference from SRFI 36: i/o-error? is true of a &read-error.
;;;
;;; SRFI 36 also asks that the R5RS I/O procedures raise these
;;; conditions.  Pseudoscheme's file-opening procedures raise an
;;; &i/o-filename-error (with &message and &irritants) when the file
;;; cannot be opened, but not the more specific subtypes, and the other
;;; procedures' failures are not classified.
(define-library (srfi 36)
  (export &i/o-error i/o-error?
          &i/o-port-error i/o-port-error? i/o-error-port
          &i/o-read-error i/o-read-error?
          &i/o-write-error i/o-write-error?
          &i/o-closed-error i/o-closed-error?
          &i/o-filename-error i/o-filename-error? i/o-error-filename
          &i/o-malformed-filename-error i/o-malformed-filename-error?
          &i/o-file-protection-error i/o-file-protection-error?
          &i/o-file-is-read-only-error i/o-file-is-read-only-error?
          &i/o-file-already-exists-error i/o-file-already-exists-error?
          &i/o-no-such-file-error i/o-no-such-file-error?
          &read-error read-error? read-error-line read-error-column
          read-error-position read-error-span)
  (import (scheme base)
          (srfi 35)
          (only (rnrs records syntactic) record-type-descriptor)
          (prefix (only (rnrs io ports)
                        &i/o i/o-error?
                        &i/o-port i/o-port-error? i/o-error-port &i/o-read
                        &i/o-filename i/o-filename-error? i/o-error-filename
                        &i/o-file-protection i/o-file-protection-error?
                        &i/o-file-is-read-only i/o-file-is-read-only-error?
                        &i/o-file-already-exists i/o-file-already-exists-error?
                        &i/o-file-does-not-exist i/o-file-does-not-exist-error?)
                  r6:))
  (begin
    (define &i/o-error (record-type-descriptor r6:&i/o))
    (define i/o-error? r6:i/o-error?)
    (define &i/o-port-error (record-type-descriptor r6:&i/o-port))
    (define i/o-port-error? r6:i/o-port-error?)
    (define i/o-error-port r6:i/o-error-port)
    (define &i/o-filename-error (record-type-descriptor r6:&i/o-filename))
    (define i/o-filename-error? r6:i/o-filename-error?)
    (define i/o-error-filename r6:i/o-error-filename)
    (define &i/o-file-protection-error (record-type-descriptor r6:&i/o-file-protection))
    (define i/o-file-protection-error? r6:i/o-file-protection-error?)
    (define &i/o-file-is-read-only-error (record-type-descriptor r6:&i/o-file-is-read-only))
    (define i/o-file-is-read-only-error? r6:i/o-file-is-read-only-error?)
    (define &i/o-file-already-exists-error (record-type-descriptor r6:&i/o-file-already-exists))
    (define i/o-file-already-exists-error? r6:i/o-file-already-exists-error?)
    (define &i/o-no-such-file-error (record-type-descriptor r6:&i/o-file-does-not-exist))
    (define i/o-no-such-file-error? r6:i/o-file-does-not-exist-error?)

    (define-condition-type &i/o-read-error &i/o-port-error
      i/o-read-error?)
    (define-condition-type &i/o-write-error &i/o-port-error
      i/o-write-error?)
    (define-condition-type &i/o-closed-error &i/o-port-error
      i/o-closed-error?)
    (define-condition-type &i/o-malformed-filename-error &i/o-filename-error
      i/o-malformed-filename-error?)
    (define &read-error
      (make-condition-type '&read-error (record-type-descriptor r6:&i/o-read)
                           '(line column position span)))
    (define (read-error-field name)
      (lambda (c)
        (and (condition? c)
             (condition-has-type? c &read-error)
             (condition-ref (extract-condition c &read-error) name))))
    (define read-error-line (read-error-field 'line))
    (define read-error-column (read-error-field 'column))
    (define read-error-position (read-error-field 'position))
    (define read-error-span (read-error-field 'span))))
