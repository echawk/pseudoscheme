;;; SRFI 124: ephemerons.  An ephemeron is a one-entry weak-key Lisp hash
;;; table, KEY -> DATUM.  SBCL's weak-key tables are ephemeral: the datum
;;; is kept only while the key is reachable from elsewhere, so a datum
;;; that refers to its own key doesn't keep the ephemeron alive.  Once the
;;; collector drops the key the table is empty, and the ephemeron is
;;; broken.  Keys that are never collected (fixnums, characters) never
;;; break their ephemerons.

(define-library (srfi 124)
  (export ephemeron? make-ephemeron ephemeron-broken? ephemeron-key
          ephemeron-datum reference-barrier)
  (import (scheme base) (pseudoscheme lisp))
  (begin
    (define-record-type ephemeron
      (wrap-table table)
      ephemeron?
      (table ephemeron-table))

    ;; src/r6rs/hashtables.lisp
    (define lisp-make-table (lisp-function "make-ephemeron-table" "pseudoscheme-r6rs"))
    (define lisp-entry (lisp-function "ephemeron-entry" "pseudoscheme-r6rs"))
    (define lisp-count (lisp-function "hash-table-count" "common-lisp"))

    (define (make-ephemeron key datum)
      (wrap-table (lisp-funcall lisp-make-table key datum)))

    (define (ephemeron-broken? e)
      (= 0 (lisp-funcall lisp-count (ephemeron-table e))))

    ;; the key and the datum, or #f and #f once it is broken
    (define (ephemeron-key e)
      (let-values (((key datum) (lisp-funcall lisp-entry (ephemeron-table e))))
        key))
    (define (ephemeron-datum e)
      (let-values (((key datum) (lisp-funcall lisp-entry (ephemeron-table e))))
        datum))

    ;; Keeps KEY reachable up to this call: an opaque call the compiler
    ;; can't drop.
    (define lisp-identity (lisp-function "identity" "common-lisp"))
    (define (reference-barrier key)
      (lisp-funcall lisp-identity key)
      (if #f #f))))
