;;; (srfi 126 private weak): weak and ephemeral eq?/eqv? hashtables for
;;; (srfi 126), made with trivial-garbage's make-weak-hash-table.
;;;
;;; A Pseudoscheme R6RS hashtable is a Lisp struct around a CL hash
;;; table (src/r6rs/hashtables.lisp); an eq or eqv one keys the CL table
;;; by the Scheme objects themselves.  So a weak eq/eqv hashtable is that
;;; struct around a weak CL table, and every (rnrs hashtables) operation
;;; works on it unchanged.  SRFI 126's weaknesses map onto
;;; trivial-garbage's:
;;;   weak-key, ephemeral-key               :key
;;;   weak-value, ephemeral-value           :value
;;;   weak-key-and-value                    :key-and-value
;;;   ephemeral-key-and-value               :key-or-value
;;; (SRFI 126 allows weak-key and weak-value tables to be ephemeral.
;;; SBCL's :key and :value tables are; on other Lisps that depends on
;;; their weak tables.)  Hashtables with a custom hash function are a
;;; CL table from hash value to bucket, which cannot be made weak.
;;;
;;; trivial-garbage is loaded the first time a weak table is made, not
;;; when this library is imported, so (srfi 126) works without it.
;;;
;;; This reaches into the struct's internal constructor and accessors
;;; (pseudoscheme-r6rs::%make-hashtable, hashtable-table, hashtable-kind),
;;; so it has to follow src/r6rs/hashtables.lisp.  lisp-funcall passes
;;; its arguments unconverted, so Lisp's NIL is written '() here.

(define-library (srfi 126 private weak)
  (export make-weak-hashtable weak-hashtable-weakness freeze-hashtable)
  (import (scheme base) (pseudoscheme lisp))
  (begin
    (define tg-make-weak-hash-table #f)
    ;; hashtable -> the SRFI 126 weakness it was made with (weak keys)
    (define registry #f)

    (define (ensure-trivial-garbage!)
      (unless tg-make-weak-hash-table
        (lisp-require "trivial-garbage")
        (set! tg-make-weak-hash-table
              (lisp-function "make-weak-hash-table" "trivial-garbage"))
        (set! registry
              (lisp-funcall tg-make-weak-hash-table
                            #:test (lisp-function "eq" "common-lisp")
                            #:weakness #:key))))

    (define (lisp-weakness weakness)
      (case weakness
        ((weak-key ephemeral-key) #:key)
        ((weak-value ephemeral-value) #:value)
        ((weak-key-and-value) #:key-and-value)
        ((ephemeral-key-and-value) #:key-or-value)
        (else (error "not a hashtable weakness" weakness))))

    ;; KIND is eq or eqv.
    (define (make-weak-hashtable kind weakness)
      (let ((lisp-weakness (lisp-weakness weakness)))
        (ensure-trivial-garbage!)
        (let* ((table (lisp-funcall tg-make-weak-hash-table
                                    #:test (lisp-function
                                            (if (eq? kind 'eq) "eq" "eql")
                                            "common-lisp")
                                    #:weakness lisp-weakness))
               (hashtable (lisp-funcall
                           (lisp-function "%make-hashtable" "pseudoscheme-r6rs")
                           table
                           (if (eq? kind 'eq) #:eq #:eqv)
                           '() '() #t)))
          (register! hashtable weakness)
          hashtable)))

    (define (register! hashtable weakness)
      (lisp (setf (gethash hashtable registry) weakness)))

    ;; The weakness HASHTABLE was made with, or #f.
    (define (weak-hashtable-weakness hashtable)
      (and registry
           (let ((w (lisp-funcall (lisp-function "gethash" "common-lisp")
                                  hashtable registry '())))
             (and (symbol? w) w))))

    ;; An immutable hashtable sharing weak HASHTABLE's contents, for an
    ;; immutable weak copy.
    (define (freeze-hashtable hashtable)
      (let ((frozen (lisp-funcall
                     (lisp-function "%make-hashtable" "pseudoscheme-r6rs")
                     (lisp-funcall (lisp-function "hashtable-table" "pseudoscheme-r6rs")
                                   hashtable)
                     (lisp-funcall (lisp-function "hashtable-kind" "pseudoscheme-r6rs")
                                   hashtable)
                     '() '() '())))
        (register! frozen (weak-hashtable-weakness hashtable))
        frozen))))
