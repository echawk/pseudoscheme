;;; Host adapter for Racket, in its R5RS language (mutable pairs, which
;;; the translator needs).  Run from the repository root:
;;;   plt-r5rs boot/hosts/racket.scm

(#%require (only srfi/9 define-record-type)
	   (only racket/base version error))

(define boot:host-name (string-append "Racket " (version)))

(define (boot:eval form) (eval form (interaction-environment)))

(define-record-type boot:record
  (boot:make-record rtd serial fields)
  boot:record?
  (rtd boot:record-rtd)
  (serial boot:record-serial)
  (fields boot:record-fields))

(for-each load
	  '("boot/base.scm" "boot/reader.scm" "boot/printer.scm"
	    "boot/runtime.scm" "boot/bootstrap.scm"))
