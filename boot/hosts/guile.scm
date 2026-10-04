;;; Host adapter for Guile.  Run from the repository root:
;;;   guile --no-auto-compile -s boot/hosts/guile.scm

(use-modules (srfi srfi-9))

(define boot:host-name (string-append "Guile " (version)))

(define (boot:eval form) (primitive-eval form))

(define-record-type boot:record
  (boot:make-record rtd serial fields)
  boot:record?
  (rtd boot:record-rtd)
  (serial boot:record-serial)
  (fields boot:record-fields))


(for-each primitive-load
	  '("boot/base.scm" "boot/reader.scm" "boot/printer.scm"
	    "boot/runtime.scm" "boot/bootstrap.scm"))
