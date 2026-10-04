;;; Host adapter for Gauche.  Run from the repository root:
;;;   gosh boot/hosts/gauche.scm

(use gauche.record)

(define boot:host-name (string-append "Gauche " (gauche-version)))

(define (boot:eval form) (eval form (interaction-environment)))

(define-record-type boot:record
  (boot:make-record rtd serial fields)
  boot:record?
  (rtd boot:record-rtd)
  (serial boot:record-serial)
  (fields boot:record-fields))

(for-each (lambda (file) (load (string-append "./" file)))
	  '("boot/base.scm" "boot/reader.scm" "boot/printer.scm"
	    "boot/runtime.scm" "boot/bootstrap.scm"))
