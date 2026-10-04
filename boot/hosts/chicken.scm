;;; Host adapter for CHICKEN.  Run from the repository root:
;;;   csi -s boot/hosts/chicken.scm

(import (chicken platform))

(define boot:host-name (string-append "CHICKEN " (chicken-version)))

(define (boot:eval form) (eval form))

(define-record-type boot:record
  (boot:make-record rtd serial fields)
  boot:record?
  (rtd boot:record-rtd)
  (serial boot:record-serial)
  (fields boot:record-fields))

(for-each load
	  '("boot/base.scm" "boot/reader.scm" "boot/printer.scm"
	    "boot/runtime.scm" "boot/bootstrap.scm"))
