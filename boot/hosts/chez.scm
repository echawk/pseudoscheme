;;; Host adapter for Chez Scheme.  Run from the repository root:
;;;   chez --script boot/hosts/chez.scm

(define boot:host-name (scheme-version))

(define (boot:eval form) (eval form))

;; Chez's DEFINE-RECORD-TYPE is R6RS's.
(define-record-type (boot:record boot:make-record boot:record?)
  (fields (immutable rtd boot:record-rtd)
	  (immutable serial boot:record-serial)
	  (immutable fields boot:record-fields)))

(for-each load
	  '("boot/base.scm" "boot/reader.scm" "boot/printer.scm"
	    "boot/runtime.scm" "boot/bootstrap.scm"))
