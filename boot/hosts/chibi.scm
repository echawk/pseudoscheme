;;; Host adapter for Chibi Scheme.  Run from the repository root:
;;;   chibi-scheme boot/hosts/chibi.scm
;;;
;;; Everything happens in the interaction environment, with R5RS
;;; imported into it.

(import (scheme base) (scheme eval) (scheme repl) (scheme load))

(define env (interaction-environment))

(eval '(import (scheme r5rs)) env)

(eval '(define boot:host-name "Chibi Scheme") env)

(eval '(define (boot:eval form) (eval form (interaction-environment))) env)

(eval '(define-record-type boot:record
	 (boot:make-record rtd serial fields)
	 boot:record?
	 (rtd boot:record-rtd)
	 (serial boot:record-serial)
	 (fields boot:record-fields))
      env)

(for-each (lambda (file) (load file env))
	  '("boot/base.scm" "boot/reader.scm" "boot/printer.scm"
	    "boot/runtime.scm" "boot/bootstrap.scm"))
