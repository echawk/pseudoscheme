; Host adapter for Scheme 48.  Run from the repository root:
;   scheme48 < boot/hosts/scheme48.scm
; It's fed to the REPL, so it can use commands: ,batch makes an error
; exit (nonzero) rather than enter a nested REPL.

,batch on
,open define-record-types signals

(define boot:host-name "Scheme 48")

(define (boot:eval form) (eval form (interaction-environment)))

(define-record-type boot:record
  (boot:make-record rtd serial fields)
  boot:record?
  (rtd boot:record-rtd)
  (serial boot:record-serial)
  (fields boot:record-fields))

,load boot/base.scm boot/reader.scm boot/printer.scm boot/runtime.scm boot/bootstrap.scm
,exit
