;;; Host adapter for s7.  Run from the repository root:
;;;   s7 boot/hosts/s7.scm
;;;
;;; UNTESTED: written from s7's documentation; no s7 was at hand.  s7
;;; has no DEFINE-RECORD-TYPE, so records are environments (INLETs);
;;; and EVAL and LOAD need the global environment, ROOTLET, explicitly
;;; (by default they use the caller's).

(define boot:host-name "s7")

(define (boot:eval form) (eval form (rootlet)))

(define (boot:make-record rtd serial fields)
  (inlet 'boot-rtd rtd 'boot-serial serial 'boot-fields fields))
(define (boot:record? x) (and (let? x) (defined? 'boot-serial x #t)))
(define (boot:record-rtd r) (r 'boot-rtd))
(define (boot:record-serial r) (r 'boot-serial))
(define (boot:record-fields r) (r 'boot-fields))

(for-each (lambda (file) (load file (rootlet)))
	  '("boot/base.scm" "boot/reader.scm" "boot/printer.scm"
	    "boot/runtime.scm" "boot/bootstrap.scm"))
