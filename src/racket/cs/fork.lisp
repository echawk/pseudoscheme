; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

;;;; Strategy A (and C): the additions Racket's fork of Chez Scheme makes,
;;;; which rumble uses, provided on --chez (docs/racket.md, "The Chez
;;;; fork's additions").  fork.scm defines the Scheme side; this file
;;;; keeps the list, so that milestone 0's inventory (FORK-INVENTORY) can
;;;; say what's still missing.

(in-package "PSEUDOSCHEME-RACKET")

(defparameter *fork-additions*
  '((:attachments "call-setting-continuation-attachment" "call-getting-continuation-attachment"
     "call-consuming-continuation-attachment" "$current-attachments")
    (:engines "make-engine" "engine-block" "engine-return" "set-timer" "timer-interrupt-handler")
    (:wrapper-procedures "make-wrapper-procedure" "make-arity-wrapper-procedure"
     "wrapper-procedure?" "wrapper-procedure-data" "set-wrapper-procedure!" "$app")
    (:records "$record-ref" "$record-set!" "$sealed-record?" "record-type-sealed?")
    (:memory "make-ephemeron-eq-hashtable" "make-phantom-bytevector" "make-guardian")
    (:ffi "foreign-procedure" "foreign-callable" "make-ftype-pointer"))
  "What rumble needs beyond Chez Scheme, by kind; filled in by milestone
0's inventory, which finds the exact list in Racket's source.")

(defun fork-inventory ()
  "Milestone 0: every identifier rumble and the schemified layers use that
--chez doesn't define, read from Racket's source tree."
  (not-yet "fork-inventory"))
