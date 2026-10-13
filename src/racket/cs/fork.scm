;;; Strategy A (and C): the Racket Chez fork's additions, on --chez
;;; (src/racket/cs/fork.lisp lists them; docs/racket.md, "The Chez
;;; fork's additions").  Loaded into the Chez top level before rumble.
;;;
;;; Each group maps onto what the Chez mode already has:
;;;   continuation attachments  -> its continuation marks (ac9fb00)
;;;   engines, timers           -> its engines (ac9fb00)
;;;   wrapper procedures        -> funcallable instances (new)
;;;   record internals          -> its records (oop.ms work)
;;;   ephemerons, phantoms      -> SBCL weak tables and finalizers
;;;   foreign-procedure, ftypes -> CFFI
;;;
;;; Nothing is defined yet: milestone 1.
