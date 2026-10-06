;;; SRFI 167: ordered key-value store.  Amirouche Boubekki's sample
;;; implementation (MIT licence in reference/srfi-167/LICENSE), whose
;;; .scm files are in reference/srfi-167/, unmodified.  The SRFI has two
;;; parts, each a library here, as in the sample (in 167/):
;;; - (srfi 167 engine), the engine record, through which code such as
;;;   SRFI 168 works with any store;
;;; - the okvs procedures, here (srfi 167 memory), the sample's
;;;   in-memory store (SRFI 146 mappings with transactions), and its
;;;   key packing, (srfi 167 pack).
;;; (srfi 167) exports both the engine and the in-memory okvs.  There is
;;; no store on disk: okvs-open's home argument is ignored, and the
;;; data lives only as long as the okvs.
(define-library (srfi 167)
  (export
   ;; engine
   make-engine engine? engine-open engine-close engine-in-transaction
   engine-ref engine-set! engine-delete! engine-range-remove! engine-range
   engine-prefix-range engine-hook-on-transaction-begin
   engine-hook-on-transaction-commit engine-pack engine-unpack
   ;; okvs
   okvs-open okvs? okvs-close make-default-state okvs-transaction?
   okvs-transaction-state okvs-in-transaction okvs-ref okvs-set!
   okvs-delete! okvs-range-remove! okvs-range okvs-prefix-range
   okvs-hook-on-transaction-begin okvs-hook-on-transaction-commit
   make-default-engine)
  (import (srfi 167 engine)
          (srfi 167 memory)))
