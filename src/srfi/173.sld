;;; SRFI 173: hooks.  Amirouche Boubekki's sample implementation,
;;; unmodified (reference/srfi-173/hook.body.scm; MIT licence, per the
;;; SRFI document, in reference/srfi-173/LICENSE).  The sample's library
;;; is named (hook); this is the same library as (srfi 173).  hook-run
;;; checks the argument count with SRFI 145's assume.
(define-library (srfi 173)
  (export make-hook
          hook?
          list->hook
          list->hook!
          hook-add!
          hook-delete!
          hook-reset!
          hook->list
          hook-run)
  (import (scheme base)
          (srfi 145))
  (include "reference/srfi-173/hook.body.scm"))
