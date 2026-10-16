;;; Strategy A (and C): additions that only Racket's fork of Chez Scheme
;;; makes, defined for --racket alone (never in the Chez mode itself;
;;; src/racket/cs/fork.lisp says why).
;;;
;;; At the Racket version pinned in build/external/racket the fork's s/primdata.ss
;;; differs from upstream Chez's (cisco/ChezScheme e3b13636) in two
;;; flags, so everything rumble uses is upstream Chez's, and src/chez/
;;; (system.scm, host.lisp) has it: continuation attachments on the
;;; marks, the $app family, system stencil vectors, record internals,
;;; timers.  The $intmap-* names of rumble/patricia.ss are Racket's alone,
;;; but that file isn't the one rumble includes (hamt-stencil.ss is).
;;;
;;; Nothing is defined here yet.
