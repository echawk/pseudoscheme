;;; SRFI 158: generators and accumulators.  The SRFI's sample
;;; implementation (Shiro Kawai, John Cowan, Thomas Gilray), in
;;; reference/srfi-158/srfi-158-impl.scm (MIT licence, per the SRFI
;;; document, in reference/srfi-158/LICENSE).  This library form follows
;;; the shipped srfi-158.sld, renamed (srfi 158).
;;;
;;; Modifications to the reference file, each marked PSEUDOSCHEME there:
;;;   gtake and make-unfold-generator are rewritten in direct style (same
;;;     behaviour, still lazy), from when Pseudoscheme's continuations
;;;     were escape-only;
;;;   make-coroutine-generator (and so make-for-each-generator) is the
;;;     sample's, resuming a continuation captured in an earlier call of
;;;     the generator, when continuations are re-entrant (the default);
;;;     with --continuations=escape, it runs its procedure to completion on
;;;     the first call and buffers the yielded values, so an infinite
;;;     coroutine hangs there.
(define-library (srfi 158)
  (import (scheme base)
          (scheme case-lambda))
  (export generator circular-generator make-iota-generator make-range-generator
          make-coroutine-generator list->generator vector->generator
          reverse-vector->generator string->generator
          bytevector->generator
          make-for-each-generator make-unfold-generator)
  (export gcons* gappend gcombine gfilter gremove
          gtake gdrop gtake-while gdrop-while
          gflatten ggroup gmerge gmap gstate-filter
          gdelete gdelete-neighbor-dups gindex gselect)
  (export generator->list generator->reverse-list
          generator->vector generator->vector! generator->string
          generator-fold generator-map->list generator-for-each generator-find
          generator-count generator-any generator-every generator-unfold)
  (export make-accumulator count-accumulator list-accumulator
          reverse-list-accumulator vector-accumulator
          reverse-vector-accumulator vector-accumulator!
          string-accumulator bytevector-accumulator bytevector-accumulator!
          sum-accumulator product-accumulator)
  (include "reference/srfi-158/srfi-158-impl.scm"))
