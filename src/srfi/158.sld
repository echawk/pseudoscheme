;;; SRFI 158: generators and accumulators.  The SRFI's sample
;;; implementation (Shiro Kawai, John Cowan, Thomas Gilray), in
;;; reference/srfi-158/srfi-158-impl.scm (MIT licence, per the SRFI
;;; document, in reference/srfi-158/LICENSE).  This library form follows
;;; the shipped srfi-158.sld, renamed (srfi 158).
;;;
;;; Modifications to the reference file, each marked PSEUDOSCHEME there:
;;; the sample make-coroutine-generator resumes a continuation captured
;;; in an earlier, already-returned call of the generator.
;;; Pseudoscheme's continuations are escape-only (re-entering a dead one
;;; can crash the Lisp image), so
;;;   gtake and make-unfold-generator are rewritten in direct style
;;;     (same behaviour, still lazy, so infinite sources work);
;;;   make-coroutine-generator (and so make-for-each-generator) runs its
;;;     procedure to completion on the first call and buffers the yielded
;;;     values.  Right for finite producers; an infinite coroutine hangs,
;;;     and the producer's side effects all happen at the first call.
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
