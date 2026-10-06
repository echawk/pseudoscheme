;;; SRFI 234: topological sorting.  The sample implementation by Shiro
;;; Kawai, John Cowan and Arne Babenhauserheide (adapted from Gauche),
;;; unmodified (reference/srfi-234/234-impl.scm; MIT licence per its SPDX
;;; header, in reference/srfi-234/LICENSE).  The library form is the
;;; shipped srfi/234.sld.
(define-library (srfi 234)
  (import
   (scheme base)
   (scheme case-lambda)
   (srfi 1)
   (srfi 11) ;; let-values
   (srfi 26)) ;; cut
  (export topological-sort
          topological-sort/details
          edgelist->graph
          edgelist/inverted->graph
          graph->edgelist
          graph->edgelist/inverted
          connected-components)
  (include "reference/srfi-234/234-impl.scm"))
