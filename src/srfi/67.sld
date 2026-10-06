;;; SRFI 67: compare procedures.  Sebastian Egner and Jens Axel Søgaard's
;;; reference implementation, unmodified (reference/srfi-67/compare.scm;
;;; MIT licence, in reference/srfi-67/LICENSE).  It needs case-lambda
;;; (SRFI 16), error (SRFI 23) and random-integer (SRFI 27), which come
;;; from (scheme case-lambda), (scheme base) and (srfi 27).
;;; reference/srfi-67/examples.scm is the reference confidence test,
;;; which tests/67.scm runs.
(define-library (srfi 67)
  (export
   ;; conditionals
   if3 if=? if<? if>? if<=? if>=? if-not=?
   ;; predicates from compare procedures
   =? <? >? <=? >=? not=?
   </<? </<=? <=/<? <=/<=? >/>? >/>=? >=/>? >=/>=?
   chain=? chain<? chain>? chain<=? chain>=?
   pairwise-not=?
   min-compare max-compare kth-largest
   ;; compare procedures from predicates
   compare-by< compare-by> compare-by<= compare-by>=
   compare-by=/< compare-by=/>
   ;; constructing compare procedures
   refine-compare select-compare cond-compare
   ;; atomic compare procedures
   boolean-compare char-compare char-compare-ci
   string-compare string-compare-ci symbol-compare
   integer-compare rational-compare real-compare complex-compare
   number-compare
   ;; compound
   pair-compare-car pair-compare-cdr pair-compare
   list-compare list-compare-as-vector
   vector-compare vector-compare-as-list
   default-compare debug-compare)
  (import (scheme base)
          (scheme case-lambda)
          (scheme char)
          (scheme complex)
          (srfi 27))
  (include "reference/srfi-67/compare.scm"))
