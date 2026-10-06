;;; SRFI 123: generic accessor and modifier operators.  Taylan Kammer's
;;; reference implementation, unmodified (reference/srfi-123/123.body.scm;
;;; MIT licence, in reference/srfi-123/LICENSE).  The library form
;;; follows the shipped reference/srfi-123/123.sld, with these changes:
;;;
;;;  - (r6rs hashtables) is (rnrs hashtables), and the record branch
;;;    imports (rnrs records procedural) and (rnrs records inspection)
;;;    only; not (rnrs records syntactic), whose define-record-type would
;;;    shadow R7RS's;
;;;  - R6RS's record-accessor and record-mutator take a field index, but
;;;    the reference calls them with a field name, as SRFI 99's
;;;    rtd-accessor and rtd-mutator do.  So record-accessor and
;;;    record-mutator here are wrappers that accept either, looking a name
;;;    up in the record type and its parents;
;;;  - set!, setter and getter-with-setter come from (srfi 17), which
;;;    therefore must be present;
;;;  - the body is expanded with a cond-expand that adds an empty else
;;;    clause, because the body's cond-expands rely on a cond-expand
;;;    with no matching clause expanding to nothing.
;;;
;;; As in SRFI 17, the exported set! is not (scheme base)'s: import
;;; (except (scheme base) set!).  Every field of a record made by R7RS
;;; define-record-type is mutable at the R6RS level, so (set! (ref r
;;; 'field) v) also assigns a field that has no modifier.
(define-library (srfi 123)
  (export
   ref ref* ~ register-getter-with-setter!
   $bracket-apply$
   set! setter getter-with-setter)
  (import
   (except (scheme base) set! define-record-type cond-expand)
   (rename (only (scheme base) cond-expand) (cond-expand r7:cond-expand))
   (scheme case-lambda)
   (rnrs hashtables)
   (srfi 1)
   (srfi 17)
   (srfi 31))
  (cond-expand
   ;; Favor SRFI-99.
   ((library (srfi 99))
    (import (srfi 99)))
   ((library (rnrs records inspection))
    (import (rename (rnrs records procedural)
                    (record-accessor r6:record-accessor)
                    (record-mutator r6:record-mutator)))
    (import (rnrs records inspection))
    (begin
      (define (field-index rtd field)
        ;; The index of FIELD in RTD, and the rtd that owns it.
        (let loop ((rtd rtd))
          (if (not rtd)
              (error "No such field of record type." field)
              (let* ((names (vector->list (record-type-field-names rtd)))
                     (tail (memq field names)))
                (if tail
                    (values rtd (- (length names) (length tail)))
                    (loop (record-type-parent rtd)))))))
      (define (record-accessor rtd field)
        (if (symbol? field)
            (call-with-values (lambda () (field-index rtd field))
              r6:record-accessor)
            (r6:record-accessor rtd field)))
      (define (record-mutator rtd field)
        (if (symbol? field)
            (call-with-values (lambda () (field-index rtd field))
              r6:record-mutator)
            (r6:record-mutator rtd field)))))
   (else
    (import (rename (only (scheme base) define-record-type)
                    (define-record-type %define-record-type)))
    (export define-record-type)))
  (cond-expand
   ((library (srfi 4))
    (import (srfi 4)))
   (else))
  (cond-expand
   ((library (srfi 111))
    (import (srfi 111)))
   (else))
  (begin
    ;; The reference's body-level cond-expands have no else clause and
    ;; rely on one that matches nothing expanding to nothing; R7RS leaves
    ;; that unspecified and Pseudoscheme reports an error.  So the body
    ;; sees this cond-expand, which supplies an empty else.
    (define-syntax cond-expand
      (syntax-rules (else)
        ((_ clause ... (else body ...))
         (r7:cond-expand clause ... (else body ...)))
        ((_ clause ...)
         (r7:cond-expand clause ... (else))))))
  (include "reference/srfi-123/123.body.scm"))
