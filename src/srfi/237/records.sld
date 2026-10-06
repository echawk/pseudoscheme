;;; (srfi 237 records), also (srfi :237 records): SRFI 237, R6RS records
;;; (refined).  Marc Nieper-Wißkirchen's sample implementation
;;; (reference/srfi-237/; MIT licence in reference/srfi-237/LICENSE), on
;;; Pseudoscheme's R6RS records.  This library includes the body of its
;;; (srfi :237 records), reference/srfi-237/records-body.scm, which has
;;; these changes, marked PSEUDOSCHEME there:
;;;   - The uid -> record-type table (record-uid->rtd) is kept in
;;;     memory.  The sample implementation reads it from, and appends to,
;;;     a file record-types.scm in the current directory.
;;;   - Pseudoscheme's psyntax has no SRFI 213 identifier properties, so
;;;     a parent clause naming a record name always becomes a parent-rtd
;;;     clause with that name's record descriptor, as it does in the
;;;     sample implementation for a parent given by an expression.
;;;   - make-record-type-descriptor accepts a #f parent, and
;;;     record-mutator returns a mutator (both bugs in the sample).
;;; Reading and writing records (#r(...) syntax, port-read-rtd and
;;; port-write-rtd) isn't supported, as in the sample implementation.
;;;
;;; define-record-type and the record procedures are different bindings
;;; from (scheme base)'s define-record-type and (rnrs records ...)'s, so
;;; import those with except, or not at all, with this library.
(define-library (srfi 237 records)
  (export
          define-record-type define-record-name fields mutable
          immutable parent protocol sealed opaque nongenerative
          generative parent-rtd record-type-descriptor
          record-constructor-descriptor make-record-type-descriptor
          record-type-descriptor? make-record-descriptor
          make-record-constructor-descriptor record-descriptor-rtd
          record-descriptor-parent record-descriptor?
          record-constructor-descriptor? record-constructor
          record-predicate record-accessor record-mutator record?
          record-rtd record-type-name record-type-parent
          record-type-uid record-type-generative? record-type-sealed?
          record-type-opaque? record-type-field-names
          record-field-mutable? record-uid->rtd port-write-rtd
          port-read-rtd)
  (import (rnrs base)
          (rnrs syntax-case)
          (rnrs lists)
          (rnrs control)
          (rnrs hashtables)
          (rnrs arithmetic fixnums)
          (only (rnrs records syntactic)
                fields mutable immutable parent protocol sealed opaque
                nongenerative parent-rtd)
          (prefix (rnrs) rnrs:)
          (srfi 237 records ports))
  (include "../reference/srfi-237/records-body.scm"))
