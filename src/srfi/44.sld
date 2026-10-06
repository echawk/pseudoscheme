;;; SRFI 44: collections.  Antero Mejr's R7RS implementation, from the
;;; SRFI repository's contrib/antero-mejr (lists and records with
;;; inheritance), not Scott G. Miller's original Tiny CLOS one.  Its
;;; definitions are reference/srfi-44/44-body.scm, the body of the
;;; contributed srfi/44.sld, verbatim (MIT licence in
;;; reference/srfi-44/LICENSE); this library form is that file's, with
;;; these changes:
;;; - It imported (srfi 256) for define-record-type with subtypes, which
;;;   Pseudoscheme lacks; (srfi private srfi-44-records) provides that
;;;   define-record-type, on R6RS records.
;;; - Its export list named map->list twice, and exported
;;;   sequence-fold-keys-left as flexible-sequence-fold-keys-right; the
;;;   duplicate is dropped and flexible-sequence-fold-keys-right is
;;;   sequence-fold-keys-right.
;;; See the notes at the top of 44-body.scm for how the implementation
;;; reads the (incomplete) specification.
;;;
;;; The library exports its own list, vector, string and map types,
;;; whose names (list, list?, make-list, list-ref, vector, vector-ref,
;;; string, string-ref, map, ...) clash with (scheme base): import it
;;; with a prefix, or exclude those names from (scheme base).

(define-library (srfi 44)
  (import (except (scheme base) define-record-type vector? make-vector vector
                  list? make-list list string? make-string string map
                  vector-ref string-ref list-ref string->list string-copy
                  vector->list vector-copy vector-set!)
          (prefix (only (scheme base) map list list-ref) r7rs:)
          (scheme case-lambda)
          (except (srfi 1) map)
          (srfi 8)
          (srfi private srfi-44-records))
  (export alist-map?
          alist-map
          ;; TODO
          alist-map-delete-all
          alist-map-delete-all!
          alist-map-delete-all-from
          alist-map-delete-all-from!
          alist-map-get-all
          alist-map-key-count
          alist-map-replace-all
          alist-map-replace-all!
          alist-map-update-all
          alist-map-update-all!
          make-alist-map
          (rename collection-empty? alist-map-empty?)
          (rename collection->list alist-map->list)
          (rename collection-clear alist-map-clear)
          (rename collection-clear! alist-map-clear!)
          (rename collection-copy alist-map-copy)
          (rename collection-fold-left alist-map-fold-left)
          (rename collection-fold-right alist-map-fold-right)
          (rename collection-get-any alist-map-get-any)
          (rename collection-size alist-map-size)
          (rename collection= alist-map=)
          (rename map-add-from alist-map-add-from)
          (rename map-add-from! alist-map-add-from!)
          (rename map-contains-key? alist-map-contains-key?)
          (rename map-count alist-map-count)
          (rename map-delete alist-map-delete)
          (rename map-delete! alist-map-delete!)
          (rename map-delete-from alist-map-delete-from)
          (rename map-delete-from! alist-map-delete-from!)
          (rename map-equivalence-function alist-map-equivalence-function)
          (rename map-fold-keys-left alist-map-fold-keys-left)
          (rename map-fold-keys-right alist-map-fold-keys-right)
          (rename map-get alist-map-get)
          (rename map-key-equivalence-function
                  alist-map-key-equivalence-function)
          (rename map-keys->list alist-map-keys->list)
          (rename map-put alist-map-put)
          (rename map-put! alist-map-put!)
          (rename map-update alist-map-update)
          (rename map-update! alist-map-update!)

          bag
          bag-add
          bag-add!
          bag-add-from
          bag-add-from!
          bag-contains?
          bag-count
          bag-delete
          bag-delete!
          bag-delete-all
          bag-delete-all!
          bag-delete-all-from
          bag-delete-all-from!
          bag-delete-from bag-delete-from!
          bag-equivalence-function
          bag?
          make-bag
          (rename collection->list bag->list)
          (rename collection-clear bag-clear)
          (rename collection-clear! bag-clear!)
          (rename collection-copy bag-copy)
          (rename collection-empty? bag-empty?)
          (rename collection-fold-left bag-fold-left)
          (rename collection-fold-right bag-fold-right)
          (rename collection-get-any bag-get-any)
          (rename collection-size bag-size)
          (rename collection= bag=)

          collection
          collection->list
          collection-clear
          collection-clear!
          collection-copy
          collection-empty?
          collection-fold-left
          collection-fold-right
          collection-get-any
          collection-name
          collection-size
          collection=
          collection?
          make-collection
          (rename bag-count collection-count)
          (rename directional-collection-insert-left! collection-insert-left!)
          (rename directional-collection-delete-left collection-delete-left)
          (rename directional-collection-delete-left! collection-delete-left!)
          (rename directional-collection-delete-right collection-delete-right)
          (rename directional-collection-delete-right! collection-delete-right!)
          (rename directional-collection-get-left collection-get-left)
          (rename directional-collection-get-right collection-get-right)
          (rename directional-collection-insert-left collection-insert-left)
          (rename ordered-collection-ordering-function
                  collection-ordering-function)
          (rename sequence-fold-keys-left collection-fold-keys-left)
          (rename sequence-fold-keys-right collection-fold-keys-right)

          directional-collection?
          directional-collection-get-left
          directional-collection-get-right
          directional-collection-insert-left
          directional-collection-insert-right
          directional-collection-insert-left!
          directional-collection-insert-right!
          directional-collection-delete-left
          directional-collection-delete-right
          directional-collection-delete-left!
          directional-collection-delete-right!

          flexible-sequence
          flexible-sequence-delete-at
          flexible-sequence-delete-at!
          flexible-sequence-insert
          flexible-sequence-insert!
          flexible-sequence?
          make-flexible-sequence
          (rename bag-count flexible-sequence-count)
          (rename collection->list flexible-sequence->list)
          (rename collection-clear flexible-sequence-clear)
          (rename collection-clear! flexible-sequence-clear!)
          (rename collection-copy flexible-sequence-copy)
          (rename collection-empty? flexible-sequence-empty?)
          (rename collection-fold-left flexible-sequence-fold-left)
          (rename collection-fold-right flexible-sequence-fold-right)
          (rename collection-get-any flexible-sequence-get-any)
          (rename collection-size flexible-sequence-size)
          (rename collection= flexible-sequence=)
          (rename directional-collection-delete-left
                  flexible-sequence-delete-left)
          (rename directional-collection-delete-left!
                  flexible-sequence-delete-left!)
          (rename directional-collection-delete-right
                  flexible-sequence-delete-right)
          (rename directional-collection-delete-right!
                  flexible-sequence-delete-right!)
          (rename directional-collection-insert-left
                  flexible-sequence-insert-left)
          (rename directional-collection-insert-left!
                  flexible-sequence-insert-left!)
          (rename sequence-fold-keys-left flexible-sequence-fold-keys-left)
          (rename sequence-fold-keys-right flexible-sequence-fold-keys-right)
          (rename sequence-insert-right flexible-sequence-insert-right)
          (rename sequence-insert-right! flexible-sequence-insert-right!)

          limited-collection?

          list?
          make-list
          list
          (rename bag-add list-add)
          (rename bag-add! list-add!)
          (rename bag-add-from list-add-from)
          (rename bag-add-from! list-add-from!)
          (rename bag-contains? list-contains?)
          (rename bag-count list-count)
          (rename bag-delete list-delete)
          (rename bag-delete! list-delete!)
          (rename bag-delete-all list-delete-all)
          (rename bag-delete-all! list-delete-all!)
          (rename bag-delete-all-from list-delete-all-from)
          (rename bag-delete-all-from! list-delete-all-from!)
          (rename bag-delete-from list-delete-from)
          (rename bag-delete-from! list-delete-from!)
          (rename bag-equivalence-function list-equivalence-function)
          (rename collection->list list->list)
          (rename collection-clear list-clear)
          (rename collection-clear! list-clear!)
          (rename collection-copy list-copy)
          (rename collection-empty? list-empty?)
          (rename collection-fold-left list-fold-left)
          (rename collection-fold-right list-fold-right)
          (rename collection-get-any list-get-any)
          (rename collection-size list-size)
          (rename collection= list=)
          (rename directional-collection-delete-left list-delete-left)
          (rename directional-collection-delete-left! list-delete-left!)
          (rename directional-collection-delete-right list-delete-right)
          (rename directional-collection-delete-right! list-delete-right!)
          (rename directional-collection-get-left list-get-left)
          (rename directional-collection-get-right list-get-right)
          (rename directional-collection-insert-left list-insert-left)
          (rename directional-collection-insert-left! list-insert-left!)
          (rename flexible-sequence-delete-at list-delete-at)
          (rename flexible-sequence-delete-at! list-delete-at!)
          (rename flexible-sequence-insert list-insert)
          (rename flexible-sequence-insert! list-insert!)
          (rename sequence-insert-right list-insert-right)
          (rename sequence-insert-right! list-insert-right!)
          (rename sequence-ref list-ref)
          (rename sequence-replace-from list-replace-from)
          (rename sequence-replace-from! list-replace-from!)
          (rename sequence-set list-set)
          (rename sequence-set! list-set!)

          map?
          map
          make-map
          map-add-from
          map-add-from!
          map-contains-key?
          map-count
          map-delete
          map-delete!
          map-delete-from
          map-delete-from!
          map-equivalence-function
          map-fold-keys-left
          map-fold-keys-right
          map-get
          map-key-equivalence-function
          map-keys->list
          map-put
          map-put!
          map-update
          map-update!
          (rename collection->list map->list)
          (rename collection-clear! map-clear!)
          (rename collection-copy map-copy)
          (rename collection-empty? map-empty?)
          (rename collection-fold-left map-fold-left)
          (rename collection-fold-right map-fold-right)
          (rename collection-get-any map-get-any)
          (rename collection-size map-size)
          (rename collection= map=)
          (rename collection-clear map-clear)

          ordered-collection?
          make-ordered-collection
          ordered-collection-ordering-function
          ordered-collection-get-left
          ordered-collection-get-right
          ordered-collection-delete-left
          ordered-collection-delete-left!
          ordered-collection-delete-right
          ordered-collection-delete-right!

          purely-mutable-collection?

          sequence?
          make-sequence
          sequence
          sequence-fold-keys-left
          sequence-fold-keys-right
          sequence-insert-right
          sequence-insert-right!
          sequence-ref
          sequence-replace-from
          sequence-replace-from!
          sequence-set
          sequence-set!
          (rename bag-add sequence-add)
          (rename bag-add! sequence-add!)
          (rename bag-count sequence-count)
          (rename collection->list sequence->list)
          (rename collection-clear sequence-clear)
          (rename collection-clear! sequence-clear!)
          (rename collection-copy sequence-copy)
          (rename collection-empty? sequence-empty?)
          (rename collection-fold-left sequence-fold-left)
          (rename collection-fold-right sequence-fold-right)
          (rename collection-get-any sequence-get-any)
          (rename collection-size sequence-size)
          (rename collection= sequence=)
          (rename directional-collection-get-left sequence-get-left)
          (rename directional-collection-get-right sequence-get-right)

          make-set
          set
          set-add
          set-add!
          set-add-from
          set-add-from!
          set-contains?
          set-count
          set-delete
          set-delete!
          set-delete-from
          set-delete-from!
          set-difference
          set-difference!
          set-equivalence-function
          set-intersection
          set-intersection!
          set-subset?
          set-symmetric-difference
          set-symmetric-difference!
          set-union
          set-union!
          set?
          (rename collection->list set->list)
          (rename collection-clear set-clear)
          (rename collection-clear! set-clear!)
          (rename collection-copy set-copy)
          (rename collection-empty? set-empty?)
          (rename collection-fold-left set-fold-left)
          (rename collection-fold-right set-fold-right)
          (rename collection-get-any set-get-any)
          (rename collection-size set-size)
          (rename collection= set=)
          ;; Won't work but are still exported for some reason.
          (rename sequence-fold-keys-left set-fold-keys-left)
          (rename sequence-fold-keys-right set-fold-keys-right)

          string?
          make-string
          string
          (rename bag-contains? string-contains?)
          (rename bag-count string-count)
          (rename bag-equivalence-function string-equivalence-function)
          (rename collection->list string->list)
          (rename collection-copy string-copy)
          (rename collection-empty? string-empty?)
          (rename collection-fold-left string-fold-left)
          (rename collection-fold-right string-fold-right)
          (rename collection-get-any string-get-any)
          (rename collection-size string-size)
          (rename collection= string=)
          (rename directional-collection-get-left string-get-left)
          (rename directional-collection-get-right string-get-right)
          (rename sequence-ref string-ref)
          (rename sequence-replace-from string-replace-from)
          (rename sequence-replace-from! string-replace-from!)
          (rename sequence-set string-set)
          (rename sequence-set! string-set!)

          make-vector
          vector
          vector?
          (rename bag-contains? vector-contains?)
          (rename bag-count vector-count)
          (rename bag-equivalence-function vector-equivalence-function)
          (rename collection->list vector->list)
          (rename collection-copy vector-copy)
          (rename collection-empty? vector-empty?)
          (rename collection-fold-left vector-fold-left)
          (rename collection-fold-right vector-fold-right)
          (rename collection-get-any vector-get-any)
          (rename collection-size vector-size)
          (rename collection= vector=)
          (rename directional-collection-get-left vector-get-left)
          (rename directional-collection-get-right vector-get-right)
          (rename sequence-ref vector-ref)
          (rename sequence-replace-from vector-replace-from)
          (rename sequence-replace-from! vector-replace-from!)
          (rename sequence-set vector-set)
          (rename sequence-set! vector-set!)
     )
  (include "reference/srfi-44/44-body.scm"))
