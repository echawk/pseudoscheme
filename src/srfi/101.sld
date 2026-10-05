;;; SRFI 101: purely functional random-access pairs and lists.  David
;;; Van Horn's R6RS reference implementation (101/random-access-lists.sls,
;;; MIT licence in its header), with only the library name changed, from
;;; (srfi :101) to (srfi :101 random-access-lists), so that this
;;; library can re-export it.
;;;
;;; The names are those of the base list procedures (quote, pair?, cons,
;;; car, list, map, ...), as the SRFI specifies, so this library is
;;; meant to be imported with a prefix, or in place of those bindings:
;;; (import (except (scheme base) quote pair? cons ...) (srfi 101)).
;;; quote makes random-access lists of quoted list data.
(define-library (srfi 101)
  (export quote pair? cons car cdr
          caar cadr cddr cdar
          caaar caadr caddr cadar cdaar cdadr cdddr cddar
          caaaar caaadr caaddr caadar cadaar cadadr cadddr caddar
          cdaaar cdaadr cdaddr cdadar cddaar cddadr cddddr cdddar
          null? list? list make-list length append reverse
          list-tail list-ref list-set list-ref/update map for-each
          random-access-list->linear-access-list
          linear-access-list->random-access-list)
  (import (srfi 101 random-access-lists)))
