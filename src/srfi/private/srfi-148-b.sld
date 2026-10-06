;;; (srfi private srfi-148-b): part of SRFI 148's sample implementation
;;; (see 148.sld): the sections List processing, Filtering, Searching, Association
;;; lists and Set operations.  It exports everything
;;; it defines, for the other parts and (srfi 148).
(define-library (srfi private srfi-148-b)
  (export em-caar em-cadr em-cdar em-cddr em-first em-second em-third
          em-fourth em-fifth em-sixth em-seventh em-eighth em-ninth em-tenth
          em-make-list em-reverse em-list-tail em-drop em-list-ref em-take
          em-take-right em-drop-right em-last em-last-pair em-filter
          em-remove em-find em-find-tail em-take-while em-drop-while em-any
          em-every em-member em-assoc em-alist-delete em-set<= em-set=
          em-set-adjoin em-set-union em-set-intersection em-set-difference
          em-set-xor)
  (import (except (scheme base) define-syntax let-syntax letrec-syntax syntax-rules)
          (srfi 147)
          (srfi 26)
          (srfi private srfi-148-a))
  (include "../reference/srfi-148/148.macros.2.scm"
           "../reference/srfi-148/148.macros.4.scm"))
