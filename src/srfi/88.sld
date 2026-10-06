;;; SRFI 88: Keyword objects.  Written for Pseudoscheme.
;;;
;;; foo: reads as a keyword after the directive #!srfi-88 (on that port,
;;; until #!no-srfi-88), since a name ending in a colon is a symbol in
;;; R7RS, and Pseudoscheme's code uses some (the prefix in
;;; (prefix (cl common-lisp) cl:), for one).  Keywords are their own
;;; type, interned by name and self-evaluating: not symbols, nor the Lisp
;;; keywords that #:name reads as (docs/interop.md).  write writes foo:
;;; as foo:.
(define-library (srfi 88)
  (export keyword? keyword->string string->keyword)
  (import (scheme base) (only (pseudoscheme lisp) lisp-eval-string))
  (begin
    (define keyword?
      (lisp-eval-string "(lambda (x) (if (ps:keyword-object-p x) t ps:false))"))
    (define %name (lisp-eval-string "(lambda (k) (copy-seq (ps:keyword-object-name k)))"))
    (define %intern (lisp-eval-string "(lambda (s) (ps:intern-keyword-object s))"))
    (define (keyword->string keyword)
      (unless (keyword? keyword) (error "keyword->string: not a keyword" keyword))
      (%name keyword))
    (define (string->keyword string)
      (unless (string? string) (error "string->keyword: not a string" string))
      (%intern string))))
