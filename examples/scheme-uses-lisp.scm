;;; Scheme using Common Lisp: an R7RS program that calls into CL.
;;;
;;;   bin/pseudoscheme examples/scheme-uses-lisp.scm
;;;   (r7rs:load "examples/scheme-uses-lisp.scm")        ; from Lisp
;;;
;;; A Lisp package is a library, (cl <package>): its functions and
;;; variables, under their Lisp names in Scheme spelling (REMOVE-IF is
;;; remove-if).  Prefixing the import keeps them apart from Scheme's own
;;; names and reads like Lisp: cl:remove-if.

(import (scheme base) (scheme write) (scheme char)
        (prefix (cl common-lisp) cl:)
        (pseudoscheme lisp))

(define (show label value)
  (display label) (display ": ") (write value) (newline))

;; Lists, strings, numbers, vectors and procedures are shared as they
;; are, so CL's sequence functions work on Scheme data directly.
(show "sort" (cl:sort (list 5 3 9 1) cl:<))
(show "remove-duplicates" (cl:remove-duplicates '(a b a c b)))
(show "string-upcase" (cl:string-upcase "hello, lisp"))
(show "reduce" (cl:reduce + (vector 1 2 3 4)))

;; Keyword arguments: #:key reads as the Lisp keyword :KEY.
(show "sort by cdr" (cl:sort (list '(a . 3) '(b . 1) '(c . 2)) cl:< #:key cl:cdr))
(show "position from end" (cl:position #\a "banana" #:from-end #t))

;; Booleans are converted at the boundary.  Lisp predicates return #f
;; for NIL (otherwise NIL would be (), which is true in Scheme), and a
;; Scheme procedure given to Lisp has its #f results passed as NIL.
(show "evenp" (list (cl:evenp 2) (cl:evenp 3)))
(show "remove-if with a Scheme predicate"
      (cl:remove-if (lambda (n) (> n 2)) '(1 2 3 4)))
(show "member (NIL -> #f)" (cl:member 9 '(1 2 3)))

;; Multiple values are multiple values.
(call-with-values (lambda () (cl:floor 17 5))
  (lambda (q r) (show "floor 17 5" (list q r))))

;; format, with #f for "return a string" (Lisp's NIL).
(show "format" (cl:format #f "~r is ~:r in ~a" 4 4 "Lisp"))

;; Hash tables: cl:equal reaches Lisp as #'EQUAL itself (a wrapper
;; crossing back is unwrapped), so it's a valid :test.  Lisp's SETF is
;; lisp-set!, from (pseudoscheme lisp).
(define table (cl:make-hash-table #:test cl:equal))
(lisp-set! (cl:gethash "one" table) 1)
(lisp-set! (cl:gethash "two" table) 2)
(show "gethash" (cl:gethash "two" table))
(show "hash-table-count" (cl:hash-table-count table))

;; Lisp special variables are variables: read them, set! them, and bind
;; them dynamically with lisp-let.
(show "*print-base*" cl:*print-base*)
(lisp-let ((cl:*print-base* 2))
  (show "255 in binary" (cl:princ-to-string 255)))
(show "most-positive-fixnum" cl:most-positive-fixnum)

;; Strings are Lisp strings, so Lisp's string functions apply.
(show "search" (cl:search "lisp" "scheme and lisp"))
(show "parse-integer" (cl:parse-integer "ff" #:radix 16))

;; Lisp errors are Scheme conditions.
(show "caught"
      (guard (e ((error-object? e) (error-object-message e)))
        (cl:parse-integer "not a number")))

;; Lisp macros and special operators are imported too.  Inside a Lisp
;; macro call the code is Lisp, with Scheme's variables and procedures
;; visible: here LOOP's FOR, IN, WHEN and COLLECT are Lisp, while
;; numbers, even? and square are Scheme's.  The call is compiled once,
;; as a Lisp function.
(define numbers '(1 2 3 4 5 6 7 8))
(define (square x) (* x x))
(show "loop" (cl:loop for n in numbers when (even? n) collect (square n)))
(show "loop sum" (cl:loop for i from 1 to 100 sum i))
(show "destructuring-bind"
      (cl:destructuring-bind (a (b c) &key (d 0)) '(1 (2 3) #:d 4) (list a b c d)))
(show "handler-case"
      (cl:handler-case (cl:parse-integer "12x")
        (cl:parse-error () 'not-a-number)))
(show "with-output-to-string"
      (cl:with-output-to-string (out)
        (cl:dolist (word '("lisp" "and" "scheme"))
          (cl:format out "~@(~a~) " word))))

;; CLOS from Scheme: define a class and a generic function with Lisp's
;; macros.  The names they define (circle, radius, area) are the Scheme
;; symbols of those names, so 'circle names the class, and
;; (lisp-function 'area) is the generic function.  (lisp ...) is Lisp
;; code in Scheme, like a Lisp macro call: here (radius c) is Lisp's
;; RADIUS, square Scheme's procedure.
(cl:defclass circle () ((radius #:initarg #:radius #:reader radius)))
(cl:defgeneric area (shape))
(cl:defmethod area ((c circle)) (* 3 (square (radius c))))
(define area (lisp-function 'area))
(show "CLOS" (area (cl:make-instance 'circle #:radius 2)))
(show "lisp form" (lisp (area (make-instance 'circle :radius 3))))

;; Type and class names are symbols: (cl:typep x cl:integer) needs no quote.
(show "typep" (list (cl:typep 42 cl:integer) (cl:typep "42" cl:integer)))

;; Escape hatches: a Lisp function by name, unconverted; Lisp source.
(show "lisp-function" ((lisp-function "string-capitalize") "hello world"))
(show "lisp-eval-string" (lisp-eval-string "(loop for i below 5 collect (* i i))"))
