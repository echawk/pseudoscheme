;;; Scheme using Common Lisp libraries: Alexandria and CL-PPCRE.
;;;
;;;   bin/pseudoscheme --quicklisp examples/scheme-uses-lisp-libraries.scm
;;;
;;; Importing (cl <package>) for a package that isn't loaded loads the
;;; system of the same name first: with Quicklisp's QUICKLOAD when
;;; Quicklisp is loaded (--quicklisp; it fetches what it doesn't have),
;;; otherwise with ASDF:LOAD-SYSTEM, which finds systems on ASDF's source
;;; registry (~/common-lisp, an ocicl or qlot project, CL_SOURCE_REGISTRY).
;;; For distributed code, list the Lisp systems in an ASDF system's
;;; :depends-on instead (examples/mixed-system/).

(import (scheme base) (scheme write)
        (prefix (cl alexandria) alex:)
        (prefix (cl cl-ppcre) re:))

(define (show label value)
  (display label) (display ": ") (write value) (newline))

;; Alexandria
(show "flatten" (alex:flatten '(1 (2 (3 4)) 5)))
(show "iota" (alex:iota 5 #:start 1))
(show "shuffle keeps length" (length (alex:shuffle (list 1 2 3 4 5))))
(show "emptyp" (list (alex:emptyp '()) (alex:emptyp "x")))     ; predicates: #t/#f
(show "compose" ((alex:compose (lambda (x) (* x 2)) (lambda (x) (+ x 1))) 5))
(show "hash-table-alist"
      (let ((h (alex:alist-hash-table '((a . 1) (b . 2)))))
        (length (alex:hash-table-alist h))))

;; CL-PPCRE: regular expressions on Scheme strings
(show "split" (re:split "\\s*,\\s*" "a , b,c ,  d"))
(show "all-matches-as-strings" (re:all-matches-as-strings "[0-9]+" "a1b22c333"))
(show "regex-replace-all" (re:regex-replace-all "o" "foo boo" "0"))
(call-with-values (lambda () (re:scan-to-strings "(\\w+)@(\\w+)" "mail: ann@example"))
  (lambda (match groups) (show "scan-to-strings" (list match groups))))
