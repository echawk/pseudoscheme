;;; SRFI 109: Extended string quasi-literals.  Written for Pseudoscheme.
;;; The reader reads &{text} (src/quasi.lisp) as ($string$ part ...),
;;; the parts strings, entity references ($entity$:name, for an entity
;;; it doesn't know) and enclosed expressions between $<<$ and $>>$.
;;; $string$ concatenates them, displaying any that isn't a string; $<<$
;;; and $>>$ are "", so that a constructor can be a plain procedure (SRFI
;;; 108).  Format specifiers (&~...) are not supported.
(define-library (srfi 109)
  (export $string$ $<<$ $>>$)
  (import (scheme base) (scheme write))
  (begin
    (define $<<$ "")
    (define $>>$ "")
    (define ($string$ . parts)
      (apply string-append
             (map (lambda (x)
                    (if (string? x)
                        x
                        (let ((port (open-output-string)))
                          (display x port)
                          (get-output-string port))))
                  parts)))))
