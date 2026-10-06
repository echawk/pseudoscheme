;;; (srfi private srfi-89-keywords): keyword objects for (srfi 89),
;;; needed both when lambda* expands and when its expansion runs.
;;;
;;; Pseudoscheme has no SRFI 88, so its keyword objects are the Lisp
;;; keywords that #:name reads as (self-evaluating, and distinct from
;;; every Scheme symbol; docs/interop.md 3.3).  In a lambda* parameter
;;; list a keyword may be written #:name or, as in SRFI 88 and the
;;; SRFI 89 document, name: -- both mean the keyword #:name.  Arguments
;;; are passed with #:name, since name: reads as a symbol and would be
;;; evaluated as a variable.
(define-library (srfi private srfi-89-keywords)
  (export keyword? keyword->string syntax-keyword? syntax-keyword
          make-perfect-hash-table)
  (import (scheme base)
          (pseudoscheme lisp))
  (begin
    (define %keywordp (lisp-function "keywordp" "cl"))

    (define (keyword? x)
      (lisp-true? (lisp-funcall %keywordp x)))

    (define (keyword->string k)
      (symbol->string k))

    ;; Is the datum D, from a parameter list, a keyword: #:name, or a
    ;; symbol name: (with a nonempty name)?
    (define (colon-symbol? d)
      (and (symbol? d)
           (not (keyword? d))
           (let ((s (symbol->string d)))
             (and (> (string-length s) 1)
                  (char=? (string-ref s (- (string-length s) 1)) #\:)))))

    (define (syntax-keyword? d)
      (or (keyword? d) (colon-symbol? d)))

    ;; The keyword object for the datum D (syntax-keyword? is true of it).
    (define (syntax-keyword d)
      (if (keyword? d)
          d
          (let ((s (symbol->string d)))
            (lisp-keyword (substring s 0 (- (string-length s) 1))))))

    ;; From the expansion-time part of Marc Feeley's implementation
    ;; (reference/srfi-89/srfi-89.scm), unmodified.
    (define (make-perfect-hash-table alist)

      ; "alist" is a list of pairs of the form "(keyword . value)"

      ; The result is a perfect hash-table represented as a vector of
      ; length 2*N, where N is the hash modulus.  If the keyword K is in
      ; the hash-table it is at index
      ;
      ;   X = (* 2 ($hash-keyword K N))
      ;
      ; and the associated value is at index X+1.

      (let loop1 ((n (length alist)))
        (let ((v (make-vector (* 2 n) #f)))
          (let loop2 ((lst alist))
            (if (pair? lst)
                (let* ((key-val (car lst))
                       (key (car key-val)))
                  (let ((x (* 2 ($hash-keyword key n))))
                    (if (vector-ref v x)
                        (loop1 (+ n 1))
                        (begin
                          (vector-set! v x key)
                          (vector-set! v (+ x 1) (cdr key-val))
                          (loop2 (cdr lst))))))
                v)))))

    (define ($hash-keyword key n)
      (let ((str (keyword->string key)))
        (let loop ((h 0) (i 0))
          (if (< i (string-length str))
              (loop (modulo (+ (* h 65536) (char->integer (string-ref str i)))
                            n)
                    (+ i 1))
              h))))))
