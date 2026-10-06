;;; SRFI 163: Enhanced array literals.  Written for Pseudoscheme.  The
;;; reader reads #2a((1 2) (3 4)), #2u32@2@3((1 2) (2 3)), #0a sym and
;;; SRFI 58's #2A:fixZ32b(...) (src/read.scm), and makes the array with
;;; array-literal, which it takes from this library the first time.
;;;
;;; The arrays are SRFI 164's.  A uniform tag (u8, f64, ..., or SRFI 58's
;;; type) is checked to be one but not kept: the elements are stored in a
;;; vector, as for a.  write doesn't print arrays as literals;
;;; format-array writes the literal (without box drawing, which the SRFI
;;; suggests but doesn't require).
(define-library (srfi 163)
  (export array-literal format-array)
  (import (scheme base) (scheme write) (scheme case-lambda)
          (only (srfi 164) array array? array-rank array-start array-end array-ref))
  (begin
    (define uniform-tags
      '("u8" "s8" "u16" "s16" "u32" "s32" "u64" "s64" "f32" "f64" "c64" "c128"))

    (define (valid-tag? tag)
      (or (string=? tag "a") (string=? tag "A")
          (member tag uniform-tags)
          ;; SRFI 58's A:type
          (and (> (string-length tag) 2) (string=? (substring tag 0 2) "A:"))
          (and (> (string-length tag) 2) (string=? (substring tag 0 2) "a:"))))

    ;; The lengths of the first RANK levels of the nested lists DATUM.
    (define (datum-lengths rank datum)
      (if (= rank 0)
          '()
          (begin
            (unless (list? datum) (error "array literal: not a list" datum))
            (cons (length datum)
                  (if (null? datum)
                      (make-list (- rank 1) 0)
                      (datum-lengths (- rank 1) (car datum)))))))

    ;; The elements of DATUM, RANK levels deep, in order, checked against
    ;; LENGTHS.
    (define (elements rank lengths datum)
      (if (= rank 0)
          (list datum)
          (begin
            (unless (and (list? datum) (= (length datum) (car lengths)))
              (error "array literal: wrong shape" datum))
            (apply append
                   (map (lambda (sub) (elements (- rank 1) (cdr lengths) sub)) datum)))))

    (define (array-literal rank tag bounds datum)
      (unless (valid-tag? tag) (error "array literal: unknown tag" tag))
      (unless (or (null? bounds) (= (length bounds) rank))
        (error "array literal: there must be as many bounds as dimensions" bounds))
      (let* ((inferred (datum-lengths rank datum))
             (lowers (if (null? bounds) (make-list rank 0) (map car bounds)))
             (lengths (if (null? bounds)
                          inferred
                          (map (lambda (b n) (or (cdr b) n)) bounds inferred))))
        (apply array
               (list->vector (map (lambda (lo n) (list lo (+ lo n))) lowers lengths))
               (elements rank lengths datum))))

    ;; The literal for array A, as SRFI 163's write would make it.
    (define (write-array a port)
      (let* ((rank (array-rank a))
             (lowers (let loop ((k 0)) (if (= k rank) '() (cons (array-start a k) (loop (+ k 1))))))
             (uppers (let loop ((k 0)) (if (= k rank) '() (cons (array-end a k) (loop (+ k 1))))))
             (lengths (map - uppers lowers)))
        (write-string "#" port)
        (write rank port)
        (write-string "a" port)
        (when (or (exists-nonzero? lowers) (memv 0 lengths))
          (for-each (lambda (lo n)
                      (unless (zero? lo) (write-string "@" port) (write lo port))
                      (write-string ":" port)
                      (write n port))
                    lowers lengths))
        (if (= rank 0)
            (begin (write-string " " port) (write (array-ref a) port))
            (let level ((k 0) (index '()))
              (if (= k rank)
                  (write (apply array-ref a (reverse index)) port)
                  (begin
                    (write-string "(" port)
                    (let loop ((i (list-ref lowers k)) (first #t))
                      (when (< i (list-ref uppers k))
                        (unless first (write-string " " port))
                        (level (+ k 1) (cons i index))
                        (loop (+ i 1) #f)))
                    (write-string ")" port)))))))

    (define (exists-nonzero? xs)
      (and (pair? xs) (or (not (zero? (car xs))) (exists-nonzero? (cdr xs)))))

    (define format-array
      (case-lambda
        ((value) (format-array value #f))
        ((value port)
         (cond ((not port)
                (let ((p (open-output-string)))
                  (format-array value p)
                  (get-output-string p)))
               ((eq? port #t) (format-array value (current-output-port)))
               ((array? value) (write-array value port))
               (else (write value port))))))))
