;;; SRFI 250 tests: the SRFI's test-srfi-250.scm and test-on-r7rs.scm
;;; (Daphne Preston-Kendal; MIT licence), with (chibi test) on SRFI 64
;;; (its (test-equal eq? ...) is chibi-test-equal here).  The stress
;;; tests run O(size^2) checks (millions at the upstream sizes, far past
;;; the 60-second budget), so their sizes are cut, each marked "was".
(import (scheme base) (scheme char) (scheme write) (scheme process-context)
        (srfi 64)
        (only (srfi 1) list-tabulate)
        (srfi 27)
        (srfi 250)
        (rename (srfi 128) (default-hash equal-hash)))

;; (chibi test) on SRFI 64: test, test-not, and a settable
;; current-test-comparator.  (test-assert, test-error and test-group
;; are SRFI 64's own.)
(define %test-comparator equal?)
(define (current-test-comparator . new)
  (if (null? new) %test-comparator (set! %test-comparator (car new))))
(define-syntax test
  (syntax-rules ()
    ((_ name expected expr)
     (test-assert name (%test-comparator expected expr)))
    ((_ expected expr)
     (test-assert (%test-comparator expected expr)))))
(define-syntax test-not
  (syntax-rules ()
    ((_ name expr) (test-assert name (not expr)))
    ((_ expr) (test-assert (not expr)))))
;; chibi's (test-equal equal expected expr), renamed: SRFI 64's
;; test-equal takes a name, not an equality.
(define-syntax chibi-test-equal
  (syntax-rules ()
    ((_ equal name expected expr) (test-assert name (equal expected expr)))
    ((_ equal expected expr) (test-assert (equal expected expr)))))
(define-syntax test-values
  (syntax-rules ()
    ((_ name expected expr)
     (test-assert name (%test-comparator
                        (call-with-values (lambda () expected) list)
                        (call-with-values (lambda () expr) list))))
    ((_ expected expr)
     (test-values #f expected expr))))

(define-syntax assert
  (syntax-rules ()
    ((_ what) (unless what (error "assertion failed")))))
(define (assertion-violation who msg . rest)
  (apply error msg rest))
(define assertion-violation? error-object?)

(test-begin "srfi-250")

(define exact-integer-comparator
  (make-comparator (lambda (x) (and (integer? x) (exact? x)))
                   =
                   #f
                   equal-hash))
(define bad-exact-integer-comparator
  (make-comparator (lambda (x) (and (integer? x) (exact? x)))
                   =
                   #f
                   (lambda (x) (+ 42 (modulo x 2)))))

(define symbol-comparator
  (make-comparator symbol? symbol=? #f symbol-hash))

(define char-comparator
  (make-comparator char? char=? char<?
                   (lambda (c) (equal-hash (char->integer c)))))

(define string-comparator
  (make-comparator string? string=? string<? equal-hash))

(define (vector-shuffle! vec src)
  (define rand (random-source-make-integers src))
  (let loop ((i (- (vector-length vec) 1)))
    (when (>= i 1)
      (let* ((j (rand i))
             (vi (vector-ref vec i))
             (vj (vector-ref vec j)))
        (vector-set! vec i vj)
        (vector-set! vec j vi)
        (loop (- i 1))))))

;; Basic growing a hash table entry-by-entry

(define size 500)  ; was 1000000

(define (test-adding-to ht)
  (let n-loop ((n 0))
    (when (<= n size)
      (test-assert (string-append "add " (number->string n))
        (begin
          (hash-table-add! ht n (number->string n))
          #t))
      (when (or (= n size)
                (< n 300))
        (test (string-append "check size is " (number->string (+ n 1)))
            (+ n 1)
          (hash-table-size ht))
        (let m-loop ((m 0))
          (when (<= m n)
            (test (string-append "retrieve "
                                 (number->string m)
                                 " with "
                                 (number->string n)
                                 " entries")
                (number->string m)
              (hash-table-ref ht m))
            (m-loop (+ m 1)))))
      (n-loop (+ n 1)))))

(test-group "Stress tests: building"
  (test-group (string-append "incremental building up to "
                             (number->string size)
                             " entries")
    (test-adding-to (make-hash-table exact-integer-comparator 0)))

  (test-group (string-append "adding with pre-determined capacity of "
                             (number->string size)
                             " entries")
    (test-adding-to (make-hash-table exact-integer-comparator size)))

  ;; Test with a bad hash function that always collides

  (set! size 100)  ; was 1000

  (test-group "incremental building with a terrible hash function"
    (test-adding-to (make-hash-table bad-exact-integer-comparator size))))

;; Test overwriting

(test-group "Stress tests: replacing"
  (set! size 100)  ; was 10000
  (test-group "replacing existing entries"
    (let ((ht (make-hash-table exact-integer-comparator size)))
      (let loop ((n 0))
        (when (< n size)
          (hash-table-add! ht n n)
          (loop (+ n 1))))
      (let loop ((n 0))
        (when (< n size)
          (test-assert
              (string-append "replacing entry for " (number->string n))
            (begin
              (hash-table-replace! ht n (* n 2))
              #t))
          (test "checking that replacement worked" (* n 2)
            (hash-table-ref ht n))
          (test "checking that size was not affected" size
            (hash-table-size ht))
          (loop (+ n 1)))))))

;; Test that add! doesn’t replace

(test-group "Stress tests: Replacing doesn’t add and vice-versa"
  (set! size 100)  ; was 10000
  (test-group "trying to add entries that already exist"
    (let ((ht (make-hash-table exact-integer-comparator size)))
      (let loop ((n 0))
        (when (< n size)
          (hash-table-add! ht n n)
          (loop (+ n 1))))
      (let loop ((n 0))
        (when (< n size)
          (test-error (string-append "adding a second entry for " (number->string n) " errors") assertion-violation?
            (hash-table-add! ht n (* n 2)))
          (test "checking that no replacement took place despite error" n
            (hash-table-ref ht n))
          (test "checking that size was not affected" size
            (hash-table-size ht))
          (loop (+ n 1))))))

  ;; Test that replace! doesn’t add

  (test-group "trying to replace entries that don’t exist"
    (let ((ht (make-hash-table exact-integer-comparator 0)))
      (let loop ((n 0))
        (when (< n size)
          (test-error (string-append "adding a second entry for " (number->string n) " errors") assertion-violation?
            (hash-table-replace! ht n n))
          (test "checking that no entry was added despite error" 'not-there
            (hash-table-ref/default ht n 'not-there))
          (test "checking that size was not affected" 0
            (hash-table-size ht))
          (loop (+ n 1)))))))

;; Deletion

(test-group "Stress tests: deletion"
  (set! size 100)  ; was 1000
  (test-group "deleting entries in insertion order"
    (let ((ht (make-hash-table exact-integer-comparator size)))
      (let loop ((n 0))
        (when (< n size)
          (hash-table-add! ht n n)
          (loop (+ n 1))))
      (let loop ((n 0))
        (when (< n size)
          (test (string-append "deleting entry for " (number->string n)) 1
            (hash-table-delete! ht n))
          (test "checking that entry is actually gone" 'not-there
            (hash-table-ref/default ht n 'not-there))
          (test "checking that size is correct" (- size n 1)
            (hash-table-size ht))
          (when (or (= n size)
                    (< n 300))
            (let m-loop ((m 0))
              (when (< m size)
                (test (string-append "retrieve "
                                     (number->string m))
                    (if (> m n) m 'not-there)
                  (hash-table-ref/default ht m 'not-there))
                (m-loop (+ m 1)))))
          (loop (+ n 1))))))

  ;; Deletion from the end
  (test-group "deleting entries in reverse insertion order"
    (let ((ht (make-hash-table exact-integer-comparator size)))
      (let loop ((n 0))
        (when (< n size)
          (hash-table-add! ht n n)
          (loop (+ n 1))))
      (let loop ((n (- size 1)))
        (unless (< n 0)
          (test (string-append "deleting entry for " (number->string n)) 1
            (hash-table-delete! ht n))
          (test "checking that entry is actually gone" 'not-there
            (hash-table-ref/default ht n 'not-there))
          (test "checking that size is correct" n
            (hash-table-size ht))
          (when (or (= n 0)
                    (>= n (- size 300)))
            (let m-loop ((m n))
              (when (>= m 0)
                (test (string-append "retrieve "
                                     (number->string m))
                    (if (< m n) m 'not-there)
                  (hash-table-ref/default ht m 'not-there))
                (m-loop (- m 1)))))
          (loop (- n 1))))))

  ;; Deletion in random order
  (test-group "deleting entries in (pseudo)random order"
    (let ((source (make-random-source))
          (ht (make-hash-table exact-integer-comparator size))
          (order (make-vector size)))
      (random-source-pseudo-randomize! source 1 2)
      (let loop ((n 0))
        (when (< n size)
          (hash-table-add! ht n n)
          (vector-set! order n n)
          (loop (+ n 1))))
      (vector-shuffle! order source)
      (let loop ((idx 0))
        (when (< idx (vector-length order))
          (let ((n (vector-ref order idx)))
            (test (string-append "deleting entry for " (number->string n)) 1
              (hash-table-delete! ht n))
            (test "checking that entry is actually gone" 'not-there
              (hash-table-ref/default ht n 'not-there))
            (test "checking that size is correct" (- size idx 1)
              (hash-table-size ht))
            (let m-loop ((m 0))
              (when (< m (vector-length order))
                (let ((test-n (vector-ref order m)))
                  (test (string-append "retrieve "
                                       (number->string test-n))
                      (if (> m idx) test-n 'not-there)
                    (hash-table-ref/default ht test-n 'not-there)))
                (m-loop (+ m 1)))))
          (loop (+ idx 1)))))))

;; Adding and deleting

(test-group "Stress tests: adding and deleting"
  (set! size 100)  ; was 1000
  (let steps-loop ((more-steps '((2 . 1) (3 . 1) (3 . 2) (4 . 1) (4 . 3))))
    (unless (null? more-steps)
      (let ((step (car more-steps)))
        (define add (car step))
        (define delete (cdr step))
        (test-group (string-append "Add " (number->string add) " then delete " (number->string delete))
          (let ((do-tests
                 (lambda (ht)
                   (let add-delete-loop ((n 0))
                     (when (< n size)
                       (let ((first-key (* n add)))
                         (let add-loop ((added 0))
                           (when (< added add)
                             (test-assert
                                 (string-append "Add " (number->string
                                                        (+ first-key added)))
                               (hash-table-add! ht
                                                (+ first-key added)
                                                (number->string (+ first-key added))))
                             (test "It’s there"
                                 (number->string (+ first-key added))
                               (hash-table-ref/default ht (+ first-key added) #f))
                             (test "Size after addition is correct" (+ (* n (- add delete)) added 1)
                               (hash-table-size ht))
                             (add-loop (+ added 1))))
                         (let delete-loop ((deleted 0))
                           (when (< deleted delete)
                             (test (string-append "Delete " (number->string
                                                             (+ first-key deleted))) 1
                               (hash-table-delete! ht (+ first-key deleted)))
                             (test "It’s gone" #f
                               (hash-table-ref/default ht (+ first-key deleted) #f))
                             (test "Size after deletion is correct" (+ (* n (- add delete)) add (- (+ deleted 1)))
                               (hash-table-size ht))
                             (delete-loop (+ deleted 1))))
                         (let check-ref-loop ((m 0))
                           (when (< m n)
                             (let check-loop ((key (* m add)))
                               (when (< key (* (+ m 1) add))
                                 (if (< key (+ (* m add) delete))
                                     (test (string-append (number->string key) " is not there") #f
                                       (hash-table-ref/default ht key #f))
                                     (test (string-append (number->string key) " is still there") (number->string key)
                                       (hash-table-ref/default ht key #f)))
                                 (check-loop (+ key 1))))
                             (check-ref-loop (+ m 1)))))
                       (add-delete-loop (+ n 1)))))))
            (test-group "With correct capacity"
              (do-tests (make-hash-table exact-integer-comparator
                                         (* size (+ (- add delete) 1)))))
            (test-group "Building incrementally"
              (do-tests (make-hash-table exact-integer-comparator 0))))))
      (steps-loop (cdr more-steps)))))

;; Some basic tests

(test-group "Avoid basic implementation errors"
  (test "Don’t confuse capacity with size" 0
    (hash-table-size (make-hash-table exact-integer-comparator 1000))))

;; Constructors

(test-group "Constructors"
  (test-group "Hash-table"
    (let ((ht #f))
      (test-assert "Makes a hash table"
        (begin
          (set! ht (hash-table symbol-comparator
                               'clubs #\x2663
                               'diamonds #\x2666
                               'hearts #\x2665
                               'spades #\x2660))
          (hash-table? ht)))
      (test-assert "Not empty" (not (hash-table-empty? ht)))
      (test "There are 4 entries" 4 (hash-table-size ht))
      (test "clubs" #\x2663 (hash-table-ref ht 'clubs))
      (test "diamonds" #\x2666 (hash-table-ref ht 'diamonds))
      (test "hearts" #\x2665 (hash-table-ref ht 'hearts))
      (test "spades" #\x2660 (hash-table-ref ht 'spades))
      (test-error "Nonexistent entry" assertion-violation?
        (hash-table-ref ht 'joker))
      (test-assert "The hash table is mutable"
        (begin
          (hash-table-add! ht 'joker #\x1F921)
          #t))
      (test "Mutation actually took place" #\x1F921
        (hash-table-ref ht 'joker))
      (test "Keys are in order"
          '(clubs diamonds hearts spades joker)
        (hash-table-map->list (lambda (k v) k) ht))
      (test "Values are in order"
          '(#\x2663 #\x2666 #\x2665 #\x2660 #\x1F921)
        (hash-table-map->list (lambda (k v) v) ht)))
    (test-error "Odd number of arguments" assertion-violation?
      (hash-table symbol-comparator 'a 1 'b)))

  (test-group "Hash-table-unfold"
    (let ((do-tests
           (lambda (maker)
             (let ((ht #f))
               (test-assert "Makes a hash table"
                 (begin
                   (set! ht (maker))
                   (hash-table? ht)))
               (test-assert "Not empty" (not (hash-table-empty? ht)))
               (test "There are 26 entries" 26 (hash-table-size ht))
               (test-assert "Keys are in order"
                 (apply char<? (hash-table-map->list (lambda (k v) k) ht)))
               (test-assert "Values are in order"
                 (apply char<? (hash-table-map->list (lambda (k v) v) ht)))
               (test-assert "The hash table is mutable"
                 (begin
                   (hash-table-add! ht #\xFE #\xDE)
                   #t))
               (test "Mutation actually took place" #\xDE
                 (hash-table-ref ht #\xFE))
               (test-error "Nonexistent entry" assertion-violation?
                 (hash-table-ref ht #\x01BF))))))
      (test-group "Without capacity"
        (do-tests (lambda ()
                    (hash-table-unfold (lambda (c) (char>? c #\z))
                                       (lambda (c) (values c (char-upcase c)))
                                       (lambda (c) (integer->char (+ 1 (char->integer c))))
                                       #\a
                                       char-comparator))))
      (test-group "With correct capacity"
        (do-tests (lambda ()
                    (hash-table-unfold (lambda (c) (char>? c #\z))
                                       (lambda (c) (values c (char-upcase c)))
                                       (lambda (c) (integer->char (+ 1 (char->integer c))))
                                       #\a
                                       char-comparator
                                       26))))
      (test-group "With too small capacity"
        (do-tests (lambda ()
                    (hash-table-unfold (lambda (c) (char>? c #\z))
                                       (lambda (c) (values c (char-upcase c)))
                                       (lambda (c) (integer->char (+ 1 (char->integer c))))
                                       #\a
                                       char-comparator
                                       10))))
      (test-group "With zero capacity"
        (do-tests (lambda ()
                    (hash-table-unfold (lambda (c) (char>? c #\z))
                                       (lambda (c) (values c (char-upcase c)))
                                       (lambda (c) (integer->char (+ 1 (char->integer c))))
                                       #\a
                                       char-comparator
                                       0))))))

  (test-group "Alist->hash-table"
    (let ((alist '((116123 . emotional-support)
                   (116117 . medical-advice)
                   (112 . emergency)
                   (112 . ambulance)
                   (112 . fire)
                   (110 . police)))
          (do-tests
           (lambda (maker)
             (let ((ht #f))
               (test-assert "Makes a hash table"
                 (begin
                   (set! ht (maker))
                   (hash-table? ht)))
               (test-assert "Not empty" (not (hash-table-empty? ht)))
               (test "There are 4 entries" 4 (hash-table-size ht))
               (test "emotional support"
                   'emotional-support
                 (hash-table-ref ht 116123))
               (test "medical advice"
                   'medical-advice
                 (hash-table-ref ht 116117))
               (test "emergency (test that the latest entry wins)"
                   'emergency
                 (hash-table-ref ht 112))
               (test "police"
                   'police
                 (hash-table-ref ht 110))
               (test-error "Nonexistent entry" assertion-violation?
                 (hash-table-ref ht 911))
               (test-assert "The hash table is mutable"
                 (begin
                   (hash-table-add! ht 116111 'help-for-children)
                   #t))
               (test "Mutation actually took place" 'help-for-children
                 (hash-table-ref ht 116111))
               (test "Keys are in order"
                   '(110 112 116117 116123 116111)
                 (hash-table-map->list (lambda (k v) k) ht))
               (test "Values are in order"
                   '(police emergency medical-advice emotional-support help-for-children)
                 (hash-table-map->list (lambda (k v) v) ht))))))
      (test-group "Without capacity"
        (do-tests (lambda () (alist->hash-table alist
                                                exact-integer-comparator))))
      (test-group "With correct capacity"
        (do-tests (lambda () (alist->hash-table alist
                                                exact-integer-comparator
                                                4))))
      (test-group "With too small capacity"
        (do-tests (lambda () (alist->hash-table alist
                                                exact-integer-comparator
                                                2))))
      (test-group "With zero capacity"
        (do-tests (lambda () (alist->hash-table alist
                                                exact-integer-comparator
                                                0)))))))

;; Predicates

(test-group "Predicates"
  (let* ((tiny-table
          (make-hash-table (make-comparator number? = #f number-hash)))
         (suits-table
          (hash-table symbol-comparator
                      'clubs #\x2663
                      'diamonds #\x2666
                      'hearts #\x2665
                      'spades #\x2660))
         (immutable-suits-table (hash-table-copy suits-table #f)))
    (test-group "Hash-table?"
      (test-assert (not (hash-table? 'a-symbol)))
      (test-assert (hash-table? tiny-table))
      (test-assert (hash-table? suits-table))
      (test-assert (hash-table? immutable-suits-table))
      (test-assert (not (hash-table? '((an-alist . not-a) (hash . table))))))

    (test-group "Hash-table-contains?"
      (test-assert (not (hash-table-contains? tiny-table 0)))
      (test-assert (hash-table-contains? suits-table 'clubs))
      (test-assert (hash-table-contains? suits-table 'spades))
      (test-assert (hash-table-contains? immutable-suits-table 'clubs))
      (test-assert (hash-table-contains? immutable-suits-table 'spades))
      (test-assert (not (hash-table-contains? suits-table 'joker)))
      (test-assert (not (hash-table-contains? immutable-suits-table 'joker)))
      (let ((tiny-table-2
             (make-hash-table (make-comparator number? = #f number-hash))))
        (test-assert (not (hash-table-contains? tiny-table-2 1/2)))
        (hash-table-add! tiny-table-2 1/2 3)
        (test-assert (hash-table-contains? tiny-table-2 1/2))
        (hash-table-delete! tiny-table-2 1/2)
        (test-assert (not (hash-table-contains? tiny-table-2 1/2)))))

    (test-group "Hash-table-empty?"
      (test-assert (hash-table-empty? tiny-table))
      (test-assert (not (hash-table-empty? suits-table)))
      (test-assert (not (hash-table-empty? immutable-suits-table)))
      (let ((tiny-table-2
             (make-hash-table (make-comparator number? = #f number-hash))))
        (test-assert (hash-table-empty? tiny-table-2))
        (hash-table-add! tiny-table-2 1/2 3)
        (test-assert (not (hash-table-empty? tiny-table-2)))
        (hash-table-delete! tiny-table-2 1/2)
        (test-assert (hash-table-empty? tiny-table-2))))

    (test-group "Hash-table-mutable?"
      (test-assert (hash-table-mutable? tiny-table))
      (test-assert (hash-table-mutable? suits-table))
      (let ((suits-table-2 (hash-table-copy suits-table #f)))
        (test-assert (not (hash-table-mutable? suits-table-2)))
        (test-error "Can’t -set! an immutable hash table" assertion-violation?
          (hash-table-set! suits-table-2 'joker #\x1F921))))))

(test-group "Accessors"
  (let ((mostly-abugidas-table
         (hash-table-unfold (lambda (c) (char>? c #\x10A0))
                            (lambda (c) (values c (char->integer c)))
                            (lambda (c) (integer->char (+ 1 (char->integer c))))
                            #\x900
                            char-comparator
                            26)))
    (test-group "Hash-table-ref with two args"
      (test "An entry that’s there" #x937
        (hash-table-ref mostly-abugidas-table #\x937))
      (test-error "An entry that’s not there" assertion-violation?
        (hash-table-ref mostly-abugidas-table #\x20)))
    (test-group "Hash-table-ref with three args"
      (test "An entry that’s there" #xBB2
        (hash-table-ref mostly-abugidas-table #\xBB2 (lambda () 'foo!)))
      (test "An entry that’s not there" 'foo!
        (hash-table-ref mostly-abugidas-table #\x20 (lambda () 'foo!))))
    (test-group "Hash-table-ref with four args"
      (test "An entry that’s there" #xC91
        (hash-table-ref mostly-abugidas-table
                        #\xC90
                        (lambda () 'foo!)
                        (lambda (val) (+ val 1))))
      (test "An entry that’s not there" 'foo!
        (hash-table-ref mostly-abugidas-table
                        #\x20
                        (lambda () 'foo!)
                        (lambda (val) (+ val 1)))))

    (test-group "Hash-table-ref/default"
      (test "An entry that’s there" #xE2B
        (hash-table-ref/default mostly-abugidas-table #\xE2B 'foo!))
      (test "An entry that’s not there" 'foo!
        (hash-table-ref/default mostly-abugidas-table #\xABCD 'foo!))))

  ;; No tests for hash-table-comparator due to its behaviour being
  ;; heavily implementation-dependent
  )

;; Mutators (see above for more tests of -add!, -replace!, and -delete!)

(test-group "Mutators"
  (test-group "Hash-table-set!"
    (test-group "Small table"
      (let ((set-test-table (make-hash-table exact-integer-comparator)))
        (test-assert "Setting a key that doesn’t exist"
          (begin
            (hash-table-set! set-test-table 12 #t)
            #t))
        (test-assert "Checking it exists"
          (hash-table-ref/default set-test-table 12 #f))
        (test-assert "Setting a key that already exists"
          (begin
            (hash-table-set! set-test-table 12 #f)
            #t))
        (test-assert "Checking it was replaced"
          (not (hash-table-ref/default set-test-table 12 #t)))))
    (test-group "Bigger table, random order"
      (let ((big-set-test-table (make-hash-table exact-integer-comparator 10))
            (order (make-vector 1000))
            (vals (make-vector 1000))
            (source (make-random-source)))
        (let loop ((idx 0))
          (when (< idx (vector-length order))
            (vector-set! order idx idx)
            (vector-set! vals idx idx)
            (loop (+ idx 1))))
        (random-source-pseudo-randomize! source 6 4)
        (vector-shuffle! order source)
        (vector-shuffle! vals source)
        (test-group "Creating new entries"
          (let loop ((idx 0))
            (when (< idx (vector-length order))
              (let ()
                (define n (vector-ref order idx))
                (define val (vector-ref vals idx))
                (test-assert (string-append "Setting " (number->string n) " to " (number->string val))
                  (begin
                    (hash-table-set! big-set-test-table n val)
                    #t))
                (test "Checking it was set" val
                  (hash-table-ref/default big-set-test-table n #f))
                (test "Size is correct" (+ idx 1)
                  (hash-table-size big-set-test-table))
                (loop (+ idx 1))))))

        (test "Insertion order" (vector->list order)
          (hash-table-map->list (lambda (k v) k) big-set-test-table))

        (test-group "Replacing old values"
          (let loop ((idx 0))
            (when (< idx (vector-length order))
              (let ()
                (define n (vector-ref order idx))
                (define val (+ n 1))
                (test-assert (string-append "Setting " (number->string n) " to " (number->string val))
                  (begin
                    (hash-table-set! big-set-test-table n val)
                    #t))
                (test "Checking it was set" val
                  (hash-table-ref/default big-set-test-table n #f))
                (test "Size is correct" 1000
                  (hash-table-size big-set-test-table))
                (loop (+ idx 1))))))))
    (test-group "Can’t set! in an immutable hash table"
      (let ((immutable-set-test-table
             (hash-table-copy (hash-table exact-integer-comparator
                                          100 101
                                          102 103)
                              #f)))
        (test-error "Can’t set existing key" assertion-violation?
          (hash-table-set! immutable-set-test-table 100 99))
        (test "Mutation didn’t happen" 101
          (hash-table-ref/default immutable-set-test-table 100 #f))
        (test-error "Can’t set nonexistent key" assertion-violation?
          (hash-table-set! immutable-set-test-table 104 105))
        (test "Mutation didn’t happen" #f
          (hash-table-ref/default immutable-set-test-table 104 #f))))
    (test-group "Multiple mutations at once"
      (let* ((multi-set-test-table (make-hash-table char-comparator))
             (keys (reverse (string->list "abcdefghijklmnopqrstuvwxyz"))))
        (test-assert "Setting"
          (begin
            (hash-table-set! multi-set-test-table
                             #\z 'consonant
                             #\y 'consonant
                             #\x 'consonant
                             #\w 'consonant
                             #\v 'consonant
                             #\u 'vowel
                             #\t 'consonant
                             #\s 'consonant
                             #\r 'consonant
                             #\q 'consonant
                             #\p 'consonant
                             #\o 'vowel
                             #\n 'consonant
                             #\m 'consonant
                             #\l 'consonant
                             #\k 'consonant
                             #\j 'consonant
                             #\i 'vowel
                             #\h 'consonant
                             #\g 'consonant
                             #\f 'consonant
                             #\e 'vowel
                             #\d 'consonant
                             #\c 'consonant
                             #\b 'consonant
                             #\a 'vowel)
            #t))
        (for-each
         (lambda (key)
           (test (string-append (string key) " is there")
               (if (memv key '(#\a #\e #\i #\o #\u)) 'vowel 'consonant)
             (hash-table-ref/default multi-set-test-table key #f)))
         keys)
        (test "Size is correct" 26
          (hash-table-size multi-set-test-table))
        (test "Insertion order" keys
          (hash-table-map->list (lambda (k v) k) multi-set-test-table))
        (test-error "Odd number of arguments" assertion-violation?
          (hash-table-set! multi-set-test-table #\xFE 'consonant #\x1BF))
        (test "No mutations happened" #f
          (hash-table-ref/default multi-set-test-table #\xFE #f))
        (test "No mutations happened" 'nowt
          (hash-table-ref/default multi-set-test-table #\x1BF 'nowt)))))

  (test-group "Hash-table-delete!"
    (let ((delete-test-table
           (hash-table-unfold (lambda (c) (char>? c #\z))
                              (lambda (c) (values (string->symbol
                                                   (string c))
                                                  c))
                              (lambda (c) (integer->char (+ 1 (char->integer c))))
                              #\a
                              symbol-comparator
                              4)))
      (test-group "Deleting one entry"
        (test "Doing the deletion" 1
          (hash-table-delete! delete-test-table 't))
        (test "It’s actually gone" #f
          (hash-table-ref/default delete-test-table 't #f))
        (test "Size is correct" 25
          (hash-table-size delete-test-table))
        (test "Deleting a second time is a no-op" 0
          (hash-table-delete! delete-test-table 't))
        (test-assert "It’s still gone"
          (not (hash-table-contains? delete-test-table 't)))
        (test "Size is still correct" 25
          (hash-table-size delete-test-table))
        (for-each
         (lambda (l)
           (test (string-append (symbol->string l) " is still there")
               (string-ref (symbol->string l) 0)
             (hash-table-ref/default delete-test-table l #f)))
         '(a b c d e f g h i j k l m n o p q r s u v w x y z))
        (test "Everything else is still in order"
            '(a b c d e f g h i j k l m n o p q r s u v w x y z)
          (hash-table-map->list (lambda (k v) k) delete-test-table)))

      (test-group "Deleting an entry that wasn’t there to begin with"
        (test-assert "It’s not there"
          (not (hash-table-contains? delete-test-table '!)))
        (test "Doing the deletion" 0
          (hash-table-delete! delete-test-table '!))
        (test "Size is correct" 25
          (hash-table-size delete-test-table))
        (for-each
         (lambda (l)
           (test (string-append (symbol->string l) " is still there")
               (string-ref (symbol->string l) 0)
             (hash-table-ref/default delete-test-table l #f)))
         '(a b c d e f g h i j k l m n o p q r s u v w x y z))
        (test "Everything else is still in order"
            '(a b c d e f g h i j k l m n o p q r s u v w x y z)
          (hash-table-map->list (lambda (k v) k) delete-test-table)))

      (test-group "Deleting multiple entries that are there"
        (test "Doing the deletion" 5
          (hash-table-delete! delete-test-table 'a 'e 'i 'o 'u))
        (test-assert "a is actually gone"
          (not (hash-table-contains? delete-test-table 'a)))
        (test-assert "e is actually gone"
          (not (hash-table-contains? delete-test-table 'e)))
        (test-assert "i is actually gone"
          (not (hash-table-contains? delete-test-table 'i)))
        (test-assert "o is actually gone"
          (not (hash-table-contains? delete-test-table 'o)))
        (test-assert "u is actually gone"
          (not (hash-table-contains? delete-test-table 'u)))
        (test "Size is correct" 20
          (hash-table-size delete-test-table))
        (test "Deleting a second time is a no-op" 0
          (hash-table-delete! delete-test-table 'a 'e 'i 'o 'u))
        (test "Size is still correct" 20
          (hash-table-size delete-test-table))
        (for-each
         (lambda (l)
           (test (string-append (symbol->string l) " is still there")
               (string-ref (symbol->string l) 0)
             (hash-table-ref delete-test-table l #f)))
         '(b c d f g h j k l m n p q r s v w x y z))
        (test "Everything else is still in order"
            '(b c d f g h j k l m n p q r s v w x y z)
          (hash-table-map->list (lambda (k v) k) delete-test-table)))

      (test-group "Deleting until the table is empty"
        (let loop ((more '(l r g v b s c p k j y h n z m x w d q f))
                   (size 20))
          (unless (null? more)
            (test-group (symbol->string (car more))
              (define sym (car more))
              (test "Doing the deletion" 1
                (hash-table-delete! delete-test-table sym))
              (test-assert "It’s actually gone"
                (not (hash-table-contains? delete-test-table sym)))
              (test "Size is correct" (- size 1)
                (hash-table-size delete-test-table))
              (for-each
               (lambda (l)
                 (test (string-append (symbol->string l) " is still there")
                     (string-ref (symbol->string l) 0)
                   (hash-table-ref/default delete-test-table l #f)))
               (cdr more)))
            (loop (cdr more) (- size 1)))))

      (test-group "Final post-deletion tests"
        (test-assert "It’s empty" (hash-table-empty? delete-test-table))
        (test "Its size is 0" 0 (hash-table-size delete-test-table))))

    (test-group "From very small hash tables with a bad hash function"
      (define (fib n)
        (cond ((= n 0) 1)
              ((= n 1) 1)
              (else (+ (fib (- n 1)) (fib (- n 2))))))
      (let loop ((size 2))
        (test-group (string-append "size " (number->string size))
          (let ((maker
                 (lambda ()
                   (hash-table-unfold (lambda (n) (>= n size))
                                      (lambda (n)
                                        (let ((fn (fib (+ n 2))))
                                          (values fn
                                                  (number->string fn))))
                                      (lambda (n) (+ n 1))
                                      0
                                      bad-exact-integer-comparator
                                      size)))
                (do-tests
                 (lambda (ht keys)
                   (let loop ((more keys))
                     (unless (null? more)
                       (let ((this-key (car more)))
                         (test-group (string-append "Deleting " (number->string this-key))
                           (test "Doing the deletion" 1
                             (hash-table-delete! ht this-key))
                           (test "It’s actually gone" #f
                             (hash-table-ref/default ht this-key #f))
                           (test "Size is correct" (length (cdr more))
                             (hash-table-size ht))
                           (for-each
                            (lambda (other-key)
                              (test (string-append (number->string other-key)
                                                   " is still there")
                                  (number->string other-key)
                                (hash-table-ref/default ht other-key #f)))
                            (cdr more))
                           (unless (or (null? (cdr more))
                                       (null? (cddr more)))
                             (test-assert "Insertion order still maintained"
                               (apply < (hash-table-map->list (lambda (k v) k) ht))))))
                       (loop (cdr more)))))))
            (test-group "Insertion order"
              (do-tests (maker)
                        (list-tabulate size (lambda (n) (fib (+ n 2))))))
            (test-group "Reverse insertion order"
              (do-tests (maker)
                        (reverse (list-tabulate size (lambda (n) (fib (+ n 2)))))))))
        (when (<= size 7)
          (loop (+ size 1)))))

    (test-group "Can’t delete! in an immutable hash table"
      (let ((immutable-delete-test-table
             (hash-table-copy (hash-table exact-integer-comparator
                                          100 101
                                          102 103)
                              #f)))
        (test-error "Can’t delete key" assertion-violation?
          (hash-table-delete! immutable-delete-test-table 100))
        (test "Mutation didn’t happen" 101
          (hash-table-ref/default immutable-delete-test-table 100 #f))
        (test-error "Can’t delete multiple keys" assertion-violation?
          (hash-table-delete! immutable-delete-test-table 100 102))
        (test "Mutation didn’t happen" 103
          (hash-table-ref/default immutable-delete-test-table 102 #f))
        (test-error "Can’t delete a nonexistent key" assertion-violation?
          (hash-table-delete! immutable-delete-test-table 104))
        (test "Mutation didn’t happen" #f
          (hash-table-ref/default immutable-delete-test-table 104 #f)))))

  (test-group "Hash-table-intern!"
    (let ((intern-test-table (make-hash-table string-comparator 10)))
      (test-group "Interning new entries"
        (let loop ((n 0))
          (when (<= n 1000)
            (let ()
              (define key (number->string n 16))
              (test (string-append "Interning " key) n
                (hash-table-intern! intern-test-table key
                                    (lambda () n)))
              (test "It was set" n
                (hash-table-ref/default intern-test-table key #f))
              (when (> n 10)
                (test-assert "Insertion order"
                  (apply < (hash-table-map->list (lambda (k v)
                                                   (string->number k 16))
                                                 intern-test-table)))))
            (loop (+ n 1)))))

      (test-group "Trying to re-intern"
        (let loop ((n 1000))
          (when (>= n 0)
            (let ()
              (define key (number->string n 16))
              (test (string-append "Re-interning " key) n
                (hash-table-intern! intern-test-table key
                                    (lambda () (* n 2))))
              (test "It wasn’t set" n
                (hash-table-ref/default intern-test-table key #f))
              (when (> n 10)
                (test-assert "Insertion order"
                  (apply < (hash-table-map->list (lambda (k v)
                                                   (string->number k 16))
                                                 intern-test-table)))))
            (loop (- n 1))))))
    (let ((immutable-intern-test-table
           (hash-table-copy (hash-table exact-integer-comparator
                                        100 101
                                        102 103)
                            #f)))
      (test-error "Can’t intern new key into an immutable hash table" assertion-violation?
        (hash-table-intern! immutable-intern-test-table 104
                            (lambda () 105)))
      (test "No mutation" #f
        (hash-table-ref/default immutable-intern-test-table 104 #f))
      (test-error "Can’t intern new key into an immutable hash table" assertion-violation?
        (hash-table-intern! immutable-intern-test-table 100
                            (lambda () 99)))
      (test "No mutation" 101
        (hash-table-ref/default immutable-intern-test-table 100 #f))))

  (test-group "Hash-table-update!"
    (test-group "Three arguments"
      (let ((ht (hash-table char-comparator
                            #\a 0
                            #\b 0
                            #\c 0
                            #\d 0)))
        (test-assert "Updating existing entry"
          (begin
            (hash-table-update! ht #\a (lambda (v) (+ v 1)))
            #t))
        (test "It was actually updated" 1
          (hash-table-ref/default ht #\a #f))
        (test "Size is correct" 4
          (hash-table-size ht))
        (test-error "Updating a non-existent entry" assertion-violation?
          (hash-table-update! ht #\z (lambda (v) (+ v 1))))
        (test "Insertion order not affected" '(#\a #\b #\c #\d)
          (hash-table-map->list (lambda (k v) k) ht))
        (let ((immutable-ht (hash-table-copy ht #f)))
          (test-error "Can’t update! an immutable hash table" assertion-violation?
            (begin
              (hash-table-update! immutable-ht #\a (lambda (v) (+ v 1)))
              #t))
          (test "No mutation took place" 1
            (hash-table-ref/default immutable-ht #\a #f)))))
    (test-group "Four arguments"
      (let ((ht (hash-table char-comparator
                            #\a 0
                            #\b 0
                            #\c 0
                            #\d 0)))
        (test-assert "Updating existing entry"
          (begin
            (hash-table-update! ht #\a (lambda (v) (+ v 1)) (lambda () -1))
            #t))
        (test "It was actually updated" 1
          (hash-table-ref/default ht #\a #f))
        (test "Size is correct" 4
          (hash-table-size ht))
        (test-assert "Updating a non-existent entry"
          (begin
            (hash-table-update! ht #\z (lambda (v) (+ v 1)) (lambda () 0))))
        (test "Previously non-existent entry was updated" 1
          (hash-table-ref/default ht #\z #f))
        (test "Size is correct" 5
          (hash-table-size ht))
        (test "Insertion order" '(#\a #\b #\c #\d #\z)
          (hash-table-map->list (lambda (k v) k) ht))
        (let ((immutable-ht (hash-table-copy ht #f)))
          (test-error "Can’t update! an immutable hash table" assertion-violation?
            (begin
              (hash-table-update! immutable-ht #\a (lambda (v) (+ v 1)) (lambda () -1))
              #t))
          (test "No mutation took place" 1
            (hash-table-ref/default immutable-ht #\a #f))
          (test-error "Can’t update! a new entry in an immutable hash table" assertion-violation?
            (begin
              (hash-table-update! immutable-ht #\y (lambda (v) (+ v 1)) (lambda () -1))
              #t))
          (test "No mutation took place" #f
            (hash-table-ref/default immutable-ht #\y #f))
          (test "Size is correct" 5
            (hash-table-size immutable-ht)))))
    (test-group "Five arguments"
      (let ((ht (hash-table char-comparator
                            #\a 0
                            #\b 0
                            #\c 0
                            #\d 0)))
        (test-assert "Updating existing entry"
          (begin
            (hash-table-update! ht #\a
                                (lambda (v) (+ v 1))
                                (lambda () -1)
                                (lambda (v) (+ v 100)))
            #t))
        (test "It was actually updated" 101
          (hash-table-ref/default ht #\a #f))
        (test "Size is correct" 4
          (hash-table-size ht))
        (test-assert "Updating a non-existent entry"
          (begin
            (hash-table-update! ht #\z
                                (lambda (v) (+ v 1))
                                (lambda () 0)
                                (lambda (v) (+ v 100)))))
        (test "Previously non-existent entry was updated" 1
          (hash-table-ref/default ht #\z #f))
        (test "Size is correct" 5
          (hash-table-size ht))
        (test "Insertion order" '(#\a #\b #\c #\d #\z)
          (hash-table-map->list (lambda (k v) k) ht))
        (let ((immutable-ht (hash-table-copy ht #f)))
          (test-error "Can’t update! an immutable hash table" assertion-violation?
            (begin
              (hash-table-update! immutable-ht #\a
                                  (lambda (v) (+ v 1))
                                  (lambda () -1)
                                  (lambda (v) (+ v 100)))
              #t))
          (test "No mutation took place" 101
            (hash-table-ref/default immutable-ht #\a #f))
          (test-error "Can’t update! a new entry in an immutable hash table" assertion-violation?
            (begin
              (hash-table-update! immutable-ht #\y
                                  (lambda (v) (+ v 1))
                                  (lambda () 0)
                                  (lambda (v) (+ v 100)))
              #t))
          (test "No mutation took place" #f
            (hash-table-ref/default immutable-ht #\y #f))
          (test "Size is correct" 5
            (hash-table-size immutable-ht))))))

  (test-group "Hash-table-update!/default"
    (let ((ht (hash-table char-comparator #\a '())))
      (test-assert "Updating existing entry"
        (begin
          (hash-table-update!/default ht #\a
                                      (lambda (v) (cons 'bar v))
                                      '(foo))
          #t))
      (test "It was actually updated" '(bar)
        (hash-table-ref/default ht #\a #f))
      (test "Size is correct" 1
        (hash-table-size ht))
      (test-assert "Updating non-existent entry"
        (begin
          (hash-table-update!/default ht #\b
                                      (lambda (v) (cons 'bar v))
                                      '(foo))
          #t))
      (test "It was actually updated" '(bar foo)
        (hash-table-ref/default ht #\b #f))
      (test "Size is correct" 2
        (hash-table-size ht))))

  (test-group "Hash-table-pop!"
    (let ((pop-test-table
           (hash-table-unfold (lambda (c) (char>? c #\z))
                              (lambda (c) (values (string->symbol
                                                   (string c))
                                                  c))
                              (lambda (c) (integer->char (+ 1 (char->integer c))))
                              #\a
                              symbol-comparator
                              4)))
      (let loop ((keys (reverse '(a b c d e f g h i j k l m n o p q r s t u v w x y z))))
        (unless (null? keys)
          (let ((key (car keys)))
            (test-values
                (string-append "Popping " (symbol->string key))
                (values key (string-ref (symbol->string key) 0))
              (hash-table-pop! pop-test-table))
            (test "It’s gone" #f
              (hash-table-ref/default pop-test-table key #f))
            (test "Size is correct" (length (cdr keys))
              (hash-table-size pop-test-table))
            (for-each
             (lambda (other-key)
               (test (string-append (symbol->string other-key) " is still there")
                   (string-ref (symbol->string other-key) 0)
                 (hash-table-ref/default pop-test-table other-key #f)))
             (cdr keys)))
          (loop (cdr keys))))

      (test-assert "Hash table is now empty"
        (hash-table-empty? pop-test-table))
      (test-error "Can’t pop from an empty hash table" assertion-violation?
        (hash-table-pop! pop-test-table)))

    (let ((immutable-pop-test-table
           (hash-table-copy (hash-table exact-integer-comparator
                                        100 101
                                        102 103)
                            #f)))
      (test-error "Can’t pop from an immutable hash table" assertion-violation?
        (hash-table-pop! immutable-pop-test-table))
      (test "No mutation took place" 103
        (hash-table-ref/default immutable-pop-test-table 102 #f))
      (test "No mutation took place" 101
        (hash-table-ref/default immutable-pop-test-table 100 #f))))

  (test-group "Hash-table-clear!"
    (let ((clear-test-table (hash-table symbol-comparator
                                        'clubs #\x2663
                                        'diamonds #\x2666
                                        'hearts #\x2665
                                        'spades #\x2660)))
      (test-assert "Emptying it"
        (begin
          (hash-table-clear! clear-test-table)
          #t))
      (test "Size is correct" 0
        (hash-table-size clear-test-table))
      (test-assert "It’s empty"
        (hash-table-empty? clear-test-table))
      (for-each
       (lambda (suit)
         (test (string-append (symbol->string suit) " is gone") #f
           (hash-table-ref/default clear-test-table suit #f)))
       '(clubs diamonds hearts spades))
      (test-assert "Can still add things"
        (begin
          (hash-table-add! clear-test-table 'joker #\x1F921)
          #t))
      (test "Can retrieve what was just added" #\x1F921
        (hash-table-ref/default clear-test-table 'joker #f)))
    (let ((immutable-clear-test-table
           (hash-table-copy (hash-table symbol-comparator
                                        'clubs #\x2663
                                        'diamonds #\x2666
                                        'hearts #\x2665
                                        'spades #\x2660)
                            #f)))
      (test-error "Can’t clear an immutable hash table" assertion-violation?
        (hash-table-clear! immutable-clear-test-table))
      (test "Size is correct" 4
        (hash-table-size immutable-clear-test-table))
      (test-assert "It’s not empty"
        (not (hash-table-empty? immutable-clear-test-table)))
      (for-each
       (lambda (suit)
         (test (string-append (symbol->string (car suit)) " is still there") (cdr suit)
           (hash-table-ref/default immutable-clear-test-table (car suit) #f)))
       '((clubs . #\x2663) (diamonds . #\x2666) (hearts . #\x2665) (spades . #\x2660))))))

;; The whole hash table

(test-group "The whole hash table"
  ;; Hash-table-size tests are spread throughout this test suite,
  ;; ensuring the size is correct after many individual mutations

  (test-group "Hash-table="
    (let ((a (hash-table symbol-comparator
                         'a 1
                         'b 2
                         'c 3
                         'd 4))
          (b (hash-table symbol-comparator
                         'a 1
                         'b 2
                         'c 3
                         'd 4))
          (c (hash-table symbol-comparator
                         'd 4
                         'b 2
                         'c 3
                         'a 1))
          (d (hash-table symbol-comparator
                         'a 2
                         'b 3
                         'c 4
                         'd 5)))
      (test-assert "When the insertion orders are the same"
        (hash-table= = a b))
      (test-assert "When the insertion orders are different"
        (hash-table= = a c))
      (test-assert "The predicate is called in the order a b"
        (hash-table= (lambda (x y)
                       (= (+ x 1) y))
                     a d))
      (hash-table-delete! a 'a)
      (test-assert "Not when one key is missing in a"
        (not (hash-table= = a b)))
      (test-assert "Not when one key is missing in b"
        (not (hash-table= = b a)))
      (test-assert "When both hash tables are empty"
        (hash-table= (lambda (x y) (assertion-violation #f "should never be called!"))
                     (make-hash-table symbol-comparator)
                     (make-hash-table symbol-comparator)))
      (test-assert "Not when one hash table is empty and the other isn’t"
        (not (hash-table= =
                          (make-hash-table symbol-comparator)
                          a)))
      (test-assert "Not when one hash table is empty and the other isn’t"
        (not (hash-table= =
                          a
                          (make-hash-table symbol-comparator))))
      (test-assert "When one hash table is immutable and the other isn’t"
        (hash-table= =
                     (hash-table-copy a #f)
                     a))
      (test-assert "Not when one hash table is immutable and the other isn’t"
        (not (hash-table= =
                          (hash-table-copy a #f)
                          b)))
      (test-assert "When both hash tables are immutable"
        (hash-table= =
                     (hash-table-copy a #f)
                     (hash-table-copy a #f)))))

  (test-group "Hash-table-find"
    (let ((find-test-table
           (hash-table symbol-comparator
                       'a 1
                       'b 2
                       'c 3
                       'd 4)))
      (test "Finding a key by a value" 'd
        (hash-table-find (lambda (k v) (if (eqv? v 4) k #f))
                         find-test-table
                         (lambda () 'not-found)))
      (test "Finds the leftmost key in insertion order" 'b
        (hash-table-find (lambda (k v) (if (even? v) k #f))
                         find-test-table
                         (lambda () 'not-found)))
      (test "Invokes the failure proc" 'not-found
        (hash-table-find (lambda (k v) (if (eqv? v 5) k #f))
                         find-test-table
                         (lambda () 'not-found)))))

  (test-group "Hash-table-count"
    (let ((count-test-table
           (hash-table symbol-comparator
                       'a 1
                       'b 2
                       'c 3
                       'd 4)))
      (test "Counts based on value" 2
        (hash-table-count (lambda (k v) (even? v)) count-test-table))
      (test "0 when there are none" 0
        (hash-table-count (lambda (k v) (> v 4)) count-test-table))
      (test "0 when the hash table is empty" 0
        (hash-table-count (lambda (k v) #t) (make-hash-table exact-integer-comparator)))))

  (test-group "Hash-table-keys, hash-table-values, hash-table-entries"
    (let ((mostly-abugidas-table
           (hash-table-unfold (lambda (c) (char>? c #\x10A0))
                              (lambda (c) (values c (char->integer c)))
                              (lambda (c) (integer->char (+ 1 (char->integer c))))
                              #\x900
                              char-comparator
                              26)))
      (let ((keys-vec (hash-table-keys mostly-abugidas-table))
            (vals-vec (hash-table-values mostly-abugidas-table)))
        (test "Keys size is the same as that of the hash table"
            (hash-table-size mostly-abugidas-table)
          (vector-length keys-vec))
        (test "Values size is the same as that of the hash table"
            (hash-table-size mostly-abugidas-table)
          (vector-length vals-vec))
        (test-assert "Insertion order of keys"
          (apply char<? (vector->list keys-vec)))
        (test-assert "Insertion order of values"
          (apply < (vector->list vals-vec)))
        (vector-for-each
         (lambda (k v)
           (test (string-append (string k) " is there") v
             (hash-table-ref/default mostly-abugidas-table k #f)))
         keys-vec vals-vec)
        (test-assert "Each invocation of -keys on a mutable hash table yields a different vector"
          (not (eq? keys-vec (hash-table-keys mostly-abugidas-table))))
        (test-assert "Each invocation of -values on a mutable hash table yields a different vector"
          (not (eq? vals-vec (hash-table-keys mostly-abugidas-table))))
        (let-values (((keys2-vec vals2-vec)
                      (hash-table-entries mostly-abugidas-table)))
          (test-assert "-entries keys vector same as -keys"
            (equal? keys2-vec keys-vec))
          (test-assert "-entries values vector same as -values"
            (equal? vals2-vec vals-vec))
          (test-assert "Modifying -entries values vector of mutable table doesn’t affect the hash table"
            (begin
              (assert (= (vector-ref vals2-vec 0) #x900))
              (vector-set! vals2-vec 0 'foo)
              (hash-table-ref mostly-abugidas-table #\x900 #x900))))
        (test-assert "Modifying -values vector of mutable table doesn’t affect the hash table"
          (begin
            (assert (= (vector-ref vals-vec 0) #x900))
            (vector-set! vals-vec 0 'foo)
            (hash-table-ref mostly-abugidas-table #\x900 #x900)))))))

;; Low-level iteration

(test-group "Low-level iteration"
  (let ((tiny-table (make-hash-table exact-integer-comparator 10))
        (mostly-abugidas-table
         (hash-table-unfold (lambda (c) (char>? c #\x10A0))
                            (lambda (c) (values c (char->integer c)))
                            (lambda (c) (integer->char (+ 1 (char->integer c))))
                            #\x900
                            char-comparator
                            26)))
    (test-assert (hash-table-cursor-at-end? tiny-table
                                            (hash-table-cursor-first tiny-table)))
    (test-assert (hash-table-cursor-at-end? tiny-table
                                            (hash-table-cursor-last tiny-table)))
    (test-assert (hash-table-cursor-at-end?
                  mostly-abugidas-table
                  (hash-table-cursor-previous mostly-abugidas-table
                                              (hash-table-cursor-first mostly-abugidas-table))))
    (test-assert (hash-table-cursor-at-end?
                  mostly-abugidas-table
                  (hash-table-cursor-next mostly-abugidas-table
                                          (hash-table-cursor-last mostly-abugidas-table))))
    (let loop ((cur (hash-table-cursor-first mostly-abugidas-table)))
      (unless (hash-table-cursor-at-end? mostly-abugidas-table
                                         (hash-table-cursor-next mostly-abugidas-table cur))
        (test-assert
            "Forwards and backwards is a no-op"
          (char=? (hash-table-cursor-key mostly-abugidas-table cur)
                  (hash-table-cursor-key mostly-abugidas-table
                                         (hash-table-cursor-previous
                                          mostly-abugidas-table
                                          (hash-table-cursor-next
                                           mostly-abugidas-table
                                           cur)))))
        (loop (hash-table-cursor-next mostly-abugidas-table cur))))
    (let loop ((cur (hash-table-cursor-last mostly-abugidas-table)))
      (unless (hash-table-cursor-at-end? mostly-abugidas-table
                                         (hash-table-cursor-previous mostly-abugidas-table cur))
        (test-assert
            "Backwards and forwards is a no-op"
          (char=? (hash-table-cursor-key mostly-abugidas-table cur)
                  (hash-table-cursor-key mostly-abugidas-table
                                         (hash-table-cursor-next
                                          mostly-abugidas-table
                                          (hash-table-cursor-previous
                                           mostly-abugidas-table
                                           cur)))))
        (loop (hash-table-cursor-previous mostly-abugidas-table cur))))
    (let loop ((cur (hash-table-cursor-first mostly-abugidas-table)))
      (unless (hash-table-cursor-at-end? mostly-abugidas-table cur)
        (test (hash-table-cursor-key mostly-abugidas-table cur) (integer->char (hash-table-cursor-value mostly-abugidas-table cur)))
        (let-values
            (((k v) (hash-table-cursor-key+value mostly-abugidas-table cur)))
          (test k (hash-table-cursor-key mostly-abugidas-table cur))
          (test v (hash-table-cursor-value mostly-abugidas-table cur)))
        (unless (hash-table-cursor-at-end? mostly-abugidas-table (hash-table-cursor-next mostly-abugidas-table cur))
          (test-assert (not (char=? (hash-table-cursor-key mostly-abugidas-table cur)
                                    (hash-table-cursor-key
                                     mostly-abugidas-table
                                     (hash-table-cursor-next mostly-abugidas-table cur))))))
        (loop (hash-table-cursor-next mostly-abugidas-table cur))))
    (let loop ((cur (hash-table-cursor-last mostly-abugidas-table)))
      (unless (hash-table-cursor-at-end? mostly-abugidas-table cur)
        (test (hash-table-cursor-key mostly-abugidas-table cur) (integer->char (hash-table-cursor-value mostly-abugidas-table cur)))
        (let-values
            (((k v) (hash-table-cursor-key+value mostly-abugidas-table cur)))
          (test k (hash-table-cursor-key mostly-abugidas-table cur))
          (test v (hash-table-cursor-value mostly-abugidas-table cur)))
        (unless (hash-table-cursor-at-end? mostly-abugidas-table (hash-table-cursor-previous mostly-abugidas-table cur))
          (test-assert (not (char=? (hash-table-cursor-key mostly-abugidas-table cur)
                                    (hash-table-cursor-key
                                     mostly-abugidas-table
                                     (hash-table-cursor-previous mostly-abugidas-table cur))))))
        (loop (hash-table-cursor-previous mostly-abugidas-table cur))))
    (let loop ((cur1 (hash-table-cursor-first mostly-abugidas-table)))
      (define cur2 (hash-table-cursor-next mostly-abugidas-table cur1))
      (unless (hash-table-cursor-at-end? mostly-abugidas-table cur2)
        (test-assert (char<? (hash-table-cursor-key mostly-abugidas-table cur1) (hash-table-cursor-key mostly-abugidas-table cur2)))
        (test-assert (< (hash-table-cursor-value mostly-abugidas-table cur1) (hash-table-cursor-value mostly-abugidas-table cur2)))
        (loop cur2)))
    (let loop ((cur1 (hash-table-cursor-last mostly-abugidas-table)))
      (define cur2 (hash-table-cursor-previous mostly-abugidas-table cur1))
      (unless (hash-table-cursor-at-end? mostly-abugidas-table cur2)
        (test-assert (char>? (hash-table-cursor-key mostly-abugidas-table cur1) (hash-table-cursor-key mostly-abugidas-table cur2)))
        (test-assert (> (hash-table-cursor-value mostly-abugidas-table cur1) (hash-table-cursor-value mostly-abugidas-table cur2)))
        (loop cur2)))
    (let loop ((cur (hash-table-cursor-first mostly-abugidas-table)))
      (unless (hash-table-cursor-at-end? mostly-abugidas-table cur)
        (let ((old-value (hash-table-cursor-value mostly-abugidas-table cur)))
          (hash-table-cursor-value-set! mostly-abugidas-table cur
                                        (- old-value #x900))
          (test (- old-value #x900) (hash-table-ref/default mostly-abugidas-table (hash-table-cursor-key mostly-abugidas-table cur) #f))
          (test (- old-value #x900) (hash-table-cursor-value mostly-abugidas-table (hash-table-cursor-for-key mostly-abugidas-table (hash-table-cursor-key mostly-abugidas-table cur)))))
        (loop (hash-table-cursor-next mostly-abugidas-table cur))))))

;; Mapping and folding

(test-group "Mapping and folding"
  (test-group "Hash-table-map"
    (let ((map-test-table
           (hash-table-unfold (lambda (c) (char<? c #\a))
                              (lambda (c) (values c (char-upcase c)))
                              (lambda (c) (integer->char (- (char->integer c) 1)))
                              #\z
                              char-comparator
                              26))
          (mapped-test-table #f))
      (test-assert "Mapping"
        (begin
          (set! mapped-test-table (hash-table-map (lambda (k v)
                                                    (string v k))
                                                  map-test-table))
          #t))
      (test-assert "It’s a hash table"
        (hash-table? mapped-test-table))
      (test "Size is correct" (hash-table-size map-test-table)
        (hash-table-size mapped-test-table))
      (test "Keys are the same and same insertion order"
          (hash-table-keys map-test-table)
        (hash-table-keys mapped-test-table))
      (string-for-each
       (lambda (c)
         (test (string-append "Correct value for " (string c)) (string (char-upcase c) c)
           (hash-table-ref/default mapped-test-table c #f)))
       "abcdefghijklmnopqrstuvwxyz"))
    (let ((map-test-table
           (hash-table-copy
            (hash-table-unfold (lambda (c) (char>? c #\z))
                               (lambda (c) (values c (char-upcase c)))
                               (lambda (c) (integer->char (+ 1 (char->integer c))))
                               #\a
                               char-comparator
                               26)
            #f))
          (mapped-test-table #f))
      (test-assert "Mapping an immutable hash table"
        (begin
          (set! mapped-test-table (hash-table-map (lambda (k v)
                                                    (string v k))
                                                  map-test-table))
          #t))
      (test-assert "It’s a hash table"
        (hash-table? mapped-test-table))
      (test-assert "It’s a mutable hash table"
        (hash-table-mutable? mapped-test-table))
      (test "Size is correct" (hash-table-size map-test-table)
        (hash-table-size mapped-test-table))
      (test "Keys are the same and same insertion order"
          (hash-table-keys map-test-table)
        (hash-table-keys mapped-test-table))
      (string-for-each
       (lambda (c)
         (test (string-append "Correct value for " (string c)) (string (char-upcase c) c)
           (hash-table-ref/default mapped-test-table c #f)))
       "abcdefghijklmnopqrstuvwxyz")))

  (test-group "Hash-table-map!"
    (let ((mapb-test-table
           (hash-table-unfold (lambda (n) (> n 900))
                              (lambda (n) (values n (+ n 100)))
                              (lambda (n) (+ n 1))
                              0
                              exact-integer-comparator)))
      (let ((vals (hash-table-values mapb-test-table)))
        (test-assert "Running map!"
          (begin
            (hash-table-map! (lambda (k v) (- v 100)) mapb-test-table)
            #t))
        (test "Every value now 100 less"
            (map (lambda (v) (- v 100)) (vector->list vals))
          (vector->list (hash-table-values mapb-test-table)))))
    (let ((immutable-mapb-test-table
           (hash-table-copy
            (hash-table-unfold (lambda (n) (> n 900))
                               (lambda (n) (values n (+ n 100)))
                               (lambda (n) (+ n 1))
                               0
                               exact-integer-comparator)
            #f)))
      (let ((vals (hash-table-values immutable-mapb-test-table)))
        (test-error "Can’t map! an immutable hash table" assertion-violation?
          (hash-table-map! (lambda (k v) (- v 100)) immutable-mapb-test-table))
        (test "No mutation took place"
            vals
          (hash-table-values immutable-mapb-test-table)))))

  (test-group "Hash-table-for-each"
    (let ((foreach-test-table
           (hash-table-unfold (lambda (c) (char>? c #\z))
                              (lambda (c) (values c (char-upcase c)))
                              (lambda (c) (integer->char (+ 1 (char->integer c))))
                              #\a
                              char-comparator
                              26))
          (out-port (open-output-string)))
      (test "Iterates"
          "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz"
        (begin
          (hash-table-for-each
           (lambda (k v)
             (display v out-port)
             (display k out-port))
           foreach-test-table)
          (get-output-string out-port)))))

  ;; Hash-table-map->list is used to test insertion order throughout
  ;; this test suite

  (test-group "Hash-table-fold"
    (let ((fold-test-table
           (hash-table-unfold (lambda (n) (> n 5))
                              (lambda (n) (values n (- n)))
                              (lambda (n) (+ n 1))
                              1
                              exact-integer-comparator)))
      (test "Folds in any order" '(15 . -120)
        (hash-table-fold (lambda (k v acc)
                           (cons (+ k (car acc))
                                 (* v (cdr acc))))
                         '(0 . 1)
                         fold-test-table))

      (test "Returns the seed when the hash table is empty" #f
        (hash-table-fold (lambda (k v acc)
                           #t)
                         #f
                         (make-hash-table exact-integer-comparator)))))

  (test-group "Hash-table-fold-left"
    (let ((fold-left-test-table
           (hash-table-unfold (lambda (c) (char>? c #\z))
                              (lambda (c) (values c (char-upcase c)))
                              (lambda (c) (integer->char (+ 1 (char->integer c))))
                              #\a
                              char-comparator
                              26)))
      (test "Folds from left to right"
          (reverse '("Aa" "Bb" "Cc" "Dd" "Ee" "Ff" "Gg" "Hh" "Ii"
                     "Jj" "Kk" "Ll" "Mm" "Nn" "Oo" "Pp" "Qq" "Rr"
                     "Ss" "Tt" "Uu" "Vv" "Ww" "Xx" "Yy" "Zz"))
        (hash-table-fold-left (lambda (acc k v)
                                (cons (string v k) acc))
                              '()
                              fold-left-test-table))

      (test "Returns the seed when the hash table is empty" #f
        (hash-table-fold (lambda (acc k v)
                           #t)
                         #f
                         (make-hash-table exact-integer-comparator)))))

  (test-group "Hash-table-fold-right"
    (let ((fold-right-test-table
           (hash-table-unfold (lambda (c) (char>? c #\z))
                              (lambda (c) (values c (char-upcase c)))
                              (lambda (c) (integer->char (+ 1 (char->integer c))))
                              #\a
                              char-comparator
                              26)))
      (test "Folds from right to left"
          '("Aa" "Bb" "Cc" "Dd" "Ee" "Ff" "Gg" "Hh" "Ii"
            "Jj" "Kk" "Ll" "Mm" "Nn" "Oo" "Pp" "Qq" "Rr"
            "Ss" "Tt" "Uu" "Vv" "Ww" "Xx" "Yy" "Zz")
        (hash-table-fold-right (lambda (k v acc)
                                 (cons (string v k) acc))
                               '()
                               fold-right-test-table))

      (test "Returns the seed when the hash table is empty" #f
        (hash-table-fold (lambda (k v acc)
                           #t)
                         #f
                         (make-hash-table exact-integer-comparator)))))

  (test-group "Hash-table-prune!"
    (let ((prune-test-table
           (hash-table-unfold (lambda (c) (char>? c #\x7F))
                              (lambda (c) (values c (char->integer c)))
                              (lambda (c) (integer->char (+ 1 (char->integer c))))
                              #\x00
                              char-comparator)))
      (test "Pruning" 42
        (hash-table-prune!
         (lambda (k v)
           (or (< v 32)
               (char-numeric? k)))
         prune-test-table))
      (test "Size is correct" 86 (hash-table-size prune-test-table))
      (let ((remaining-keys
             (append (string->list " !\"#$%&'()*+,-./:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~") '(#\x7F))))
        (for-each
         (lambda (c)
           (test (string-append "#\\x" (number->string (char->integer c)) " is still there")
               (char->integer c)
             (hash-table-ref/default prune-test-table c #f)))
         remaining-keys)
        (test "Everything’s still in order"
            (map (lambda (c) (cons c (char->integer c))) remaining-keys)
          (hash-table-map->list cons prune-test-table))
        (let loop ((more remaining-keys))
          (unless (null? more)
            (let ((key (car more)))
              (test (string-append "Pruning only \\x" (number->string (char->integer key))) 1
                (hash-table-prune! (lambda (k v) (eqv? k key))
                                   prune-test-table))
              (test "Size is correct" (length (cdr more))
                (hash-table-size prune-test-table))
              (test "Everything’s still in order"
                  (map (lambda (c) (cons c (char->integer c))) (cdr more))
                (hash-table-map->list cons prune-test-table)))
            (loop (cdr more))))))
    (test-group "Hash table is left in a consistent state when the procedure errors"
      (let
          ((broken-prune-test-table
            (hash-table-unfold (lambda (n) (>= n 10))
                               (lambda (n) (values n (number->string n)))
                               (lambda (n) (+ n 1))
                               0
                               exact-integer-comparator))
           (work-twice
            (let ((used 0))
              (lambda (k v)
                (if (< used 2)
                    (begin
                      (set! used (+ used 1))
                      #t)
                    (assertion-violation #f "broken!"))))))
        (test-assert "Pruning"
          (guard
              (exn ((assertion-violation? exn) #t)
                   (else #f))
            (hash-table-prune! work-twice broken-prune-test-table)))
        (test "Size is correct" 8
          (hash-table-size broken-prune-test-table))
        (test "0 is gone" #f
          (hash-table-ref/default broken-prune-test-table 0 #f))
        (test "1 is gone" #f
          (hash-table-ref/default broken-prune-test-table 0 #f))
        (for-each
         (lambda (k)
           (test (string-append (number->string k) " is still there")
               (number->string k)
             (hash-table-ref/default broken-prune-test-table k #f)))
         '(2 3 4 5 6 7 8 9))
        (test-values "Insertion order maintained"
            (values '#(2 3 4 5 6 7 8 9) '#("2" "3" "4" "5" "6" "7" "8" "9"))
          (hash-table-entries broken-prune-test-table))))
    (test-group "Can’t prune an immutable hash table"
      (let ((immutable-prune-test-table
             (hash-table-copy
              (hash-table-unfold (lambda (n) (>= n 10))
                                 (lambda (n) (values n (number->string n)))
                                 (lambda (n) (+ n 1))
                                 0
                                 exact-integer-comparator)
              #f)))
        (test-error "Can’t prune" assertion-violation?
          (hash-table-prune! (lambda (k v) (even? k)) immutable-prune-test-table))
        (test-error "Can’t prune even if the proc always returns #f" assertion-violation?
          (hash-table-prune! (lambda (k v) #f) immutable-prune-test-table))
        (test "Size is correct" 10
          (hash-table-size immutable-prune-test-table))
        (for-each
         (lambda (k)
           (test (string-append (number->string k) " is still there")
               (number->string k)
             (hash-table-ref/default immutable-prune-test-table k #f)))
         '(0 1 2 3 4 5 6 7 8 9))
        (test-values "Insertion order maintained"
            (values '#(0 1 2 3 4 5 6 7 8 9)
                    '#("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
          (hash-table-entries immutable-prune-test-table))))))


;; Copying and conversion
(test-group "Copying and conversion"
  (test-group "Hash-table-copy"
    ;; Hash-table-copy with mutable? set to #f is tested throughout
    ;; this test suite
    (let* ((copy-test-table
            (hash-table-unfold (lambda (n) (>= n 10))
                               (lambda (n) (values n (number->string n)))
                               (lambda (n) (+ n 1))
                               0
                               exact-integer-comparator))
           (copy2-test-table (hash-table-copy copy-test-table #t)))
      (test "Sizes are the same" (hash-table-size copy-test-table) (hash-table-size copy2-test-table))
      (test-values "Entries are the same" (hash-table-entries copy-test-table) (hash-table-entries copy2-test-table))
      (test "Mutating one copy’s entry doesn’t mutate the other" "0"
        (begin
          (hash-table-replace! copy-test-table 0 "zero")
          (hash-table-ref/default copy2-test-table 0 #f)))
      (test "Adding an entry to one copy doesn’t mutate the other" #f
        (begin
          (hash-table-add! copy-test-table 10 "10")
          (hash-table-ref/default copy2-test-table 10 #f)))
      (test "Mutating the other copy’s entry doesn’t mutate the first" "9"
        (begin
          (hash-table-replace! copy2-test-table 9 "nine")
          (hash-table-ref/default copy-test-table 9 #f)))
      (test "Adding an entry to the other copy doesn’t mutate the first" #f
        (begin
          (hash-table-add! copy2-test-table -1 "-1")
          (hash-table-ref/default copy-test-table -1 #f)))))

  (test-group "Hash-table-empty-copy"
    (let* ((copy-test-table
            (hash-table-unfold (lambda (n) (>= n 10))
                               (lambda (n) (values n (number->string n)))
                               (lambda (n) (+ n 1))
                               0
                               exact-integer-comparator))
           (empty-copy-test-table (hash-table-empty-copy copy-test-table)))
      (test "Size is correct" 0
        (hash-table-size empty-copy-test-table))
      (test-assert "It’s empty" (hash-table-empty? empty-copy-test-table))
      (hash-table-for-each
       (lambda (k v)
         (test (string-append (number->string k) " isn’t there") #f
           (hash-table-ref/default empty-copy-test-table k #f)))
       copy-test-table)
      (hash-table-for-each
       (lambda (k v)
         (test-assert (string-append (number->string k) " can be added")
           (begin
             (hash-table-add! empty-copy-test-table k #t)
             (hash-table-ref/default empty-copy-test-table k #f))))
       copy-test-table)))


  (test-group "Hash-table->alist"
    (let ((alist-test-table
           (hash-table-unfold (lambda (n) (>= n 10))
                              (lambda (n) (values n (number->string n)))
                              (lambda (n) (+ n 1))
                              0
                              exact-integer-comparator)))
      (test "Converts to an alist"
          '((9 . "9") (8 . "8") (7 . "7") (6 . "6") (5 . "5")
            (4 . "4") (3 . "3") (2 . "2") (1 . "1") (0 . "0"))
        (hash-table->alist alist-test-table)))))


;; Hash tables as sets

(test-group "Hash tables as sets"
  (test-group "Hash-table-union!"
    (let ((evens-table
           (hash-table-unfold (lambda (n) (>= n 10))
                              (lambda (n) (values n #t))
                              (lambda (n) (+ n 2))
                              0
                              exact-integer-comparator))
          (odds-table
           (hash-table-unfold (lambda (n) (>= n 10))
                              (lambda (n) (values n #f))
                              (lambda (n) (+ n 2))
                              1
                              exact-integer-comparator)))
      (chibi-test-equal eq? "Doing the union" evens-table
        (hash-table-union! evens-table odds-table))
      (test "Size is correct" 10
        (hash-table-size evens-table))
      (test "Insertion order"
          '#(0 2 4 6 8 1 3 5 7 9)
        (hash-table-keys evens-table))
      (for-each
       (lambda (k)
         (test (string-append (number->string k) " has the correct value")
             (even? k)
           (hash-table-ref/default evens-table k 'not-there)))
       '(0 1 2 3 4 5 6 7 8 9))
      (test-values "Odds table is the same"
          (values '#(1 3 5 7 9)
                  '#(#f #f #f #f #f))
        (hash-table-entries odds-table)))
    (let ((one-two-three (hash-table exact-integer-comparator
                                     1 'a
                                     2 'a
                                     3 'a))
          (three-four-five (hash-table exact-integer-comparator
                                       3 'b
                                       4 'b
                                       5 'b)))
      (chibi-test-equal eq? "Doing the union" one-two-three
        (hash-table-union! one-two-three three-four-five))
      (test "Size is correct" 5
        (hash-table-size one-two-three))
      (test "Insertion order" '#(1 2 3 4 5)
        (hash-table-keys one-two-three))
      (test "1 is a" 'a (hash-table-ref/default one-two-three 1 #f))
      (test "2 is a" 'a (hash-table-ref/default one-two-three 2 #f))
      (test "3 is a" 'a (hash-table-ref/default one-two-three 3 #f))
      (test "4 is b" 'b (hash-table-ref/default one-two-three 4 #f))
      (test "5 is b" 'b (hash-table-ref/default one-two-three 5 #f))))

  (test-group "Hash-table-intersection!"
    (let ((one-two-three (hash-table exact-integer-comparator
                                     1 'a
                                     2 'a
                                     3 'a))
          (three-four-five (hash-table exact-integer-comparator
                                       3 'b
                                       4 'b
                                       5 'b)))
      (chibi-test-equal eq? "Doing the intersection" one-two-three
        (hash-table-intersection! one-two-three three-four-five))
      (test "Size is correct" 1
        (hash-table-size one-two-three))
      (test "1 is gone" #f (hash-table-ref/default one-two-three 1 #f))
      (test "2 is gone" #f (hash-table-ref/default one-two-three 2 #f))
      (test "3 is a" 'a (hash-table-ref/default one-two-three 3 #f))
      (test "4 isn’t there" #f (hash-table-ref/default one-two-three 4 #f))
      (test "5 isn’t there" #f (hash-table-ref/default one-two-three 5 #f))
      (test-values "The other table is unaffected"
          (values '#(3 4 5)
                  '#(b b b))
        (hash-table-entries three-four-five))))

  (test-group "Hash-table-difference!"
    (let ((one-two-three (hash-table exact-integer-comparator
                                     1 'a
                                     2 'a
                                     3 'a))
          (three-four-five (hash-table exact-integer-comparator
                                       3 'b
                                       4 'b
                                       5 'b)))
      (chibi-test-equal eq? "Doing the difference" one-two-three
        (hash-table-difference! one-two-three three-four-five))
      (test "Size is correct" 2
        (hash-table-size one-two-three))
      (test "1 is a" 'a (hash-table-ref/default one-two-three 1 #f))
      (test "2 is a" 'a (hash-table-ref/default one-two-three 2 #f))
      (test "3 is gone" #f (hash-table-ref/default one-two-three 3 #f))
      (test "4 isn’t there" #f (hash-table-ref/default one-two-three 4 #f))
      (test "5 isn’t there" #f (hash-table-ref/default one-two-three 5 #f))
      (test-values "The other table is unaffected"
          (values '#(3 4 5)
                  '#(b b b))
        (hash-table-entries three-four-five))))

  (test-group "Hash-table-xor!"
    (let ((one-two-three (hash-table exact-integer-comparator
                                     1 'a
                                     2 'a
                                     3 'a))
          (three-four-five (hash-table exact-integer-comparator
                                       3 'b
                                       4 'b
                                       5 'b)))
      (chibi-test-equal eq? "Doing the xor" one-two-three
        (hash-table-xor! one-two-three three-four-five))
      (test "Size is correct" 4
        (hash-table-size one-two-three))
      (test "1 is a" 'a (hash-table-ref/default one-two-three 1 #f))
      (test "2 is a" 'a (hash-table-ref/default one-two-three 2 #f))
      (test "3 is gone" #f (hash-table-ref/default one-two-three 3 #f))
      (test "4 is b" 'b (hash-table-ref/default one-two-three 4 #f))
      (test "5 is b" 'b (hash-table-ref/default one-two-three 5 #f))
      (test-values "The other table is unaffected"
          (values '#(3 4 5)
                  '#(b b b))
        (hash-table-entries three-four-five)))))

;; local Variables:
;; eval: (put 'test 'scheme-indent-function 2)
;; eval: (put 'test-equal 'scheme-indent-function 'defun)
;; eval: (put 'test-error 'scheme-indent-function 'defun)
;; eval: (put 'test-group 'scheme-indent-function 1)
;; eval: (put 'test-values 'scheme-indent-function 2)
;; End:

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-250")
  (exit (if (zero? failures) 0 1)))
