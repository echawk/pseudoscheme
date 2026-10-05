;;; (srfi private char-set): SRFI 14 character sets over all of Unicode.
;;; Written for Pseudoscheme.  (srfi 14) re-exports it; (srfi 115) also
;;; uses char-set-ranges, to compile a char set into a cl-ppcre class.
;;;
;;; A char set is a record holding an inversion list: a vector of
;;; strictly increasing code points #(lo0 hi0 lo1 hi1 ...), standing for
;;; the union of the half-open ranges [lo, hi).  The universe is every
;;; Unicode scalar value, U+0000..U+10FFFF minus the surrogates
;;; U+D800..U+DFFF (which are not characters).  Set algebra is a merge
;;; of two inversion lists, membership a binary search, so sets as large
;;; as char-set:letter or complements cost a few hundred numbers.
;;;
;;; The standard sets follow the SRFI's Unicode definitions, by general
;;; category, taken from cl-unicode (a dependency of Pseudoscheme's
;;; R6RS runtime) in one pass over the code space when the library is
;;; loaded:
;;;   lower-case Ll; upper-case Lu; title-case Lt; letter L*; digit Nd;
;;;   punctuation P*; symbol S*; graphic L* M* N* P* S*;
;;;   whitespace Zs Zl Zp and U+0009..U+000D; blank Zs and U+0009;
;;;   printing graphic + whitespace; iso-control U+0000..U+001F and
;;;   U+007F..U+009F; hex-digit 0-9 A-F a-f; ascii U+0000..U+007F.
(define-library (srfi private char-set)
  (export char-set? char-set= char-set<= char-set-hash
          char-set-cursor char-set-ref char-set-cursor-next end-of-char-set?
          char-set-fold char-set-unfold char-set-unfold!
          char-set-for-each char-set-map
          char-set-copy char-set
          list->char-set string->char-set list->char-set! string->char-set!
          char-set-filter ucs-range->char-set ->char-set
          char-set-filter! ucs-range->char-set!
          char-set->list char-set->string
          char-set-size char-set-count char-set-contains?
          char-set-every char-set-any
          char-set-adjoin char-set-delete char-set-adjoin! char-set-delete!
          char-set-complement char-set-union char-set-intersection
          char-set-complement! char-set-union! char-set-intersection!
          char-set-difference char-set-xor char-set-diff+intersection
          char-set-difference! char-set-xor! char-set-diff+intersection!
          char-set:lower-case char-set:upper-case char-set:title-case
          char-set:letter char-set:digit char-set:letter+digit
          char-set:graphic char-set:printing char-set:whitespace
          char-set:iso-control char-set:punctuation char-set:symbol
          char-set:hex-digit char-set:blank char-set:ascii
          char-set:empty char-set:full
          ;; Not SRFI 14:
          char-set-ranges ranges->char-set char-set-general-category)
  (import (scheme base) (scheme case-lambda)
          (pseudoscheme lisp)
          (prefix (cl cl-unicode) u:))
  (begin

    (define-record-type <char-set>
      (make-cs ranges)
      char-set?
      (ranges cs-ranges set-cs-ranges!))

    ;; The inversion list of CS, a fresh vector (for (srfi 115)).
    (define (char-set-ranges cs) (vector-copy (ranges-of cs 'char-set-ranges)))
    ;; A char set from an inversion list (trusted: sorted and in range).
    (define (ranges->char-set v) (make-cs (vector-copy v)))

    (define (ranges-of cs who)
      (if (char-set? cs)
          (cs-ranges cs)
          (error "not a char-set" cs who)))

    (define (check-char c who)
      (if (char? c) (char->integer c) (error "not a character" c who)))

    (define universe (vector 0 #xD800 #xE000 #x110000))

    ;;; Inversion lists

    ;; Number of boundaries in V that are <= CP; CP is in the set iff it
    ;; is odd.
    (define (count<= v cp)
      (let loop ((lo 0) (hi (vector-length v)))
        (if (< lo hi)
            (let ((mid (quotient (+ lo hi) 2)))
              (if (<= (vector-ref v mid) cp)
                  (loop (+ mid 1) hi)
                  (loop lo mid)))
            lo)))

    (define (rv-contains? v cp) (odd? (count<= v cp)))

    ;; Merge inversion lists A and B under OP, a function of the two
    ;; membership booleans.
    (define (rv-merge a b op)
      (let ((la (vector-length a)) (lb (vector-length b)))
        (let loop ((i 0) (j 0) (ina #f) (inb #f) (cur (if (op #f #f) #t #f)) (out '()))
          (if (and (= i la) (= j lb))
              (list->vector (reverse out))
              (let* ((x (cond ((= i la) (vector-ref b j))
                              ((= j lb) (vector-ref a i))
                              (else (min (vector-ref a i) (vector-ref b j)))))
                     (ai (and (< i la) (= (vector-ref a i) x)))
                     (bj (and (< j lb) (= (vector-ref b j) x)))
                     (ina (if ai (not ina) ina))
                     (inb (if bj (not inb) inb))
                     (new (if (op ina inb) #t #f)))
                (loop (if ai (+ i 1) i) (if bj (+ j 1) j) ina inb new
                      (if (eq? new cur) out (cons x out))))))))

    (define (rv-union a b) (rv-merge a b (lambda (x y) (or x y))))
    (define (rv-intersection a b) (rv-merge a b (lambda (x y) (and x y))))
    (define (rv-difference a b) (rv-merge a b (lambda (x y) (and x (not y)))))
    (define (rv-xor a b) (rv-merge a b (lambda (x y) (not (eq? x y)))))
    (define (rv-complement a) (rv-difference universe a))

    ;; Sort a list of integers (merge sort).
    (define (sort-ints l)
      (define (merge a b)
        (cond ((null? a) b)
              ((null? b) a)
              ((<= (car a) (car b)) (cons (car a) (merge (cdr a) b)))
              (else (cons (car b) (merge a (cdr b))))))
      (define (split l a b)
        (if (null? l) (values a b) (split (cdr l) b (cons (car l) a))))
      (if (or (null? l) (null? (cdr l)))
          l
          (call-with-values (lambda () (split l '() '()))
            (lambda (a b) (merge (sort-ints a) (sort-ints b))))))

    ;; The inversion list of a list of code points.
    (define (codes->rv codes)
      (let loop ((l (sort-ints codes)) (out '()))
        (cond ((null? l) (list->vector (reverse out)))
              ((and (pair? out) (<= (car l) (car out)))
               ;; extends or repeats the last range [.., (car out))
               (loop (cdr l)
                     (if (= (car l) (car out)) (cons (+ (car l) 1) (cdr out)) out)))
              (else (loop (cdr l) (cons (+ (car l) 1) (cons (car l) out)))))))

    (define (chars->rv chars who)
      (codes->rv (map (lambda (c) (check-char c who)) chars)))

    (define (rv-size v)
      (let loop ((i 0) (n 0))
        (if (< i (vector-length v))
            (loop (+ i 2) (+ n (- (vector-ref v (+ i 1)) (vector-ref v i))))
            n)))

    ;; (proc code acc) over every code point of V, ascending.
    (define (rv-fold proc acc v)
      (let ranges ((i 0) (acc acc))
        (if (< i (vector-length v))
            (let ((hi (vector-ref v (+ i 1))))
              (let codes ((cp (vector-ref v i)) (acc acc))
                (if (< cp hi)
                    (codes (+ cp 1) (proc cp acc))
                    (ranges (+ i 2) acc))))
            acc)))

    ;; The first code point of V for which (pred char) is true, or #f;
    ;; returns (pred char)'s value with it.
    (define (rv-find pred v)
      (let ranges ((i 0))
        (if (< i (vector-length v))
            (let ((hi (vector-ref v (+ i 1))))
              (let codes ((cp (vector-ref v i)))
                (if (< cp hi)
                    (let ((r (pred (integer->char cp))))
                      (if r r (codes (+ cp 1))))
                    (ranges (+ i 2)))))
            #f)))

    ;;; Predicates and hashing

    (define (char-set= . sets)
      (or (null? sets)
          (let ((v (ranges-of (car sets) 'char-set=)))
            (let loop ((l (cdr sets)))
              (or (null? l)
                  (and (equal? v (ranges-of (car l) 'char-set=))
                       (loop (cdr l))))))))

    (define (char-set<= . sets)
      (or (null? sets)
          (let loop ((v (ranges-of (car sets) 'char-set<=)) (l (cdr sets)))
            (or (null? l)
                (let ((w (ranges-of (car l) 'char-set<=)))
                  (and (= 0 (vector-length (rv-difference v w)))
                       (loop w (cdr l))))))))

    (define char-set-hash
      (case-lambda
        ((cs) (char-set-hash cs 0))
        ((cs bound)
         (let ((bound (if (and (exact-integer? bound) (positive? bound)) bound 4194304))
               (v (ranges-of cs 'char-set-hash)))
           (let loop ((i 0) (h 0))
             (if (< i (vector-length v))
                 (loop (+ i 1) (modulo (+ (* h 37) (vector-ref v i)) 536870909))
                 (modulo h bound)))))))

    ;;; Cursors: a cursor is a code point in the set, or #f at the end.

    (define (char-set-cursor cs)
      (let ((v (ranges-of cs 'char-set-cursor)))
        (if (= 0 (vector-length v)) #f (vector-ref v 0))))

    (define (char-set-ref cs cursor)
      (if (and (exact-integer? cursor) (rv-contains? (ranges-of cs 'char-set-ref) cursor))
          (integer->char cursor)
          (error "char-set-ref: bad cursor" cursor)))

    (define (char-set-cursor-next cs cursor)
      (let* ((v (ranges-of cs 'char-set-cursor-next))
             (next (+ cursor 1))
             (n (count<= v next)))
        (cond ((odd? n) next)
              ((< n (vector-length v)) (vector-ref v n))
              (else #f))))

    (define (end-of-char-set? cursor) (not cursor))

    ;;; Iteration

    (define (char-set-fold kons knil cs)
      (rv-fold (lambda (cp acc) (kons (integer->char cp) acc))
               knil (ranges-of cs 'char-set-fold)))

    (define (char-set-for-each proc cs)
      (rv-fold (lambda (cp acc) (proc (integer->char cp)) acc)
               #f (ranges-of cs 'char-set-for-each))
      (if #f #f))

    (define (char-set-map proc cs)
      (make-cs (chars->rv (char-set-fold (lambda (c acc) (cons (proc c) acc))
                                         '() cs)
                          'char-set-map)))

    (define (char-set-count pred cs)
      (char-set-fold (lambda (c n) (if (pred c) (+ n 1) n)) 0 cs))

    (define (char-set-every pred cs)
      ;; the last (pred c), or #t for the empty set
      (let ((v (ranges-of cs 'char-set-every)))
        (let ranges ((i 0) (last #t))
          (if (< i (vector-length v))
              (let ((hi (vector-ref v (+ i 1))))
                (let codes ((cp (vector-ref v i)) (last last))
                  (if (< cp hi)
                      (let ((r (pred (integer->char cp))))
                        (and r (codes (+ cp 1) r)))
                      (ranges (+ i 2) last))))
              last))))

    (define (char-set-any pred cs)
      (rv-find pred (ranges-of cs 'char-set-any)))

    (define (char-set->list cs)
      (reverse (char-set-fold cons '() cs)))

    (define (char-set->string cs)
      (list->string (char-set->list cs)))

    (define (char-set-size cs) (rv-size (ranges-of cs 'char-set-size)))

    (define (char-set-contains? cs char)
      (rv-contains? (ranges-of cs 'char-set-contains?)
                    (check-char char 'char-set-contains?)))

    ;;; Construction

    (define (base-rv maybe-base who)
      (if (pair? maybe-base) (ranges-of (car maybe-base) who) (vector)))

    (define (char-set-copy cs) (make-cs (ranges-of cs 'char-set-copy)))

    (define (char-set . chars) (make-cs (chars->rv chars 'char-set)))

    (define (list->char-set chars . base)
      (make-cs (rv-union (chars->rv chars 'list->char-set)
                         (base-rv base 'list->char-set))))

    (define (list->char-set! chars base)
      (set-cs-ranges! base (rv-union (chars->rv chars 'list->char-set!)
                                     (ranges-of base 'list->char-set!)))
      base)

    (define (string->char-set s . base)
      (apply list->char-set (string->list s) base))

    (define (string->char-set! s base)
      (list->char-set! (string->list s) base))

    (define (char-set-unfold-list p f g seed)
      (let loop ((seed seed) (acc '()))
        (if (p seed) acc (loop (g seed) (cons (f seed) acc)))))

    (define (char-set-unfold p f g seed . base)
      (apply list->char-set (char-set-unfold-list p f g seed) base))

    (define (char-set-unfold! p f g seed base)
      (list->char-set! (char-set-unfold-list p f g seed) base))

    (define (filter-rv pred cs who)
      (codes->rv (rv-fold (lambda (cp acc)
                            (if (pred (integer->char cp)) (cons cp acc) acc))
                          '() (ranges-of cs who))))

    (define (char-set-filter pred cs . base)
      (make-cs (rv-union (filter-rv pred cs 'char-set-filter)
                         (base-rv base 'char-set-filter))))

    (define (char-set-filter! pred cs base)
      (set-cs-ranges! base (rv-union (filter-rv pred cs 'char-set-filter!)
                                     (ranges-of base 'char-set-filter!)))
      base)

    ;; [lo, hi) as an inversion list, minus what is not a character.
    ;; ERROR? true makes a non-character in the range an error.
    (define (range-rv lo hi error? who)
      (if (not (and (exact-integer? lo) (exact-integer? hi) (<= 0 lo hi)))
          (error "bad range" lo hi who))
      (let* ((v (if (< lo hi) (vector lo hi) (vector)))
             (r (rv-intersection v universe)))
        (if (and error? (not (equal? r v)))
            (error "range contains a non-character" lo hi who))
        r))

    (define (ucs-range->char-set lo hi . opts)
      (let ((error? (and (pair? opts) (car opts)))
            (base (if (and (pair? opts) (pair? (cdr opts))) (cdr opts) '())))
        (make-cs (rv-union (range-rv lo hi error? 'ucs-range->char-set)
                           (base-rv base 'ucs-range->char-set)))))

    (define (ucs-range->char-set! lo hi error? base)
      (set-cs-ranges! base (rv-union (range-rv lo hi error? 'ucs-range->char-set!)
                                     (ranges-of base 'ucs-range->char-set!)))
      base)

    (define (->char-set x)
      (cond ((char-set? x) x)
            ((string? x) (string->char-set x))
            ((char? x) (char-set x))
            (else (error "->char-set: not a char-set, string or character" x))))

    ;;; Algebra

    (define (char-set-adjoin cs . chars)
      (make-cs (rv-union (ranges-of cs 'char-set-adjoin)
                         (chars->rv chars 'char-set-adjoin))))
    (define (char-set-delete cs . chars)
      (make-cs (rv-difference (ranges-of cs 'char-set-delete)
                              (chars->rv chars 'char-set-delete))))
    (define (char-set-adjoin! cs . chars)
      (set-cs-ranges! cs (cs-ranges (apply char-set-adjoin cs chars)))
      cs)
    (define (char-set-delete! cs . chars)
      (set-cs-ranges! cs (cs-ranges (apply char-set-delete cs chars)))
      cs)

    (define (char-set-complement cs)
      (make-cs (rv-complement (ranges-of cs 'char-set-complement))))
    (define (char-set-complement! cs)
      (set-cs-ranges! cs (rv-complement (ranges-of cs 'char-set-complement!)))
      cs)

    (define (fold-sets op init sets who)
      (let loop ((v init) (l sets))
        (if (null? l) v (loop (op v (ranges-of (car l) who)) (cdr l)))))

    (define (char-set-union . sets)
      (make-cs (fold-sets rv-union (vector) sets 'char-set-union)))
    (define (char-set-intersection . sets)
      (make-cs (fold-sets rv-intersection universe sets 'char-set-intersection)))
    (define (char-set-difference cs . sets)
      (make-cs (fold-sets rv-difference (ranges-of cs 'char-set-difference)
                          sets 'char-set-difference)))
    (define (char-set-xor . sets)
      (make-cs (fold-sets rv-xor (vector) sets 'char-set-xor)))

    (define (char-set-diff+intersection cs . sets)
      (let ((v (ranges-of cs 'char-set-diff+intersection))
            (u (fold-sets rv-union (vector) sets 'char-set-diff+intersection)))
        (values (make-cs (rv-difference v u))
                (make-cs (rv-intersection v u)))))

    (define (update! cs result)
      (set-cs-ranges! cs (cs-ranges result))
      cs)

    (define (char-set-union! cs . sets)
      (update! cs (apply char-set-union cs sets)))
    (define (char-set-intersection! cs . sets)
      (update! cs (apply char-set-intersection cs sets)))
    (define (char-set-difference! cs . sets)
      (update! cs (apply char-set-difference cs sets)))
    (define (char-set-xor! cs . sets)
      (update! cs (apply char-set-xor cs sets)))
    (define (char-set-diff+intersection! cs1 cs2 . sets)
      (call-with-values
          (lambda () (apply char-set-diff+intersection cs1 cs2 sets))
        (lambda (d i)
          (update! cs1 d)
          (update! cs2 i)
          (values cs1 cs2))))

    ;;; The standard sets

    ;; An alist from general category name ("Lu", ...) to its inversion
    ;; list, computed in Lisp in one pass over the code space.
    ;; cl-unicode names the surrogates' category "CS"; they are not
    ;; characters here, and drop out by intersection with the universe.
    (define category-ranges
      (map (lambda (p) (cons (car p) (list->vector (cdr p))))
           (lisp
            (let ((h (make-hash-table :test (function equal)))
                  (prev '()))
              (dotimes (cp #x110001)
                (let ((cat (if (< cp #x110000) (u:general-category cp) '())))
                  (unless (equal cat prev)
                    (when prev (push cp (gethash prev h)))
                    (when cat (push cp (gethash cat h)))
                    (setq prev cat))))
              (loop for k being the hash-keys of h using (hash-value v)
                    collect (cons k (reverse v)))))))

    (define (categories . names)
      (let loop ((names names) (v (vector)))
        (if (null? names)
            (rv-intersection v universe)
            (let ((p (assoc (car names) category-ranges)))
              (loop (cdr names) (if p (rv-union v (cdr p)) v))))))

    ;; A char set of the code points in general category NAME ("Lu"),
    ;; or of a major class ("L"): not SRFI 14, for (srfi 115).
    (define (char-set-general-category name)
      (make-cs
       (if (= 1 (string-length name))
           (apply categories
                  (let loop ((l category-ranges) (acc '()))
                    (cond ((null? l) acc)
                          ((char=? (string-ref (caar l) 0) (string-ref name 0))
                           (loop (cdr l) (cons (caar l) acc)))
                          (else (loop (cdr l) acc)))))
           (categories name))))

    (define rv:letter (categories "Lu" "Ll" "Lt" "Lm" "Lo"))
    (define rv:digit (categories "Nd"))
    (define rv:punctuation (categories "Pc" "Pd" "Ps" "Pe" "Pi" "Pf" "Po"))
    (define rv:symbol (categories "Sm" "Sc" "Sk" "So"))
    (define rv:graphic
      (rv-union (rv-union rv:letter rv:punctuation)
                (rv-union rv:symbol (categories "Mn" "Mc" "Me" "Nd" "Nl" "No"))))
    (define rv:whitespace (rv-union (categories "Zs" "Zl" "Zp") (vector 9 14)))

    (define char-set:lower-case (make-cs (categories "Ll")))
    (define char-set:upper-case (make-cs (categories "Lu")))
    (define char-set:title-case (make-cs (categories "Lt")))
    (define char-set:letter (make-cs rv:letter))
    (define char-set:digit (make-cs rv:digit))
    (define char-set:letter+digit (make-cs (rv-union rv:letter rv:digit)))
    (define char-set:graphic (make-cs rv:graphic))
    (define char-set:printing (make-cs (rv-union rv:graphic rv:whitespace)))
    (define char-set:whitespace (make-cs rv:whitespace))
    (define char-set:iso-control (make-cs (vector 0 #x20 #x7F #xA0)))
    (define char-set:punctuation (make-cs rv:punctuation))
    (define char-set:symbol (make-cs rv:symbol))
    (define char-set:hex-digit (make-cs (vector 48 58 65 71 97 103)))
    (define char-set:blank (make-cs (rv-union (categories "Zs") (vector 9 10))))
    (define char-set:ascii (make-cs (vector 0 128)))
    (define char-set:empty (make-cs (vector)))
    (define char-set:full (make-cs universe))))
