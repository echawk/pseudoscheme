;;; SRFI 115: Scheme regular expressions (SREs).  Written for
;;; Pseudoscheme on cl-ppcre (Edi Weitz, BSD licence), which is loaded
;;; (through Quicklisp or ASDF) when this library is first imported.
;;;
;;; An SRE is compiled to a cl-ppcre parse tree (:sequence, :register,
;;; :greedy-repetition, ...), which cl-ppcre turns into a backtracking
;;; matcher.  Character sets are computed here, as SRFI 14 char sets
;;; over all of Unicode ((srfi private char-set)), and handed to cl-ppcre
;;; as a :char-class when small or as a (:property test) closure, a
;;; binary search over the set's inversion list, when large.  Case
;;; folding (w/nocase) is done here too, by expanding characters and
;;; char sets to all their case variants, so it follows the SRFI's
;;; rules for char sets (expansion at the terminals).
;;;
;;; The procedures that iterate over matches (regexp-fold,
;;; regexp-extract, regexp-split, regexp-partition, regexp-replace,
;;; regexp-replace-all) follow Alex Shinn's chibi-scheme implementation,
;;; the SRFI's sample implementation (BSD licence).
;;;
;;; Features: regexp-non-greedy, regexp-look-around, regexp-backrefs and
;;; regexp-unicode are all supported.  Differences from the SRFI:
;;; - Matching is backtracking (Perl-style, leftmost-first), not
;;;   leftmost-longest: (or "a" "ab") matches "a" of "ab".
;;; - grapheme, bog and eog approximate UAX #29 extended grapheme
;;;   clusters: CR LF, Hangul syllable sequences, regional indicator
;;;   pairs, and a base followed by marks (Mn, Mc, Me, ZWJ, emoji
;;;   modifiers); prepend characters and emoji ZWJ sequences are not
;;;   handled.
;;; - look-behind requires a fixed-length pattern (as the SRFI allows).
(define-library (srfi 115)
  (export regexp rx regexp->sre char-set->sre valid-sre? regexp?
          regexp-matches regexp-matches? regexp-search
          regexp-fold regexp-extract regexp-split regexp-partition
          regexp-replace regexp-replace-all
          regexp-match? regexp-match-count regexp-match-submatch
          regexp-match-submatch-start regexp-match-submatch-end
          regexp-match->list)
  (import (scheme base) (scheme char) (scheme cxr)
          (srfi private char-set)
          (pseudoscheme lisp)
          (prefix (cl common-lisp) cl:)
          (prefix (cl cl-unicode) u:)
          (prefix (cl cl-ppcre) re:))
  (begin

    ;;; ------------------------------------------------------------
    ;;; Lazily computed Unicode sets

    (define-syntax define-lazy
      (syntax-rules ()
        ((_ name expr)
         (define name
           (let ((value #f))
             (lambda ()
               (if (not value) (set! value expr))
               value))))))

    ;; The char set of the code points that have the Unicode property
    ;; NAME ("Lowercase", "Alphabetic", ...), per cl-unicode.  Only
    ;; assigned code points are tested.
    (define (property-char-set name)
      (let ((v (char-set-ranges
                (char-set-difference char-set:full (char-set-general-category "Cn")))))
        (ranges->char-set
         (list->vector
          (lisp
           (let ((test (u:property-test name)) (out '()) (open '()) (last 0))
             (do ((i 0 (cl:+ i 2))) ((cl:>= i (array-dimension v 0)))
               (do ((cp (svref v i) (cl:1+ cp))) ((cl:>= cp (svref v (cl:1+ i))))
                 (when (funcall test (code-char cp))
                   (if (and open (cl:= cp (cl:1+ last)))
                       (setq last cp)
                       (progn (when open (push (cl:1+ last) out))
                              (push cp out)
                              (setq open 1 last cp))))))
             (when open (push (cl:1+ last) out))
             (nreverse out)))))))

    (define-lazy u:lower (property-char-set "Lowercase"))
    (define-lazy u:upper (property-char-set "Uppercase"))
    (define-lazy u:alpha (property-char-set "Alphabetic"))
    (define-lazy u:control (char-set-general-category "C"))
    (define-lazy u:alnum (char-set-union (u:alpha) char-set:digit))
    (define-lazy u:graphic
      (char-set-union (u:alnum) char-set:punctuation char-set:symbol))
    (define-lazy u:printing (char-set-union (u:graphic) char-set:whitespace))
    (define-lazy u:word (char-set-adjoin (u:alnum) #\_))

    (define ascii:lower (ucs-range->char-set 97 123))
    (define ascii:upper (ucs-range->char-set 65 91))
    (define ascii:alpha (char-set-union ascii:lower ascii:upper))
    (define ascii:digit (ucs-range->char-set 48 58))
    (define ascii:alnum (char-set-union ascii:alpha ascii:digit))
    (define ascii:punct (string->char-set "!\"#%&'()*,-./:;?@[\\]_{}"))
    (define ascii:symbol (string->char-set "$+<=>^`|~"))
    (define ascii:graphic (char-set-union ascii:alnum ascii:punct ascii:symbol))
    (define ascii:space
      (char-set #\space #\tab #\newline #\return (integer->char 12)))
    (define ascii:printing (char-set-union ascii:graphic ascii:space))
    (define ascii:control (ucs-range->char-set 0 32))
    (define ascii:word (char-set-adjoin ascii:alnum #\_))
    (define nonl (char-set-delete char-set:full #\newline #\return))

    ;; Case classes: a vector, sorted by code point, of (cp . class) for
    ;; each code point that has case variants, CLASS being the list of
    ;; them all (itself included).  Two characters are variants when
    ;; lowercasing their uppercase gives the same character (so DŽ Dž dž,
    ;; and K k KELVIN SIGN, are each one class).  Computed in Lisp, from
    ;; cl-unicode's simple case mappings, on first use of w/nocase.
    (define-lazy case-classes
      (let ((classes
             (lisp
              (let ((h (make-hash-table)) (out '()))
                (dotimes (cp #x110000)
                  (unless (cl:<= #xD800 cp #xDFFF)
                    (let ((k (char-code (u:lowercase-mapping
                                         (u:uppercase-mapping (code-char cp))))))
                      (push cp (gethash k h)))))
                (maphash (lambda (k l)
                           (when (or (cl:cdr l) (cl:/= k (cl:car l)))
                             (push (if (cl:member k l) l (cl:cons k l)) out)))
                         h)
                out))))
        (list->vector
         (sort-pairs
          (apply append
                 (map (lambda (class) (map (lambda (cp) (cons cp class)) class))
                      classes))))))

    (define (sort-pairs l)
      (define (merge a b)
        (cond ((null? a) b)
              ((null? b) a)
              ((<= (caar a) (caar b)) (cons (car a) (merge (cdr a) b)))
              (else (cons (car b) (merge a (cdr b))))))
      (let sort ((l l))
        (if (or (null? l) (null? (cdr l)))
            l
            (let split ((l l) (a '()) (b '()))
              (if (null? l)
                  (merge (sort a) (sort b))
                  (split (cdr l) b (cons (car l) a)))))))

    ;; CS with all its characters' case variants added.
    (define (case-closure cs ascii?)
      (if ascii?
          (char-set-union
           cs
           (char-set-map char-downcase (char-set-intersection cs ascii:upper))
           (char-set-map char-upcase (char-set-intersection cs ascii:lower)))
          (let ((v (case-classes)))
            (let loop ((i 0) (extra '()))
              (if (< i (vector-length v))
                  (let ((p (vector-ref v i)))
                    (loop (+ i 1)
                          (if (char-set-contains? cs (integer->char (car p)))
                              (append (cdr p) extra)
                              extra)))
                  (char-set-union
                   cs (list->char-set (map integer->char extra))))))))

    ;;; ------------------------------------------------------------
    ;;; Hangul and grapheme-cluster sets (UAX #29, approximately)

    (define (ranges . bounds) (ranges->char-set (list->vector bounds)))
    (define-lazy g:l (ranges #x1100 #x1160 #xA960 #xA97D))
    (define-lazy g:v (ranges #x1160 #x11A8 #xD7B0 #xD7C7))
    (define-lazy g:t (ranges #x11A8 #x1200 #xD7CB #xD7FC))
    (define-lazy g:lv
      (let loop ((cp #xAC00) (l '()))
        (if (> cp #xD7A3)
            (list->char-set l)
            (loop (+ cp 28) (cons (integer->char cp) l)))))
    (define-lazy g:lvt
      (char-set-difference (ranges #xAC00 #xD7A4) (g:lv)))
    (define-lazy g:ri (ranges #x1F1E6 #x1F200))
    (define-lazy g:mark
      (char-set-union (char-set-general-category "M")
                      (ranges #x200D #x200E #x1F3FB #x1F400)))
    (define-lazy g:control
      (char-set-difference
       (char-set-union (char-set-general-category "Cc")
                       (char-set-general-category "Zl")
                       (char-set-general-category "Zp")
                       (char-set-general-category "Cf"))
       (char-set (integer->char #x200C) (integer->char #x200D))))

    (define-lazy grapheme-sre
      `(or (: (* ,(g:l)) (+ ,(g:v)) (* ,(g:t)) (* ,(g:mark)))
           (: (* ,(g:l)) ,(g:lv) (* ,(g:v)) (* ,(g:t)) (* ,(g:mark)))
           (: (* ,(g:l)) ,(g:lvt) (* ,(g:t)) (* ,(g:mark)))
           (: (+ ,(g:l)) (* ,(g:mark)))
           (: (+ ,(g:t)) (* ,(g:mark)))
           (: (= 2 ,(g:ri)) (* ,(g:mark)))
           "\r\n"
           (: ,(char-set-difference char-set:full (g:control)
                                    (char-set #\return #\newline))
              (* ,(g:mark)))
           ,(g:control)))

    ;; A position that is not a grapheme cluster boundary.
    (define-lazy grapheme-non-boundary-sre
      (let ((not-control (char-set-difference char-set:full (g:control)
                                              (char-set #\return #\newline))))
        `(or (: (look-behind #\return) #\newline)
             (: (look-behind ,not-control) ,(g:mark))
             (: (look-behind ,(g:l)) (or ,(g:l) ,(g:v) ,(g:lv) ,(g:lvt)))
             (: (look-behind (or ,(g:lv) ,(g:v))) (or ,(g:v) ,(g:t)))
             (: (look-behind (or ,(g:lvt) ,(g:t))) ,(g:t)))))

    ;;; ------------------------------------------------------------
    ;;; Char-set SREs

    (define (bar? x) (and (symbol? x) (string=? (symbol->string x) "|")))

    (define (named-cset name ascii?)
      (case name
        ((any) (if ascii? char-set:ascii char-set:full))
        ((nonl) (if ascii? (char-set-intersection nonl char-set:ascii) nonl))
        ((ascii) char-set:ascii)
        ((lower-case lower) (if ascii? ascii:lower (u:lower)))
        ((upper-case upper) (if ascii? ascii:upper (u:upper)))
        ((title-case title) (if ascii? char-set:empty char-set:title-case))
        ((alphabetic alpha) (if ascii? ascii:alpha (u:alpha)))
        ((numeric num digit) (if ascii? ascii:digit char-set:digit))
        ((alphanumeric alphanum alnum) (if ascii? ascii:alnum (u:alnum)))
        ((punctuation punct) (if ascii? ascii:punct char-set:punctuation))
        ((symbol) (if ascii? ascii:symbol char-set:symbol))
        ((graphic graph) (if ascii? ascii:graphic (u:graphic)))
        ((whitespace white space) (if ascii? ascii:space char-set:whitespace))
        ((printing print) (if ascii? ascii:printing (u:printing)))
        ((control cntrl) (if ascii? ascii:control (u:control)))
        ((hex-digit xdigit) char-set:hex-digit)
        (else #f)))

    (define (cset-sre? x)
      (cond ((char? x) #t)
            ((char-set? x) #t)
            ((string? x) (= 1 (string-length x)))
            ((symbol? x) (and (named-cset x #f) #t))
            ((and (pair? x) (list? x))
             (let ((op (car x)) (args (cdr x)))
               (cond ((string? op) (null? args))
                     ((bar? op) (every cset-sre? args))
                     (else
                      (case op
                        ((char-set) (and (= 1 (length args)) (string? (car args))))
                        ((/ char-range) (every range-spec? args))
                        ((or and & ~ complement) (every cset-sre? args))
                        ((- difference) (and (pair? args) (every cset-sre? args)))
                        ((w/case w/nocase w/ascii w/unicode)
                         (and (= 1 (length args)) (cset-sre? (car args))))
                        (else #f))))))
            (else #f)))

    (define (range-spec? x) (or (string? x) (char? x)))

    (define (every pred l)
      (or (null? l) (and (pred (car l)) (every pred (cdr l)))))

    ;; The char set of a cset-sre, in a context.
    (define (sre->cset x nocase? ascii?)
      (define (fold-case cs) (if nocase? (case-closure cs ascii?) cs))
      (define (universe) (if ascii? char-set:ascii char-set:full))
      (define (sub y) (sre->cset y nocase? ascii?))
      (cond ((char? x) (fold-case (char-set x)))
            ((char-set? x) (fold-case x))
            ((string? x) (fold-case (string->char-set x)))
            ((symbol? x)
             (let ((cs (named-cset x ascii?)))
               (if cs (fold-case cs) (error "not a char-set SRE" x))))
            ((string? (car x)) (fold-case (string->char-set (car x))))
            ((bar? (car x)) (apply char-set-union (map sub (cdr x))))
            (else
             (let ((args (cdr x)))
               (case (car x)
                 ((char-set) (fold-case (string->char-set (car args))))
                 ((/ char-range)
                  (let loop ((chars (apply append
                                           (map (lambda (s)
                                                  (if (string? s) (string->list s) (list s)))
                                                args)))
                             (cs char-set:empty))
                    (cond ((null? chars) (fold-case cs))
                          ((null? (cdr chars)) (error "odd number of chars in range" x))
                          (else
                           (loop (cddr chars)
                                 (char-set-union
                                  cs (ucs-range->char-set
                                      (char->integer (car chars))
                                      (+ 1 (char->integer (cadr chars))))))))))
                 ((or) (apply char-set-union (map sub args)))
                 ((and &) (apply char-set-intersection (universe) (map sub args)))
                 ((- difference) (apply char-set-difference (map sub args)))
                 ((~ complement)
                  (char-set-difference (universe) (apply char-set-union (map sub args))))
                 ((w/case) (sre->cset (car args) #f ascii?))
                 ((w/nocase) (sre->cset (car args) #t ascii?))
                 ((w/ascii) (sre->cset (car args) nocase? #t))
                 ((w/unicode) (sre->cset (car args) nocase? #f))
                 (else (error "not a char-set SRE" x)))))))

    ;; The cl-ppcre node matching one character of CS.
    (define (cset-node cs)
      (let* ((v (char-set-ranges cs))
             (n (vector-length v)))
        (cond ((and (= n 2) (= (vector-ref v 1) (+ 1 (vector-ref v 0))))
               (integer->char (vector-ref v 0)))
              ((and (> n 0) (<= n 16))
               (cons #:char-class
                     (let loop ((i 0))
                       (if (< i n)
                           (let ((lo (vector-ref v i)) (hi (- (vector-ref v (+ i 1)) 1)))
                             (cons (if (= lo hi)
                                       (integer->char lo)
                                       (list #:range (integer->char lo) (integer->char hi)))
                                   (loop (+ i 2))))
                           '()))))
              (else (ranges-node v)))))

    ;; (:property test), TEST a Lisp closure: binary search of the
    ;; inversion list V.
    (define (ranges-node v)
      (lisp
       (cl:list
        #:property
        (lambda (ch)
          (let ((cp (char-code ch)) (lo 0) (hi (array-dimension v 0)))
            (do () ((cl:>= lo hi))
              (let ((mid (ash (cl:+ lo hi) -1)))
                (if (cl:<= (svref v mid) cp)
                    (setq lo (cl:1+ mid))
                    (setq hi mid))))
            (oddp lo))))))

    ;;; ------------------------------------------------------------
    ;;; SRE -> cl-ppcre parse tree

    ;; Compile SRE.  Returns the tree, the number of registers, and an
    ;; alist (name . register) in register order.
    (define (compile-sre sre)
      (define count 0)
      (define names '())
      (define (new-register! name)
        (set! count (+ count 1))
        (if name (set! names (cons (cons name count) names)))
        count)
      (define (seq trees)
        (cond ((null? trees) #:void)
              ((null? (cdr trees)) (car trees))
              (else (cons #:sequence trees))))
      (define (count? n) (and (exact-integer? n) (>= n 0)))
      (define (comp x nocase? ascii? capture?)
        (define (sub y) (comp y nocase? ascii? capture?))
        (define (subs ys) (seq (map sub ys)))
        (define (cset y) (cset-node (sre->cset y nocase? ascii?)))
        (define (word-cs) (if ascii? ascii:word (u:word)))
        (define (bow) (list #:sequence
                            (list #:negative-lookbehind (cset-node (word-cs)))
                            (list #:positive-lookahead (cset-node (word-cs)))))
        (define (eow) (list #:sequence
                            (list #:positive-lookbehind (cset-node (word-cs)))
                            (list #:negative-lookahead (cset-node (word-cs)))))
        (define (repeat greedy? lo hi body)
          (list (if greedy? #:greedy-repetition #:non-greedy-repetition)
                lo (or hi '()) (subs body)))
        (cond
         ((string? x)
          (cond ((= 0 (string-length x)) #:void)
                ((not nocase?) x)
                (else (seq (map (lambda (c) (cset c)) (string->list x))))))
         ((or (char? x) (char-set? x)) (cset x))
         ((symbol? x)
          (case x
            ((bos) #:modeless-start-anchor)
            ((eos) #:modeless-end-anchor-no-newline)
            ((bol)
             (list #:alternation #:modeless-start-anchor
                   (list #:positive-lookbehind #\newline)
                   (list #:sequence (list #:positive-lookbehind #\return)
                         (list #:negative-lookahead #\newline))))
            ((eol)
             (list #:alternation #:modeless-end-anchor-no-newline
                   (list #:positive-lookahead #\return)
                   (list #:sequence (list #:positive-lookahead #\newline)
                         (list #:negative-lookbehind #\return))))
            ((bow) (bow))
            ((eow) (eow))
            ((nwb) (list #:negative-lookahead (list #:alternation (bow) (eow))))
            ((word) (sub '(word+ any)))
            ((bog eog)
             (if ascii?
                 #:void
                 (list #:negative-lookahead (comp (grapheme-non-boundary-sre) #f #f #f))))
            ((grapheme)
             (if ascii? (cset 'any) (comp (grapheme-sre) #f #f #f)))
            ((epsilon) #:void)
            (else
             (if (named-cset x ascii?)
                 (cset x)
                 (error "invalid SRE" x)))))
         ((and (pair? x) (list? x))
          (let ((op (car x)) (args (cdr x)))
            (cond
             ((string? op) (cset x))
             ((bar? op) (sub (cons 'or args)))
             (else
              (case op
                ((: seq) (subs args))
                ((or)
                 (cond ((null? args) (cset-node char-set:empty))
                       ((every cset-sre? args) (cset x))
                       ((null? (cdr args)) (sub (car args)))
                       (else (cons #:alternation (map sub args)))))
                ((* zero-or-more) (repeat #t 0 #f args))
                ((+ one-or-more) (repeat #t 1 #f args))
                ((? optional) (repeat #t 0 1 args))
                ((*? non-greedy-zero-or-more) (repeat #f 0 #f args))
                ((?? non-greedy-optional) (repeat #f 0 1 args))
                ((= exactly)
                 (if (count? (car args))
                     (repeat #t (car args) (car args) (cdr args))
                     (error "invalid SRE" x)))
                ((>= at-least)
                 (if (count? (car args))
                     (repeat #t (car args) #f (cdr args))
                     (error "invalid SRE" x)))
                ((** repeated **? non-greedy-repeated)
                 (let ((lo (car args)) (hi (cadr args)))
                   (if (and (count? lo) (or (not hi) (and (count? hi) (<= lo hi))))
                       (repeat (memq op '(** repeated)) lo hi (cddr args))
                       (error "invalid SRE" x))))
                (($ submatch)
                 (if capture?
                     (let ((n (new-register! #f)))
                       (list #:register (subs args)))
                     (subs args)))
                ((-> submatch-named)
                 (if (not (symbol? (car args))) (error "invalid SRE" x))
                 (if capture?
                     (let ((n (new-register! (car args))))
                       (list #:register (subs (cdr args))))
                     (subs (cdr args))))
                ((backref)
                 (let* ((ref (car args))
                        (n (cond ((exact-integer? ref) ref)
                                 ((assq ref (reverse names)) => cdr)
                                 (else (error "backref to unknown submatch" x)))))
                   (if (or (< n 1) (> n count))
                       (error "backref to unknown submatch" x))
                   (list #:back-reference n)))
                ((w/case) (seq (map (lambda (y) (comp y #f ascii? capture?)) args)))
                ((w/nocase) (seq (map (lambda (y) (comp y #t ascii? capture?)) args)))
                ((w/ascii) (seq (map (lambda (y) (comp y nocase? #t capture?)) args)))
                ((w/unicode) (seq (map (lambda (y) (comp y nocase? #f capture?)) args)))
                ((w/nocapture) (seq (map (lambda (y) (comp y nocase? ascii? #f)) args)))
                ((look-ahead) (list #:positive-lookahead (subs args)))
                ((neg-look-ahead) (list #:negative-lookahead (subs args)))
                ((look-behind) (list #:positive-lookbehind (subs args)))
                ((neg-look-behind) (list #:negative-lookbehind (subs args)))
                ((word) (seq (append (list (bow)) (map sub args) (list (eow)))))
                ((word+)
                 (sub `(word (+ (and (or alphanumeric "_") (or ,@args))))))
                (else
                 (if (cset-sre? x) (cset x) (error "invalid SRE" x))))))))
         (else (error "invalid SRE" x))))
      (let ((tree (comp sre #f #f #t)))
        (values tree count (reverse names))))

    ;;; ------------------------------------------------------------
    ;;; Regexps and matches

    (define-record-type regexp-type
      (make-regexp sre searcher matcher count names)
      regexp?
      (sre regexp->sre)
      (searcher regexp-searcher)
      (matcher regexp-matcher)
      (count regexp-count)
      (names regexp-names))

    (define (regexp re)
      (if (regexp? re)
          re
          (call-with-values (lambda () (compile-sre re))
            (lambda (tree count names)
              (make-regexp
               re
               (re:create-scanner (list #:sequence tree))
               (re:create-scanner (list #:sequence #:modeless-start-anchor tree
                                        #:modeless-end-anchor-no-newline))
               count names)))))

    (define-syntax rx
      (syntax-rules ()
        ((_ sre ...) (regexp `(: sre ...)))))

    (define (valid-sre? x)
      (call/cc
       (lambda (k)
         (with-exception-handler
          (lambda (e) (k #f))
          (lambda () (regexp x) #t)))))

    ;; The SRE (/ c1 c2 ...) of a char set, ranges as character pairs.
    (define (char-set->sre cs)
      (let ((v (char-set-ranges cs)))
        (if (= 0 (vector-length v))
            '(or)
            (cons '/
                  (let loop ((i 0))
                    (if (< i (vector-length v))
                        (cons (integer->char (vector-ref v i))
                              (cons (integer->char (- (vector-ref v (+ i 1)) 1))
                                    (loop (+ i 2))))
                        '()))))))

    (define-record-type regexp-match-type
      (make-regexp-match string starts ends names)
      regexp-match?
      (string match-string)
      (starts match-starts)
      (ends match-ends)
      (names match-names))

    ;; Run SCANNER over STR[start, end); REAL-START is the start for bos
    ;; and look-behind (regexp-fold's later searches).
    (define (run rx scanner str start end real-start)
      (if (not (and (exact-integer? start) (exact-integer? end)
                    (<= 0 start end (string-length str))))
          (error "regexp: bad start or end" start end))
      (call-with-values
          (lambda ()
            (re:scan scanner str #:start start #:end end #:real-start-pos real-start))
        ;; one value, NIL, when there is no match
        (lambda (ms . more)
          (and (exact-integer? ms)
               (let ((n (regexp-count rx)))
                 (let ((me (car more)) (rs (cadr more)) (re (caddr more))
                       (starts (make-vector (+ n 1) #f))
                       (ends (make-vector (+ n 1) #f)))
                   (vector-set! starts 0 ms)
                   (vector-set! ends 0 me)
                   (do ((i 0 (+ i 1))) ((= i n))
                     (let ((s (vector-ref rs i)))
                       (when (exact-integer? s)
                         (vector-set! starts (+ i 1) s)
                         (vector-set! ends (+ i 1) (vector-ref re i)))))
                   (make-regexp-match str starts ends (regexp-names rx))))))))

    (define (start-arg o) (if (and (pair? o) (car o)) (car o) 0))
    (define (end-arg str o)
      (if (and (pair? o) (pair? (cdr o)) (cadr o)) (cadr o) (string-length str)))

    (define (regexp-matches re str . o)
      (let ((rx (regexp re)) (start (start-arg o)))
        (run rx (regexp-matcher rx) str start (end-arg str o) start)))

    (define (regexp-matches? re str . o)
      (and (apply regexp-matches re str o) #t))

    (define (regexp-search re str . o)
      (let ((rx (regexp re)) (start (start-arg o)))
        (run rx (regexp-searcher rx) str start (end-arg str o) start)))

    ;; The index of submatch FIELD, or #f if there is none (an error
    ;; the SRFI leaves unspecified; this follows the sample
    ;; implementation, whose accessors then return #f).
    (define (field-index m field)
      (cond ((exact-integer? field)
             (and (< -1 field (vector-length (match-starts m))) field))
            ((symbol? field)
             ;; the first submatch of that name which matched
             (let loop ((l (match-names m)) (found #f))
               (cond ((null? l) found)
                     ((eq? (caar l) field)
                      (if (vector-ref (match-starts m) (cdar l))
                          (cdar l)
                          (loop (cdr l) (or found (cdar l)))))
                     (else (loop (cdr l) found)))))
            (else (error "regexp-match: bad submatch field" field))))

    (define (submatch-ref v m field)
      (let ((i (field-index m field)))
        (and i (vector-ref v i))))

    (define (regexp-match-submatch-start m field)
      (submatch-ref (match-starts m) m field))

    (define (regexp-match-submatch-end m field)
      (submatch-ref (match-ends m) m field))

    (define (regexp-match-submatch m field)
      (let ((s (regexp-match-submatch-start m field)))
        (and s (substring (match-string m) s (regexp-match-submatch-end m field)))))

    (define (regexp-match-count m) (- (vector-length (match-starts m)) 1))

    (define (regexp-match->list m)
      (let loop ((i (regexp-match-count m)) (acc '()))
        (if (< i 0) acc (loop (- i 1) (cons (regexp-match-submatch m i) acc)))))

    ;;; ------------------------------------------------------------
    ;;; Iterating over matches (after chibi-scheme's (chibi regexp))

    (define (regexp-fold re kons knil str . o)
      (let* ((rx (regexp re))
             (finish (if (pair? o) (car o) (lambda (from m str acc) acc)))
             (o (if (pair? o) (cdr o) o))
             (start (start-arg o))
             (end (end-arg str o)))
        (let lp ((i start) (from start) (acc knil))
          (let ((m (and (< i end) (run rx (regexp-searcher rx) str i end start))))
            (if m
                (let ((j (regexp-match-submatch-end m 0)))
                  (lp (if (and (= i j) (< j end)) (+ j 1) j)
                      j
                      (kons from m str acc)))
                (finish from #f str acc))))))

    (define (regexp-extract re str . o)
      (apply regexp-fold re
             (lambda (from m str a)
               (let ((s (regexp-match-submatch m 0)))
                 (if (equal? s "") a (cons s a))))
             '()
             str
             (lambda (from m str a) (reverse a))
             o))

    (define (regexp-split re str . o)
      (let ((start (start-arg o)) (end (end-arg str o)))
        (regexp-fold
         re
         (lambda (from m str a)
           (let ((i (regexp-match-submatch-start m 0))
                 (j (regexp-match-submatch-end m 0)))
             (if (= i j)
                 a
                 (cons j (cons (substring str (car a) i) (cdr a))))))
         (cons start '())
         str
         (lambda (from m str a)
           (reverse (cons (substring str (car a) end) (cdr a))))
         start
         end)))

    (define (regexp-partition re str . o)
      (let ((start (start-arg o)) (end (end-arg str o)))
        (define (kons from m str a)
          (let ((i (regexp-match-submatch-start m 0))
                (j (regexp-match-submatch-end m 0)))
            (if (= i j)
                a
                (cons j (cons (regexp-match-submatch m 0)
                              (cons (substring str (car a) i) (cdr a)))))))
        (define (final from m str a)
          (if (or (< (car a) end) (null? (cdr a)))
              (cons (substring str (car a) end) (cdr a))
              (cdr a)))
        (reverse (regexp-fold re kons (cons start '()) str final start end))))

    ;; The pieces (in reverse) of substitution SUBST for match M.
    (define (apply-subst m str subst start end)
      (let lp ((ls (if (pair? subst) subst (list subst))) (res '()))
        (cond
         ((null? ls) res)
         ((or (exact-integer? (car ls)) (and (symbol? (car ls))
                                             (not (memq (car ls) '(pre post)))))
          (lp (cdr ls) (cons (or (regexp-match-submatch m (car ls)) "") res)))
         ((eq? (car ls) 'pre)
          (lp (cdr ls) (cons (substring str start (regexp-match-submatch-start m 0)) res)))
         ((eq? (car ls) 'post)
          (lp (cdr ls) (cons (substring str (regexp-match-submatch-end m 0) end) res)))
         ((procedure? (car ls)) (lp (cdr ls) (cons ((car ls) m) res)))
         ((string? (car ls)) (lp (cdr ls) (cons (car ls) res)))
         (else (error "regexp-replace: bad substitution" (car ls))))))

    (define (string-concatenate-reverse l)
      (apply string-append (reverse l)))

    (define (regexp-replace re str subst . o)
      (let* ((rx (regexp re))
             (start (start-arg o))
             (end (end-arg str o))
             (count (if (and (pair? o) (pair? (cdr o)) (pair? (cddr o))) (caddr o) 0)))
        (let lp ((i start) (count count))
          (let ((m (and (<= i end) (run rx (regexp-searcher rx) str i end start))))
            (cond
             ((not m) (substring str start end))
             ((positive? count)
              (let ((j (regexp-match-submatch-end m 0)))
                (lp (if (= j (regexp-match-submatch-start m 0)) (+ j 1) j)
                    (- count 1))))
             (else
              (string-concatenate-reverse
               (cons (substring str (regexp-match-submatch-end m 0) end)
                     (append (apply-subst m str subst start end)
                             (list (substring str start
                                              (regexp-match-submatch-start m 0))))))))))))

    (define (regexp-replace-all re str subst . o)
      (let ((start (start-arg o)) (end (end-arg str o)))
        (regexp-fold
         re
         (lambda (i m str acc)
           (let ((m-start (regexp-match-submatch-start m 0)))
             (append (apply-subst m str subst start end)
                     (if (>= i m-start) acc (cons (substring str i m-start) acc)))))
         '()
         str
         (lambda (i m str acc)
           (string-concatenate-reverse
            (if (>= i end) acc (cons (substring str i end) acc))))
         start
         end)))))
