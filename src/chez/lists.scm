;;; -*- Mode: Scheme -*-
;;;; (chezscheme): lists, vectors, strings, symbols and paths beyond
;;;; R6RS, as Chez Scheme 10 has them.  Part of the library's body
;;;; (src/chez/chez.lisp).

;;; Lists

(define (sort pred l) (list-sort pred l))
(define (sort! pred l) (list-sort pred l))

(define (merge pred a b)
  (cond ((null? a) b)
        ((null? b) a)
        ((pred (car b) (car a)) (cons (car b) (merge pred a (cdr b))))
        (else (cons (car a) (merge pred (cdr a) b)))))
(define (merge! pred a b) (merge pred a b))

(define list* cons*)

;; andmap and ormap: for-all and exists, which already return the last
;; value and the first true one
(define andmap for-all)
(define ormap exists)

(define (append! . lists) (apply append lists))
(define (reverse! l) (reverse l))
(define (list-copy l) (map (lambda (x) x) l))
(define (enumerate l) (iota (length l)))

(define (make-subst same?)
  (lambda (new old tree)
    (let walk ((x tree))
      (cond ((same? x old) new)
            ((pair? x) (cons (walk (car x)) (walk (cdr x))))
            (else x)))))
(define subst (make-subst equal?))
(define substq (make-subst eq?))
(define substv (make-subst eqv?))
(define subst! subst)
(define substq! substq)
(define substv! substv)

;;; Vectors and strings

(define vector-copy
  (case-lambda
    ((v) (vector-copy v 0 (vector-length v)))
    ((v start) (vector-copy v start (vector-length v)))
    ((v start end)
     (let ((new (make-vector (- end start))))
       (do ((i start (+ i 1))) ((= i end) new)
         (vector-set! new (- i start) (vector-ref v i)))))))

(define (vector-append . vs)
  (list->vector (apply append (map vector->list vs))))

(define (vector-set/copy v i x)
  (let ((new (vector-copy v)))
    (vector-set! new i x)
    new))

;; Immutable vectors, strings and bytevectors aren't a separate kind
;; here: the conversions copy, and everything is mutable.
(define (vector->immutable-vector v) v)
(define (string->immutable-string s) s)
(define (bytevector->immutable-bytevector bv) bv)
(define (immutable-vector . xs) (list->vector xs))
(define (mutable-vector? x) (vector? x))
(define (immutable-vector? x) #f)
(define (mutable-string? x) (string? x))
(define (immutable-string? x) #f)
(define (mutable-bytevector? x) (bytevector? x))
(define (immutable-bytevector? x) #f)

(define (substring-fill! s start end c)
  (do ((i start (+ i 1))) ((= i end)) (string-set! s i c)))

(define (char- a b) (- (char->integer a) (char->integer b)))

;;; Symbols: gensyms and property lists

(define (gensym->unique-string g) (symbol->string g))
(define (gensym? x) (and (symbol? x) (%chez:uninterned-symbol? x)))
(define string->uninterned-symbol %chez:string->uninterned-symbol)
(define uninterned-symbol? gensym?)

(define property-lists (make-eq-hashtable))
(define (property-list s) (hashtable-ref property-lists s '()))
(define (getprop s key . default)
  (let ((p (assq key (property-list s))))
    (cond (p (cdr p))
          ((pair? default) (car default))
          (else #f))))
(define (putprop s key value)
  (let ((p (assq key (property-list s))))
    (if p
        (set-cdr! p value)
        (hashtable-set! property-lists s (cons (cons key value) (property-list s))))))
(define (remprop s key)
  (hashtable-set! property-lists s
                  (remp (lambda (p) (eq? (car p) key)) (property-list s))))

;;; Paths, as Chez's path procedures take them apart: separators are /,
;;; and runs of them count as one.

(define (directory-separator? c) (char=? c #\/))

(define (path-absolute? p)
  (and (> (string-length p) 0)
       (memv (string-ref p 0) '(#\/ #\~))
       #t))

(define (last-separator p)
  (let loop ((i (- (string-length p) 1)))
    (cond ((< i 0) #f)
          ((directory-separator? (string-ref p i)) i)
          (else (loop (- i 1))))))

(define (first-separator p)
  (let loop ((i 0))
    (cond ((= i (string-length p)) #f)
          ((directory-separator? (string-ref p i)) i)
          (else (loop (+ i 1))))))

(define (skip-separators-back p i)
  ;; the index after the last non-separator before I, at least 1 if P
  ;; starts with one (so "/" stays)
  (let loop ((j i))
    (cond ((and (> j 0) (directory-separator? (string-ref p (- j 1)))) (loop (- j 1)))
          ((and (= j 0) (> (string-length p) 0) (directory-separator? (string-ref p 0))) 1)
          (else j))))

(define (path-last p)
  (let ((i (last-separator p)))
    (if i (substring p (+ i 1) (string-length p)) p)))

(define (path-parent p)
  (let ((i (last-separator p)))
    (if i (substring p 0 (skip-separators-back p i)) "")))

(define (path-first p)
  (cond ((path-absolute? p)
         (let ((i (first-separator p)))
           (cond ((not i) p)
                 ((= i 0) "/")
                 (else (substring p 0 i)))))
        (else
         (let ((i (first-separator p)))
           (if i (substring p 0 i) "")))))

(define (path-rest p)
  (let ((i (first-separator p)))
    (if i
        (let loop ((j (+ i 1)))
          (if (and (< j (string-length p)) (directory-separator? (string-ref p j)))
              (loop (+ j 1))
              (substring p j (string-length p))))
        p)))

(define (extension-dot p)
  ;; the index of the dot before the extension, in the last component
  (let ((start (let ((i (last-separator p))) (if i (+ i 1) 0))))
    (let loop ((i (- (string-length p) 1)))
      (cond ((< i start) #f)
            ((char=? (string-ref p i) #\.) i)
            (else (loop (- i 1)))))))

(define (path-extension p)
  (let ((i (extension-dot p)))
    (if i (substring p (+ i 1) (string-length p)) "")))

(define (path-root p)
  (let ((i (extension-dot p)))
    (if i (substring p 0 i) p)))

(define (path-build a b)
  (cond ((string=? a "") b)
        ((directory-separator? (string-ref a (- (string-length a) 1))) (string-append a b))
        (else (string-append a "/" b))))
;;; Characters: the grapheme-cluster break property (UAX 29), from the
;;; general category and a few ranges.  Chez's answer differs only for
;;; the rare Prepend characters, which come out Other.

(define (char-grapheme-break-property c)
  (let ((n (char->integer c)))
    (cond ((= n 13) 'CR)
          ((= n 10) 'LF)
          ((= n #x200D) 'ZWJ)
          ((<= #x1F1E6 n #x1F1FF) 'Regional_Indicator)
          ((<= #x1100 n #x115F) 'L)
          ((<= #xA960 n #xA97C) 'L)
          ((<= #x1160 n #x11A7) 'V)
          ((<= #xD7B0 n #xD7C6) 'V)
          ((<= #x11A8 n #x11FF) 'T)
          ((<= #xD7CB n #xD7FB) 'T)
          ((<= #xAC00 n #xD7A3) (if (zero? (mod (- n #xAC00) 28)) 'LV 'LVT))
          ((= n #x200C) 'Extend)
          ((<= #xFE00 n #xFE0F) 'Extend)
          ((<= #x1F3FB n #x1F3FF) 'Extend)
          (else
           (case (char-general-category c)
             ((Mn Me) 'Extend)
             ((Mc) 'SpacingMark)
             ((Cc Zl Zp Cf) 'Control)
             (else 'Other))))))

(define (char-extended-pictographic? c)
  (let ((n (char->integer c)))
    (or (<= #x1F300 n #x1FAFF) (<= #x2600 n #x27BF) (<= #x1F000 n #x1F2FF)
        (memv n '(#xA9 #xAE #x203C #x2049 #x2122 #x2139 #x231A #x231B #x2328 #x23CF)))))

;;; Bytevectors

(define bytevector %bytevector)
(define (bytevector-truncate! bv n)
  (let ((new (make-bytevector n)))
    (bytevector-copy! bv 0 new 0 n)
    new))

;;; fxvectors: vectors of fixnums (Lisp's (simple-array fixnum (*))),
;;; written #vfx(...)

(define fxvector? %chez:fxvector?)
(define make-fxvector %chez:make-fxvector)
(define fxvector %chez:fxvector)
(define list->fxvector %chez:list->fxvector)
(define fxvector-length %chez:fxvector-length)
(define fxvector-ref %chez:fxvector-ref)
(define fxvector-set! %chez:fxvector-set!)
(define fxvector->list %chez:fxvector->list)
(define fxvector-fill! %chez:fxvector-fill!)
(define fxvector-copy %chez:fxvector-copy)
