;;; -*- Mode: Scheme -*-
;;;; (quasischeme chez ftype-helpers): Chez Scheme's ftypes
;;;; (src/chez/ftype.scm) -- the descriptors of ftypes, their layouts, the
;;;; code the accessors expand into and the procedures that code calls.  A
;;;; library of its own, which (chezscheme) imports (src/chez/chez.lisp),
;;;; because a transformer can't use definitions of the library it is
;;;; defined in.
;;;;
;;;; An ftype is a descriptor (ftd), registered under a number as it is
;;;; made.  define-ftype binds each name to a macro that, used as
;;;; (name k arg ...), expands into (k number arg ...): ftype-ref and the
;;;; others use their ftype name this way (ft-cps) to find the number, from
;;;; which the transformers here look up the descriptor in the table
;;;; (ft-vec), and the code they make names descriptors by number.  The
;;;; native names (integer-8, double, ...) are numbers of their own.  The
;;;; table is of this process, as the expansion is: code expanded from
;;;; define-ftype can't be saved and loaded in another.
;;;;
;;;; Memory is a base and an offset: an integer address, or a bytevector
;;;; (what a scheme-object pointer points into) read by the host's
;;;; chez:ftype-... procedures (src/chez/ftype.lisp).
;;;;
;;;; Not here: the reading of Scheme objects from memory (an
;;;; ftype-scheme-object-pointer stored in memory can only be null), and
;;;; the stable address of a bytevector (ftype-pointer-address of a pointer
;;;; into one is its address now).

(library (quasischeme chez ftype-helpers)
  (export ft-cps ft-define-start ft-define-finish ft-expand ft-sizeof-expand
          ft-make-pointer ft-pointer-of? ft-make-sop ft-check-fptr
          ft-base ft-base-off ft-any-base ft-make-ref
          ft-scalar-ref ft-scalar-set! ft-bitfield-ref ft-bitfield-set!
          ft-pointer-ref ft-pointer-set! ft-function-ref ft-read-pointer
          ft-check-array-index ft-check-pointer-index ft-check-function-index
          ft-check-offset ft-locked-add! ft-init-lock! ft-lock! ft-spin-lock!
          ft-unlock!
          ft-pointer? ft-pointer-address ft-pointer-null? ft-pointer=?
          ft-pointer-ftype ft-pointer->sexpr ft-sop? ft-sop-object ft-sop-offset)
  (import (rnrs) (rnrs mutable-pairs) (rnrs r5rs) (prefix (quasischeme chez host) %))

  ;; the identifier whose context the expansions' code is in: this library's
  (define ft-ctx-id #'here)

  (define native-endian 'little)

  (define (ft-err who msg . irritants)
    (apply assertion-violation who msg irritants))

  (define (ft-round-up n a) (* a (quotient (+ n a -1) a)))

  ;; the largest power of two, 8 at most, that divides N
  (define (ft-odd-align n)
    (let loop ((a 8))
      (if (zero? (mod n a)) a (loop (quotient a 2)))))

  (define (ft-check-size who size)
    (unless (<= size (greatest-fixnum))
      (ft-err who "non-fixnum overall size for ftype" size)))

  ;; ----------------------------------------------------------------
  ;; Descriptors
  ;;
  ;; kind      what A, B and C hold
  ;; scalar    A the name (integer-32, double, ...), B the class (sint, uint,
  ;;           float, char, bool, fixnum, fptr, sop)
  ;; pointer   A the number of the target's descriptor, B its sexpr
  ;; struct,   A the members, each (name descriptor offset sexpr), B the names
  ;; union     that can be accessed, each (name descriptor . offset), as
  ;;           members of anonymous members are
  ;; array     A the element's descriptor, B the length (0: unbounded), C its sexpr
  ;; bits      A the fields, each (name bitfield), B the names, each
  ;;           (name . bitfield), C the unit's size in bytes
  ;; bitfield  A the unit's size, B (shift . width), C whether it is signed
  ;; function  A the conventions, B the argument specs, C the result spec,
  ;;           each spec (foreign-type wrapping)
  ;;
  ;; SEXPR is what ftype-pointer-ftype returns; PARENTS are the descriptors
  ;; of the pointer types a type made by define-ftype with a uid is a
  ;; subtype of; ENDIAN is little or big.

  (define-record-type ftd
    (fields (mutable id) kind (mutable sexpr) size align endian a b c
            (mutable uid) (mutable parents)))

  (define ft-vec (make-vector 64 #f))
  (define ft-count 0)

  ;; a new number
  (define (ft-reserve!)
    (when (= ft-count (vector-length ft-vec))
      (let ((v (make-vector (* 2 ft-count) #f)))
        (do ((i 0 (+ i 1))) ((= i ft-count))
          (vector-set! v i (vector-ref ft-vec i)))
        (set! ft-vec v)))
    (let ((i ft-count))
      (set! ft-count (+ i 1))
      i))

  (define (ft-new kind sexpr size align endian a b c)
    (let* ((id (ft-reserve!))
           (d (make-ftd id kind sexpr size align endian a b c #f '())))
      (vector-set! ft-vec id d)
      d))

  (define (ft-desc id) (vector-ref ft-vec id))

  (define native-specs
    '((integer-8 sint 1) (unsigned-8 uint 1) (integer-16 sint 2) (unsigned-16 uint 2)
      (integer-24 sint 3) (unsigned-24 uint 3) (integer-32 sint 4) (unsigned-32 uint 4)
      (integer-40 sint 5) (unsigned-40 uint 5) (integer-48 sint 6) (unsigned-48 uint 6)
      (integer-56 sint 7) (unsigned-56 uint 7) (integer-64 sint 8) (unsigned-64 uint 8)
      (single-float float 4) (double-float float 8) (float float 4) (double float 8)
      (short sint 2) (unsigned-short uint 2) (int sint 4) (unsigned uint 4)
      (unsigned-int uint 4) (long sint 8) (unsigned-long uint 8) (long-long sint 8)
      (unsigned-long-long uint 8)
      (char char 1) (wchar char 4) (boolean bool 4) (stdbool bool 1)
      (void* uint 8) (iptr sint 8) (uptr uint 8) (fixnum fixnum 8)
      (ftype-pointer fptr 8) (ftype-scheme-object-pointer sop 8)))

  (define (ft-make-scalar name class size endian)
    (ft-new 'scalar
            (if (eq? endian native-endian) name (list 'endian 'swapped name))
            size (ft-odd-align size) endian name class #f))

  ;; native name -> descriptor, in the machine's byte order
  (define ft-natives
    (let ((t (make-eq-hashtable)))
      (for-each (lambda (s)
                  (hashtable-set! t (car s)
                                  (ft-make-scalar (car s) (cadr s) (caddr s) native-endian)))
                native-specs)
      t))

  ;; native name -> descriptor, in the other byte order
  (define ft-swapped (make-eq-hashtable))

  (define ft-fptr-base (hashtable-ref ft-natives 'ftype-pointer #f))
  (define ft-sop-base (hashtable-ref ft-natives 'ftype-scheme-object-pointer #f))
  ;; a scheme-object pointer is an ftype pointer
  (define ft-sop-parent (ftd-parents-set! ft-sop-base (list ft-fptr-base)))

  (define (ft-native? sym) (and (symbol? sym) (hashtable-contains? ft-natives sym)))

  (define (ft-native-id sym)
    (and (ft-native? sym) (ftd-id (hashtable-ref ft-natives sym #f))))

  ;; the native SYM with byte order ENDIAN (#f: the machine's)
  (define (ft-scalar sym endian)
    (let ((n (hashtable-ref ft-natives sym #f)))
      (cond ((or (not endian) (eq? endian native-endian) (= (ftd-size n) 1)) n)
            ((hashtable-ref ft-swapped sym #f))
            (else
             (let ((d (ft-make-scalar sym (ftd-b n) (ftd-size n) endian)))
               (hashtable-set! ft-swapped sym d)
               d)))))

  ;; uid name -> the descriptor of a nongenerative type
  (define ft-uids (make-hashtable string-hash string=?))

  ;; ----------------------------------------------------------------
  ;; What a pointer to one type may be used as: the type itself, the type of
  ;; its first member (of its first element), and the pointer types it is a
  ;; subtype of

  (define (ft-first-member d)
    (case (ftd-kind d)
      ((struct) (let ((ms (ftd-a d))) (and (pair? ms) (cadr (car ms)))))
      ((array) (ftd-a d))
      (else #f)))

  (define (ft-chain d)
    (cons d
          (append (let ((f (ft-first-member d))) (if f (ft-chain f) '()))
                  (apply append (map ft-chain (ftd-parents d))))))

  (define (ft-same? a b)
    (or (eq? a b)
        (and (eq? (ftd-kind a) 'scalar) (eq? (ftd-kind b) 'scalar)
             (not (ftd-uid a)) (not (ftd-uid b))
             (null? (ftd-parents a)) (null? (ftd-parents b))
             (eq? (ftd-b a) (ftd-b b))
             (= (ftd-size a) (ftd-size b))
             (eq? (ftd-endian a) (ftd-endian b)))))

  ;; may a pointer to HAVE be used as a pointer to WANT?
  (define (ft-compatible? have want)
    (let loop ((cs (ft-chain have)))
      (and (pair? cs)
           (or (ft-same? (car cs) want) (loop (cdr cs))))))

  (define (ft-sop-desc? d) (and (memq ft-sop-base (ft-chain d)) #t))

  ;; ----------------------------------------------------------------
  ;; Parsing an ftype: (ft-parse form scope endian packed? who) returns
  ;; its descriptor, its sexpr as written, its alignment as a member (of
  ;; a packed type: 1), and whether the descriptor is new.  SCOPE is
  ;; (names-of-defined-ftypes . names-of-the-group), each an alist of name
  ;; and number; a group's name has no descriptor until it is defined
  ;; (only a pointer may point to it before).  ENDIAN is little, big or
  ;; #f; it, and packed, are the context's: they are of the types written
  ;; in the form and not of the ftypes it names.

  (define (ft-parse t scope endian packed? who)
    (cond
      ((symbol? t) (ft-parse-name t scope endian packed? who))
      ((and (pair? t) (list? t) (symbol? (car t)))
       (case (car t)
         ((struct union) (ft-parse-aggregate t scope endian packed? who))
         ((array) (ft-parse-array t scope endian packed? who))
         ((*) (ft-parse-pointer t scope endian packed? who))
         ((bits) (ft-parse-bits t endian packed? who))
         ((function) (ft-parse-function t scope endian who))
         ((packed unpacked)
          (unless (= (length t) 2) (ft-err who "invalid ftype" t))
          (let-values (((d sx al fresh?)
                        (ft-parse (cadr t) scope endian (eq? (car t) 'packed) who)))
            (let ((nsx (list (car t) sx)))
              (when fresh? (ftd-sexpr-set! d nsx))
              (values d nsx al fresh?))))
         ((endian)
          (unless (and (= (length t) 3) (memq (cadr t) '(big little native)))
            (ft-err who "invalid ftype" t))
          (let ((e (case (cadr t) ((big) 'big) ((little) 'little) (else native-endian))))
            (let-values (((d sx al fresh?) (ft-parse (caddr t) scope e packed? who)))
              (if (and (not fresh?) (eq? (ftd-kind d) 'scalar))
                  (values d (ftd-sexpr d) al #f)
                  (let ((nsx (list 'endian (cadr t) sx)))
                    (when fresh? (ftd-sexpr-set! d nsx))
                    (values d nsx al fresh?))))))
         (else (ft-err who "invalid ftype" t))))
      (else (ft-err who "invalid ftype" t))))

  (define (ft-parse-name s scope endian packed? who)
    (cond
      ((assq s (car scope))
       => (lambda (p)
            (let ((d (ft-desc (cdr p))))
              (values d s (ftd-align d) #f))))
      ((ft-native? s)
       (let ((d (ft-scalar s endian)))
         (values d s (if packed? 1 (ftd-align d)) #f)))
      ((assq s (cdr scope))
       => (lambda (p)
            (let ((d (ft-desc (cdr p))))
              (unless d (ft-err who "invalid recursive or forward reference" s))
              (values d s (ftd-align d) #f))))
      (else (ft-err who "unknown ftype" s))))

  ;; the number and sexpr of what a pointer or a function's argument points
  ;; to: a name, possibly one defined later in the group
  (define (ft-parse-target tt scope endian packed? who)
    (cond
      ((symbol? tt)
       (cond ((assq tt (car scope)) => (lambda (p) (values (cdr p) tt)))
             ((ft-native? tt) (values (ftd-id (ft-scalar tt endian)) tt))
             ((assq tt (cdr scope)) => (lambda (p) (values (cdr p) tt)))
             (else (ft-err who "unknown ftype" tt))))
      (else
       (let-values (((d sx al fresh?) (ft-parse tt scope endian packed? who)))
         (values (ftd-id d) sx)))))

  (define (ft-parse-aggregate t scope endian packed? who)
    (let ((union? (eq? (car t) 'union)))
      (let loop ((fs (cdr t)) (end 0) (align 1) (members '()) (lookup '()))
        (if (null? fs)
            (let ((size (ft-round-up end align))
                  (members (reverse members)))
              (ft-check-size who size)
              (let* ((sx (cons (car t) (map (lambda (m) (list (car m) (cadddr m))) members)))
                     (d (ft-new (car t) sx size align #f members (reverse lookup) #f)))
                (values d sx align #t)))
            (let ((f (car fs)))
              (unless (and (list? f) (= (length f) 2) (symbol? (car f)))
                (ft-err who "invalid field" f))
              (let-values (((d sx al fresh?) (ft-parse (cadr f) scope endian packed? who)))
                (when (eq? (ftd-kind d) 'function)
                  (ft-err who "misplaced function type" f))
                (let* ((name (car f))
                       (pal (if packed? 1 al))
                       (off (if union? 0 (ft-round-up end pal)))
                       (entries
                        (cond
                          ((eq? name '=>)
                           (unless (memq (ftd-kind d) '(struct union bits))
                             (ft-err who "invalid anonymous field ftype" f))
                           (map (lambda (e)
                                  (if (eq? (ftd-kind d) 'bits)
                                      (cons (car e) (cons (cdr e) off))
                                      (cons (car e) (cons (cadr e) (+ off (cddr e))))))
                                (ftd-b d)))
                          ((eq? name '_) '())
                          (else (list (cons name (cons d off)))))))
                  (for-each (lambda (e)
                              (when (assq (car e) lookup)
                                (ft-err who "duplicate field name" (car e))))
                            entries)
                  (loop (cdr fs)
                        (if union? (max end (ftd-size d)) (+ off (ftd-size d)))
                        (max align pal)
                        (cons (list name d off sx) members)
                        (append (reverse entries) lookup)))))))))

  (define (ft-parse-array t scope endian packed? who)
    (unless (and (= (length t) 3)
                 (let ((n (cadr t))) (and (integer? n) (exact? n) (>= n 0))))
      (ft-err who "invalid array ftype" t))
    (let ((len (cadr t)))
      (let-values (((d sx al fresh?) (ft-parse (caddr t) scope endian packed? who)))
        (when (eq? (ftd-kind d) 'function) (ft-err who "misplaced function type" t))
        (let ((size (* len (ftd-size d)))
              (align (if packed? 1 al))
              (sx2 (list 'array len sx)))
          (ft-check-size who size)
          (values (ft-new 'array sx2 size align #f d len sx) sx2 align #t)))))

  (define (ft-parse-pointer t scope endian packed? who)
    (unless (= (length t) 2) (ft-err who "invalid ftype" t))
    (let-values (((tid tsx) (ft-parse-target (cadr t) scope endian packed? who)))
      (let ((sx (list '* tsx)))
        (values (ft-new 'pointer sx 8 8 (or endian native-endian) tid tsx #f)
                sx (if packed? 1 8) #t))))

  (define (ft-parse-bits t endian packed? who)
    (let ((e (or endian native-endian)))
      (let loop ((fs (cdr t)) (used 0) (fields '()))
        (if (pair? fs)
            (let ((f (car fs)))
              (unless (and (list? f) (= (length f) 3) (symbol? (car f))
                           (memq (cadr f) '(signed unsigned))
                           (integer? (caddr f)) (exact? (caddr f)) (> (caddr f) 0))
                (ft-err who "invalid bit field" f))
              (loop (cdr fs) (+ used (caddr f)) (cons (list (car f) (cadr f) (caddr f) used) fields)))
            (begin
              (unless (and (> used 0) (zero? (mod used 8)) (<= used 64))
                (ft-err who "the bit fields of bits must total 8, 16, ... 64 bits" t))
              (let* ((unit (quotient used 8))
                     (fields (reverse fields))
                     (descs
                      (map (lambda (f)
                             (let ((width (caddr f)))
                               (list (car f)
                                     (ft-new 'bitfield #f unit 1 e unit
                                             (cons (if (eq? e 'little)
                                                       (cadddr f)
                                                       (- used (cadddr f) width))
                                                   width)
                                             (eq? (cadr f) 'signed)))))
                           fields))
                     (lookup '()))
                (for-each (lambda (d)
                            (unless (eq? (car d) '_)
                              (when (assq (car d) lookup)
                                (ft-err who "duplicate field name" (car d)))
                              (set! lookup (cons (cons (car d) (cadr d)) lookup))))
                          descs)
                (values (ft-new 'bits t unit (if packed? 1 (ft-odd-align unit)) e
                                descs (reverse lookup) unit)
                        t (if packed? 1 (ft-odd-align unit)) #t)))))))

  ;; the foreign type and wrapping of a function's argument or result
  (define (ft-foreign-spec x scope endian who)
    (define (bad) (ft-err who "unsupported type in function ftype" x))
    (define (of-desc d)
      (case (ftd-kind d)
        ((scalar) (of-native (ftd-a d)))
        ((pointer) (list 'uptr (list 'fptr (ftd-a d))))
        (else (bad))))
    (define (of-native s)
      (case s
        ((integer-8 unsigned-8 integer-16 unsigned-16 integer-32 unsigned-32
          integer-64 unsigned-64 single-float double-float float double short
          unsigned-short int unsigned unsigned-int long unsigned-long long-long
          unsigned-long-long char boolean void* iptr uptr fixnum)
         (list s #f))
        ((wchar) (list 'unsigned-32 'wchar))
        ((stdbool) (list 'integer-8 'bool))
        ((ftype-pointer) (list 'uptr (list 'fptr #f)))
        (else (bad))))
    (cond
      ((eq? x 'void) (list 'void #f))
      ((symbol? x)
       (cond ((assq x (car scope)) => (lambda (p) (of-desc (ft-desc (cdr p)))))
             ((ft-native? x) (of-native x))
             ((assq x (cdr scope)) => (lambda (p) (let ((d (ft-desc (cdr p)))) (if d (of-desc d) (bad)))))
             (else (bad))))
      ((and (pair? x) (eq? (car x) '*) (list? x) (= (length x) 2))
       (let-values (((tid tsx) (ft-parse-target (cadr x) scope endian #f who)))
         (list 'uptr (list 'fptr tid))))
      (else (bad))))

  (define (ft-parse-function t scope endian who)
    (let ((r (reverse (cdr t))))
      (unless (and (>= (length r) 2) (list? (cadr r)))
        (ft-err who "invalid function ftype" t))
      (let ((result (ft-foreign-spec (car r) scope endian who))
            (args (map (lambda (a) (ft-foreign-spec a scope endian who)) (cadr r))))
        (values (ft-new 'function t #f 1 #f (reverse (cddr r)) args result) t 1 #t))))

  ;; ----------------------------------------------------------------
  ;; define-ftype

  ;; (define-ftype name ftype opt ...) or (define-ftype (name ftype opt ...) ...):
  ;; a list of (name-identifier ftype options)
  (define (ft-split-define x)
    (syntax-case x ()
      ((_ name type opt ...)
       (identifier? #'name)
       (list (list #'name (syntax->datum #'type) (syntax->datum #'(opt ...)))))
      ((_ (name type opt ...) ...)
       (and (pair? #'(name ...)) (for-all identifier? #'(name ...)))
       (map (lambda (n t o) (list n t o))
            #'(name ...) (syntax->datum #'(type ...)) (syntax->datum #'((opt ...) ...))))
      (_ (syntax-violation 'define-ftype "invalid syntax" x))))

  ;; the names the ftypes written in TYPES use that are defined elsewhere
  (define (ft-externals types group)
    (let ((acc '()))
      (define (add! s)
        (unless (or (ft-native? s) (memq s group) (memq s acc) (eq? s 'void))
          (set! acc (cons s acc))))
      (define (foreign x)
        (cond ((symbol? x) (add! x))
              ((and (pair? x) (eq? (car x) '*) (list? x) (= (length x) 2)) (walk (cadr x)))))
      (define (walk t)
        (cond
          ((symbol? t) (add! t))
          ((and (pair? t) (list? t) (symbol? (car t)))
           (case (car t)
             ((struct union)
              (for-each (lambda (f) (when (and (list? f) (= (length f) 2)) (walk (cadr f))))
                        (cdr t)))
             ((array) (when (= (length t) 3) (walk (caddr t))))
             ((* packed unpacked) (when (= (length t) 2) (walk (cadr t))))
             ((endian) (when (= (length t) 3) (walk (caddr t))))
             ((function)
              (let ((r (reverse (cdr t))))
                (when (and (>= (length r) 2) (list? (cadr r)))
                  (foreign (car r))
                  (for-each foreign (cadr r)))))))))
      (for-each walk types)
      (reverse acc)))

  ;; (define-ftype ...) -> (resolve-k (name ...) () finish-k form): resolve-k
  ;; has each name found the number of its ftype, and then finish-k is
  ;; used as (finish-k ((name number) ...) form)
  (define (ft-define-start x resolve-k finish-k)
    (let* ((defs (ft-split-define x))
           (group (map (lambda (d) (syntax->datum (car d))) defs))
           (externals (ft-externals (map cadr defs) group)))
      (with-syntax (((n ...) (map (lambda (s) (datum->syntax (car (car defs)) s)) externals))
                    (rk resolve-k) (fk finish-k) (form x))
        #'(rk (n ...) () fk form))))

  (define (ft-opts->uid opts who)
    (cond ((null? opts) #f)
          ((and (pair? opts) (null? (cdr opts)) (pair? (car opts))
                (eq? (caar opts) 'nongenerative)
                (pair? (cdar opts)) (null? (cddar opts)))
           (let ((u (cadar opts)))
             (unless (and (symbol? u) (%chez:uninterned-symbol? u))
               (ft-err who "invalid uid" u))
             u))
          (else (ft-err who "invalid define-ftype option" opts))))

  ;; the descriptor of one define-ftype's ftype
  (define (ft-define-one type opts scope who)
    (let ((uid (ft-opts->uid opts who)))
      (let-values (((d sx al fresh?) (ft-parse type scope #f #f who)))
        (cond
          ((not uid) d)
          (fresh?
           (let* ((key (symbol->string uid))
                  (old (hashtable-ref ft-uids key #f)))
             (cond (old
                    (if (and (eq? (ftd-kind old) (ftd-kind d))
                             (equal? (ftd-sexpr old) (ftd-sexpr d)))
                        old
                        (ft-err who "ftype does not match the earlier one with its uid" uid)))
                   (else
                    (ftd-uid-set! d uid)
                    (hashtable-set! ft-uids key d)
                    d))))
          ((and (eq? (ftd-kind d) 'scalar) (memq (ftd-b d) '(fptr sop)))
           ;; a new pointer type, a subtype of the one named
           (let* ((key (symbol->string uid))
                  (old (hashtable-ref ft-uids key #f)))
             (cond (old
                    (if (and (eq? (ftd-kind old) 'scalar) (equal? (ftd-parents old) (list d)))
                        old
                        (ft-err who "ftype does not match the earlier one with its uid" uid)))
                   (else
                    (let ((nd (ft-new 'scalar (ftd-sexpr d) (ftd-size d) (ftd-align d)
                                      (ftd-endian d) (ftd-a d) (ftd-b d) #f)))
                      (ftd-uid-set! nd uid)
                      (ftd-parents-set! nd (list d))
                      (hashtable-set! ft-uids key nd)
                      nd)))))
          (else (ft-err who "an alias of an ftype can't have a uid" uid))))))

  ;; DEFS, a list of (name ftype options): their numbers
  (define (ft-define-group defs resolved who)
    (let ((reserved (map (lambda (d) (cons (car d) (ft-reserve!))) defs)))
      (let ((scope (cons resolved reserved)))
        (let loop ((ds defs) (rs reserved) (ids '()) (seen '()))
          (if (null? ds)
              (reverse ids)
              (begin
                (when (memq (car (car ds)) seen)
                  (ft-err who "duplicate ftype name" (car (car ds))))
                (let ((d (ft-define-one (cadr (car ds)) (caddr (car ds)) scope who)))
                  (vector-set! ft-vec (cdr (car rs)) d)
                  (loop (cdr ds) (cdr rs) (cons (ftd-id d) ids) (cons (car (car ds)) seen)))))))))

  ;; (finish-k ((name number) ...) form): the form's definitions, each name
  ;; defined by (name-k name number)
  (define (ft-define-finish x name-k)
    (syntax-case x ()
      ((_ ((n idv) ...) form)
       (let* ((defs (ft-split-define #'form))
              (resolved (map (lambda (n i) (cons (syntax->datum n) (syntax->datum i)))
                             #'(n ...) #'(idv ...)))
              (names (map car defs))
              (ids (ft-define-group
                    (map (lambda (d) (list (syntax->datum (car d)) (cadr d) (caddr d))) defs)
                    resolved 'define-ftype)))
         (with-syntax (((name ...) names)
                       ((id ...) (map (lambda (n i) (datum->syntax n i)) names ids))
                       (nk name-k))
           #'(begin (nk name id) ...))))))

  ;; ----------------------------------------------------------------
  ;; Using an ftype: its name is found, and then the form is used again with
  ;; its number

  ;; (op name . rest) -> (k number . rest)
  (define (ft-cps x k)
    (syntax-case x ()
      ((_ n . rest)
       (identifier? #'n)
       (let ((idn (ft-native-id (syntax->datum #'n))))
         (if idn
             (with-syntax ((kk k) (idv (datum->syntax #'n idn)))
               #'(kk idv . rest))
             (with-syntax ((kk k))
               #'(n kk . rest)))))
      (_ (syntax-violation #f "invalid syntax" x))))

  (define (ft-sizeof-expand x)
    (syntax-case x ()
      ((_ idv)
       (let ((size (ftd-size (ft-desc (syntax->datum #'idv)))))
         (unless size
           (syntax-violation 'ftype-sizeof "function ftypes have unknown size" x))
         (datum->syntax ft-ctx-id size)))
      (_ (syntax-violation 'ftype-sizeof "invalid syntax" x))))

  (define ft-op-names
    '((ref . ftype-ref) (set . ftype-set!) (ref& . ftype-&ref)
      (any-ref . ftype-any-ref) (any-set . ftype-any-set!)
      (incr . ftype-locked-incr!) (decr . ftype-locked-decr!)
      (init-lock . ftype-init-lock!) (lock . ftype-lock!)
      (spin-lock . ftype-spin-lock!) (unlock . ftype-unlock!)))

  ;; X is (k number (accessor ...) fptr [index] [value]), OP what the
  ;; operation is
  (define (ft-expand op x)
    (let ((who (cdr (assq op ft-op-names)))
          (setter? (and (memq op '(set any-set)) #t)))
      (syntax-case x ()
        ((k idv (acc ...) ptr arg ...)
         (let* ((args #'(arg ...))
                (nidx (- (length args) (if setter? 1 0))))
           (unless (<= 0 nidx 1)
             (syntax-violation who "invalid syntax" x))
           (ft-build op who x (syntax->datum #'idv) #'(acc ...) #'ptr
                     (and (= nidx 1) (car args))
                     (and setter? (car (reverse args))))))
        (_ (syntax-violation who "invalid syntax" x)))))

  (define (ft-star? stx)
    (and (identifier? stx) (eq? (syntax->datum stx) '*)))

  (define (ft-literal-integer stx)
    (let ((v (syntax->datum stx)))
      (and (integer? v) (exact? v) v)))

  (define (ft-bad-accessor who e x)
    (syntax-violation who "unexpected accessor" x e))

  (define (ft-check-lockable d who x)
    (case (ftd-kind d)
      ((scalar)
       (unless (and (memq (ftd-b d) '(sint uint)) (= (ftd-size d) 8))
         (syntax-violation who "unsupported non-integer or non-word-size type" x))
       (unless (eq? (ftd-endian d) native-endian)
         (syntax-violation who "unsupported swapped type" x)))
      (else (syntax-violation who "unsupported non-base type" x))))

  ;; the syntax of the operation: a let* binding the pointer, the indices
  ;; and the value, with the code made by walking the accessors (a datum
  ;; naming ft$p, ft$ix, ft$i0 ... and ft$v)
  (define (ft-build op who x id path fptr-stx idx-stx val-stx)
    (define any? (and (memq op '(any-ref any-set)) #t))
    (define desc (ft-desc id))
    (define exprs '())
    (define nexprs 0)
    (define nbases 0)
    ;; the symbol an index expression is bound to
    (define (new-index! stx)
      (let ((s (string->symbol (string-append "ft$i" (number->string nexprs)))))
        (set! nexprs (+ nexprs 1))
        (set! exprs (cons (cons s stx) exprs))
        s))
    (define (new-base!)
      (set! nbases (+ nbases 1))
      (string->symbol (string-append "ft$b" (number->string nbases))))
    ;; an offset is (constant term ...), in reverse order
    (define (offset-datum off)
      (let ((c (car off)) (ts (reverse (cdr off))))
        (cond ((null? ts) c)
              (else `(+ ,@ts ,@(if (eqv? c 0) '() (list c)))))))
    (define (off+ off n) (cons (+ (car off) n) (cdr off)))
    (define (off-term off t) (cons (car off) (cons t (cdr off))))
    (define (walk d path base off)
      (if (null? path)
          (finish d base off)
          (let ((e (car path)) (rest (cdr path)))
            (case (ftd-kind d)
              ((struct union)
               (let* ((s (and (identifier? e) (syntax->datum e)))
                      (m (and s (assq s (ftd-b d)))))
                 (unless m (ft-bad-accessor who e x))
                 (walk (cadr m) rest base (off+ off (cddr m)))))
              ((bits)
               (let* ((s (and (identifier? e) (syntax->datum e)))
                      (m (and s (assq s (ftd-b d)))))
                 (unless m (ft-bad-accessor who e x))
                 (walk (cdr m) rest base off)))
              ((array)
               (let* ((len (ftd-b d))
                      (elem (ftd-a d))
                      (stride (ftd-size elem))
                      (lit (ft-literal-integer e)))
                 (cond
                   (lit
                    (unless (and (>= lit 0) (or (= len 0) (< lit len)) (fixnum? (* lit stride)))
                      (syntax-violation who "invalid index" x e))
                    (walk elem rest base (off+ off (* lit stride))))
                   (else
                    (let ((i (new-index! e)))
                      `(begin
                         (ft-check-array-index ',who ,i ,len ,stride)
                         ,(walk elem rest base (off-term off `(* ,i ,stride)))))))))
              ((pointer)
               (let* ((target (ft-desc (ftd-a d)))
                      (tsize (ftd-size target))
                      (nb (new-base!))
                      (lit (if (ft-star? e) 0 (ft-literal-integer e))))
                 `(let ((,nb (ft-read-pointer ,(ftd-id d) ,base ,(offset-datum off))))
                    ,(cond
                       (lit
                        (when (and (not tsize) (not (eqv? lit 0)))
                          (syntax-violation who "cannot calculate offset for function index" x e))
                        (unless (fixnum? (* lit (or tsize 0)))
                          (syntax-violation who "invalid index" x e))
                        (walk target rest nb (list (* lit (or tsize 0)))))
                       (else
                        (let ((i (new-index! e)))
                          `(begin
                             ,(if tsize
                                  `(ft-check-pointer-index ',who ,i ,tsize)
                                  `(ft-check-function-index ',who ,i))
                             ,(walk target rest nb
                                    (if tsize (list 0 `(* ,i ,tsize)) (list 0))))))))))
              (else (ft-bad-accessor who e x))))))
    (define (finish d base off)
      (let ((o (offset-datum off)) (did (ftd-id d)))
        (case op
          ((ref any-ref)
           (case (ftd-kind d)
             ((scalar) `(ft-scalar-ref ,did ,base ,o))
             ((bitfield) `(ft-bitfield-ref ,did ,base ,o))
             ((pointer) `(ft-pointer-ref ,did ,base ,o))
             ((function) `(ft-function-ref ,did ,base ,o))
             (else (syntax-violation who "target cannot be referenced" x))))
          ((set any-set)
           (case (ftd-kind d)
             ((scalar) `(ft-scalar-set! ,did ,base ,o ft$v))
             ((bitfield) `(ft-bitfield-set! ,did ,base ,o ft$v))
             ((pointer) `(ft-pointer-set! ,did ,base ,o ft$v))
             (else (syntax-violation who "cannot assign a non-scalar type" x))))
          ((ref&)
           (if (eq? (ftd-kind d) 'bitfield)
               (syntax-violation who "cannot take the address of a bit field" x)
               `(ft-make-ref ,did ,base ,o)))
          (else
           (ft-check-lockable d who x)
           (case op
             ((incr) `(ft-locked-add! ,base ,o 1))
             ((decr) `(ft-locked-add! ,base ,o -1))
             ((init-lock) `(ft-init-lock! ,base ,o))
             ((lock) `(ft-lock! ,base ,o))
             ((spin-lock) `(ft-spin-lock! ,base ,o))
             (else `(ft-unlock! ,base ,o)))))))
    (let* ((idx (and idx-stx (not (ft-star? idx-stx)) idx-stx))
           (size (ftd-size desc))
           (base0 (if any?
                      `(ft-any-base ',who ft$p)
                      `(ft-base (ft-check-fptr ',who ft$p ,id))))
           (off0 (cons 0 (if any? '((ft-base-off ft$p)) '())))
           (off1 (cond ((not idx) off0)
                       (any? (off-term off0 'ft$ix))
                       ((not size) off0)
                       (else (off-term off0 `(* ft$ix ,size)))))
           (check (cond ((not idx) #f)
                        (any? `(ft-check-offset ',who ft$ix))
                        (size `(ft-check-pointer-index ',who ft$ix ,size))
                        (else `(ft-check-function-index ',who ft$ix))))
           (code (walk desc (syntax->list* path) 'ft$b off1))
           (body `(let ((ft$b ,base0))
                    ,(if check `(begin ,check ,code) code)))
           (binds (append (list (cons 'ft$p fptr-stx))
                          (if idx (list (cons 'ft$ix idx)) '())
                          (reverse exprs)
                          (if val-stx (list (cons 'ft$v val-stx)) '()))))
      (with-syntax ((((b e) ...)
                     (map (lambda (p) (list (datum->syntax ft-ctx-id (car p)) (cdr p))) binds))
                    (body (datum->syntax ft-ctx-id body)))
        #'(let* ((b e) ...) body))))

  ;; the elements of a list of syntax objects (#'(a ...) is one)
  (define (syntax->list* l) l)

  ;; ----------------------------------------------------------------
  ;; Pointers

  ;; BASE is an integer address, or an object (a bytevector) and OFF an offset
  ;; in it; of a pointer to a scheme object, the object
  (define-record-type fptr
    (fields ftd base off))

  (define (ft-pointer? x) (fptr? x))

  (define (ft-pointer-of? id x)
    (and (fptr? x) (ft-compatible? (fptr-ftd x) (ft-desc id))))

  (define (ft-check-fptr who p id)
    (unless (fptr? p) (ft-err who "not an ftype pointer" p))
    (unless (ft-compatible? (fptr-ftd p) (ft-desc id))
      (ft-err who "ftype mismatch" p (ftd-sexpr (ft-desc id))))
    p)

  (define (ft-base p) (fptr-base p))
  (define (ft-base-off p) (fptr-off p))

  (define (ft-any-base who p)
    (unless (fptr? p) (ft-err who "not an ftype pointer" p))
    (fptr-base p))

  (define (ft-pointer-address p)
    (unless (fptr? p) (ft-err 'ftype-pointer-address "not an ftype pointer" p))
    (let ((b (fptr-base p)))
      (cond ((integer? b) (+ b (fptr-off p)))
            ((bytevector? b) (+ (%chez:ftype-object-address b) (fptr-off p)))
            (else (ft-err 'ftype-pointer-address "the pointer has no address" p)))))

  (define (ft-pointer-null? p)
    (unless (fptr? p) (ft-err 'ftype-pointer-null? "not an ftype pointer" p))
    (eqv? (ft-pointer-address p) 0))

  (define (ft-pointer=? p q)
    (unless (fptr? p) (ft-err 'ftype-pointer=? "not an ftype pointer" p))
    (unless (fptr? q) (ft-err 'ftype-pointer=? "not an ftype pointer" q))
    (= (ft-pointer-address p) (ft-pointer-address q)))

  (define (ft-pointer-ftype p)
    (unless (fptr? p) (ft-err 'ftype-pointer-ftype "not an ftype pointer" p))
    (ftd-sexpr (fptr-ftd p)))

  ;; (make-ftype-pointer ftype address) and, for a function ftype, a procedure
  (define (ft-make-pointer id addr)
    (let ((d (ft-desc id)))
      (when (ft-sop-desc? d)
        (ft-err 'make-ftype-pointer "cannot use a scheme-object pointer ftype" (ftd-sexpr d)))
      (cond ((and (integer? addr) (exact? addr) (<= 0 addr) (< addr (expt 2 64)))
             (make-fptr d addr 0))
            ((and (procedure? addr) (eq? (ftd-kind d) 'function))
             (make-fptr d (ft-make-callable d addr) 0))
            (else (ft-err 'make-ftype-pointer "invalid address" addr)))))

  ;; (make-ftype-scheme-object-pointer object offset ftype)
  (define (ft-make-sop obj off id)
    (let ((d (if id (ft-desc id) ft-sop-base)))
      (unless (ft-sop-desc? d)
        (ft-err 'make-ftype-scheme-object-pointer "not a scheme-object pointer ftype" (ftd-sexpr d)))
      (unless (and (integer? off) (exact? off))
        (ft-err 'make-ftype-scheme-object-pointer "invalid offset" off))
      (make-fptr d obj off)))

  (define (ft-sop? x) (and (fptr? x) (ft-sop-desc? (fptr-ftd x))))

  (define (ft-sop-object p)
    (unless (ft-sop? p) (ft-err 'ftype-scheme-object-pointer-object "not a scheme-object pointer" p))
    (fptr-base p))

  (define (ft-sop-offset p)
    (unless (ft-sop? p) (ft-err 'ftype-scheme-object-pointer-offset "not a scheme-object pointer" p))
    (fptr-off p))

  ;; the pointer to the ftype of DID at BASE + OFF
  (define (ft-make-ref did base off)
    (let ((d (ft-desc did)))
      (if (integer? base)
          (make-fptr d (+ base off) 0)
          (make-fptr d base off))))

  ;; ----------------------------------------------------------------
  ;; Checks of indices

  (define (ft-check-array-index who i len stride)
    (unless (and (fixnum? i) (>= i 0) (or (= len 0) (< i len)) (fixnum? (* i stride)))
      (ft-err who "invalid index" i)))

  (define (ft-check-pointer-index who i size)
    (unless (and (fixnum? i) (fixnum? (* i size)))
      (ft-err who "invalid index" i)))

  (define (ft-check-function-index who i)
    (unless (eqv? i 0)
      (ft-err who "cannot calculate offset for function index" i)))

  (define (ft-check-offset who i)
    (unless (fixnum? i) (ft-err who "invalid index" i)))

  ;; ----------------------------------------------------------------
  ;; Reading and writing

  (define (ft-big? d) (eq? (ftd-endian d) 'big))

  ;; may the integer V be stored in BITS bits, as a signed or an unsigned integer?
  (define (ft-int-fits? v bits)
    (and (integer? v) (exact? v)
         (<= (- (expt 2 (- bits 1))) v)
         (< v (expt 2 bits))))

  (define (ft-signed u bits)
    (if (>= u (expt 2 (- bits 1))) (- u (expt 2 bits)) u))

  (define (ft-scalar-ref did base off)
    (let* ((d (ft-desc did))
           (n (ftd-size d))
           (big? (ft-big? d)))
      (case (ftd-b d)
        ((sint fixnum) (ft-signed (%chez:ftype-read-uint base off n big?) (* 8 n)))
        ((uint) (%chez:ftype-read-uint base off n big?))
        ((float) (%chez:ftype-read-float base off n big?))
        ((char) (integer->char (%chez:ftype-read-uint base off n big?)))
        ((bool) (not (zero? (%chez:ftype-read-uint base off n big?))))
        ((fptr) (make-fptr d (%chez:ftype-read-uint base off n big?) 0))
        (else
         (let ((u (%chez:ftype-read-uint base off n big?)))
           (unless (zero? u)
             (ft-err 'ftype-ref "can't read a scheme object from memory" u))
           (make-fptr d 0 0))))))

  (define (ft-scalar-set! did base off v)
    (let* ((d (ft-desc did))
           (n (ftd-size d))
           (bits (* 8 n))
           (big? (ft-big? d)))
      (define (put! u) (%chez:ftype-write-uint! base off n big? u))
      (define (bad) (ft-err 'ftype-set! "invalid value for ftype" v (ftd-sexpr d)))
      (case (ftd-b d)
        ((sint uint)
         (unless (ft-int-fits? v bits) (bad))
         (put! (mod v (expt 2 bits))))
        ((fixnum)
         (unless (fixnum? v) (bad))
         (put! (mod v (expt 2 bits))))
        ((float)
         (unless (real? v) (bad))
         (%chez:ftype-write-float! base off n big? v))
        ((char)
         (unless (and (char? v) (< (char->integer v) (expt 2 bits))) (bad))
         (put! (char->integer v)))
        ((bool) (put! (if v 1 0)))
        (else
         (unless (fptr? v) (bad))
         (put! (ft-pointer-address v))))
      (if #f #f)))

  (define (ft-bitfield-ref did base off)
    (let* ((d (ft-desc did))
           (width (cdr (ftd-b d)))
           (u (%chez:ftype-read-uint base off (ftd-a d) (ft-big? d)))
           (v (bitwise-and (bitwise-arithmetic-shift-right u (car (ftd-b d)))
                           (- (expt 2 width) 1))))
      (if (and (ftd-c d) (bitwise-bit-set? v (- width 1)))
          (- v (expt 2 width))
          v)))

  (define (ft-bitfield-set! did base off v)
    (let* ((d (ft-desc did))
           (width (cdr (ftd-b d)))
           (shift (car (ftd-b d)))
           (unit (ftd-a d))
           (mask (- (expt 2 width) 1)))
      (unless (ft-int-fits? v width)
        (ft-err 'ftype-set! "invalid value for bit field" v width))
      (let* ((u (%chez:ftype-read-uint base off unit (ft-big? d)))
             (nu (bitwise-ior (bitwise-and u (bitwise-not (bitwise-arithmetic-shift-left mask shift)))
                              (bitwise-arithmetic-shift-left (bitwise-and v mask) shift))))
        (%chez:ftype-write-uint! base off unit (ft-big? d) nu))
      (if #f #f)))

  ;; the address a pointer ftype's value is
  (define (ft-read-pointer did base off)
    (%chez:ftype-read-uint base off 8 (ft-big? (ft-desc did))))

  (define (ft-pointer-ref did base off)
    (let ((d (ft-desc did)))
      (make-fptr (ft-desc (ftd-a d)) (ft-read-pointer did base off) 0)))

  (define (ft-pointer-set! did base off v)
    (let ((d (ft-desc did)))
      (unless (and (fptr? v) (ft-compatible? (fptr-ftd v) (ft-desc (ftd-a d))))
        (ft-err 'ftype-set! "invalid pointer value for ftype" v (ftd-sexpr d)))
      (%chez:ftype-write-uint! base off 8 (ft-big? d) (ft-pointer-address v))
      (if #f #f)))

  ;; ----------------------------------------------------------------
  ;; Function ftypes: a foreign procedure at an address, and a foreign
  ;; callable for a procedure

  (define (ft-wrap-in spec raw)
    (let ((w (cadr spec)))
      (cond ((not w) raw)
            ((eq? w 'wchar) (integer->char raw))
            ((eq? w 'bool) (not (zero? raw)))
            (else (make-fptr (if (cadr w) (ft-desc (cadr w)) ft-fptr-base) raw 0)))))

  (define (ft-wrap-out spec v)
    (let ((w (cadr spec)))
      (cond ((not w) v)
            ((eq? w 'wchar) (char->integer v))
            ((eq? w 'bool) (if v 1 0))
            (else (ft-pointer-address v)))))

  (define (ft-function-ref did base off)
    (let* ((d (ft-desc did))
           (args (ftd-b d))
           (res (ftd-c d))
           (f (%chez:foreign-procedure (+ base off) (map car args) (car res))))
      (lambda actual
        (ft-wrap-in res (apply f (map ft-wrap-out args actual))))))

  ;; the entry point of a foreign callable for PROC, of function ftype D
  (define (ft-make-callable d proc)
    (let ((args (ftd-b d)) (res (ftd-c d)))
      (%chez:foreign-callable
       (lambda actual
         (ft-wrap-out res (apply proc (map ft-wrap-in args actual))))
       (map car args) (car res))))

  ;; ----------------------------------------------------------------
  ;; Locks: on integers of a word, in the machine's byte order

  (define (ft-locked-add! base off delta)
    (zero? (%chez:ftype-atomic-add! base off 8 delta)))

  (define (ft-init-lock! base off)
    (%chez:ftype-write-uint! base off 8 #f 0)
    (if #f #f))

  (define (ft-lock! base off)
    (%chez:ftype-cas! base off 8 0 1))

  (define (ft-spin-lock! base off)
    (%chez:ftype-spin-lock! base off 8)
    (if #f #f))

  (define (ft-unlock! base off)
    (%chez:ftype-write-uint! base off 8 #f 0)
    (if #f #f))

  ;; ----------------------------------------------------------------
  ;; ftype-pointer->sexpr: the value of what is pointed to

  (define (ft-pointer->sexpr p)
    (unless (fptr? p) (ft-err 'ftype-pointer->sexpr "not an ftype pointer" p))
    (if (ft-sop? p)
        (if (eqv? (fptr-off p) 0)
            (fptr-base p)
            (list 'offset (fptr-base p) (fptr-off p)))
        (ft-sexpr (fptr-ftd p) (fptr-base p) (fptr-off p)
                  (make-hashtable equal-hash equal?))))

  (define (ft-sexpr d base off seen)
    (define (memo! head)
      ;; the list being made, for a pointer to this to find
      (when (integer? base)
        (hashtable-set! seen (cons (+ base off) (ftd-id d)) head))
      head)
    (define (seen-before)
      (and (integer? base) (hashtable-ref seen (cons (+ base off) (ftd-id d)) #f)))
    (case (ftd-kind d)
      ((scalar)
       (let ((v (ft-scalar-ref (ftd-id d) base off)))
         (if (fptr? v)
             (if (eqv? (fptr-base v) 0) 'null (fptr-base v))
             v)))
      ((bitfield) (ft-bitfield-ref (ftd-id d) base off))
      ((pointer)
       (let ((u (ft-read-pointer (ftd-id d) base off)))
         (if (zero? u)
             'null
             (list '* (ft-sexpr (ft-desc (ftd-a d)) u 0 seen)))))
      ((function) (ftd-sexpr d))
      ((struct union)
       (or (seen-before)
           (let ((head (memo! (list (ftd-kind d)))))
             (set-cdr! head
                       (map (lambda (m)
                              (list (car m)
                                    (if (eq? (car m) '_)
                                        '_
                                        (ft-sexpr (cadr m) base (+ off (caddr m)) seen))))
                            (ftd-a d)))
             head)))
      ((array)
       (or (seen-before)
           (let ((head (memo! (list 'array (ftd-b d))))
                 (elem (ftd-a d)))
             (set-cdr! (cdr head)
                       (let loop ((i 0) (acc '()))
                         (if (= i (ftd-b d))
                             (reverse acc)
                             (loop (+ i 1)
                                   (cons (ft-sexpr elem base (+ off (* i (ftd-size elem))) seen)
                                         acc)))))
             head)))
      ((bits)
       (cons 'bits
             (map (lambda (f)
                    (list (car f)
                          (if (eq? (car f) '_)
                              '_
                              (ft-bitfield-ref (ftd-id (cadr f)) base off))))
                  (ftd-a d))))
      (else (ft-err 'ftype-pointer->sexpr "unsupported ftype" (ftd-sexpr d))))))
