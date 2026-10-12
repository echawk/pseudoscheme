;;; -*- Mode: Scheme -*-
;;;; The first part of the body of Pseudoscheme's (chezscheme) library:
;;;; the Chez Scheme extensions the Chez variants of Akku packages
;;;; (foo.chezscheme.sls) use.  The library form around this and the
;;;; other src/chez/*.scm files, which re-exports R6RS as Chez's does, is
;;;; built by src/chez/chez.lisp; host procedures come in with a %
;;;; prefix.  docs/chez.md says what is and isn't here.

;; Chez's parameters: called with a value, they set it
(define make-parameter
  (case-lambda ((init) (%chez:make-parameter init))
               ((init converter) (%chez:make-parameter init converter))))
(define (void) (if #f #f))
(define (add1 n) (+ n 1))
(define (sub1 n) (- n 1))
(define call/1cc call/cc)
(define gensym
  (case-lambda (() (%chez:make-gensym "g")) ((name) (%chez:make-gensym name))))
(define getenv %get-environment-variable)
(define system %chez:system)
(define with-input-from-string %chez:with-input-from-string)
(define with-output-to-string %chez:with-output-to-string)
(define current-directory %chez:current-directory)
(define file-directory? %chez:file-directory?)
(define file-regular? %chez:file-regular?)
(define file-symbolic-link? %chez:file-symbolic-link?)
(define directory-list %chez:directory-list)
(define mkdir %chez:mkdir)
(define delete-directory %chez:delete-directory)
(define rename-file %chez:rename-file)
(define (directory-separator) #\/)
(define machine-type %chez:machine-type)
(define get-mode %chez:get-mode)
(define chmod %chez:chmod)
(define (source-directories) '("."))
(define record-writer %chez:record-writer)
;; collect: a full collection, whatever the generations asked for
(define collect-maximum-generation
  (make-parameter 4
    (lambda (g)
      (unless (and (fixnum? g) (<= 1 g 254))
        (assertion-violation 'collect-maximum-generation "invalid generation" g))
      g)))
(define (check-generation g)
  (unless (and (fixnum? g) (<= 0 g (collect-maximum-generation)))
    (assertion-violation 'collect "invalid generation" g)))
(define collect
  (case-lambda
    (() (%chez:collect))
    ((g) (check-generation g) (%chez:collect))
    ((g tg)
     (check-generation g)
     (unless (or (eq? tg 'static)
                 (and (fixnum? tg) (<= g tg (collect-maximum-generation)) (<= tg (+ g 1))))
       (assertion-violation 'collect "invalid target generation" tg))
     (%chez:collect))
    ((g tg objs) (collect g tg) (if #f #f))))
;; Virtual registers: 16 per thread, each initially 0
(define (virtual-register-count) 16)
(define (check-virtual-register who i)
  (unless (and (fixnum? i) (fx<= 0 i 15)) (assertion-violation who "invalid virtual register" i)))
(define (virtual-register i) (check-virtual-register 'virtual-register i) (%chez:virtual-register i))
(define (set-virtual-register! i x)
  (check-virtual-register 'set-virtual-register! i)
  (%chez:set-virtual-register! i x))
;; (bytes-allocated [g [space]]): the heap in use; SBCL's generations
;; aren't Chez's, so every generation and space is all of it
(define bytes-allocated
  (case-lambda
    (() (%chez:bytes-allocated))
    ((g) (unless (or (not g) (eq? g 'static) (and (fixnum? g) (<= 0 g (collect-maximum-generation))))
           (assertion-violation 'bytes-allocated "invalid generation" g))
         (%chez:bytes-allocated))
    ((g space) (bytes-allocated g))))
;; guardians: src/r6rs/guardians.lisp, checked after a collect
(define make-guardian
  (case-lambda
    (() (%chez:make-guardian #f))
    ((ordered?) (%chez:make-guardian ordered?))))
(define guardian? %chez:guardian?)
(define unregister-guardian %chez:unregister-guardian)
(define open-input-string %open-input-string)
(define open-output-string %open-output-string)
(define get-output-string %get-output-string)
(define (last-pair l) (if (pair? (cdr l)) (last-pair (cdr l)) l))
(define (fxquotient a b) (quotient a b))
(define (fxremainder a b) (remainder a b))
(define (fxmodulo a b) (modulo a b))
(define (fx1+ n) (fx+ n 1))
(define (fx1- n) (fx- n 1))
(define fxlogand fxand)
(define fxlogor fxior)
(define fxlogxor fxxor)
(define (fxsll n k) (fxarithmetic-shift-left n k))
(define (fxsra n k) (fxarithmetic-shift-right n k))
(define (list-head l k) (if (= k 0) '() (cons (car l) (list-head (cdr l) (- k 1)))))
(define (remq! x l) (remq x l))
(define (remv! x l) (remv x l))
(define (remove! x l) (remove x l))
(define (string-copy! src s-start dst d-start n)   ; Chez's argument order
  (do ((i 0 (+ i 1))) ((= i n)) (string-set! dst (+ d-start i) (string-ref src (+ s-start i)))))
(define make-list %make-list)
(define (fxabs n) (if (fx<? n 0) (fx- 0 n) n))

;; (with-implicit (k id ...) body ...): bind each id to an identifier
;; named id in the context of k.
(define-syntax with-implicit
  (syntax-rules ()
    ((_ (k id ...) body1 body2 ...)
     (with-syntax ((id (datum->syntax #'k 'id)) ...) body1 body2 ...))))

(define weak-cons cons)
(define weak-pair? pair?)
(define (bwp-object? x) #f)

(define (iota n . rest)
  (let ((start (if (pair? rest) (car rest) 0))
        (step (if (and (pair? rest) (pair? (cdr rest))) (cadr rest) 1)))
    (let loop ((i (- n 1)) (acc '()))
      (if (< i 0) acc (loop (- i 1) (cons (+ start (* i step)) acc))))))

;; Boxes, the host's (#&x reads as one)
(define box %chez:box)
(define box? %chez:box?)
(define unbox %chez:unbox)
(define set-box! %chez:set-box!)
(define box-cas! %chez:box-cas!)

;; format, printf, fprintf, errorf, warningf: src/chez/conditions.scm

(define (pretty-print x . port)
  (let ((p (if (pair? port) (car port) (current-output-port))))
    (write x p)
    (newline p)))

(define-syntax fluid-let
  (syntax-rules ()
    ((_ () b1 b2 ...) (let () b1 b2 ...))
    ((_ ((x v) ...) b1 b2 ...)
     (let ((new (list v ...)) (old #f))
       (dynamic-wind
        (lambda () (set! old (list x ...)) (fluid-let-assign (x ...) new))
        (lambda () b1 b2 ...)
        (lambda () (set! new (list x ...)) (fluid-let-assign (x ...) old)))))))

(define-syntax fluid-let-assign
  (syntax-rules ()
    ((_ () vals) (if #f #f))
    ((_ (x y ...) vals) (begin (set! x (car vals)) (fluid-let-assign (y ...) (cdr vals))))))

(define-syntax time
  (syntax-rules ()
    ((_ e)
     (let ((start (%current-jiffy)))
       (call-with-values (lambda () e)
         (lambda vals
           (let ((p (current-error-port)))
             (display "    " p)
             (display (/ (inexact (- (%current-jiffy) start)) (%jiffies-per-second)) p)
             (display "s elapsed time\n" p))
           (apply values vals)))))))

;;; Time (Chez's time and date objects, the subset libraries use: chez-srfi's
;;; SRFI 19 compat layer is built on these).  CPU time is approximated by
;;; real time since start-up; there is no GC time.

(define-record-type (time-type* make-time time?)
  (fields (mutable type time-type set-time-type!)
          (mutable nanosecond time-nanosecond set-time-nanosecond!)
          (mutable second time-second set-time-second!)))

(define (seconds->time type s)
  (let* ((whole (exact (floor s)))
         (ns (exact (round (* (- s whole) 1000000000)))))
    (make-time type ns whole)))

(define current-time
  (case-lambda
    (() (current-time 'time-utc))
    ((type)
     (case type
       ((time-utc time-monotonic) (seconds->time type (%current-second)))
       ((time-process time-thread) (seconds->time type (/ (%current-jiffy) (%jiffies-per-second))))
       ((time-collector-cpu time-collector-real) (make-time type 0 0))
       (else (assertion-violation 'current-time "unknown time type" type))))))

(define (cpu-time) (exact (round (/ (* 1000 (%current-jiffy)) (%jiffies-per-second)))))
(define (real-time) (cpu-time))

(define-record-type (sstats %make-sstats sstats?)
  (fields (mutable cpu sstats-cpu %set-sstats-cpu!)
          (mutable real sstats-real %set-sstats-real!)
          (mutable bytes sstats-bytes %set-sstats-bytes!)
          (mutable gc-count sstats-gc-count %set-sstats-gc-count!)
          (mutable gc-cpu sstats-gc-cpu %set-sstats-gc-cpu!)
          (mutable gc-real sstats-gc-real %set-sstats-gc-real!)
          (mutable gc-bytes sstats-gc-bytes %set-sstats-gc-bytes!)))
(define (check-sstats-time who x)
  (unless (time? x) (assertion-violation who "not a time" x)))
(define (check-sstats-count who x)
  (unless (and (integer? x) (exact? x)) (assertion-violation who "not an exact integer" x)))
(define (make-sstats cpu real bytes gc-count gc-cpu gc-real gc-bytes)
  (for-each (lambda (t) (check-sstats-time 'make-sstats t)) (list cpu real gc-cpu gc-real))
  (for-each (lambda (n) (check-sstats-count 'make-sstats n)) (list bytes gc-count gc-bytes))
  (%make-sstats cpu real bytes gc-count gc-cpu gc-real gc-bytes))
(define-syntax define-sstats-setter
  (syntax-rules ()
    ((_ name set check)
     (define (name s x) (check 'name x) (set s x)))))
(define-sstats-setter set-sstats-cpu! %set-sstats-cpu! check-sstats-time)
(define-sstats-setter set-sstats-real! %set-sstats-real! check-sstats-time)
(define-sstats-setter set-sstats-bytes! %set-sstats-bytes! check-sstats-count)
(define-sstats-setter set-sstats-gc-count! %set-sstats-gc-count! check-sstats-count)
(define-sstats-setter set-sstats-gc-cpu! %set-sstats-gc-cpu! check-sstats-time)
(define-sstats-setter set-sstats-gc-real! %set-sstats-gc-real! check-sstats-time)
(define-sstats-setter set-sstats-gc-bytes! %set-sstats-gc-bytes! check-sstats-count)
(define (statistics)
  (let ((gc (%chez:gc-statistics)))     ; #(count run-time-seconds)
    (%make-sstats (current-time 'time-thread) (current-time 'time-monotonic)
                  (%chez:bytes-consed) (vector-ref gc 0)
                  (seconds->time 'time-collector-cpu (vector-ref gc 1))
                  (seconds->time 'time-collector-real (vector-ref gc 1))
                  0)))
(define (sstats-difference a b)
  (define (dt f) (time-difference (f a) (f b)))
  (unless (sstats? a) (assertion-violation 'sstats-difference "not an sstats record" a))
  (unless (sstats? b) (assertion-violation 'sstats-difference "not an sstats record" b))
  (%make-sstats (dt sstats-cpu) (dt sstats-real)
                (- (sstats-bytes a) (sstats-bytes b))
                (- (sstats-gc-count a) (sstats-gc-count b))
                (dt sstats-gc-cpu) (dt sstats-gc-real)
                (- (sstats-gc-bytes a) (sstats-gc-bytes b))))
(define sstats-print
  (case-lambda
    ((s) (sstats-print s (current-output-port)))
    ((s port)
     (define (secs t) (inexact (/ (time->nanoseconds t) 1000000000)))
     (format port "    ~a collections~%    ~ss elapsed cpu time, including ~ss collecting~%    ~ss elapsed real time, including ~ss collecting~%    ~a bytes allocated~%"
             (sstats-gc-count s) (secs (sstats-cpu s)) (secs (sstats-gc-cpu s))
             (secs (sstats-real s)) (secs (sstats-gc-real s)) (sstats-bytes s)))))

(define-record-type (date-type* make-date date?)
  (fields nanosecond second minute hour day month year zone-offset)
  (nongenerative))
(define (date-nanosecond d) (date-type*-nanosecond d))
(define (date-second d) (date-type*-second d))
(define (date-minute d) (date-type*-minute d))
(define (date-hour d) (date-type*-hour d))
(define (date-day d) (date-type*-day d))
(define (date-month d) (date-type*-month d))
(define (date-year d) (date-type*-year d))
(define (date-zone-offset d) (date-type*-zone-offset d))

(define (time->nanoseconds t) (+ (* (time-second t) 1000000000) (time-nanosecond t)))
(define (nanoseconds->time type ns)
  (make-time type (mod ns 1000000000) (div ns 1000000000)))
(define (copy-time t) (make-time (time-type t) (time-nanosecond t) (time-second t)))
(define (time-difference a b)
  (nanoseconds->time 'time-duration (- (time->nanoseconds a) (time->nanoseconds b))))
(define (time-difference! a b) (time-difference a b))
(define (add-duration t d)
  (nanoseconds->time (time-type t) (+ (time->nanoseconds t) (time->nanoseconds d))))
(define (add-duration! t d) (add-duration t d))
(define (subtract-duration t d)
  (nanoseconds->time (time-type t) (- (time->nanoseconds t) (time->nanoseconds d))))
(define (subtract-duration! t d) (subtract-duration t d))
(define (time=? a b) (= (time->nanoseconds a) (time->nanoseconds b)))
(define (time<? a b) (< (time->nanoseconds a) (time->nanoseconds b)))
(define (time<=? a b) (<= (time->nanoseconds a) (time->nanoseconds b)))
(define (time>? a b) (> (time->nanoseconds a) (time->nanoseconds b)))
(define (time>=? a b) (>= (time->nanoseconds a) (time->nanoseconds b)))

(define (date->time-utc d)
  ;; days from the civil date (Howard Hinnant's algorithm)
  (let* ((y (if (<= (date-month d) 2) (- (date-year d) 1) (date-year d)))
         (era (div y 400))
         (yoe (- y (* era 400)))
         (m (date-month d))
         (doy (+ (div (+ (* 153 (+ m (if (> m 2) -3 9))) 2) 5) (- (date-day d) 1)))
         (doe (+ (* yoe 365) (div yoe 4) (- (div yoe 100)) doy))
         (days (+ (* era 146097) doe -719468)))
    (make-time 'time-utc (date-nanosecond d)
               (- (+ (* days 86400) (* 3600 (date-hour d)) (* 60 (date-minute d)) (date-second d))
                  (date-zone-offset d)))))

(define (date-week-day d)
  (mod (+ 4 (div (time-second (date->time-utc d)) 86400)) 7))

(define (time-utc->date t . offset)
  ;; days-from-civil inverted (Howard Hinnant's algorithm)
  (let* ((off (if (pair? offset) (car offset) (%chez:timezone-offset)))
         (secs (+ (time-second t) off))
         (days (floor (/ secs 86400)))
         (rem (- secs (* days 86400)))
         (z (+ days 719468))
         (era (floor (/ z 146097)))
         (doe (- z (* era 146097)))
         (yoe (floor (/ (- doe (floor (/ doe 1460)) (- (floor (/ doe 36524))) (floor (/ doe 146096))) 365)))
         (doy (- doe (- (+ (* 365 yoe) (floor (/ yoe 4))) (floor (/ yoe 100)))))
         (mp (floor (/ (+ (* 5 doy) 2) 153)))
         (d (+ (- doy (floor (/ (+ (* 153 mp) 2) 5))) 1))
         (m (if (< mp 10) (+ mp 3) (- mp 9)))
         (y (+ yoe (* era 400) (if (<= m 2) 1 0))))
    (make-date (time-nanosecond t) (mod rem 60) (mod (div rem 60) 60) (div rem 3600)
               d m y off)))
(define (current-date . offset) (apply time-utc->date (current-time) offset))

;; Stencil vectors, #5vs(a b)
(define stencil-vector %chez:stencil-vector)
(define stencil-vector? %chez:stencil-vector?)
(define stencil-vector-mask %chez:stencil-vector-mask)
(define stencil-vector-length %chez:stencil-vector-length)
(define stencil-vector-ref %chez:stencil-vector-ref)
(define stencil-vector-set! %chez:stencil-vector-set!)
(define stencil-vector-mask-width %chez:stencil-vector-mask-width)
(define stencil-vector-update %chez:stencil-vector-update)
(define stencil-vector-truncate! %chez:stencil-vector-truncate!)

;; File times, as time-utc times
(define (file-time i)
  (lambda (path . follow?)
    (make-time 'time-utc 0 (list-ref (apply %chez:file-times path follow?) i))))
(define file-access-time (file-time 0))
(define file-change-time (file-time 1))
(define file-modification-time (file-time 2))

;; R6RS's versions of procedures Chez extends, as Chez names them
(define-syntax define-r6rs-versions
  (syntax-rules ()
    ((_ (r6rs-name name) ...)
     (begin (define (r6rs-name a b . more) (apply name a b more)) ...))))
(define-r6rs-versions
  (r6rs:char=? char=?) (r6rs:char<? char<?) (r6rs:char>? char>?)
  (r6rs:char<=? char<=?) (r6rs:char>=? char>=?)
  (r6rs:char-ci=? char-ci=?) (r6rs:char-ci<? char-ci<?) (r6rs:char-ci>? char-ci>?)
  (r6rs:char-ci<=? char-ci<=?) (r6rs:char-ci>=? char-ci>=?)
  (r6rs:string=? string=?) (r6rs:string<? string<?) (r6rs:string>? string>?)
  (r6rs:string<=? string<=?) (r6rs:string>=? string>=?)
  (r6rs:string-ci=? string-ci=?) (r6rs:string-ci<? string-ci<?)
  (r6rs:string-ci>? string-ci>?) (r6rs:string-ci<=? string-ci<=?)
  (r6rs:string-ci>=? string-ci>=?))
(define (r6rs:fx+ a b) (fx+ a b))
(define (r6rs:fx- a b) (fx- a b))
(define (r6rs:fx* a b) (fx* a b))

;; (char-name name) is the character, (char-name char) its name, or #f;
;; (char-name name char) names it (char #f: unnames the name)
(define char-names
  (list (cons 'space #\space) (cons 'tab #\tab) (cons 'return #\return)
        (cons 'page #\page) (cons 'newline #\newline) (cons 'linefeed #\newline)
        (cons 'backspace #\backspace) (cons 'delete #\delete) (cons 'rubout #\delete)
        (cons 'nul #\nul) (cons 'vtab #\vtab) (cons 'alarm #\alarm)
        (cons 'esc #\esc) (cons 'bel #\alarm) (cons 'vt #\vtab)
        (cons 'nel (integer->char #x85)) (cons 'ls (integer->char #x2028))))
(define char-name
  (case-lambda
    ((x)
     (cond ((symbol? x) (let ((e (assq x char-names))) (and e (cdr e))))
           ((char? x)
            (let loop ((l char-names))
              (cond ((null? l) #f)
                    ((char=? (cdar l) x) (caar l))
                    (else (loop (cdr l))))))
           (else (error 'char-name "not a symbol or character" x))))
    ((name c)
     (set! char-names
           (let remove ((l char-names))
             (cond ((null? l) '())
                   ((or (eq? (caar l) name) (and c (char=? (cdar l) c))) (remove (cdr l)))
                   (else (cons (car l) (remove (cdr l)))))))
     (when c (set! char-names (cons (cons name c) char-names))))))

(define (flonum->fixnum x)
  (unless (flonum? x) (error 'flonum->fixnum "not a flonum" x))
  (exact (truncate x)))
(define (flsingle x)
  (let ((b (make-bytevector 4)))
    (bytevector-ieee-single-native-set! b 0 x)
    (bytevector-ieee-single-native-ref b 0)))
(define (immutable-vector-append . vs) (vector->immutable-vector (apply vector-append vs)))
(define (immutable-vector-copy v . range)
  (vector->immutable-vector (apply vector-copy v range)))
(define (vector-cas! v i old new)
  (and (eq? (vector-ref v i) old) (begin (vector-set! v i new) #t)))

;; Bytevector integers of 3, 5, 6 and 7 bytes
(define-syntax define-bytevector-int-accessors
  (syntax-rules ()
    ((_ (size u-ref s-ref u-set! s-set!) ...)
     (begin
       (begin
         (define (u-ref bv i endianness) (bytevector-uint-ref bv i endianness size))
         (define (s-ref bv i endianness) (bytevector-sint-ref bv i endianness size))
         (define (u-set! bv i n endianness) (bytevector-uint-set! bv i n endianness size))
         (define (s-set! bv i n endianness) (bytevector-sint-set! bv i n endianness size)))
       ...))))
(define-bytevector-int-accessors
  (3 bytevector-u24-ref bytevector-s24-ref bytevector-u24-set! bytevector-s24-set!)
  (5 bytevector-u40-ref bytevector-s40-ref bytevector-u40-set! bytevector-s40-set!)
  (6 bytevector-u48-ref bytevector-s48-ref bytevector-u48-set! bytevector-s48-set!)
  (7 bytevector-u56-ref bytevector-s56-ref bytevector-u56-set! bytevector-s56-set!))

;; The pretty printer's formats for keywords: accepted, not used
(define pretty-format
  (case-lambda ((name) #f) ((name format) (if #f #f))))
(define decode-float %chez:decode-float)
(define (display-string s . port) (apply display s port))
