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
(define (atom? x) (not (pair? x)))
(define (syntax-object->datum x) (syntax->datum x))
(define (datum->syntax-object id x) (datum->syntax id x))
;; buffered input and output discarded
(define clear-input-port
  (case-lambda (() (clear-input-port (current-input-port))) ((p) (%chez:clear-port p #t))))
(define clear-output-port
  (case-lambda (() (clear-output-port (current-output-port))) ((p) (%chez:clear-port p #f))))
(define (sub1 n) (- n 1))
(define call/1cc call/cc)
(define gensym
  (case-lambda (() (%chez:make-gensym "g" #t)) ((name) (%chez:make-gensym name))))
(define getenv %get-environment-variable)
(define system %chez:system)
(define open-process-ports
  (case-lambda
    ((command) (open-process-ports command (buffer-mode block) #f))
    ((command mode) (open-process-ports command mode #f))
    ((command mode transcoder) (%chez:open-process-ports command transcoder))))
(define (process command)
  (call-with-values
    (lambda () (open-process-ports command (buffer-mode block) (native-transcoder)))
    (lambda (to-stdin from-stdout from-stderr pid)
      (list from-stdout to-stdin pid))))
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
;; where include and the like look for source files: a list of strings
(define source-directories
  (make-parameter '(".")
    (lambda (x)
      (unless (and (list? x) (for-all string? x))
        (assertion-violation 'source-directories "not a list of strings" x))
      x)))
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
  (%chez:check-mutable 'string-copy! dst "string")
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

(define-record-type (time-type* %make-time time?)
  (fields (mutable type time-type %set-time-type!)
          (mutable nanosecond time-nanosecond %set-time-nanosecond!)
          (mutable second time-second %set-time-second!)))

(define (check-time-type who t)
  (unless (memq t '(time-duration time-monotonic time-process time-thread time-utc
                    time-collector-cpu time-collector-real))
    (assertion-violation who "invalid time type" t)))
(define (check-time-nanosecond who n)
  (unless (and (integer? n) (exact? n) (<= 0 n 999999999))
    (assertion-violation who "invalid nanosecond" n)))
(define (check-time-second who s)
  (unless (and (integer? s) (exact? s)) (assertion-violation who "invalid second" s)))
(define (make-time type ns s)
  (check-time-type 'make-time type)
  (check-time-nanosecond 'make-time ns)
  (check-time-second 'make-time s)
  (%make-time type ns s))
(define (set-time-type! t type)
  (unless (time? t) (assertion-violation 'set-time-type! "not a time record" t))
  (check-time-type 'set-time-type! type)
  (%set-time-type! t type))
(define (set-time-nanosecond! t n)
  (unless (time? t) (assertion-violation 'set-time-nanosecond! "not a time record" t))
  (check-time-nanosecond 'set-time-nanosecond! n)
  (%set-time-nanosecond! t n))
(define (set-time-second! t s)
  (unless (time? t) (assertion-violation 'set-time-second! "not a time record" t))
  (check-time-second 'set-time-second! s)
  (%set-time-second! t s))

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
       ((time-collector-cpu time-collector-real time-duration) (make-time type 0 0))
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

(define-record-type (date-type* %make-date date?)
  (fields nanosecond second minute hour day month year zone-offset)
  (nongenerative))
(define (days-in-month month year)
  (case month
    ((1 3 5 7 8 10 12) 31)
    ((4 6 9 11) 30)
    (else (if (and (zero? (mod year 4)) (or (not (zero? (mod year 100))) (zero? (mod year 400)))) 29 28))))
(define (check-date-offset who tz)
  (unless (and (integer? tz) (exact? tz) (<= (- 86400) tz 86400))
    (assertion-violation who "invalid time zone offset" tz)))
(define make-date
  (case-lambda
    ((ns s m h d mo y) (make-date ns s m h d mo y (%chez:timezone-offset)))
    ((ns s m h d mo y tz)
     (let ((int? (lambda (x) (and (integer? x) (exact? x)))))
       (unless (and (int? ns) (<= 0 ns 999999999)) (assertion-violation 'make-date "invalid nanosecond" ns))
       (unless (and (int? s) (<= 0 s 61)) (assertion-violation 'make-date "invalid second" s))
       (unless (and (int? m) (<= 0 m 59)) (assertion-violation 'make-date "invalid minute" m))
       (unless (and (int? h) (<= 0 h 23)) (assertion-violation 'make-date "invalid hour" h))
       (unless (int? y) (assertion-violation 'make-date "invalid year" y))
       (unless (and (int? mo) (<= 1 mo 12)) (assertion-violation 'make-date "invalid month" mo))
       (unless (and (int? d) (<= 1 d (days-in-month mo y))) (assertion-violation 'make-date "invalid day" d))
       (check-date-offset 'make-date tz)
       (%make-date ns s m h d mo y tz)))))
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
  (unless (date? d) (assertion-violation 'date->time-utc "not a date record" d))
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
  (unless (date? d) (assertion-violation 'date-week-day "not a date record" d))
  ;; of the date's own day, whatever its zone offset
  (let* ((y (if (<= (date-month d) 2) (- (date-year d) 1) (date-year d)))
         (era (div y 400))
         (yoe (- y (* era 400)))
         (m (date-month d))
         (doy (+ (div (+ (* 153 (+ m (if (> m 2) -3 9))) 2) 5) (- (date-day d) 1)))
         (doe (+ (* yoe 365) (div yoe 4) (- (div yoe 100)) doy))
         (days (+ (* era 146097) doe -719468)))
    (mod (+ 4 days) 7)))

(define (time-utc->date t . offset)
  (unless (time? t) (assertion-violation 'time-utc->date "not a time record" t))
  (when (pair? offset) (check-date-offset 'time-utc->date (car offset)))
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
;; "Mon Mar 23 15:01:37 2015", as ctime shows a date
(define (date-and-time . date)
  (let* ((d (cond ((null? date) (current-date))
                  ((and (null? (cdr date)) (date? (car date))) (car date))
                  (else (assertion-violation 'date-and-time "not a date record" date))))
         (pad (lambda (n) (if (< n 10) (string-append "0" (number->string n)) (number->string n)))))
    (string-append
     (vector-ref '#("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat") (date-week-day d)) " "
     (vector-ref '#("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec") (- (date-month d) 1)) " "
     (let ((day (date-day d))) (if (< day 10) (string-append " " (number->string day)) (number->string day))) " "
     (pad (date-hour d)) ":" (pad (date-minute d)) ":" (pad (date-second d)) " "
     (number->string (date-year d)))))

(record-writer (record-type-descriptor time-type*)
  (lambda (t p wr)
    (let* ((ns (+ (* (time-second t) 1000000000) (time-nanosecond t)))
           (frac (number->string (mod (abs ns) 1000000000))))
      (display "#<" p)
      (display (time-type t) p)
      (display " " p)
      (when (< ns 0) (display "-" p))
      (display (div (abs ns) 1000000000) p)
      (display "." p)
      (display (make-string (- 9 (string-length frac)) #\0) p)
      (display frac p)
      (display ">" p))))

(record-writer (record-type-descriptor date-type*)
  (lambda (d p wr)
    (display "#<date " p)
    (display (date-and-time d) p)
    (display ">" p)))

(define current-date
  (case-lambda
    (() (time-utc->date (current-time)))
    ((offset) (check-date-offset 'current-date offset) (time-utc->date (current-time) offset))))

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
    (let ((t (list-ref (apply %chez:file-times path follow?) i)))
      (make-time 'time-utc (cdr t) (car t)))))
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
(define immutable-vector-append %chez:immutable-vector-append)
(define immutable-vector-copy %chez:immutable-vector-copy)
(define (vector-cas! v i old new)
  (%chez:check-mutable 'vector-cas! v "vector")
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

;; Tracing, as Chez prints it: each call and its value on a line of its
;; own, after |, | , | |, ... for its depth
(define trace-depth (make-parameter 0))
(define (trace-prefix n)
  (let loop ([i 0] [acc '()])
    (if (> i n) (list->string (reverse acc)) (loop (+ i 1) (cons (if (even? i) #\| #\space) acc)))))
(define (%trace-procedure name f)
  (lambda args
    (let ([depth (trace-depth)])
      (display (trace-prefix depth) (trace-output-port))
      (write (cons name args) (trace-output-port))
      (newline (trace-output-port))
      (call-with-values
        (lambda () (parameterize ([trace-depth (+ depth 1)]) (apply f args)))
        (lambda vals
          (display (trace-prefix depth) (trace-output-port))
          (for-each (lambda (v) (write v (trace-output-port))) vals)
          (newline (trace-output-port))
          (apply values vals))))))
(define trace-output-port (make-parameter (current-output-port)))
(define-syntax trace-lambda
  (syntax-rules ()
    [(_ name formals body1 body2 ...) (%trace-procedure 'name (lambda formals body1 body2 ...))]))
(define-syntax trace-case-lambda
  (syntax-rules ()
    [(_ name clause ...) (%trace-procedure 'name (case-lambda clause ...))]))
(define-syntax trace-define
  (syntax-rules ()
    [(_ (name . formals) body1 body2 ...) (define name (trace-lambda name formals body1 body2 ...))]
    [(_ name expr) (define name (let ([f expr]) (%trace-procedure 'name f)))]))
(define-syntax trace-let
  (syntax-rules ()
    [(_ name ([x e] ...) body1 body2 ...)
     ((letrec ([name (trace-lambda name (x ...) body1 body2 ...)]) name) e ...)]))

;; (define-structure (name field ...) ((field init) ...)): make-name of
;; the first fields, name?, name-field and set-name-field! for all
(define-syntax define-structure
  (lambda (x)
    (define (id k . parts)
      (datum->syntax k
        (string->symbol
          (apply string-append
            (map (lambda (p) (if (string? p) p (symbol->string (syntax->datum p)))) parts)))))
    (syntax-case x ()
      [(k (name field ...)) #'(k (name field ...) ())]
      [(k (name field ...) ([afield init] ...))
       (with-syntax ([make (id #'k "make-" #'name)]
                     [pred (id #'k #'name "?")]
                     [(get ...) (map (lambda (f) (id #'k #'name "-" f)) #'(field ... afield ...))]
                     [(set ...) (map (lambda (f) (id #'k "set-" #'name "-" f "!")) #'(field ... afield ...))]
                     [(f ...) #'(field ... afield ...)])
         #'(module (make pred get ... set ...)
             (define-record-type (name tmp pred)
               (fields (mutable f get set) ...))
             (define (make field ...) (let* ([afield init] ...) (tmp field ... afield ...)))))])))

;; Chez's syntax-error, a procedure: a syntax violation about FORM
(define (syntax-error form . strings)
  (syntax-violation #f (if (null? strings) "invalid syntax" (apply string-append strings)) form))
;; the character just read, put back
(define unread-char
  (case-lambda
    ((c) (%chez:unread-char c (current-input-port)))
    ((c p) (%chez:unread-char c p))))

;; Compile-time values: what (define-syntax id (make-compile-time-value
;; x)) binds id to; a transformer's lookup procedure, (r id), gives x
(define (make-compile-time-value x) (cons '$ctv x))
(define (compile-time-value? x) (and (pair? x) (eq? (car x) '$ctv)))
(define (compile-time-value-value x)
  (unless (compile-time-value? x)
    (assertion-violation 'compile-time-value-value "not a compile-time value" x))
  (cdr x))

;; Parameters of the system, mostly without effect here
(define internal-defines-as-letrec* (make-parameter #t (lambda (x) (and x #t))))
(define file-buffer-size
  (make-parameter 65536
    (lambda (n)
      (unless (and (fixnum? n) (> n 0)) (assertion-violation 'file-buffer-size "invalid buffer size" n))
      n)))
(define print-select-flonum-exponential-format (make-parameter #f))
(define compress-level
  (make-parameter 'medium
    (lambda (x)
      (unless (memq x '(minimum low medium high maximum))
        (assertion-violation 'compress-level "invalid compression level" x))
      x)))
(define compress-format
  (make-parameter 'lz4
    (lambda (x)
      (unless (memq x '(lz4 gzip)) (assertion-violation 'compress-format "invalid compression format" x))
      x)))
(define console-input-port (make-parameter (current-input-port)))
(define console-output-port (make-parameter (current-output-port)))
(define console-error-port (make-parameter (current-error-port)))
(define current-transcoder (make-parameter (native-transcoder)))
(define (oblist) (%chez:oblist))
(define r6rs:dynamic-wind (lambda (before thunk after) (dynamic-wind before thunk after)))
(define file-port? %chez:file-port?)
(define port-length %chez:port-length)
(define port-has-port-length? %chez:port-has-port-length?)
(define (port-has-set-port-length!? p) (file-port? p))
(define ephemeron-cons cons)
(define ephemeron-pair? pair?)
(define (library-list) (%chez:library-list))
(define transcoder? %chez:transcoder?)
;; Compressed files aren't made here: a port marked compressed reads and
;; writes its file as it is (docs/chez.md)
(define (port-file-compressed! p)
  (unless (file-port? p) (assertion-violation 'port-file-compressed! "not a file port" p))
  (if #f #f))
(define (r6rs:current-input-port) (current-input-port))
(define (r6rs:current-output-port) (current-output-port))
(define (r6rs:current-error-port) (current-error-port))

;; Continuation marks (src/chez/host.lisp)
(define-syntax with-continuation-mark
  (syntax-rules ()
    [(_ key value body) (%chez:call-with-mark key value (lambda () body))]))
(define (current-continuation-marks) (%chez:current-continuation-marks))
(define (continuation-marks? x) (%chez:continuation-marks? x))
(define (continuation-next-marks k) (%chez:continuation-next-marks k))
(define (check-marks who set)
  (unless (continuation-marks? set) (assertion-violation who "not a continuation-marks object" set)))
(define continuation-marks-first
  (case-lambda
    [(set key) (continuation-marks-first set key #f)]
    [(set key none) (check-marks 'continuation-marks-first set)
     (%chez:continuation-marks-first set key none)]))
(define (continuation-marks->list set key)
  (check-marks 'continuation-marks->list set)
  (%chez:continuation-marks->list set key))
(define continuation-marks->iterator
  (case-lambda
    [(set keys) (continuation-marks->iterator set keys #f)]
    [(set keys none)
     (check-marks 'continuation-marks->iterator set)
     (unless (vector? keys) (assertion-violation 'continuation-marks->iterator "not a vector" keys))
     (%chez:continuation-marks->iterator set keys none)]))
(define call-with-immediate-continuation-mark
  (case-lambda
    [(key proc) (call-with-immediate-continuation-mark key #f proc)]
    [(key default proc)
     (unless (procedure? proc) (assertion-violation 'call-with-immediate-continuation-mark "not a procedure" proc))
     (%chez:call-with-immediate-mark key proc default)]))
;; a macro whose expansions are traced (here: an ordinary macro)
(define-syntax trace-define-syntax
  (syntax-rules () [(_ name e) (define-syntax name e)]))
;; (indirect-export id indirect-id ...): what a library's macros may refer
;; to; library exports here are all visible to their expansions already
(define-syntax indirect-export
  (syntax-rules () [(_ id indirect ...) (begin)]))
