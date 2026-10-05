;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER -*-

;;;; Tests for the SRFIs backed by Common Lisp libraries: 18 (threads,
;;;; bordeaux-threads), 106 (sockets, usocket), 170 (POSIX, osicat) and
;;;; 229 (tagged procedures, closer-mop).  The Lisp libraries are loaded
;;;; on first import, from Quicklisp when it is installed.
;;;;
;;;; Usage:  sbcl --script tests/run-srfi-system-tests.lisp


(require :asdf)
(let ((setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file setup) (load setup)))
(let* ((here (make-pathname :name nil :type nil :defaults (or *load-truename* *load-pathname*)))
       (root (merge-pathnames (make-pathname :directory '(:relative :up)) here)))
  (pushnew (truename root) asdf:*central-registry* :test #'equal))
(let ((*standard-output* (make-broadcast-stream)) (*error-output* (make-broadcast-stream)))
  (handler-bind ((warning #'muffle-warning))
    (if (find-package "QL") (uiop:symbol-call "QL" "QUICKLOAD" :r7rs :silent t) (asdf:load-system :r7rs))))
(ps:disable-float-traps)
(defvar *run* 0)
(defvar *passed* 0)
(defun read-all-from-string (string)
  (with-input-from-string (in string)
    (loop for form = (funcall ps:*scheme-read* in) until (eq form ps:eof-object) collect form)))
(defun check (name thunk expected)
  (incf *run*)
  (let ((got (handler-case (funcall thunk)
	       (error (e) (list :error (remove #\Newline (princ-to-string e)))))))
    (let ((want (car (read-all-from-string expected))))
      (if (ps::scheme-equal-p got want)
	  (incf *passed*)
	  (format t "~&FAIL ~A~%  got:  ~S~%  want: ~S~%" name got want)))))
(defmacro deftest (name source expected)
  `(check ,name (lambda () (r7rs:eval ,source)) ,expected))


;;; ------------------------------------------------------------------
;;; SRFI 18
(defparameter *imp* "(import (scheme base) (srfi 18)) ")
(defmacro t18 (name src expected) `(deftest ,name (concatenate 'string *imp* ,src) ,expected))

(t18 "join returns the thunk's value"
  "(thread-join! (thread-start! (make-thread (lambda () 42))))" "42")
(t18 "thread?, name, specific"
  "(let ((t (make-thread (lambda () 1) 'worker)))
     (thread-specific-set! t 'spec)
     (list (thread? t) (thread? 5) (thread-name t) (thread-specific t)))"
  "(#t #f worker spec)")
(t18 "current-thread is a thread, stable"
  "(list (thread? (current-thread)) (eq? (current-thread) (current-thread)))" "(#t #t)")
(t18 "current-thread inside a thread is that thread"
  "(letrec ((t (make-thread (lambda () (eq? (current-thread) t))))) (thread-join! (thread-start! t)))" "#t")
(t18 "thread-start! returns the thread"
  "(let ((t (make-thread (lambda () 1)))) (eq? t (thread-start! t)))" "#t")
(t18 "many threads, mutex-protected counter"
  "(define m (make-mutex))
   (define n 0)
   (define (work) (do ((i 0 (+ i 1))) ((= i 1000)) (mutex-lock! m) (set! n (+ n 1)) (mutex-unlock! m)))
   (define ts (map (lambda (i) (thread-start! (make-thread work))) '(1 2 3 4 5 6 7 8)))
   (for-each thread-join! ts)
   n" "8000")
(t18 "join timeout with timeout-val"
  "(thread-join! (thread-start! (make-thread (lambda () (thread-sleep! 1) 'late))) 0.05 'timeout)"
  "timeout")
(t18 "join timeout raises join-timeout-exception"
  "(guard (e ((join-timeout-exception? e) 'jt))
     (thread-join! (thread-start! (make-thread (lambda () (thread-sleep! 1)))) 0.05))" "jt")
(t18 "join with an absolute time object"
  "(thread-join! (thread-start! (make-thread (lambda () (thread-sleep! 1))))
                 (seconds->time (+ (time->seconds (current-time)) 0.05)) 'abs)" "abs")
(t18 "uncaught exception"
  "(guard (e ((uncaught-exception? e) (list 'uncaught (uncaught-exception-reason e))))
     (thread-join! (thread-start! (make-thread (lambda () (raise 'boom))))))" "(uncaught boom)")
(t18 "uncaught Lisp error"
  "(guard (e ((uncaught-exception? e) (error-object? (uncaught-exception-reason e))))
     (thread-join! (thread-start! (make-thread (lambda () (car 5))))))" "#t")
(t18 "terminate a sleeping thread"
  "(define after #f)
   (define t (thread-start! (make-thread (lambda () (dynamic-wind (lambda () #f) (lambda () (thread-sleep! 10)) (lambda () (set! after #t)))))))
   (thread-sleep! 0.05)
   (thread-terminate! t)
   (list (guard (e ((terminated-thread-exception? e) 'terminated)) (thread-join! t)) after)"
  "(terminated #t)")
(t18 "a thread terminates itself"
  "(guard (e ((terminated-thread-exception? e) 'terminated))
     (thread-join! (thread-start! (make-thread (lambda () (thread-terminate! (current-thread)) 'not-here)))))"
  "terminated")
(t18 "terminate a thread that was never started"
  "(let ((t (make-thread (lambda () 1)))) (thread-terminate! t)
     (guard (e ((terminated-thread-exception? e) 'terminated)) (thread-join! t)))" "terminated")
(t18 "thread-start! twice is an error"
  "(let ((t (make-thread (lambda () 1)))) (thread-start! t)
     (guard (e (#t 'error)) (thread-start! t)))" "error")
(t18 "mutex states"
  "(define m (make-mutex 'm))
   (define s1 (mutex-state m))
   (mutex-lock! m)
   (define s2 (eq? (mutex-state m) (current-thread)))
   (mutex-unlock! m)
   (mutex-lock! m #f #f)
   (define s3 (mutex-state m))
   (mutex-unlock! m)
   (list (mutex? m) (mutex? 1) (mutex-name m) s1 s2 s3 (mutex-state m))"
  "(#t #f m not-abandoned #t not-owned not-abandoned)")
(t18 "mutex-specific"
  "(let ((m (make-mutex))) (mutex-specific-set! m 7) (mutex-specific m))" "7")
(t18 "mutex-lock! timeout returns #f"
  "(define m (make-mutex)) (mutex-lock! m)
   (thread-join! (thread-start! (make-thread (lambda () (mutex-lock! m 0.05)))))" "#f")
(t18 "a mutex can be unlocked by another thread"
  "(define m (make-mutex)) (mutex-lock! m)
   (thread-join! (thread-start! (make-thread (lambda () (mutex-unlock! m)))))
   (mutex-state m)" "not-abandoned")
(t18 "abandoned mutex"
  "(define m (make-mutex))
   (thread-join! (thread-start! (make-thread (lambda () (mutex-lock! m) 'done))))
   (define s (mutex-state m))
   (list s (guard (e ((abandoned-mutex-exception? e) 'abandoned)) (mutex-lock! m))
         (eq? (mutex-state m) (current-thread)))"
  "(abandoned abandoned #t)")
(t18 "waiter wakes when owner dies"
  "(define m (make-mutex))
   (define cv (make-condition-variable))
   (define t (thread-start! (make-thread (lambda () (mutex-lock! m) (thread-sleep! 0.1) 'bye))))
   (thread-sleep! 0.03)
   (guard (e ((abandoned-mutex-exception? e) 'abandoned)) (mutex-lock! m 5))" "abandoned")
(t18 "condition variable: producer/consumer"
  "(define m (make-mutex)) (define cv (make-condition-variable 'cv))
   (define q '())
   (define consumer
     (thread-start!
      (make-thread
       (lambda ()
         (let loop ((got '()))
           (if (= (length got) 5) (reverse got)
               (begin (mutex-lock! m)
                      (if (null? q)
                          (begin (mutex-unlock! m cv) (loop got))
                          (let ((x (car q))) (set! q (cdr q)) (mutex-unlock! m) (loop (cons x got)))))))))))
   (do ((i 0 (+ i 1))) ((= i 5))
     (mutex-lock! m) (set! q (append q (list i))) (condition-variable-signal! cv) (mutex-unlock! m)
     (thread-sleep! 0.01))
   (list (condition-variable? cv) (condition-variable-name cv) (thread-join! consumer 5))"
  "(#t cv (0 1 2 3 4))")
(t18 "mutex-unlock! with cv times out"
  "(let ((m (make-mutex)) (cv (make-condition-variable)))
     (mutex-lock! m) (mutex-unlock! m cv 0.05))" "#f")
(t18 "broadcast wakes all"
  "(define m (make-mutex)) (define cv (make-condition-variable)) (define go #f)
   (define (waiter) (mutex-lock! m) (let loop () (if go (begin (mutex-unlock! m) 'woke) (begin (mutex-unlock! m cv) (mutex-lock! m) (loop)))))
   (define ts (map (lambda (i) (thread-start! (make-thread waiter))) '(1 2 3)))
   (thread-sleep! 0.05)
   (mutex-lock! m) (set! go #t) (condition-variable-broadcast! cv) (mutex-unlock! m)
   (map (lambda (t) (thread-join! t 5 'stuck)) ts)" "(woke woke woke)")
(t18 "condition-variable-specific"
  "(let ((c (make-condition-variable))) (condition-variable-specific-set! c 'x) (condition-variable-specific c))" "x")
(t18 "time objects"
  "(let* ((t1 (current-time)) (s (time->seconds t1)))
     (list (time? t1) (time? s) (> s 1.7e9) (time? (seconds->time 0)) (time->seconds (seconds->time 10))))"
  "(#t #f #t #t 10.)")
(t18 "thread-sleep! with a time object, and yield"
  "(let ((t0 (time->seconds (current-time))))
     (thread-sleep! (seconds->time (+ t0 0.05)))
     (thread-yield!)
     (>= (- (time->seconds (current-time)) t0) 0.04))" "#t")
(t18 "threads inherit current-output-port"
  "(import (scheme write))
   (let ((p (open-output-string)))
     (parameterize ((current-output-port p))
       (thread-join! (thread-start! (make-thread (lambda () (display \"hi\"))))))
     (get-output-string p))" "\"hi\"")
(t18 "current-exception-handler is the installed handler"
  "(let ((h (lambda (e) 'handled)))
     (with-exception-handler h (lambda () (eq? h (current-exception-handler)))))" "#t")
(t18 "(import (scheme base) (srfi 18)) has no conflict, raise works"
  "(guard (e ((symbol? e) e)) (raise 'x))" "x")


;;; ------------------------------------------------------------------
;;; SRFI 106
(defparameter *imp* "(import (scheme base) (srfi 18) (srfi 106)) ")
(defmacro t106 (name src expected) `(deftest ,name (concatenate 'string *imp* ,src) ,expected))

(t106 "flag macros and constants"
  "(list (address-family inet) (address-family inet6) (address-family unspec)
         (socket-domain stream) (socket-domain datagram)
         (ip-protocol ip) (ip-protocol tcp) (ip-protocol udp)
         (= (message-type none) 0) (= (message-type peek oob) (socket-merge-flags *msg-peek* *msg-oob*))
         (= (shutdown-method read write) *shut-rdwr*) (= (shutdown-method write) *shut-wr*)
         (= (address-info v4mapped addrconfig) (socket-merge-flags *ai-v4mapped* *ai-addrconfig*))
         (= (socket-purge-flags (socket-merge-flags *ai-all* *ai-v4mapped*) *ai-all*) *ai-v4mapped*))"
  "(2 10 0 1 2 0 6 17 #t #t #t #t #t #t)")

(t106 "TCP echo: server in a thread, send/recv"
  "(define server (make-server-socket \"47811\"))
   (define echo
     (thread-start!
      (make-thread
       (lambda ()
         (let ((c (socket-accept server)))
           (let loop ((total 0))
             (let ((bv (socket-recv c 100)))
               (if (zero? (bytevector-length bv))
                   (begin (socket-close c) total)
                   (begin (socket-send c bv) (loop (+ total (bytevector-length bv))))))))))))
   (define client (make-client-socket \"localhost\" \"47811\"))
   (define sent (socket-send client (bytevector 1 2 3 4 5)))
   (define got (socket-recv client 5 *msg-waitall*))
   (socket-shutdown client *shut-wr*)
   (define eof (socket-recv client 10))
   (socket-close client)
   (define total (thread-join! echo 5))
   (socket-close server)
   (list (socket? client) sent got eof total)"
  "(#t 5 #u8(1 2 3 4 5) #u8() 5)")

(t106 "ports over sockets, call-with-socket"
  "(import (scheme write))
   (define server (make-server-socket \"47812\" *af-inet* *sock-stream* *ipproto-ip*))
   (define t (thread-start! (make-thread (lambda ()
     (call-with-socket (socket-accept server)
       (lambda (c)
         (let ((in (socket-input-port c)) (out (socket-output-port c)))
           (let ((line (utf8->string (read-bytevector 5 in))))
             (write-bytevector (string->utf8 (string-append line \"!\")) out)
             (flush-output-port out)
             line))))))))
   (define reply
     (call-with-socket (make-client-socket \"127.0.0.1\" \"47812\" *af-inet* *sock-stream*
                                           (socket-merge-flags *ai-v4mapped* *ai-addrconfig*) *ipproto-ip*)
       (lambda (c)
         (let ((out (socket-output-port c)) (in (socket-input-port c)))
           (write-bytevector (string->utf8 \"hello\") out)
           (flush-output-port out)
           (list (utf8->string (read-bytevector 6 in)) (eof-object? (read-u8 in)))))))
   (socket-close server)
   (list reply (thread-join! t 5))"
  "((\"hello!\" #t) \"hello\")")

(t106 "UDP"
  "(define server (make-server-socket \"47813\" *af-inet* *sock-dgram*))
   (define client (make-client-socket \"127.0.0.1\" \"47813\" *af-inet* *sock-dgram*))
   (socket-send client (bytevector 9 8 7))
   (define got (socket-recv server 10))
   (socket-close client) (socket-close server)
   got"
  "#u8(9 8 7)")

(t106 "connection refused is an error"
  "(guard (e (#t 'refused)) (make-client-socket \"127.0.0.1\" \"47819\"))" "refused")
(t106 "service names"
  "(guard (e ((error-object? e) (error-object-message e))) (make-client-socket \"127.0.0.1\" \"no-such-service\"))"
  "\"unknown service\"")


;;; ------------------------------------------------------------------
;;; SRFI 229
(defparameter *imp* "(import (scheme base) (srfi 229)) ")
(defmacro t229 (name src expected) `(deftest ,name (concatenate 'string *imp* ,src) ,expected))
(t229 "lambda/tag basics"
  "(define f (lambda/tag 'tag1 (x y) (+ x y)))
   (list (f 1 2) (procedure? f) (procedure/tag? f) (procedure-tag f)
         (procedure/tag? car) (procedure/tag? (lambda (x) x)) (procedure/tag? 5))"
  "(3 #t #t tag1 #f #f #f)")
(t229 "same lambda/tag expression, different tags (no shared constant closure)"
  "(define (mk t) (lambda/tag t (x) x))
   (define a (mk 'a)) (define b (mk 'b))
   (list (procedure-tag a) (procedure-tag b) (a 1) (b 2))" "(a b 1 2)")
(t229 "#f result and #f tag"
  "(define f (lambda/tag #f () #f))
   (list (f) (procedure-tag f) (procedure/tag? f))" "(#f #f #t)")
(t229 "case-lambda/tag"
  "(define f (case-lambda/tag 42 ((x) (list 'one x)) ((x y) (list 'two x y)) ((x . r) (list 'many x r))))
   (list (f 1) (f 1 2) (f 1 2 3) (procedure-tag f))"
  "((one 1) (two 1 2) (many 1 (2 3)) 42)")
(t229 "tagged procedure in apply, map, and from Lisp"
  "(import (prefix (cl common-lisp) cl:))
   (define sq (lambda/tag 'square (x) (* x x)))
   (list (apply sq '(3)) (map sq '(1 2 3)) (cl:mapcar sq '(4 5)) (cl:funcall sq 6) (cl:functionp sq))"
  "(9 (1 4 9) (16 25) 36 #t)")
(t229 "procedure-tag of an untagged procedure is an error"
  "(guard (e ((error-object? e) 'error)) (procedure-tag car))" "error")
(t229 "recursion through a tagged procedure, closures"
  "(define n 0)
   (define fact (lambda/tag 'fact (k) (set! n (+ n 1)) (if (= k 0) 1 (* k (fact (- k 1))))))
   (list (fact 10) n)" "(3628800 11)")


;;; ------------------------------------------------------------------
;;; SRFI 170
(defparameter *dir* (namestring (merge-pathnames (format nil "pseudoscheme-srfi-170-test-~D/" (random 1000000 (make-random-state t)))
                                                  (uiop:temporary-directory))))
(uiop:delete-directory-tree (pathname *dir*) :validate t :if-does-not-exist :ignore)
(ensure-directories-exist *dir*)
(defparameter *imp* (format nil "(import (scheme base) (scheme file) (scheme write) (srfi 170) (scheme process-context) (only (srfi 151) bitwise-and bitwise-ior) (only (srfi 19) make-time time? time-second time-nanosecond time-type time-utc time-monotonic)) (define dir ~S) (define (f name) (string-append dir name)) " *dir*))
(defmacro t170 (name src expected) `(deftest ,name (concatenate 'string *imp* ,src) ,expected))

(t170 "file-info of a regular file"
  "(call-with-output-file (f \"a.txt\") (lambda (p) (write-string \"hello\" p)))
   (let ((i (file-info (f \"a.txt\") #t)))
     (list (file-info? i) (file-info:size i) (file-info-regular? i) (file-info-directory? i)
           (= (file-info:uid i) (user-uid)) (time? (file-info:mtime i))
           (> (time-second (file-info:mtime i)) 1700000000) (integer? (file-info:inode i))
           (>= (file-info:nlinks i) 1)))"
  "(#t 5 #t #f #t #t #t #t #t)")
(t170 "create-directory, directory-files, dotfiles, delete-directory"
  "(create-directory (f \"d\"))
   (call-with-output-file (f \"d/x\") (lambda (p) #t))
   (call-with-output-file (f \"d/.hidden\") (lambda (p) #t))
   (let ((r (list (file-info-directory? (file-info (f \"d\") #t))
                  (directory-files (f \"d\"))
                  (let ((l (directory-files (f \"d\") #t))) (if (string<? (car l) (cadr l)) l (reverse l)))
                  (guard (e ((posix-error? e) (posix-error-name e))) (delete-directory (f \"d\"))))))
     (delete-file (f \"d/x\")) (delete-file (f \"d/.hidden\")) (delete-directory (f \"d\"))
     (append r (list (file-exists? (f \"d\")))))
   " "(#t (\"x\") (\".hidden\" \"x\") ENOTEMPTY #f)")
(t170 "open-directory / read-directory / generator"
  "(create-directory (f \"g\"))
   (call-with-output-file (f \"g/one\") (lambda (p) #t))
   (let* ((d (open-directory (f \"g\")))
          (a (read-directory d)) (b (read-directory d)))
     (close-directory d)
     (let* ((gen (make-directory-files-generator (f \"g\"))) (x (gen)) (y (gen)))
       (delete-file (f \"g/one\")) (delete-directory (f \"g\"))
       (list a (eof-object? b) x (eof-object? y))))"
  "(\"one\" #t \"one\" #t)")
(t170 "posix errors"
  "(guard (e ((posix-error? e) (list (posix-error-name e) (string? (posix-error-message e)) (error-object? e))))
     (file-info (f \"nonexistent\") #t))" "(ENOENT #t #t)")
(t170 "posix-error? of other things"
  "(list (posix-error? 5) (guard (e (#t (posix-error? e))) (car 1)))" "(#f #f)")
(t170 "rename-file, symlinks, hard links"
  "(call-with-output-file (f \"r1\") (lambda (p) (write-string \"abc\" p)))
   (rename-file (f \"r1\") (f \"r2\"))
   (create-symlink (f \"r2\") (f \"sl\"))
   (create-hard-link (f \"r2\") (f \"hl\"))
   (let ((r (list (file-exists? (f \"r1\")) (read-symlink (f \"sl\"))
                  (file-info-symlink? (file-info (f \"sl\") #f))
                  (file-info-symlink? (file-info (f \"sl\") #t))
                  (file-info:nlinks (file-info (f \"r2\") #t))
                  (string=? (real-path (f \"sl\")) (real-path (f \"r2\"))))))
     (delete-file (f \"sl\")) (delete-file (f \"hl\")) (delete-file (f \"r2\"))
     (list (car r) (string=? (cadr r) (f \"r2\")) (list-ref r 2) (list-ref r 3) (list-ref r 4) (list-ref r 5)))"
  "(#f #t #t #f 2 #t)")
(t170 "set-file-mode, umask"
  "(call-with-output-file (f \"m\") (lambda (p) #t))
   (set-file-mode (f \"m\") #o640)
   (let ((mode (bitwise-and (file-info:mode (file-info (f \"m\") #t)) #o777))
         (old (umask)))
     (set-umask! #o027)
     (let ((new (umask)))
       (set-umask! old)
       (delete-file (f \"m\"))
       (list mode new (= old (umask)))))"
  "(416 23 #t)")
(t170 "truncate-file, set-file-times"
  "(call-with-output-file (f \"t\") (lambda (p) (write-string \"0123456789\" p)))
   (truncate-file (f \"t\") 4)
   (set-file-times (f \"t\") (make-time time-utc 0 1000000000) (make-time time-utc 500 1500000000))
   (let ((i (file-info (f \"t\") #t)))
     (set-file-times (f \"t\") time/unchanged time/now)
     (let ((j (file-info (f \"t\") #t)))
       (delete-file (f \"t\"))
       (list (file-info:size i) (time-second (file-info:atime i)) (time-second (file-info:mtime i))
             (time-second (file-info:atime j)) (> (time-second (file-info:mtime j)) 1700000000))))"
  "(4 1000000000 1500000000 1000000000 #t)")
(t170 "create-temp-file, temp-file-prefix, call-with-temporary-filename"
  "(define name (create-temp-file (f \"tmp\")))
   (define mode (bitwise-and (file-info:mode (file-info name #t)) #o777))
   (define d (call-with-temporary-filename (lambda (n) (create-directory n) n) (f \"tdir.\")))
   (define r (list (string? (temp-file-prefix)) (file-exists? name) mode
                   (file-info-directory? (file-info d #t))))
   (delete-file name) (delete-directory d)
   r" "(#t #t 384 #t)")
(t170 "call-with-temporary-filename retries on #f"
  "(define n 0)
   (call-with-temporary-filename (lambda (name) (set! n (+ n 1)) (and (= n 3) n)) (f \"x\"))" "3")
(t170 "open-file: textual output with flags, binary input, exclusive"
  "(define p (open-file (f \"o\") textual-output (bitwise-ior open/create open/truncate) #o600))
   (write-string \"line\" p) (newline p) (close-port p)
   (define p2 (open-file (f \"o\") textual-output (bitwise-ior open/append)))
   (write-string \"more\" p2) (close-port p2)
   (define in (open-file (f \"o\") binary-input 0))
   (define bytes (read-bytevector 100 in)) (close-port in)
   (define tin (open-file (f \"o\") textual-input 0))
   (define line (read-line tin)) (close-port tin)
   (define excl (guard (e ((posix-error? e) (posix-error-name e)))
                  (open-file (f \"o\") binary-output (bitwise-ior open/create open/exclusive))))
   (define bout (open-file (f \"b\") binary-output (bitwise-ior open/create open/truncate)))
   (write-bytevector (bytevector 1 2 3) bout) (close-port bout)
   (define io (open-file (f \"b\") binary-input/output 0))
   (define first (read-u8 io)) (close-port io)
   (define r (list (string=? (utf8->string bytes) (string-append \"line\" (string #\\newline) \"more\")) line excl
                   (file-info:size (file-info (f \"b\") #t)) first
                   (bitwise-and (file-info:mode (file-info (f \"o\") #t)) #o777)))
   (delete-file (f \"o\")) (delete-file (f \"b\"))
   r" "(#t \"line\" EEXIST 3 1 384)")
(t170 "file-info and truncate on a port, fd->port"
  "(define p (open-file (f \"fp\") binary-output (bitwise-ior open/create)))
   (write-bytevector (bytevector 1 2 3 4 5 6) p) (flush-output-port p)
   (define s1 (file-info:size (file-info p #f)))
   (truncate-file p 2)
   (define s2 (file-info:size (file-info (f \"fp\") #t)))
   (close-port p) (delete-file (f \"fp\"))
   (list s1 s2 (> (file-space dir) 0) (input-port? (fd->port 0 textual-input)))" "(6 2 #t #t)")
(t170 "process state"
  "(list (exact-integer? (pid)) (> (pid) 0) (exact-integer? (user-uid)) (exact-integer? (user-gid))
         (= (user-uid) (user-effective-uid)) (list? (user-supplementary-gids)) (exact-integer? (nice 0)))"
  "(#t #t #t #t #t #t #t)")
(t170 "current-directory / set-current-directory!"
  "(define old (current-directory))
   (set-current-directory! dir)
   (call-with-output-file \"rel.txt\" (lambda (p) (write-string \"r\" p)))
   (define here (file-exists? (f \"rel.txt\")))
   (define cwd (current-directory))
   (delete-file (f \"rel.txt\"))
   (set-current-directory! old)
   (list here (string=? (real-path cwd) (real-path dir)) (string=? old (current-directory)))"
  "(#t #t #t)")
(t170 "user-info, group-info"
  "(define u (user-info (user-uid)))
   (define g (group-info (user-gid)))
   (list (user-info? u) (= (user-info:uid u) (user-uid)) (string? (user-info:name u))
         (string? (user-info:home-dir u)) (string? (user-info:shell u))
         (user-info? (user-info (user-info:name u))) (list? (user-info:parsed-full-name u))
         (user-info \"no-such-user-xyzzy\")
         (group-info? g) (= (group-info:gid g) (user-gid)) (string? (group-info:name g))
         (group-info? (group-info (group-info:name g))) (group-info \"no-such-group-xyzzy\"))"
  "(#t #t #t #t #t #t #t #f #t #t #t #t #f)")
(t170 "posix-time / monotonic-time"
  "(define a (monotonic-time)) (define b (monotonic-time)) (define t (posix-time))
   (list (eq? (time-type t) time-utc) (> (time-second t) 1700000000)
         (eq? (time-type a) time-monotonic)
         (or (> (time-second b) (time-second a)) (and (= (time-second b) (time-second a)) (>= (time-nanosecond b) (time-nanosecond a)))))"
  "(#t #t #t #t)")
(t170 "environment variables"
  "(set-environment-variable! \"SRFI170_TEST\" \"yes\")
   (define a (get-environment-variable \"SRFI170_TEST\"))
   (delete-environment-variable! \"SRFI170_TEST\")
   (list a (get-environment-variable \"SRFI170_TEST\"))" "(\"yes\" #f)")
(t170 "terminal?"
  "(list (boolean? (terminal? (current-output-port))) (terminal? (open-input-string \"x\")))" "(#t #f)")
(t170 "create-fifo"
  "(create-fifo (f \"fifo\"))
   (let ((r (file-info-fifo? (file-info (f \"fifo\") #t)))) (delete-file (f \"fifo\")) r)" "#t")


(uiop:delete-directory-tree (pathname *dir*) :validate t :if-does-not-exist :ignore)

(format t "~&SRFI 18, 106, 170, 229: ~A of ~A tests passed.~%" *passed* *run*)
(uiop:quit (if (= *passed* *run*) 0 1))
