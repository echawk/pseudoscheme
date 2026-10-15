#lang racket/base
;; Custom ports, pipes, peeking and reading into buffers, specials,
;; progress events, file ports and regexps on ports
;; (src/racket/native/ports.lisp, port-ops.lisp).  The .expected file is
;; what Racket 9.3 prints.

(define-syntax-rule (show e)
  (with-handlers ([exn:fail? (lambda (x) (printf "~s: error: ~a\n" 'e (exn-message x)))])
    (call-with-values (lambda () e)
                      (lambda vs (printf "~s => ~s\n" 'e vs)))))

;; the first line of an error message only: the rest names a complete path
(define-syntax-rule (show1 e)
  (with-handlers ([exn:fail? (lambda (x)
                               (printf "~s: error: ~a\n" 'e
                                       (car (regexp-split #rx"\n" (exn-message x)))))])
    (call-with-values (lambda () e)
                      (lambda vs (printf "~s => ~s\n" 'e vs)))))

;; custom ports
(show (let* ([n 0]
             [i (make-input-port 'x (lambda (bs)
                                      (if (< n 3)
                                          (begin (bytes-set! bs 0 (+ 97 n)) (set! n (add1 n)) 1)
                                          eof))
                                 #f void)])
        (list (peek-bytes 2 0 i) (read-bytes 5 i) (read-byte i))))
(show (let* ([got '()]
             [o (make-output-port 'o always-evt
                                  (lambda (bs s e non-block? breakable?)
                                    (set! got (cons (subbytes bs s e) got))
                                    (- e s))
                                  void)])
        (write-bytes #"hi" o) (write-string "yo" o) (flush-output o)
        (reverse got)))
(show (make-input-port 'x 1 #f void))

;; pipes
(show (let-values ([(i o) (make-pipe)])
        (write-bytes #"hello\nworld" o)
        (close-output-port o)
        (list (pipe-content-length i) (read-line i) (read-bytes-line i) (read-line i))))
(show (let-values ([(i o) (make-pipe 5)])
        (list (write-bytes-avail* #"abcdefgh" o) (write-bytes-avail* #"zz" o) (pipe-content-length i))))

;; reading into buffers, peeking
(show (let ([i (open-input-bytes #"abcdef")])
        (list (peek-bytes-avail!* (make-bytes 3) 2 #f i)
              (read-bytes-avail!* (make-bytes 10) i)
              (read-bytes-avail!* (make-bytes 10) i))))
(show (let ([i (open-input-string "a\r\nb\rc\nd")])
        (list (read-line i 'any) (read-line i 'any) (read-line i 'any) (read-line i 'any))))
(show (let ([b (make-bytes 4 0)]) (list (read-bytes! b (open-input-bytes #"xy") 1) b)))
(show (read-bytes! (make-bytes 3) (open-input-bytes #"abcdef") 5))
(show (let ([s (make-string 3 #\-)]) (list (read-string! s (open-input-string "hey!")) s)))
(show (peek-string 2 1 (open-input-string "abcd")))

;; specials
(show (let ([sp (make-input-port 'sp
                                 (lambda (bs) eof)
                                 (lambda (bs skip evt)
                                   (if (= skip 0) (lambda (src line col pos) 'sp) eof))
                                 void)])
        (list (peek-byte-or-special sp) (peek-byte-or-special sp 0 #f 'special))))
(show (write-special 'x (open-output-bytes)))

;; progress events
(show (let ([i (open-input-bytes #"abc")])
        (let ([e (port-progress-evt i)])
          (list (sync/timeout 0 e) (read-byte i) (and (sync/timeout 0 e) #t)
                (port-commit-peeked 1 e always-evt i)))))

;; locations
(show (let ([i (open-input-bytes #"abc")])
        (port-count-lines! i)
        (set-port-next-location! i 5 6 7)
        (read-byte i)
        (call-with-values (lambda () (port-next-location i)) list)))
(show (let ([i (open-input-bytes #"abc")])
        (read-byte i)
        (call-with-values (lambda () (port-next-location i)) list)))

;; closed ports
(show (let ([o (open-output-bytes)]) (close-output-port o) (write-bytes #"a" o)))
(show (let ([i (open-input-bytes #"a")]) (close-input-port i) (read-byte i)))
(show (string-port? (open-input-string "")))
(show (file-stream-buffer-mode (open-input-bytes #"") 'none))
(show (port-file-identity (open-input-bytes #"")))

;; files
;; relative to the repository, where tests/racket/run.sh runs, so that
;; error messages naming paths are the same everywhere
(define dir (build-path "build" "racket-port-ops"))
(unless (directory-exists? dir) (make-directory dir))
(define p (build-path dir "f.txt"))
(show (let ([o (open-output-file p #:exists 'truncate)])
        (write-bytes #"hello" o)
        (list (file-position o) (file-stream-buffer-mode o)
              (begin (file-truncate o 2) (close-output-port o)
                     (call-with-input-file p read-line)))))
(show1 (begin (open-output-file p) 'opened))
(show (begin (call-with-output-file p (lambda (o) (write-string "one\ntwo" o)) #:exists 'replace)
             (with-input-from-file p (lambda () (list (read-line) (read-line) (read-line))))))
(show (begin (with-output-to-file p (lambda () (display "x")) #:exists 'append)
             (call-with-input-file p (lambda (i) (read-string 100 i)))))
(show (file-or-directory-permissions p 'bits))
(show1 (begin (open-input-file (build-path dir "nonexistent")) 'opened))
(delete-file p)
(delete-directory dir)

;; regexps on ports
(show (regexp-match-peek-positions #rx"b+" (open-input-bytes #"abbc") 1))
(show (regexp-match-peek #rx"b(b)" (open-input-bytes #"abbc")))
(show (regexp-match-peek-positions #rx"é" (open-input-bytes (string->bytes/utf-8 "aéb"))))
(show (regexp-match-peek-immediate #rx"b" (open-input-bytes #"abc")))
(show (regexp-capture-group-count #rx"(a(b))(?:c)[(]"))
