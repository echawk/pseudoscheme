;;; A log-analysis pipeline in Scheme on five Common Lisp libraries.
;;;
;;;   bin/pseudoscheme --quicklisp tests/programs/log-report.scm
;;;
;;; An access log is parsed with CL-PPCRE (its register-groups-bind
;;; macro, with Scheme code in its body), timestamps with local-time
;;; (CLOS objects that Scheme keeps in records and does arithmetic on),
;;; summarized with Lisp hash tables and Scheme lists, and written out as
;;; JSON with YASON's streaming macros and as CSV with cl-csv, into Scheme
;;; string ports.  Both are read back.  The report is signed with an
;;; Ironclad HMAC over a Scheme bytevector.  The expected numbers were
;;; worked out independently.

(import (scheme base) (scheme inexact) (scheme process-context)
        (srfi 1) (only (srfi 13) string-prefix?) (srfi 64) (only (srfi 132) list-sort)
        (pseudoscheme lisp)
        (prefix (cl common-lisp) cl:)
        (prefix (cl cl-ppcre) re:)
        (prefix (cl local-time) lt:)
        (prefix (cl yason) json:)
        (prefix (cl cl-csv) csv:)
        (prefix (cl ironclad) ic:))

(test-begin "log-report")

(define log-text "\
10.0.0.1 alice [2026-10-06T09:00:01Z] \"GET /index.html\" 200 5120 12
10.0.0.2 bob [2026-10-06T09:00:05Z] \"GET /about\" 200 2048 8
10.0.0.1 alice [2026-10-06T09:01:10Z] \"POST /api/orders\" 201 512 45
10.0.0.3 - [2026-10-06T09:02:00Z] \"GET /missing\" 404 128 3
10.0.0.2 bob [2026-10-06T09:15:30Z] \"GET /api/orders\" 200 8192 30
garbage that is not a log line
10.0.0.4 carol [2026-10-06T10:00:00Z] \"GET /index.html\" 200 5120 15
10.0.0.4 carol [2026-10-06T10:00:02Z] \"GET /style.css\" 304 0 2
10.0.0.1 alice [2026-10-06T10:30:00Z] \"DELETE /api/orders/7\" 500 256 120
10.0.0.5 dave [2026-10-06T10:45:12Z] \"GET /api/report\" 200 65536 250
10.0.0.9 eve [yesterday-ish] \"GET /\" 200 1 1
10.0.0.2 bob [2026-10-06T11:05:00Z] \"PUT /api/orders/3\" 200 1024 40
10.0.0.3 - [2026-10-06T11:06:00Z] \"GET /admin\" 403 64 1
10.0.0.1 alice [2026-10-06T11:59:59Z] \"GET /api/orders\" 200 8192 28
10.0.0.5 dave [2026-10-06T12:00:00Z] \"GET /api/report\" 503 0 1000
10.0.0.4 carol [2026-10-06T12:30:00Z] \"POST /api/orders\" 201 640 50
")

;;; ------------------------------------------------------------------
;;; Parsing

(define-record-type entry
  (make-entry ip user time method path status size ms)
  entry?
  (ip entry-ip) (user entry-user) (time entry-time) (method entry-method)
  (path entry-path) (status entry-status) (size entry-size) (ms entry-ms))

;; A local-time timestamp, or #f: local-time's parse error is a Lisp
;; error, which a Scheme guard catches like any other.
(define (parse-time text)
  (guard (e (#t #f))
    (lt:parse-timestring text)))

(define line-regex
  (re:create-scanner "^(\\S+) (\\S+) \\[([^\\]]+)\\] \"(\\S+) (\\S+)\" (\\d{3}) (\\d+) (\\d+)$"))

;; An entry, or #f.  register-groups-bind is a Lisp macro: its body is
;; Lisp code, in which make-entry and parse-time are Scheme's.  When the
;; line doesn't match, its value is NIL -- which Scheme sees as ().
(define (parse-line line)
  (let ((parsed
         (re:register-groups-bind (ip user stamp method path status size ms) (line-regex line)
           (let ((time (parse-time stamp)))
             (and time
                  (make-entry ip user time method path
                              (cl:parse-integer status) (cl:parse-integer size)
                              (cl:parse-integer ms)))))))
    (and (entry? parsed) parsed)))

(define lines (re:split "\\n" log-text))
(define entries (filter-map parse-line lines))

(test-equal "lines" 16 (length lines))
(test-equal "entries parsed" 14 (length entries))
(test-equal "rejected: not a log line, and a bad timestamp" 2 (- (length lines) (length entries)))
(test-equal "a non-matching line is () from register-groups-bind" '()
            (re:register-groups-bind (a) ("(x)" "no match here") a))
(test-equal "bytes in all" 96832 (fold + 0 (map entry-size entries)))

;;; ------------------------------------------------------------------
;;; Summaries

;; Requests by status class, counted in a Lisp hash table
(define (status-classes entries)
  (let ((table (cl:make-hash-table #:test cl:equal)))
    (for-each (lambda (e)
                (let ((class (string-append (number->string (quotient (entry-status e) 100)) "xx")))
                  (lisp-set! (cl:gethash class table) (+ 1 (cl:gethash class table 0)))))
              entries)
    (list-sort (lambda (a b) (string<? (car a) (car b)))
               (map (lambda (key) (cons key (cl:gethash key table)))
                    (lisp (loop for k being the hash-keys of table collect k))))))

(test-equal "status classes" '(("2xx" . 9) ("3xx" . 1) ("4xx" . 2) ("5xx" . 2))
            (status-classes entries))

;; local-time's timestamp< is a predicate whose name doesn't say so (it
;; doesn't end in p), so its NIL comes back as () -- which is true in
;; Scheme.  lisp-true? reads it the way Lisp does.
(define (earlier? a b) (lisp-true? (lt:timestamp< a b)))

(test-equal "timestamp< returns NIL, that is (), for false" '()
            (lt:timestamp< (entry-time (second entries)) (entry-time (first entries))))
(test-equal "so ask lisp-true?" '(#t #f)
            (list (earlier? (entry-time (first entries)) (entry-time (second entries)))
                  (earlier? (entry-time (second entries)) (entry-time (first entries)))))

(define-record-type user-summary
  (make-user-summary name requests bytes mean-ms first last)
  user-summary?
  (name user-name) (requests user-requests) (bytes user-bytes)
  (mean-ms user-mean-ms) (first user-first) (last user-last))

(define (summarize-users entries)
  (let* ((named (remove (lambda (e) (string=? (entry-user e) "-")) entries))
         (names (delete-duplicates (map entry-user named))))
    (map (lambda (name)
           (let* ((mine (filter (lambda (e) (string=? (entry-user e) name)) named))
                  (times (map entry-time mine)))
             (make-user-summary
              name (length mine)
              (fold + 0 (map entry-size mine))
              (/ (fold + 0 (map entry-ms mine)) (length mine))
              (reduce (lambda (a b) (if (earlier? a b) a b)) #f times)
              (reduce (lambda (a b) (if (earlier? a b) b a)) #f times))))
         names)))

;; Sorted by Common Lisp's sort, keyed by a Scheme record accessor
(define users (cl:sort (summarize-users entries) cl:> #:key user-bytes))

(test-equal "users by bytes" '("dave" "alice" "bob" "carol") (map user-name users))
(test-equal "requests" '(2 4 3 3) (map user-requests users))
(test-equal "bytes" '(65536 14080 11264 5760) (map user-bytes users))
(test-equal "mean latency, exact" (list 625 205/4 26 67/3) (map user-mean-ms users))
(test-equal "time between first and last request (local-time)" '(4488 10798 7495 9000)
            (map (lambda (u) (lt:timestamp-difference (user-last u) (user-first u))) users))

(define (hour-of e) (lt:timestamp-hour (entry-time e) #:timezone lt:+utc-zone+))
(define (busiest-hour entries)
  (let ((counts (map (lambda (h) (cons h (count (lambda (e) (= (hour-of e) h)) entries)))
                     (delete-duplicates (map hour-of entries)))))
    (fold (lambda (a best) (if (> (cdr a) (cdr best)) a best)) (car counts) (cdr counts))))

(test-equal "busiest hour (UTC) and its requests" '(9 . 5) (busiest-hour entries))

;; Endpoints, with numeric ids folded together by a regex
(define (endpoint e) (re:regex-replace-all "/\\d+" (entry-path e) "/:id"))
(test-equal "endpoints" '(("/index.html" . 2) ("/api/orders" . 4) ("/api/orders/:id" . 2) ("/api/report" . 2))
            (filter (lambda (p) (> (cdr p) 1))
                    (map (lambda (path) (cons path (count (lambda (e) (string=? (endpoint e) path)) entries)))
                         (delete-duplicates (map endpoint entries)))))

(define (percentile p numbers)
  (let ((sorted (list-sort < numbers)))
    (list-ref sorted (- (exact (ceiling (* p (length sorted)))) 1))))
(test-equal "median and p90 latency" '(28 250)
            (list (percentile 1/2 (map entry-ms entries)) (percentile 9/10 (map entry-ms entries))))

(test-equal "local-time formats a timestamp parsed in Scheme" "2026-10-06 09:00"
            (lt:format-timestring #f (entry-time (car entries))
                                  #:format '((#:year 4) #\- (#:month 2) #\- (#:day 2) #\space
                                             (#:hour 2) #\: (#:min 2))
                                  #:timezone lt:+utc-zone+))
(test-assert "timestamp arithmetic: 2h59m58s after alice's first request is her last"
             (lisp-true? (lt:timestamp= (lt:timestamp+ (lt:timestamp+ (user-first (second users)) 3 #:hour) -2 #:sec)
                            (user-last (second users)))))

;;; ------------------------------------------------------------------
;;; JSON, with YASON's streaming encoder

(define (user->json u)
  (let ((object (cl:make-hash-table #:test cl:equal)))
    (lisp-set! (cl:gethash "name" object) (user-name u))
    (lisp-set! (cl:gethash "requests" object) (user-requests u))
    (lisp-set! (cl:gethash "bytes" object) (user-bytes u))
    (lisp-set! (cl:gethash "meanMs" object) (inexact (user-mean-ms u)))
    object))

;; The macros' bodies are Lisp code: for-each, length and the rest are
;; Scheme's, and YASON's *json-output* is bound around all of it.
(define (report->json entries users)
  (json:with-output-to-string* ()
    (json:with-object ()
      (json:encode-object-element "requests" (length entries))
      (json:encode-object-element "busiestHour" (car (busiest-hour entries)))
      ;; Scheme's #f is Lisp's NIL, which YASON writes as null; JSON's
      ;; false is the symbol YASON:FALSE
      (json:encode-object-element "healthy" (if (any (lambda (e) (>= (entry-status e) 500)) entries)
                                                json:false
                                                json:true))
      (json:with-object-element ("users")
        (json:with-array ()
          (for-each (lambda (u) (json:encode-array-element (user->json u))) users))))))

;; YASON reads strings into adjustable Lisp strings (with fill
;; pointers), which Scheme's string procedures don't take: Scheme
;; strings are simple strings.  string=? happens to work on them, but
;; string? is #f, so copy them into Scheme strings.
(define (scheme-string lisp-string) (cl:coerce lisp-string (lisp-symbol "simple-string" "cl")))
(define (json-ref object key) (cl:gethash key object))
(define (json-string object key) (scheme-string (json-ref object key)))

(define report (report->json entries users))
(define parsed (json:parse report))

(test-assert "the report is a JSON object" (string-prefix? "{\"requests\":14," report))
(test-equal "read back: requests" 14 (cl:gethash "requests" parsed))
(test-equal "read back: user names, in order" '("dave" "alice" "bob" "carol")
            (map (lambda (u) (json-string u "name")) (json-ref parsed "users")))
(test-assert "a string YASON read, copied, is a Scheme string"
             (let ((name (json-string (car (json-ref parsed "users")) "name")))
               (and (string? name) (= 4 (string-length name)))))
(test-equal "read back: mean latencies" '(625.0 51.25 26.0)
            (map (lambda (u) (cl:gethash "meanMs" u)) (take (cl:gethash "users" parsed) 3)))
(test-assert "read back: carol's mean, a double"
             (< (abs (- (cl:gethash "meanMs" (fourth (cl:gethash "users" parsed))) 22.3333)) 0.001))

;; JSON's false and null both parse as NIL, which is Scheme's () -- true!
(test-equal "by default false is NIL, which is ()" '(#t () () ())
            (json:parse "[true, false, null, []]"))
(test-assert "() is true in Scheme" (if (cl:gethash "healthy" parsed) #t #f))
;; YASON can parse them as symbols, under a Lisp special bound by Scheme
(test-equal "false as YASON:FALSE, bound with lisp-let"
            (list json:true json:false '())
            (lisp-let ((json:*parse-json-booleans-as-symbols* #t))
              (json:parse "[true, false, null]")))
(test-assert "the report says unhealthy (two 5xx)"
             (eq? json:false
                  (lisp-let ((json:*parse-json-booleans-as-symbols* #t))
                    (cl:gethash "healthy" (json:parse report)))))
(test-equal "#f and () both encode as null" "[true,null,null,false]"
            (json:with-output-to-string* () (json:encode (list #t #f '() json:false))))

;;; ------------------------------------------------------------------
;;; CSV, through Scheme string ports (which are Lisp streams)

(define (users->csv users)
  (let ((out (open-output-string)))
    (csv:write-csv-row '("user" "requests" "bytes" "note") #:stream out)
    (for-each (lambda (u)
                (csv:write-csv-row (list (user-name u) (number->string (user-requests u))
                                         (number->string (user-bytes u))
                                         (if (> (user-bytes u) 50000) "big, \"heavy\" user" ""))
                                   #:stream out))
              users)
    (get-output-string out)))

(define csv-text (users->csv users))
(define csv-rows (csv:read-csv (open-input-string csv-text)))

(test-equal "CSV rows" 5 (length csv-rows))
(test-equal "CSV header" '("user" "requests" "bytes" "note") (car csv-rows))
(test-equal "CSV quoting round trip" '("dave" "2" "65536" "big, \"heavy\" user") (cadr csv-rows))
(test-equal "CSV totals agree with the summaries"
            (fold + 0 (map user-bytes users))
            (fold + 0 (map (lambda (row) (string->number (third row))) (cdr csv-rows))))

;;; ------------------------------------------------------------------
;;; Signing the report: Ironclad on Scheme bytevectors

(define (hmac-sha256 key bytes)
  (let ((mac (ic:make-hmac key #:sha256)))
    (ic:update-hmac mac bytes)
    (ic:hmac-digest mac)))

(define (hex bytes) (ic:byte-array-to-hex-string bytes))

(test-equal "SHA-256 test vector"
            "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
            (hex (ic:digest-sequence #:sha256 (string->utf8 "abc"))))
(test-equal "HMAC-SHA-256, RFC 4231 test case 2"
            "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843"
            (hex (hmac-sha256 (string->utf8 "Jefe") (string->utf8 "what do ya want for nothing?"))))

(define key (string->utf8 "a secret only the log server knows"))
(define signature (hmac-sha256 key (string->utf8 report)))

(test-assert "a digest is a bytevector" (and (bytevector? signature) (= 32 (bytevector-length signature))))
(test-assert "the same report, the same signature"
             (equal? signature (hmac-sha256 key (string->utf8 (report->json entries users)))))
;; constant-time-equal's name doesn't say predicate, so its NIL is ();
;; lisp-true? reads it as Lisp would
(test-equal "a tampered report fails verification" '(#t #f)
            (let ((tampered (re:regex-replace "\"requests\":14" report "\"requests\":15")))
              (map (lambda (text)
                     (lisp-true? (ic:constant-time-equal signature (hmac-sha256 key (string->utf8 text)))))
                   (list report tampered))))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "log-report")
  (exit (if (zero? failures) 0 1)))
