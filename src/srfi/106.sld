;;; SRFI 106: basic socket interface.  Written for Pseudoscheme, on
;;; usocket, which is loaded from Quicklisp or ASDF the first time this
;;; library is imported.
;;;
;;; A socket is a record around a usocket socket: a TCP connection, a
;;; listening TCP socket, or a UDP socket.  TCP data goes through the
;;; connection's binary stream; socket-input-port and socket-output-port
;;; are (rnrs io ports) custom binary ports over socket-recv and
;;; socket-send.
;;;
;;; Limits, from what usocket offers:
;;; - The flag constants have Linux's values, and most are advice:
;;;   address-info flags are ignored; *af-inet6* selects IPv6 only for a
;;;   server socket (it listens on ::), and a client connects to
;;;   whatever address the node resolves to.
;;; - socket-recv understands *msg-waitall* (wait for SIZE bytes, or the
;;;   end of the stream); *msg-peek* and *msg-oob* raise an error.
;;; - A service is a port number as a string ("8080") or one of a few
;;;   well-known names (service-ports below); there is no getservbyname.
(define-library (srfi 106)
  (export make-client-socket make-server-socket socket? socket-accept
          socket-send socket-recv socket-shutdown socket-close
          socket-input-port socket-output-port call-with-socket
          address-family address-info socket-domain ip-protocol
          message-type shutdown-method
          socket-merge-flags socket-purge-flags
          *af-unspec* *af-inet* *af-inet6*
          *sock-stream* *sock-dgram*
          *ai-canonname* *ai-numerichost* *ai-v4mapped* *ai-all*
          *ai-addrconfig*
          *ipproto-ip* *ipproto-tcp* *ipproto-udp*
          *msg-peek* *msg-oob* *msg-waitall*
          *shut-rd* *shut-wr* *shut-rdwr*)
  (import (scheme base) (scheme char)
          (only (rnrs io ports (6))
                make-custom-binary-input-port make-custom-binary-output-port)
          (pseudoscheme lisp)
          (prefix (cl usocket) us:)
          (prefix (only (cl common-lisp) read-byte write-sequence
                        finish-output listen logior logand lognot
                        unsigned-byte subseq make-array)
                  cl:))
  (begin
    ;; ------------------------------------------------------------
    ;; Flags

    (define *af-unspec* 0)
    (define *af-inet* 2)
    (define *af-inet6* 10)
    (define *sock-stream* 1)
    (define *sock-dgram* 2)
    (define *ai-canonname* 2)
    (define *ai-numerichost* 4)
    (define *ai-v4mapped* 8)
    (define *ai-all* 16)
    (define *ai-addrconfig* 32)
    (define *ipproto-ip* 0)
    (define *ipproto-tcp* 6)
    (define *ipproto-udp* 17)
    (define *msg-oob* 1)
    (define *msg-peek* 2)
    (define *msg-waitall* 256)
    (define *shut-rd* 0)
    (define *shut-wr* 1)
    (define *shut-rdwr* 2)

    (define (socket-merge-flags . flags) (apply cl:logior flags))
    (define (socket-purge-flags base . flags)
      (cl:logand base (cl:lognot (apply cl:logior flags))))

    (define-syntax address-family
      (syntax-rules (inet inet6 unspec)
        ((_ inet) *af-inet*)
        ((_ inet6) *af-inet6*)
        ((_ unspec) *af-unspec*)))

    (define-syntax socket-domain
      (syntax-rules (stream datagram)
        ((_ stream) *sock-stream*)
        ((_ datagram) *sock-dgram*)))

    (define-syntax ip-protocol
      (syntax-rules (ip tcp udp)
        ((_ ip) *ipproto-ip*)
        ((_ tcp) *ipproto-tcp*)
        ((_ udp) *ipproto-udp*)))

    (define-syntax address-info
      (syntax-rules ()
        ((_ name ...) (socket-merge-flags (address-info-flag name) ...))))
    (define-syntax address-info-flag
      (syntax-rules (canoname canonname numerichost v4mapped all addrconfig)
        ((_ canoname) *ai-canonname*)   ; the SRFI's spelling
        ((_ canonname) *ai-canonname*)
        ((_ numerichost) *ai-numerichost*)
        ((_ v4mapped) *ai-v4mapped*)
        ((_ all) *ai-all*)
        ((_ addrconfig) *ai-addrconfig*)))

    (define-syntax message-type
      (syntax-rules ()
        ((_ name ...) (socket-merge-flags (message-type-flag name) ...))))
    (define-syntax message-type-flag
      (syntax-rules (none peek oob wait-all)
        ((_ none) 0)
        ((_ peek) *msg-peek*)
        ((_ oob) *msg-oob*)
        ((_ wait-all) *msg-waitall*)))

    ;; read and write together are *shut-rdwr*.
    (define-syntax shutdown-method
      (syntax-rules ()
        ((_ name ...) (shutdown-flags (list (shutdown-method-flag name) ...)))))
    (define-syntax shutdown-method-flag
      (syntax-rules (read write)
        ((_ read) 'read)
        ((_ write) 'write)))
    (define (shutdown-flags names)
      (let ((r (memq 'read names)) (w (memq 'write names)))
        (cond ((and r w) *shut-rdwr*) (r *shut-rd*) (w *shut-wr*)
              (else (error "shutdown-method: read or write expected")))))

    ;; ------------------------------------------------------------
    ;; Sockets

    (define-record-type socket
      (make-socket kind usocket)
      socket?
      (kind socket-kind)                ; connection server datagram
      (usocket socket-usocket set-socket-usocket!))

    (define octet (list cl:unsigned-byte 8))

    (define service-ports
      '(("echo" . 7) ("discard" . 9) ("daytime" . 13) ("ftp" . 21)
        ("ssh" . 22) ("telnet" . 23) ("smtp" . 25) ("domain" . 53)
        ("http" . 80) ("pop3" . 110) ("nntp" . 119) ("ntp" . 123)
        ("imap" . 143) ("https" . 443) ("submission" . 587)
        ("imaps" . 993) ("pop3s" . 995)))

    (define (service->port service)
      (cond ((exact-integer? service) service)
            ((string->number service))
            ((assoc (string-downcase service) service-ports) => cdr)
            (else (error "unknown service" service))))

    (define (make-client-socket node service . opts)
      (let ((socktype (if (and (pair? opts) (pair? (cdr opts)))
                          (cadr opts)
                          *sock-stream*))
            (port (service->port service)))
        (if (= socktype *sock-dgram*)
            (make-socket 'datagram
                         (us:socket-connect node port #:protocol #:datagram
                                            #:element-type octet))
            (make-socket 'connection
                         (us:socket-connect node port #:element-type octet)))))

    (define (make-server-socket service . opts)
      (let* ((family (if (pair? opts) (car opts) *af-inet*))
             (socktype (if (and (pair? opts) (pair? (cdr opts)))
                           (cadr opts)
                           *sock-stream*))
             (host (if (= family *af-inet6*) "::" us:*wildcard-host*))
             (port (service->port service)))
        (if (= socktype *sock-dgram*)
            (make-socket 'datagram
                         (us:socket-connect #f #f #:protocol #:datagram
                                            #:element-type octet
                                            #:local-host host #:local-port port))
            (make-socket 'server
                         (us:socket-listen host port #:reuse-address #t
                                           #:element-type octet)))))

    (define (socket-accept socket)
      (check socket 'server 'socket-accept)
      (make-socket 'connection
                   (us:socket-accept (socket-usocket socket) #:element-type octet)))

    (define (check socket kind who)
      (unless (and (socket? socket) (socket-usocket socket))
        (error "not an open socket" who socket))
      (unless (eq? (socket-kind socket) kind)
        (error "wrong kind of socket" who socket (socket-kind socket))))

    (define (connected socket who)
      (unless (and (socket? socket) (socket-usocket socket)
                   (memq (socket-kind socket) '(connection datagram)))
        (error "not a connected socket" who socket))
      (socket-usocket socket))

    (define (socket-send socket bv . flags)
      (let ((u (connected socket 'socket-send)))
        (if (eq? (socket-kind socket) 'datagram)
            (us:socket-send u bv (bytevector-length bv))
            (let ((s (us:socket-stream u)))
              (cl:write-sequence bv s)
              (cl:finish-output s)
              (bytevector-length bv)))))

    ;; At most SIZE bytes: waits for the first, then takes what has
    ;; arrived (all SIZE with *msg-waitall*).  An empty bytevector means
    ;; the peer closed the connection.
    (define (socket-recv socket size . flags)
      (let ((u (connected socket 'socket-recv))
            (flags (if (pair? flags) (car flags) 0)))
        (unless (zero? (cl:logand flags (socket-merge-flags *msg-peek* *msg-oob*)))
          (error "socket-recv: *msg-peek* and *msg-oob* are not supported" flags))
        (if (eq? (socket-kind socket) 'datagram)
            (let ((buf (make-bytevector size 0)))
              (call-with-values (lambda () (us:socket-receive u buf size))
                (lambda (buf n . _) (bytevector-copy buf 0 n))))
            (let ((s (us:socket-stream u))
                  (wait-all? (not (zero? (cl:logand flags *msg-waitall*))))
                  (buf (make-bytevector size 0)))
              (let loop ((n 0))
                (if (and (< n size) (or (= n 0) wait-all? (cl:listen s)))
                    (let ((b (cl:read-byte s #f 'eof)))
                      (if (eq? b 'eof)
                          (bytevector-copy buf 0 n)
                          (begin (bytevector-u8-set! buf n b) (loop (+ n 1)))))
                    (if (= n size) buf (bytevector-copy buf 0 n))))))))

    (define (socket-shutdown socket how)
      (let ((u (connected socket 'socket-shutdown)))
        (us:socket-shutdown u (cond ((= how *shut-rd*) #:input)
                                    ((= how *shut-wr*) #:output)
                                    (else #:io)))
        (if #f #f)))

    (define (socket-close socket)
      (let ((u (socket-usocket socket)))
        (when u
          (set-socket-usocket! socket #f)
          (us:socket-close u))
        (if #f #f)))

    (define (call-with-socket socket proc)
      (call-with-values (lambda () (proc socket))
        (lambda results (socket-close socket) (apply values results))))

    (define (socket-input-port socket)
      (connected socket 'socket-input-port)
      (make-custom-binary-input-port
       "socket"
       (lambda (bv start count)
         (let ((got (socket-recv socket count)))
           (bytevector-copy! bv start got)
           (bytevector-length got)))
       #f #f #f))

    (define (socket-output-port socket)
      (connected socket 'socket-output-port)
      (make-custom-binary-output-port
       "socket"
       (lambda (bv start count)
         (socket-send socket (bytevector-copy bv start (+ start count)))
         count)
       #f #f #f))))
