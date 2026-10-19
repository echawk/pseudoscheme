; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-RACKET -*-

;;;; Racket's #%network instance, natively: TCP listeners and
;;;; connections, and UDP sockets (racket/src/io/network/*.rkt).
;;;;
;;;; The system calls are the C library's, by SB-ALIEN, with the
;;;; platform's struct layouts written out here (as the Guile mode's
;;;; sockets.lisp does).  A TCP connection's two ports are ours
;;;; (ports.lisp): the input port is a lazy one whose fill procedure
;;;; reads the descriptor, the output port a custom one whose write
;;;; procedure sends on it, so every port procedure works on them.  The
;;;; output is not buffered (a flush has nothing to do); closing an end
;;;; shuts that direction down, and the descriptor is closed with the
;;;; second end (tcp-abandon-port leaves the shutdown out).
;;;; There is one thread, so a blocking call blocks the process; an
;;;; event (tcp-accept-evt, udp-receive!-evt, ...) is a revt of kind
;;;; :POLL whose target is a thunk that returns (values RESULT READY)
;;;; without blocking (control.lisp's EVT-READY runs it).  A listener is
;;;; such an event too: ready, with itself as the result, when a
;;;; connection waits.
;;;; Not yet: custodians (sockets live until closed), listeners on more
;;;; than one address of a host name, IPv6 multicast, scope ids.

(in-package "QUASISCHEME-RACKET")

;;; ------------------------------------------------------------------
;;; The platform's constants

(defparameter +net-af-inet+ 2)
(defparameter +net-af-inet6+ #+darwin 30 #-darwin 10)
(defparameter +net-sock-stream+ 1)
(defparameter +net-sock-dgram+ 2)
(defparameter +net-sol-socket+ #+darwin #xffff #-darwin 1)
(defparameter +net-so-reuseaddr+ #+darwin 4 #-darwin 2)
(defparameter +net-so-reuseport+ #+darwin #x200 #-darwin 15)
(defparameter +net-so-keepalive+ #+darwin 8 #-darwin 9)
(defparameter +net-so-rcvbuf+ #+darwin #x1002 #-darwin 8)
(defparameter +net-ipproto-tcp+ 6)
(defparameter +net-tcp-nodelay+ 1)
(defparameter +net-ipproto-ip+ 0)
(defparameter +net-ipproto-ipv6+ 41)
(defparameter +net-ipv6-v6only+ #+darwin 27 #-darwin 26)
(defparameter +net-ip-ttl+ #+darwin 4 #-darwin 2)
(defparameter +net-ip-multicast-if+ #+darwin 9 #-darwin 32)
(defparameter +net-ip-multicast-ttl+ #+darwin 10 #-darwin 33)
(defparameter +net-ip-multicast-loop+ #+darwin 11 #-darwin 34)
(defparameter +net-ip-add-membership+ #+darwin 12 #-darwin 35)
(defparameter +net-ip-drop-membership+ #+darwin 13 #-darwin 36)
(defparameter +net-ipv6-unicast-hops+ #+darwin 4 #-darwin 16)
(defparameter +net-ipv6-multicast-hops+ #+darwin 10 #-darwin 18)
(defparameter +net-ipv6-multicast-loop+ #+darwin 11 #-darwin 19)
(defparameter +net-msg-dontwait+ #+darwin #x80 #-darwin #x40)
(defparameter +net-msg-nosignal+ #+darwin 0 #-darwin #x4000)
(defparameter +net-eafnosupport+ #+darwin 47 #-darwin 97)

;;; ------------------------------------------------------------------
;;; System calls

(defmacro net-libc (name result &rest args)
  "Call C function NAME, of RESULT type, on ARGS: (type value) pairs."
  `(sb-alien:alien-funcall
    (sb-alien:extern-alien ,name (function ,result ,@(mapcar #'first args)))
    ,@(mapcar #'second args)))

(defmacro net-syscall (form)
  "FORM's integer result and, when it is negative, the errno it left (else 0)."
  `(let ((r ,form))
     (values r (if (< r 0) (sb-alien:get-errno) 0))))

(defmacro with-net-sap ((sap octets &optional (offset 0)) &body body)
  "BODY with SAP pointing OFFSET bytes into OCTETS, a simple octet vector, pinned."
  `(sb-sys:with-pinned-objects (,octets)
     (let ((,sap (sb-sys:sap+ (sb-sys:vector-sap ,octets) ,offset)))
       ,@body)))

(defun net-retry (function)
  "Call FUNCTION, a system call giving (values result errno), until it isn't interrupted."
  (loop (multiple-value-bind (r e) (funcall function)
	  (unless (and (< r 0) (= e sb-unix:eintr))
	    (return (values r e))))))

(defun net-close-fd (fd)
  (net-libc "close" sb-alien:int (sb-alien:int fd)))

(defun net-shutdown (fd how)
  (net-libc "shutdown" sb-alien:int (sb-alien:int fd) (sb-alien:int how)))

(defun net-poll (fd events timeout)
  "poll(2) on FD for EVENTS (1 read, 4 write), TIMEOUT milliseconds (-1: forever):
the events that happened (0: none; hang-ups and errors count)."
  (let ((pfd (make-octets 8)))
    (loop
      (setf (sb-sys:signed-sap-ref-32 (sb-sys:vector-sap pfd) 0) fd
	    (sb-sys:sap-ref-16 (sb-sys:vector-sap pfd) 4) events
	    (sb-sys:sap-ref-16 (sb-sys:vector-sap pfd) 6) 0)
      (multiple-value-bind (r e)
	  (with-net-sap (sap pfd)
	    (net-syscall (net-libc "poll" sb-alien:int (sb-sys:system-area-pointer sap)
				   (sb-alien:unsigned 1) (sb-alien:int timeout))))
	(cond ((and (< r 0) (= e sb-unix:eintr)))
	      ((< r 0) (return 1))	; let the call itself report it
	      (t (return (sb-sys:sap-ref-16 (sb-sys:vector-sap pfd) 6))))))))

(defun net-ready-p (fd events &optional (timeout 0))
  (not (zerop (net-poll fd events timeout))))

(defun net-setsockopt (fd level option value &optional (size 4))
  "setsockopt with the integer VALUE (SIZE bytes: 4, or 1 for the options that are a char): the errno, or 0."
  (let ((buf (make-octets 4)))
    (if (= size 1)
	(setf (aref buf 0) (logand value #xFF))
	(setf (sb-sys:signed-sap-ref-32 (sb-sys:vector-sap buf) 0) value))
    (nth-value 1 (with-net-sap (sap buf)
		   (net-syscall (net-libc "setsockopt" sb-alien:int (sb-alien:int fd) (sb-alien:int level)
					  (sb-alien:int option) (sb-sys:system-area-pointer sap)
					  (sb-alien:unsigned size)))))))

(defun net-getsockopt (fd level option)
  "getsockopt of an integer option (a char's or an int's): its value, or NIL and the errno."
  (let ((buf (make-octets 4)) (len (make-octets 4)))
    (setf (sb-sys:sap-ref-32 (sb-sys:vector-sap len) 0) 4)
    (multiple-value-bind (r e)
	(with-net-sap (sap buf)
	  (with-net-sap (lsap len)
	    (net-syscall (net-libc "getsockopt" sb-alien:int (sb-alien:int fd) (sb-alien:int level)
				   (sb-alien:int option) (sb-sys:system-area-pointer sap)
				   (sb-sys:system-area-pointer lsap)))))
      (if (< r 0)
	  (values nil e)
	  (values (sb-sys:sap-ref-32 (sb-sys:vector-sap buf) 0) 0)))))

(defun net-socket (family type)
  "A new socket: its descriptor, or NIL and the errno."
  (multiple-value-bind (fd e)
      (net-syscall (net-libc "socket" sb-alien:int (sb-alien:int family) (sb-alien:int type) (sb-alien:int 0)))
    (if (< fd 0)
	(values nil e)
	(progn
	  ;; no SIGPIPE on a closed connection (Linux's send has a flag for it)
	  #+darwin (net-setsockopt fd +net-sol-socket+ #x1022 1)
	  (values fd 0)))))

;;; ------------------------------------------------------------------
;;; Addresses: struct sockaddr octets, and the host database

(defun sockaddr-family (o)
  #+darwin (aref o 1)
  #-darwin (logior (aref o 0) (ash (aref o 1) 8)))

(defun sockaddr-port (o) (logior (ash (aref o 2) 8) (aref o 3)))

(defun net-ntop6 (o start)
  "The text of the IPv6 address at O[START, START+16)."
  (let ((in (subseq o start (+ start 16))) (out (make-octets 64)))
    (with-net-sap (isap in)
      (with-net-sap (osap out)
	(net-libc "inet_ntop" sb-sys:system-area-pointer (sb-alien:int +net-af-inet6+)
		  (sb-sys:system-area-pointer isap) (sb-sys:system-area-pointer osap)
		  (sb-alien:unsigned 64))))
    (sb-ext:octets-to-string out :end (position 0 out) :external-format :latin-1)))

(defun sockaddr-host (o)
  "The address in sockaddr O as text; an IPv4 address inside an IPv6 one shows as IPv4."
  (let ((family (sockaddr-family o)))
    (flet ((dotted (start) (format nil "~D.~D.~D.~D" (aref o start) (aref o (+ start 1))
				   (aref o (+ start 2)) (aref o (+ start 3)))))
      (cond ((= family +net-af-inet+) (dotted 4))
	    ((= family +net-af-inet6+)
	     (if (and (every #'zerop (subseq o 8 18)) (= (aref o 18) #xFF) (= (aref o 19) #xFF))
		 (dotted 20)
		 (net-ntop6 o 8)))
	    (t "0.0.0.0")))))

(defun net-resolve (host port family socktype passive)
  "The addresses of HOST (a string, or NIL) and PORT (an integer, or NIL):
a list of (family . sockaddr-octets).  Failing, NIL and getaddrinfo's error code."
  (let ((hints (make-octets 48))
	(result (make-octets 8))
	(service (cond (port (princ-to-string port)) (host nil) (t "0"))))
    (let ((h (sb-sys:vector-sap hints)))
      (setf (sb-sys:signed-sap-ref-32 h 0) (if passive 1 0)
	    (sb-sys:signed-sap-ref-32 h 4) family
	    (sb-sys:signed-sap-ref-32 h 8) socktype))
    (let ((code (with-net-sap (hsap hints)
		  (with-net-sap (rsap result)
		    (net-libc "getaddrinfo" sb-alien:int
			      (sb-alien:c-string host) (sb-alien:c-string service)
			      (sb-sys:system-area-pointer hsap) (sb-sys:system-area-pointer rsap))))))
      (if (/= code 0)
	  (values nil code)
	  (let ((head (sb-sys:sap-ref-sap (sb-sys:vector-sap result) 0)) (found '())
		(addr-at #+darwin 32 #-darwin 24))
	    (unwind-protect
		 (loop for ai = head then (sb-sys:sap-ref-sap ai 40)
		       until (zerop (sb-sys:sap-int ai))
		       do (let* ((len (sb-sys:sap-ref-32 ai 16))
				 (addr (sb-sys:sap-ref-sap ai addr-at))
				 (o (make-octets (max len 28))))
			    (dotimes (i len) (setf (aref o i) (sb-sys:sap-ref-8 addr i)))
			    (push (cons (sb-sys:signed-sap-ref-32 ai 4) o) found)))
	      (net-libc "freeaddrinfo" sb-alien:void (sb-sys:system-area-pointer head)))
	    (values (nreverse found) 0))))))

(defun sockaddr-length (o)
  "The length of the struct sockaddr that O holds."
  (let ((family (sockaddr-family o)))
    (cond ((= family +net-af-inet+) 16)
	  ((= family +net-af-inet6+) 28)
	  (t (length o)))))

(defun fd-address (fd peer)
  "The local (or, with PEER, the remote) sockaddr of FD, or NIL and the errno."
  (let ((o (make-octets 128)) (len (make-octets 4)))
    (setf (sb-sys:sap-ref-32 (sb-sys:vector-sap len) 0) 128)
    (multiple-value-bind (r e)
	(with-net-sap (sap o)
	  (with-net-sap (lsap len)
	    (if peer
		(net-syscall (net-libc "getpeername" sb-alien:int (sb-alien:int fd)
				       (sb-sys:system-area-pointer sap) (sb-sys:system-area-pointer lsap)))
		(net-syscall (net-libc "getsockname" sb-alien:int (sb-alien:int fd)
				       (sb-sys:system-area-pointer sap) (sb-sys:system-area-pointer lsap))))))
      (if (< r 0) (values nil e) (values o 0)))))

;;; ------------------------------------------------------------------
;;; Errors

(defun net-error-message (who what detail text label code)
  (format nil "~@[~A: ~]~A~@[~A~]~%  system error: ~A; ~A=~D" who what detail text label code))

(defun raise-net-posix (who what errno &optional detail)
  "exn:fail:network:errno for the errno ERRNO."
  (do-raise (make-exn "exn:fail:network:errno"
		      (net-error-message who what detail (sb-int:strerror errno) "errno" errno)
		      (cons errno (rsym "posix")))))

(defun raise-net-gai (who what code &optional detail)
  "exn:fail:network:errno for getaddrinfo's error CODE."
  (do-raise (make-exn "exn:fail:network:errno"
		      (net-error-message who what detail (net-libc "gai_strerror" sb-alien:c-string (sb-alien:int code))
					 "gai_err" code)
		      (cons code (rsym "gai")))))

(defun raise-net-socket (who what object-text)
  "exn:fail:network about a socket, with no errno."
  (do-raise (make-exn "exn:fail:network" (format nil "~A: ~A~%  socket: ~A" who what object-text))))

(defun net-port-number-p (v) (and (integerp v) (<= 1 v 65535)))
(defun net-listen-port-number-p (v) (and (integerp v) (<= 0 v 65535)))

(defun net-check-string (who v &optional or-false)
  (unless (or (stringp v) (and or-false (eq v qs:false)))
    (arg-error who (if or-false "(or/c string? #f)" "string?") v)))

(defun net-check-port (who v &optional or-false)
  (unless (or (net-port-number-p v) (and or-false (eq v qs:false)))
    (arg-error who (if or-false "(or/c port-number? #f)" "port-number?") v)))

(defun net-check-bstr (who bstr start end)
  "udp-send's and udp-receive!'s checks of the byte string and its range: START and END (NIL: the end)."
  (unless (typep bstr 'octets) (arg-error who "bytes?" bstr))
  (unless (and (integerp start) (>= start 0)) (arg-error who "exact-nonnegative-integer?" start))
  (let ((end (or end (length bstr))))
    (unless (and (integerp end) (>= end 0)) (arg-error who "exact-nonnegative-integer?" end))
    (let ((len (length bstr)))
      (unless (<= start len)
	(raise-range-error-from "raise-range-error" (rsym who) *default-realm* "byte string" "starting "
				start bstr 0 len qs:false))
      (unless (<= start end len)
	(raise-range-error-from "raise-range-error" (rsym who) *default-realm* "byte string" "ending "
				end bstr 0 len start)))
    end))

(defun net-opt (v) (if (eq v qs:false) nil v))

;;; ------------------------------------------------------------------
;;; Events

(defstruct (net-evt (:include revt (kind :poll)) (:constructor make-net-evt (target name))) name)
(defmethod print-object ((x net-evt) stream) (format stream "#<~A>" (net-evt-name x)))

(defun make-ready-evt (name poll)
  "An event that is ready, with itself as the result, when POLL (a thunk) says so."
  (let ((e (make-net-evt nil name)))
    (setf (revt-target e) (lambda () (if (funcall poll) (values e t) (values nil nil))))
    e))

;;; ------------------------------------------------------------------
;;; TCP: listeners

(defstruct (tcp-listener (:include revt (kind :poll)) (:constructor make-tcp-listener (fd)))
  fd (closed nil))
(defmethod print-object ((x tcp-listener) stream) (write-string "#<tcp-listener>" stream))

(defun listener-live (who l)
  (when (tcp-listener-closed l)
    (racket-error who "listener is closed~%  listener: #<tcp-listener>")))

(defun check-listener (who l)
  (unless (tcp-listener-p l) (arg-error who "tcp-listener?" l)))

(defun listener-accept-ready-p (l)
  (net-ready-p (tcp-listener-fd l) 1))

(defun start-listening (fd octets backlog reuse any-v6)
  "Bind FD to sockaddr OCTETS and listen: the errno, or 0."
  (when reuse
    (net-setsockopt fd +net-sol-socket+ +net-so-reuseaddr+ 1))
  (when any-v6
    ;; accept IPv4 peers too
    (net-setsockopt fd +net-ipproto-ipv6+ +net-ipv6-v6only+ 0))
  (multiple-value-bind (r e)
      (with-net-sap (sap octets)
	(net-syscall (net-libc "bind" sb-alien:int (sb-alien:int fd) (sb-sys:system-area-pointer sap)
			       (sb-alien:unsigned (sockaddr-length octets)))))
    (if (< r 0)
	e
	(multiple-value-bind (r e)
	    (net-syscall (net-libc "listen" sb-alien:int (sb-alien:int fd) (sb-alien:int backlog)))
	  (if (< r 0) e 0)))))

(defnative "tcp-listen" (port &optional (backlog 4) (reuse qs:false) (hostname qs:false)) (:instance "#%network")
  (unless (net-listen-port-number-p port) (arg-error "tcp-listen" "listen-port-number?" port))
  (unless (and (integerp backlog) (>= backlog 0)) (arg-error "tcp-listen" "exact-nonnegative-integer?" backlog))
  (net-check-string "tcp-listen" hostname t)
  (block done
   (let* ((host (net-opt hostname))
	 (reuse (truthy reuse))
	 (backlog (min backlog 10000))
	 (detail (format nil "~@[~%  hostname: ~A~]~%  port number: ~D" host port))
	 (candidates '()))
    (if host
	(multiple-value-bind (found code) (net-resolve host port 0 +net-sock-stream+ t)
	  (unless found (raise-net-gai "tcp-listen" "address-resolution error" code detail))
	  (setq candidates (mapcar (lambda (c) (list (car c) (cdr c) nil)) found)))
	;; any address: dual-stack IPv6 where there is IPv6, else IPv4
	(progn
	  (dolist (family (list +net-af-inet6+ +net-af-inet+))
	    (dolist (c (net-resolve nil port family +net-sock-stream+ t))
	      (push (list (car c) (cdr c) (= family +net-af-inet6+)) candidates)))
	  (setq candidates (nreverse candidates))
	  (unless candidates
	    (raise-net-posix "tcp-listen" "listen failed" +net-eafnosupport+ detail))))
    (let ((last-errno 0))
      (dolist (c candidates)
	(destructuring-bind (family octets any-v6) c
	  (multiple-value-bind (fd e) (net-socket family +net-sock-stream+)
	    (if (null fd)
		(setq last-errno e)
		(let ((e (start-listening fd octets backlog reuse any-v6)))
		  (if (zerop e)
		      (let ((l (make-tcp-listener fd)))
			(setf (revt-target l)
			      (lambda ()
				(if (or (tcp-listener-closed l) (listener-accept-ready-p l))
				    (values l t)
				    (values nil nil))))
			(return-from done l))
		      (progn (net-close-fd fd) (setq last-errno e))))))))
      (raise-net-posix "tcp-listen" "listen failed" last-errno detail)))))

(defnative "tcp-listener?" (x) (:instance "#%network" :leaf t) (bool (tcp-listener-p x)))

(defun close-listener (l)
  (setf (tcp-listener-closed l) t)
  (net-close-fd (tcp-listener-fd l)))

(defnative "tcp-close" (l) (:instance "#%network")
  (check-listener "tcp-close" l)
  (listener-live "tcp-close" l)
  (close-listener l)
  *void*)

(defnative "tcp-accept-ready?" (l) (:instance "#%network")
  (check-listener "tcp-accept-ready?" l)
  (listener-live "tcp-accept-ready?" l)
  (bool (listener-accept-ready-p l)))

;;; ------------------------------------------------------------------
;;; TCP: connections and their ports

(defstruct (tcp-conn (:constructor make-tcp-conn (fd)))
  fd					; NIL once both ends are closed
  (refs 2)				; the ends still open
  (abandon-in nil) (abandon-out nil)	; abandoned: closing doesn't shut down
  (eof nil))

(defvar *tcp-ports* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "A TCP port, either end -> its TCP-CONN.")

(defun tcp-end-closer (conn end)
  "The procedure that closes END (:IN or :OUT) of CONN."
  (lambda ()
    (let ((fd (tcp-conn-fd conn)))
      (when fd
	(unless (if (eq end :in) (tcp-conn-abandon-in conn) (tcp-conn-abandon-out conn))
	  (net-shutdown fd (if (eq end :in) 0 1)))
	(when (zerop (decf (tcp-conn-refs conn)))
	  (net-close-fd fd)
	  (setf (tcp-conn-fd conn) nil))))))

(defun tcp-filler (conn)
  "The fill procedure of a connection's input port."
  (let ((buf (make-octets 8192)))
    (lambda (p need blocking)
      (declare (ignore need))
      (let ((fd (tcp-conn-fd conn)))
	(cond ((or (null fd) (tcp-conn-eof conn)) :eof)
	      ((and (not blocking) (not (net-ready-p fd 1))) :block)
	      (t (multiple-value-bind (n e)
		     (net-retry (lambda ()
				  (with-net-sap (sap buf)
				    (net-syscall (net-libc "recv" sb-alien:long (sb-alien:int fd)
							   (sb-sys:system-area-pointer sap)
							   (sb-alien:unsigned-long (length buf)) (sb-alien:int 0))))))
		   (cond ((> n 0) (ip-append p buf 0 n) :data)
			 ((= n 0) (setf (tcp-conn-eof conn) t) :eof)
			 (t (raise-net-posix nil "error reading from stream port" e))))))))))

(defun tcp-writer (conn)
  "The write procedure of a connection's output port."
  (lambda (bytes start end nonblock enable-break)
    (declare (ignore enable-break))
    (let ((fd (tcp-conn-fd conn))
	  (bytes (coerce bytes 'octets))
	  (n (- end start)))
      (if (or (<= n 0) (null fd))
	  0
	  (loop
	    (when (and (truthy nonblock) (not (net-ready-p fd 4)))
	      (return qs:false))
	    (multiple-value-bind (k e)
		(net-retry (lambda ()
			     (with-net-sap (sap bytes start)
			       (net-syscall (net-libc "send" sb-alien:long (sb-alien:int fd)
						      (sb-sys:system-area-pointer sap)
						      (sb-alien:unsigned-long n)
						      (sb-alien:int +net-msg-nosignal+))))))
	      (cond ((> k 0) (return k))
		    ((= e sb-unix:eagain) (net-poll fd 4 -1))
		    (t (raise-net-posix nil "error writing to stream port" e)))))))))

(defun open-tcp-ports (fd name)
  "The input and output ports of connected socket FD."
  (net-setsockopt fd +net-ipproto-tcp+ +net-tcp-nodelay+ 1)
  (net-setsockopt fd +net-sol-socket+ +net-so-keepalive+ 1)
  (let* ((conn (make-tcp-conn fd))
	 (in (new-lazy-inport name :tcp (tcp-filler conn)
			      :custom (make-custom-in :close (tcp-end-closer conn :in))))
	 (out (finish-outport (make-routport name))))
    (setf (routport-kind out) :custom
	  (routport-custom out) (make-custom-out :write-out (tcp-writer conn)
						 :close (tcp-end-closer conn :out)))
    (setf (gethash in *tcp-ports*) conn
	  (gethash out *tcp-ports*) conn)
    (values in out)))

(defun tcp-connect-1 (who hostname port-no local-hostname local-port-no)
  (net-check-string who hostname)
  (net-check-port who port-no)
  (net-check-string who local-hostname t)
  (net-check-port who local-port-no t)
  (let ((local-host (net-opt local-hostname)) (local-port (net-opt local-port-no)))
    (when (and local-host (not local-port))
      (racket-error who "no local port number supplied when local hostname was supplied~%  hostname: ~A"
		    (written local-host)))
    (flet ((detail (h p) (format nil "~@[~%  hostname: ~A~]~@[~%  port number: ~D~]" h p)))
      (multiple-value-bind (remotes code) (net-resolve hostname port-no 0 +net-sock-stream+ nil)
	(unless remotes
	  (raise-net-gai who "host not found" code (detail hostname port-no)))
	(let ((locals nil))
	  (when (or local-host local-port)
	    (multiple-value-bind (found code) (net-resolve local-host local-port 0 +net-sock-stream+ nil)
	      (unless found
		(raise-net-gai who "local host not found" code (detail local-host local-port)))
	      (setq locals found)))
	  (let ((last-errno +net-eafnosupport+))
	    (dolist (remote remotes)
	      (let ((local (and locals (find (car remote) locals :key #'car))))
		(unless (and locals (not local))
		  (multiple-value-bind (fd e) (net-socket (car remote) +net-sock-stream+)
		    (if (null fd)
			(setq last-errno e)
			(let ((e (tcp-connect-fd fd (cdr remote) (and local (cdr local)))))
			  (if (zerop e)
			      (return-from tcp-connect-1 (open-tcp-ports fd (copy-seq hostname)))
			      (progn (net-close-fd fd) (setq last-errno e)))))))))
	    (raise-net-posix who "connection failed" last-errno (detail hostname port-no))))))))

(defun tcp-connect-fd (fd remote local)
  "Bind FD to sockaddr LOCAL (if any) and connect it to sockaddr REMOTE: the errno, or 0."
  (when local
    (let ((e (nth-value 1 (with-net-sap (sap local)
			    (net-syscall (net-libc "bind" sb-alien:int (sb-alien:int fd)
						   (sb-sys:system-area-pointer sap)
						   (sb-alien:unsigned (sockaddr-length local))))))))
      (unless (zerop e) (return-from tcp-connect-fd e))))
  (multiple-value-bind (r e)
      (with-net-sap (sap remote)
	(net-syscall (net-libc "connect" sb-alien:int (sb-alien:int fd) (sb-sys:system-area-pointer sap)
			       (sb-alien:unsigned (sockaddr-length remote)))))
    (cond ((>= r 0) 0)
	  ((= e sb-unix:eintr)
	   ;; the connection goes on: wait for it, and see how it ended
	   (net-poll fd 4 -1)
	   (multiple-value-bind (v e2) (net-getsockopt fd +net-sol-socket+ #+darwin #x1007 #-darwin 4)
	     (if v v e2)))
	  (t e))))

(defnative "tcp-connect" (hostname port-no &optional (local-hostname qs:false) (local-port-no qs:false))
    (:instance "#%network")
  (tcp-connect-1 "tcp-connect" hostname port-no local-hostname local-port-no))

(defnative "tcp-connect/enable-break" (hostname port-no &optional (local-hostname qs:false) (local-port-no qs:false))
    (:instance "#%network")
  (tcp-connect-1 "tcp-connect/enable-break" hostname port-no local-hostname local-port-no))

(defun accept-from (who l)
  "Accept a connection of listener L (waiting for one): its two ports."
  (multiple-value-bind (fd e)
      (net-retry (lambda ()
		   (net-syscall (net-libc "accept" sb-alien:int (sb-alien:int (tcp-listener-fd l))
					  (sb-sys:system-area-pointer (sb-sys:int-sap 0))
					  (sb-sys:system-area-pointer (sb-sys:int-sap 0))))))
    (when (< fd 0)
      (raise-net-posix who "accept from listener failed" e))
    #+darwin (net-setsockopt fd +net-sol-socket+ #x1022 1)
    (open-tcp-ports fd "tcp-accepted")))

(defnative "tcp-accept" (l) (:instance "#%network")
  (check-listener "tcp-accept" l)
  (listener-live "tcp-accept" l)
  (accept-from "tcp-accept" l))

(defnative "tcp-accept/enable-break" (l) (:instance "#%network")
  (check-listener "tcp-accept/enable-break" l)
  (listener-live "tcp-accept/enable-break" l)
  (accept-from "tcp-accept/enable-break" l))

(defnative "tcp-accept-evt" (l) (:instance "#%network")
  (check-listener "tcp-accept-evt" l)
  (make-net-evt (lambda ()
		  (listener-live "tcp-accept-evt" l)
		  (if (listener-accept-ready-p l)
		      (multiple-value-bind (in out) (accept-from "tcp-accept-evt" l)
			(values (list in out) t))
		      (values nil nil)))
		"tcp-accept-evt"))

(defnative "tcp-port?" (p) (:instance "#%network" :leaf t)
  (bool (gethash p *tcp-ports*)))

(defnative "tcp-abandon-port" (p) (:instance "#%network")
  (let ((conn (gethash p *tcp-ports*)))
    (when conn
      (if (rinport-p p)
	  (setf (tcp-conn-abandon-in conn) t)
	  (setf (tcp-conn-abandon-out conn) t))
      (if (rinport-p p) (close-inport p) (close-outport p))))
  *void*)

;;; ------------------------------------------------------------------
;;; UDP sockets

(defstruct (net-udp (:constructor make-net-udp (fd family)))
  fd					; NIL when closed
  family
  (bound nil) (connected nil))
(defmethod print-object ((x net-udp) stream) (write-string "#<udp>" stream))

(defun check-udp (who u)
  (unless (net-udp-p u) (arg-error who "udp?" u)))

(defun udp-live-fd (who u)
  (or (net-udp-fd u) (raise-net-socket who "udp socket is closed" "#<udp>")))

(defun udp-detail (host port)
  (format nil "~%  address: ~A~@[~%  port number: ~D~]" (or host "<unspec>") port))

(defun pick-family (candidates family)
  "CANDIDATES (family . octets) with those of FAMILY first."
  (append (remove family candidates :key #'car :test #'/=)
	  (remove family candidates :key #'car :test #'=)))

(defnative "udp-open-socket" (&optional (family-hostname qs:false) (family-port-no qs:false)) (:instance "#%network")
  (net-check-string "udp-open-socket" family-hostname t)
  (net-check-port "udp-open-socket" family-port-no t)
  (let* ((host (net-opt family-hostname)) (port (net-opt family-port-no))
	 (family +net-af-inet+))
    (when (or host port)
      (multiple-value-bind (found code) (net-resolve host port 0 +net-sock-dgram+ nil)
	(unless found
	  (raise-net-gai "udp-open-socket" "can't resolve address" code (udp-detail host port)))
	(setq family (car (first found)))))
    (multiple-value-bind (fd e) (net-socket family +net-sock-dgram+)
      (unless fd (raise-net-posix "udp-open-socket" "creation failed" e))
      (make-net-udp fd family))))

(defnative "udp?" (x) (:instance "#%network" :leaf t) (bool (net-udp-p x)))

(defnative "udp-close" (u) (:instance "#%network")
  (check-udp "udp-close" u)
  (unless (net-udp-fd u)
    (raise-net-socket "udp-close" "udp socket was already closed" "#<udp>"))
  (net-close-fd (net-udp-fd u))
  (setf (net-udp-fd u) nil)
  *void*)

(defnative "udp-bound?" (u) (:instance "#%network" :leaf t)
  (check-udp "udp-bound?" u)
  (bool (net-udp-bound u)))

(defnative "udp-connected?" (u) (:instance "#%network" :leaf t)
  (check-udp "udp-connected?" u)
  (bool (net-udp-connected u)))

(defnative "udp-bind!" (u hostname port-no &optional (reuse qs:false)) (:instance "#%network")
  (check-udp "udp-bind!" u)
  (net-check-string "udp-bind!" hostname t)
  (unless (net-listen-port-number-p port-no) (arg-error "udp-bind!" "listen-port-number?" port-no))
  (block done
   (let ((host (net-opt hostname)) (reuse (truthy reuse)))
    (multiple-value-bind (found code) (net-resolve host port-no 0 +net-sock-dgram+ t)
      (unless found
	(raise-net-gai "udp-bind!" "can't resolve address" code (udp-detail host port-no)))
      (let ((fd (udp-live-fd "udp-bind!" u)))
	(when (net-udp-bound u)
	  (racket-error "udp-bind!" "udp socket is already bound~%  socket: #<udp>"))
	(when reuse
	  (net-setsockopt fd +net-sol-socket+ +net-so-reuseaddr+ 1)
	  (net-setsockopt fd +net-sol-socket+ +net-so-reuseport+ 1))
	(let ((last-errno +net-eafnosupport+))
	  (dolist (c (pick-family found (net-udp-family u)))
	    (multiple-value-bind (r e)
		(with-net-sap (sap (cdr c))
		  (net-syscall (net-libc "bind" sb-alien:int (sb-alien:int fd) (sb-sys:system-area-pointer sap)
					 (sb-alien:unsigned (sockaddr-length (cdr c))))))
	      (if (>= r 0)
		  (progn (setf (net-udp-bound u) t)
			 (return-from done *void*))
		  (setq last-errno e))))
	  (raise-net-posix "udp-bind!" (if reuse "can't bind as reusable" "can't bind") last-errno
			   (format nil "~%  address: ~A~%  port number: ~D" (or host "<unspec>") port-no))))))) )

(defnative "udp-connect!" (u hostname port-no) (:instance "#%network")
  (check-udp "udp-connect!" u)
  (net-check-string "udp-connect!" hostname t)
  (net-check-port "udp-connect!" port-no t)
  (block done
   (let ((host (net-opt hostname)) (port (net-opt port-no)))
    (unless (eq (not host) (not port))
      (racket-error "udp-connect!"
		    "last second and third arguments must be both #f or both non-#f~%  second argument: ~A~%  third argument: ~A"
		    (written hostname) (written port-no)))
    (cond
      ((not host)
       (let ((fd (udp-live-fd "udp-connect!" u)))
	 (when (net-udp-connected u)
	   ;; an address of family AF_UNSPEC dissolves the association
	   (let ((o (make-octets 16)))
	     #+darwin (setf (aref o 0) 16)
	     (multiple-value-bind (r e)
		 (with-net-sap (sap o)
		   (net-syscall (net-libc "connect" sb-alien:int (sb-alien:int fd) (sb-sys:system-area-pointer sap)
					  (sb-alien:unsigned 16))))
	       (when (and (< r 0) (/= e +net-eafnosupport+))
		 (raise-net-posix "udp-connect!" "can't disconnect" e))))
	   (setf (net-udp-connected u) nil))
	 *void*))
      (t
       (multiple-value-bind (found code) (net-resolve host port 0 +net-sock-dgram+ nil)
	 (unless found
	   (raise-net-gai "udp-connect!" "can't resolve address" code (udp-detail host port)))
	 (let ((fd (udp-live-fd "udp-connect!" u)) (last-errno +net-eafnosupport+))
	   (dolist (c (pick-family found (net-udp-family u)))
	     (multiple-value-bind (r e)
		 (with-net-sap (sap (cdr c))
		   (net-syscall (net-libc "connect" sb-alien:int (sb-alien:int fd) (sb-sys:system-area-pointer sap)
					  (sb-alien:unsigned (sockaddr-length (cdr c))))))
	       (if (>= r 0)
		   (progn (setf (net-udp-connected u) t)
			  (return-from done *void*))
		   (setq last-errno e))))
	   (raise-net-posix "udp-connect!" "can't connect" last-errno
			    (format nil "~%  address: ~A~%  port number: ~D" host port)))))))))

;;; Sending

(defun udp-resolve-one (who u host port)
  "The sockaddr of HOST and PORT for datagrams from U."
  (multiple-value-bind (found code) (net-resolve host port 0 +net-sock-dgram+ nil)
    (unless found
      (raise-net-gai who "can't resolve address" code (udp-detail host port)))
    (cdr (first (pick-family found (net-udp-family u))))))

(defun udp-send-addr (who u addr bstr start end wait)
  "Send BSTR[START,END) from U, to sockaddr ADDR or, with none, to its peer:
T if sent; NIL if it would block (not WAIT)."
  (let ((fd (udp-live-fd who u)) (n (- end start)))
    (cond ((and addr (net-udp-connected u))
	   (raise-net-socket who "udp socket is connected" "#<udp>"))
	  ((and (not addr) (not (net-udp-connected u)))
	   (raise-net-socket who "udp socket is not connected" "#<udp>")))
    (setf (net-udp-bound u) t)		; sending binds
    (loop
      (multiple-value-bind (k e)
	  (net-retry (lambda ()
		       (with-net-sap (sap bstr start)
			 (let ((flags (logior +net-msg-nosignal+ (if wait 0 +net-msg-dontwait+))))
			   (if addr
			       (with-net-sap (asap addr)
				 (net-syscall (net-libc "sendto" sb-alien:long (sb-alien:int fd)
							(sb-sys:system-area-pointer sap)
							(sb-alien:unsigned-long n) (sb-alien:int flags)
							(sb-sys:system-area-pointer asap)
							(sb-alien:unsigned (sockaddr-length addr)))))
			       (net-syscall (net-libc "send" sb-alien:long (sb-alien:int fd)
						      (sb-sys:system-area-pointer sap)
						      (sb-alien:unsigned-long n) (sb-alien:int flags))))))))
	(cond ((= k n) (return t))
	      ((>= k 0)
	       (do-raise (make-exn "exn:fail:network"
				   (format nil "~A: didn't send enough~%  requested bytes: ~D~%  sent bytes: ~D"
					   who n k))))
	      ((or (= e sb-unix:eagain) (= e #+darwin 55 #-darwin 105))
	       (if wait (net-poll fd 4 -1) (return nil)))
	      (t (raise-net-posix who "send failed" e)))))))

(defmacro define-udp-sends (&rest specs)
  "Each spec: (name wait to-p result), the natives udp-send and udp-send-to and their variants."
  `(progn
     ,@(loop for (name wait to-p star) in specs
	     collect
	     (if to-p
		 `(defnative ,name (u hostname port-no bstr &optional (start 0) end) (:instance "#%network")
		    (check-udp ,name u)
		    (net-check-string ,name hostname)
		    (net-check-port ,name port-no)
		    (let ((end (net-check-bstr ,name bstr start end)))
		      (let ((r (udp-send-addr ,name u (udp-resolve-one ,name u hostname port-no) bstr start end ,wait)))
			,(if star '(bool r) '*void*))))
		 `(defnative ,name (u bstr &optional (start 0) end) (:instance "#%network")
		    (check-udp ,name u)
		    (let ((end (net-check-bstr ,name bstr start end)))
		      (let ((r (udp-send-addr ,name u nil bstr start end ,wait)))
			,(if star '(bool r) '*void*))))))))

(define-udp-sends
  ("udp-send" t nil nil)
  ("udp-send*" nil nil t)
  ("udp-send/enable-break" t nil nil)
  ("udp-send-to" t t nil)
  ("udp-send-to*" nil t t)
  ("udp-send-to/enable-break" t t nil))

(defnative "udp-send-evt" (u bstr &optional (start 0) end) (:instance "#%network")
  (check-udp "udp-send-evt" u)
  (let ((end (net-check-bstr "udp-send-evt" bstr start end)))
    (make-net-evt (lambda ()
		    (if (udp-send-addr "udp-send-evt" u nil bstr start end nil)
			(values *void* t)
			(values nil nil)))
		  "udp-send-evt")))

(defnative "udp-send-to-evt" (u hostname port-no bstr &optional (start 0) end) (:instance "#%network")
  (check-udp "udp-send-to-evt" u)
  (net-check-string "udp-send-to-evt" hostname)
  (net-check-port "udp-send-to-evt" port-no)
  (let ((end (net-check-bstr "udp-send-to-evt" bstr start end))
	(addr (udp-resolve-one "udp-send-to-evt" u hostname port-no)))
    (make-net-evt (lambda ()
		    (if (udp-send-addr "udp-send-to-evt" u addr bstr start end nil)
			(values *void* t)
			(values nil nil)))
		  "udp-send-to-evt")))

(defnative "udp-send-ready-evt" (u) (:instance "#%network")
  (check-udp "udp-send-ready-evt" u)
  (make-ready-evt "udp-send-ready-evt"
		  (lambda () (or (null (net-udp-fd u)) (net-ready-p (net-udp-fd u) 4)))))

;;; Receiving

(defun udp-recv (who u bstr start end wait)
  "Receive a datagram into BSTR[START,END): (values bytes host port), or three NILs if
there is none and not WAIT."
  (let ((fd (udp-live-fd who u)) (n (- end start)))
    (unless (net-udp-bound u)
      (raise-net-socket who "udp socket is not bound" "#<udp>"))
    (loop
      (when (and (not wait) (not (net-ready-p fd 1)))
	(return (values nil nil nil)))
      (let ((from (make-octets 128)) (len (make-octets 4)))
	(setf (sb-sys:sap-ref-32 (sb-sys:vector-sap len) 0) 128)
	(multiple-value-bind (k e)
	    (net-retry (lambda ()
			 (with-net-sap (sap bstr start)
			   (with-net-sap (asap from)
			     (with-net-sap (lsap len)
			       (net-syscall (net-libc "recvfrom" sb-alien:long (sb-alien:int fd)
						      (sb-sys:system-area-pointer sap)
						      (sb-alien:unsigned-long n)
						      (sb-alien:int (if wait 0 +net-msg-dontwait+))
						      (sb-sys:system-area-pointer asap)
						      (sb-sys:system-area-pointer lsap))))))))
	  (cond ((>= k 0) (return (values k (sockaddr-host from) (sockaddr-port from))))
		((and (= e sb-unix:eagain) (not wait)) (return (values nil nil nil)))
		((= e sb-unix:eagain) (net-poll fd 1 -1))
		(t (raise-net-posix who "receive failed" e))))))))

(macrolet ((define-receives (&rest specs)
	     `(progn
		,@(loop for (name wait) in specs
			collect `(defnative ,name (u bstr &optional (start 0) end) (:instance "#%network")
				   (check-udp ,name u)
				   (let ((end (net-check-bstr ,name bstr start end)))
				     ;; none waiting: #f #f #f
				     (multiple-value-bind (n host port) (udp-recv ,name u bstr start end ,wait)
				       (if n
					   (values n host port)
					   (values qs:false qs:false qs:false)))))))))
  (define-receives
    ("udp-receive!" t)
    ("udp-receive!*" nil)
    ("udp-receive!/enable-break" t)))

(defnative "udp-receive!-evt" (u bstr &optional (start 0) end) (:instance "#%network")
  (check-udp "udp-receive!-evt" u)
  (let ((end (net-check-bstr "udp-receive!-evt" bstr start end)))
    (make-net-evt (lambda ()
		    (multiple-value-bind (n host port) (udp-recv "udp-receive!-evt" u bstr start end nil)
		      (if n
			  (values (list n host port) t)
			  (values nil nil))))
		  "udp-receive-evt")))

(defnative "udp-receive-ready-evt" (u) (:instance "#%network")
  (check-udp "udp-receive-ready-evt" u)
  (make-ready-evt "udp-receive-ready-evt"
		  (lambda () (or (null (net-udp-fd u)) (net-ready-p (net-udp-fd u) 1)))))

(defnative "udp-set-receive-buffer-size!" (u size) (:instance "#%network")
  (check-udp "udp-set-receive-buffer-size!" u)
  (unless (and (integerp size) (> size 0)) (arg-error "udp-set-receive-buffer-size!" "exact-positive-integer?" size))
  (let ((fd (udp-live-fd "udp-set-receive-buffer-size!" u)))
    (unless (typep size '(signed-byte 31))
      (do-raise (make-exn "exn:fail:network"
			  (format nil "udp-set-receive-buffer-size!: given size is too large~%  given size: ~D" size))))
    (let ((e (net-setsockopt fd +net-sol-socket+ +net-so-rcvbuf+ size)))
      (unless (zerop e) (raise-net-posix "udp-set-receive-buffer-size!" "setsockopt failed" e)))
    *void*))

;;; Options: TTL, and multicast's

(defun udp-option (u kind)
  "The level, option and size of the option KIND (:ttl :mttl :mloop) for U's family."
  (if (= (net-udp-family u) +net-af-inet6+)
      (ecase kind
	(:ttl (values +net-ipproto-ipv6+ +net-ipv6-unicast-hops+ 4))
	(:mttl (values +net-ipproto-ipv6+ +net-ipv6-multicast-hops+ 4))
	(:mloop (values +net-ipproto-ipv6+ +net-ipv6-multicast-loop+ 4)))
      (ecase kind
	(:ttl (values +net-ipproto-ip+ +net-ip-ttl+ 4))
	(:mttl (values +net-ipproto-ip+ +net-ip-multicast-ttl+ 1))
	(:mloop (values +net-ipproto-ip+ +net-ip-multicast-loop+ 1)))))

(defun udp-get-option (who u kind)
  (let ((fd (udp-live-fd who u)))
    (multiple-value-bind (level option) (udp-option u kind)
      (multiple-value-bind (v e) (net-getsockopt fd level option)
	(unless v (raise-net-posix who "getsockopt failed" e))
	v))))

(defun udp-set-option (who u kind value)
  (let ((fd (udp-live-fd who u)))
    (multiple-value-bind (level option size) (udp-option u kind)
      (let ((e (net-setsockopt fd level option value size)))
	(unless (zerop e) (raise-net-posix who "setsockopt failed" e))
	*void*))))

(defun net-byte-p (v) (and (integerp v) (<= 0 v 255)))

(defnative "udp-ttl" (u) (:instance "#%network")
  (check-udp "udp-ttl" u)
  (udp-get-option "udp-ttl" u :ttl))

(defnative "udp-set-ttl!" (u ttl) (:instance "#%network")
  (check-udp "udp-set-ttl!" u)
  (unless (net-byte-p ttl) (arg-error "udp-set-ttl!" "byte?" ttl))
  (udp-set-option "udp-set-ttl!" u :ttl ttl))

(defnative "udp-multicast-ttl" (u) (:instance "#%network")
  (check-udp "udp-multicast-ttl" u)
  (udp-get-option "udp-multicast-ttl" u :mttl))

(defnative "udp-multicast-set-ttl!" (u ttl) (:instance "#%network")
  (check-udp "udp-multicast-set-ttl!" u)
  (unless (net-byte-p ttl) (arg-error "udp-multicast-set-ttl!" "byte?" ttl))
  (udp-set-option "udp-multicast-set-ttl!" u :mttl ttl))

(defnative "udp-multicast-loopback?" (u) (:instance "#%network")
  (check-udp "udp-multicast-loopback?" u)
  (bool (/= 0 (logand #xFF (udp-get-option "udp-multicast-loopback?" u :mloop)))))

(defnative "udp-multicast-set-loopback!" (u loopback) (:instance "#%network")
  (check-udp "udp-multicast-set-loopback!" u)
  (udp-set-option "udp-multicast-set-loopback!" u :mloop (if (truthy loopback) 1 0)))

(defun multicast-in-addr (who host which)
  "The four bytes of HOST's IPv4 address (NIL: 0.0.0.0), for WHICH (\"multicast \", \"interface \" or \"\")."
  (if (null host)
      (make-octets 4)
      (multiple-value-bind (found code) (net-resolve host nil +net-af-inet+ +net-sock-dgram+ nil)
	(unless found
	  (raise-net-gai who (format nil "can't resolve ~Aaddress" which) code
			 (format nil "~%  address: ~A" host)))
	(subseq (cdr (first found)) 4 8))))

(defun udp-change-group (who u multicast-hostname hostname option)
  (check-udp who u)
  (net-check-string who multicast-hostname)
  (net-check-string who hostname t)
  (let* ((group (multicast-in-addr who multicast-hostname "multicast "))
	 (intf (multicast-in-addr who (net-opt hostname) "interface "))
	 (fd (udp-live-fd who u))
	 (mreq (concatenate 'octets group intf))
	 (e (with-net-sap (sap mreq)
	      (nth-value 1 (net-syscall (net-libc "setsockopt" sb-alien:int (sb-alien:int fd)
						  (sb-alien:int +net-ipproto-ip+) (sb-alien:int option)
						  (sb-sys:system-area-pointer sap) (sb-alien:unsigned 8)))))))
    (unless (zerop e) (raise-net-posix who "setsockopt failed" e))
    *void*))

(defnative "udp-multicast-join-group!" (u multicast-hostname hostname) (:instance "#%network")
  (udp-change-group "udp-multicast-join-group!" u multicast-hostname hostname +net-ip-add-membership+))

(defnative "udp-multicast-leave-group!" (u multicast-hostname hostname) (:instance "#%network")
  (udp-change-group "udp-multicast-leave-group!" u multicast-hostname hostname +net-ip-drop-membership+))

(defnative "udp-multicast-interface" (u) (:instance "#%network")
  (check-udp "udp-multicast-interface" u)
  (let ((fd (udp-live-fd "udp-multicast-interface" u)) (buf (make-octets 4)) (len (make-octets 4)))
    (setf (sb-sys:sap-ref-32 (sb-sys:vector-sap len) 0) 4)
    (multiple-value-bind (r e)
	(with-net-sap (sap buf)
	  (with-net-sap (lsap len)
	    (net-syscall (net-libc "getsockopt" sb-alien:int (sb-alien:int fd) (sb-alien:int +net-ipproto-ip+)
				   (sb-alien:int +net-ip-multicast-if+) (sb-sys:system-area-pointer sap)
				   (sb-sys:system-area-pointer lsap)))))
      (when (< r 0) (raise-net-posix "udp-multicast-interface" "getsockopt failed" e))
      (format nil "~D.~D.~D.~D" (aref buf 0) (aref buf 1) (aref buf 2) (aref buf 3)))))

(defnative "udp-multicast-set-interface!" (u hostname) (:instance "#%network")
  (check-udp "udp-multicast-set-interface!" u)
  (net-check-string "udp-multicast-set-interface!" hostname t)
  (let* ((in-addr (multicast-in-addr "udp-multicast-set-interface!" (net-opt hostname) ""))
	 (fd (udp-live-fd "udp-multicast-set-interface!" u))
	 (e (with-net-sap (sap in-addr)
	      (nth-value 1 (net-syscall (net-libc "setsockopt" sb-alien:int (sb-alien:int fd)
						  (sb-alien:int +net-ipproto-ip+)
						  (sb-alien:int +net-ip-multicast-if+)
						  (sb-sys:system-area-pointer sap) (sb-alien:unsigned 4)))))))
    (unless (zerop e) (raise-net-posix "udp-multicast-set-interface!" "setsockopt failed" e))
    *void*))

;;; ------------------------------------------------------------------
;;; tcp-addresses: of a TCP port, a listener or a UDP socket

(defnative "tcp-addresses" (p &optional (port-numbers qs:false)) (:instance "#%network")
  (let (local peer)
    (cond ((tcp-listener-p p)
	   (listener-live "tcp-addresses" p)
	   (setq local (fd-address (tcp-listener-fd p) nil)))
	  ((net-udp-p p)
	   (let ((fd (udp-live-fd "tcp-addresses" p)))
	     (setq local (fd-address fd nil)
		   peer (fd-address fd t))))
	  ((gethash p *tcp-ports*)
	   (let ((conn (gethash p *tcp-ports*)))
	     (when (or (null (tcp-conn-fd conn)) (if (rinport-p p) (rinport-closed p) (routport-closed p)))
	       (racket-error "tcp-addresses" "port is closed~%  port: ~A" (port-desc p)))
	     (multiple-value-bind (o e) (fd-address (tcp-conn-fd conn) nil)
	       (unless o (raise-net-posix "tcp-addresses" "could not get address" e))
	       (setq local o))
	     (multiple-value-bind (o e) (fd-address (tcp-conn-fd conn) t)
	       (unless o (raise-net-posix "tcp-addresses" "could not get peer address" e))
	       (setq peer o))))
	  (t (arg-error "tcp-addresses" "(or/c tcp-port? tcp-listener? udp?)" p)))
    (unless local
      (raise-net-posix "tcp-addresses" "could not get address" (sb-alien:get-errno)))
    (let ((local-host (sockaddr-host local))
	  (peer-host (if peer (sockaddr-host peer) "0.0.0.0")))
      (if (truthy port-numbers)
	  (values local-host (sockaddr-port local) peer-host (if peer (sockaddr-port peer) 0))
	  (values local-host peer-host)))))
