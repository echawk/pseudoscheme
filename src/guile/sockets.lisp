; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-GUILE -*-

;;;; Sockets, the network database and poll, as Guile's
;;;;
;;;; A socket is a port on its file descriptor (ports.lisp's FD-PORT, of
;;;; kind "socket").  A socket address is Guile's vector: #(family address
;;;; port [flowinfo scope-id]) for IPv4 and IPv6, #(family path) for Unix;
;;;; an address info is #(flags family socktype protocol address
;;;; canonical-name).  The system calls are the C library's, by SB-ALIEN,
;;;; with the platform's struct layouts written out here.  Behaviour is
;;;; from Guile's manual and the installed Guile's, not libguile's sources.

(in-package "QUASISCHEME-GUILE")

;;; The platform's constants, as Guile defines them

(defparameter +socket-constants+
  #+darwin
  '(("AF_INET" 2) ("AF_INET6" 30) ("AF_UNIX" 1) ("AF_UNSPEC" 0)
    ("PF_INET" 2) ("PF_INET6" 30) ("PF_UNIX" 1) ("PF_UNSPEC" 0)
    ("AI_ADDRCONFIG" 1024) ("AI_ALL" 256) ("AI_CANONNAME" 2) ("AI_NUMERICHOST" 4)
    ("AI_NUMERICSERV" 4096) ("AI_PASSIVE" 1) ("AI_V4MAPPED" 2048)
    ("EAI_ADDRFAMILY" 1) ("EAI_AGAIN" 2) ("EAI_BADFLAGS" 3) ("EAI_FAIL" 4) ("EAI_FAMILY" 5)
    ("EAI_MEMORY" 6) ("EAI_NODATA" 7) ("EAI_NONAME" 8) ("EAI_OVERFLOW" 14) ("EAI_SERVICE" 9)
    ("EAI_SOCKTYPE" 10) ("EAI_SYSTEM" 11)
    ("IP_ADD_MEMBERSHIP" 12) ("IP_DROP_MEMBERSHIP" 13) ("IP_MULTICAST_IF" 9) ("IP_MULTICAST_TTL" 10)
    ("IPV6_V6ONLY" 27)
    ("MSG_DONTROUTE" 4) ("MSG_DONTWAIT" 128) ("MSG_OOB" 1) ("MSG_PEEK" 2)
    ("SO_BROADCAST" 32) ("SO_DEBUG" 1) ("SO_DONTROUTE" 16) ("SO_ERROR" 4103) ("SO_KEEPALIVE" 8)
    ("SO_LINGER" 128) ("SO_OOBINLINE" 256) ("SO_RCVBUF" 4098) ("SO_RCVTIMEO" 4102)
    ("SO_REUSEADDR" 4) ("SO_REUSEPORT" 512) ("SO_SNDBUF" 4097) ("SO_SNDTIMEO" 4101) ("SO_TYPE" 4104)
    ("SOL_SOCKET" 65535))
  #-darwin				; glibc's
  '(("AF_INET" 2) ("AF_INET6" 10) ("AF_UNIX" 1) ("AF_UNSPEC" 0)
    ("PF_INET" 2) ("PF_INET6" 10) ("PF_UNIX" 1) ("PF_UNSPEC" 0)
    ("AI_ADDRCONFIG" 32) ("AI_ALL" 16) ("AI_CANONNAME" 2) ("AI_NUMERICHOST" 4)
    ("AI_NUMERICSERV" 1024) ("AI_PASSIVE" 1) ("AI_V4MAPPED" 8)
    ("EAI_ADDRFAMILY" -9) ("EAI_AGAIN" -3) ("EAI_BADFLAGS" -1) ("EAI_FAIL" -4) ("EAI_FAMILY" -6)
    ("EAI_MEMORY" -10) ("EAI_NODATA" -5) ("EAI_NONAME" -2) ("EAI_OVERFLOW" -12) ("EAI_SERVICE" -8)
    ("EAI_SOCKTYPE" -7) ("EAI_SYSTEM" -11)
    ("IP_ADD_MEMBERSHIP" 35) ("IP_DROP_MEMBERSHIP" 36) ("IP_MULTICAST_IF" 32) ("IP_MULTICAST_TTL" 33)
    ("IPV6_V6ONLY" 26)
    ("MSG_DONTROUTE" 4) ("MSG_DONTWAIT" 64) ("MSG_OOB" 1) ("MSG_PEEK" 2)
    ("SO_BROADCAST" 6) ("SO_DEBUG" 1) ("SO_DONTROUTE" 5) ("SO_ERROR" 4) ("SO_KEEPALIVE" 9)
    ("SO_LINGER" 13) ("SO_OOBINLINE" 10) ("SO_RCVBUF" 8) ("SO_RCVTIMEO" 20)
    ("SO_REUSEADDR" 2) ("SO_REUSEPORT" 15) ("SO_SNDBUF" 7) ("SO_SNDTIMEO" 21) ("SO_TYPE" 3)
    ("SOL_SOCKET" 1) ("SOCK_NONBLOCK" 2048) ("SOCK_CLOEXEC" 524288))
  "The platform's values of the constants Guile defines for sockets.")

(defparameter +socket-common-constants+
  '(("IN6ADDR_ANY" 0) ("IN6ADDR_LOOPBACK" 1) ("INADDR_ANY" 0) ("INADDR_BROADCAST" 4294967295)
    ("INADDR_LOOPBACK" 2130706433) ("INADDR_NONE" 4294967295)
    ("IPPROTO_IP" 0) ("IPPROTO_IPV6" 41) ("IPPROTO_TCP" 6) ("IPPROTO_UDP" 17)
    ("SOCK_DGRAM" 2) ("SOCK_RAW" 3) ("SOCK_RDM" 4) ("SOCK_SEQPACKET" 5) ("SOCK_STREAM" 1)
    ("TCP_NODELAY" 1) ("SHUT_RD" 0) ("SHUT_WR" 1) ("SHUT_RDWR" 2)))

(defun socket-constant (name)
  (second (or (assoc name +socket-constants+ :test #'string=)
	      (assoc name +socket-common-constants+ :test #'string=))))

(defconstant +af-inet+ 2)
(defconstant +af-unix+ 1)
(defparameter +af-inet6+ (socket-constant "AF_INET6"))

;;; System calls

(defmacro libc (name result &rest args)
  "Call C function NAME, of RESULT type, on ARGS: (type value) pairs."
  `(sb-alien:alien-funcall
    (sb-alien:extern-alien ,name (function ,result ,@(mapcar #'first args)))
    ,@(mapcar #'second args)))

(defun socket-error (who)
  (port-system-error who (sb-alien:get-errno)))

(defmacro with-octets-sap ((sap octets) &body body)
  "Run BODY with SAP pointing at OCTETS, a simple octet vector, pinned."
  `(sb-sys:with-pinned-objects (,octets)
     (let ((,sap (sb-sys:vector-sap ,octets)))
       ,@body)))

(defun simple-octets (bv)
  "BV's bytes as a simple octet vector: BV itself if it is one."
  (if (typep bv '(simple-array (unsigned-byte 8) (*))) bv (coerce bv '(simple-array (unsigned-byte 8) (*)))))

(defun socket-fd (who sock)
  "The file descriptor of SOCK, a port or a descriptor."
  (cond ((integerp sock) sock)
	((and (gport-p sock) (integerp (port-fd sock))) (port-fd sock))
	(t (wrong-type who 1 sock))))

;;; Socket addresses: Guile's vectors, and the platform's structs

(defun check-address (who address bits)
  (unless (and (integerp address) (<= 0 address (1- (ash 1 bits))))
    (guile-error (ssym "out-of-range") who "Value out of range: ~S" (list address) (list address))))

(defun sockaddr-octets (who family args)
  "The struct sockaddr of FAMILY and ARGS (an address and port, and an
IPv6 address's flowinfo and scope-id; or a path), and its length."
  (flet ((header (octets length)
	   #+darwin (setf (aref octets 0) length (aref octets 1) family)
	   #-darwin (setf (aref octets 0) (ldb (byte 8 0) family) (aref octets 1) (ldb (byte 8 8) family))
	   octets)
	 (big-endian (octets start n value)
	   (dotimes (i n) (setf (aref octets (+ start i)) (ldb (byte 8 (* 8 (- n i 1))) value)))))
    (cond ((= family +af-inet+)
	   (destructuring-bind (address port &rest more) args
	     (declare (ignore more))
	     (check-address who address 32)
	     (let ((o (header (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0) 16)))
	       (big-endian o 2 2 port)
	       (big-endian o 4 4 address)
	       (values o 16))))
	  ((= family +af-inet6+)
	   (destructuring-bind (address port &optional (flowinfo 0) (scope-id 0)) args
	     (check-address who address 128)
	     (let ((o (header (make-array 28 :element-type '(unsigned-byte 8) :initial-element 0) 28)))
	       (big-endian o 2 2 port)
	       (big-endian o 4 4 flowinfo)
	       (big-endian o 8 16 address)
	       ;; the scope id is in host order
	       (dotimes (i 4) (setf (aref o (+ 24 i)) (ldb (byte 8 (* 8 i)) scope-id)))
	       (values o 28))))
	  ((= family +af-unix+)
	   (let* ((path (sb-ext:string-to-octets (first args) :external-format :utf-8))
		  (length (+ 2 (length path)))
		  (o (header (make-array (max length 110) :element-type '(unsigned-byte 8) :initial-element 0)
			     length)))
	     (replace o path :start1 2)
	     (values o length)))
	  (t (guile-error (ssym "system-error") who "~A" (list "Address family not supported")
			  (list sb-posix:eafnosupport))))))

(defun octets-sockaddr (octets)
  "Guile's socket address of the struct sockaddr in OCTETS."
  (flet ((big-endian (start n)
	   (let ((v 0)) (dotimes (i n v) (setq v (logior (ash v 8) (aref octets (+ start i))))))))
    (let ((family #+darwin (aref octets 1) #-darwin (logior (aref octets 0) (ash (aref octets 1) 8))))
      (cond ((= family +af-inet+) (vector family (big-endian 4 4) (big-endian 2 2)))
	    ((= family +af-inet6+)
	     (vector family (big-endian 8 16) (big-endian 2 2) (big-endian 4 4)
		     (let ((v 0)) (dotimes (i 4 v) (setq v (logior v (ash (aref octets (+ 24 i)) (* 8 i))))))))
	    ((= family +af-unix+)
	     (let* ((length #+darwin (min (aref octets 0) (length octets)) #-darwin (length octets))
		    (end (or (position 0 octets :start 3 :end length) length)))
	       (vector family (sb-ext:octets-to-string octets :start 2 :end end :external-format :utf-8))))
	    (t (vector family))))))

(defun address-arguments (who family-or-address args)
  "The struct sockaddr octets and length of a socket address given as
Guile's procedures take one: a socket address, or a family and its parts."
  (if (vectorp family-or-address)
      (sockaddr-octets who (svref family-or-address 0) (coerce (subseq family-or-address 1) 'list))
      (sockaddr-octets who family-or-address args)))

(defguile "make-socket-address" (family address &rest more)
  (cond ((= family +af-unix+) (vector family address))
	((= family +af-inet6+)
	 (destructuring-bind (port &optional (flowinfo 0) (scope-id 0)) more
	   (vector family address port flowinfo scope-id)))
	(t (vector family address (first more)))))
(defguile "sockaddr:fam" (sa) (svref sa 0))
(defguile "sockaddr:path" (sa) (svref sa 1))
(defguile "sockaddr:addr" (sa) (svref sa 1))
(defguile "sockaddr:port" (sa) (svref sa 2))
(defguile "sockaddr:flowinfo" (sa) (svref sa 3))
(defguile "sockaddr:scopeid" (sa) (svref sa 4))

;;; Addresses as text

(defguile "inet-pton" (family string)
  (let ((buf (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0))
	(n (if (= family +af-inet+) 4 16)))
    (with-octets-sap (sap buf)
      (let ((r (libc "inet_pton" sb-alien:int (sb-alien:int family) (sb-alien:c-string string)
		     (sb-sys:system-area-pointer sap))))
	(cond ((= r 1) (let ((v 0)) (dotimes (i n v) (setq v (logior (ash v 8) (aref buf i))))))
	      ((= r 0) (guile-error (ssym "misc-error") "inet-pton" "Bad address" '()))
	      (t (socket-error "inet-pton")))))))

(defguile "inet-ntop" (family address)
  (let ((n (if (= family +af-inet+) 4 16)))
    (check-address "inet-ntop" address (* 8 n))
    (let ((buf (make-array 16 :element-type '(unsigned-byte 8)))
	  (out (make-array 64 :element-type '(unsigned-byte 8) :initial-element 0)))
      (dotimes (i n) (setf (aref buf i) (ldb (byte 8 (* 8 (- n i 1))) address)))
      (with-octets-sap (in buf)
	(with-octets-sap (o out)
	  (when (zerop (sb-sys:sap-int
			(libc "inet_ntop" sb-sys:system-area-pointer (sb-alien:int family)
			      (sb-sys:system-area-pointer in) (sb-sys:system-area-pointer o)
			      (sb-alien:unsigned 64))))
	    (socket-error "inet-ntop"))))
      (sb-ext:octets-to-string out :end (position 0 out) :external-format :latin-1))))

;;; Sockets

(defun socket-port (fd)
  (let ((port (fd-port fd "r+")))
    (setf (port-kind port) "socket")
    port))

(defguile "socket" (family style proto)
  (let ((fd (libc "socket" sb-alien:int (sb-alien:int family) (sb-alien:int style) (sb-alien:int proto))))
    (when (minusp fd) (socket-error "socket"))
    (socket-port fd)))

(defguile "socketpair" (family style proto)
  (let ((fds (make-array 8 :element-type '(unsigned-byte 8))))
    (with-octets-sap (sap fds)
      (when (minusp (libc "socketpair" sb-alien:int (sb-alien:int family) (sb-alien:int style)
			  (sb-alien:int proto) (sb-sys:system-area-pointer sap)))
	(socket-error "socketpair"))
      (cons (socket-port (sb-sys:signed-sap-ref-32 sap 0)) (socket-port (sb-sys:signed-sap-ref-32 sap 4))))))

(macrolet ((address-call (name who)
	     `(defguile ,who (sock family-or-address &rest args)
		(let ((fd (socket-fd ,who sock)))
		  (multiple-value-bind (octets length) (address-arguments ,who family-or-address args)
		    (with-octets-sap (sap octets)
		      (when (minusp (libc ,name sb-alien:int (sb-alien:int fd) (sb-sys:system-area-pointer sap)
					  (sb-alien:unsigned length)))
			(socket-error ,who))))
		  *unspecified*))))
  (address-call "bind" "bind")
  (address-call "connect" "connect"))

(defguile "listen" (sock backlog)
  (when (minusp (libc "listen" sb-alien:int (sb-alien:int (socket-fd "listen" sock)) (sb-alien:int backlog)))
    (socket-error "listen"))
  *unspecified*)

(defun sockaddr-call (who function fd)
  "Call FUNCTION, accept(2) or the like, on FD with a struct sockaddr to
fill: its result and the address."
  (let ((octets (make-array 128 :element-type '(unsigned-byte 8) :initial-element 0))
	(length (make-array 4 :element-type '(unsigned-byte 8))))
    (setf (sb-sys:sap-ref-32 (sb-sys:vector-sap length) 0) 128)
    (with-octets-sap (sap octets)
      (with-octets-sap (lsap length)
	(let ((r (funcall function fd sap lsap)))
	  (values r (and (>= r 0) (octets-sockaddr octets)) who))))))

(defguile "accept" (sock &optional (flags 0))
  (declare (ignore flags))
  (let ((fd (socket-fd "accept" sock)))
    (loop
      (multiple-value-bind (new address)
	  (sockaddr-call "accept"
			 (lambda (fd sap lsap)
			   (libc "accept" sb-alien:int (sb-alien:int fd) (sb-sys:system-area-pointer sap)
				 (sb-sys:system-area-pointer lsap)))
			 fd)
	(cond ((>= new 0) (return (cons (socket-port new) address)))
	      ((= (sb-alien:get-errno) sb-unix:eintr))
	      ((= (sb-alien:get-errno) sb-unix:eagain) (return qs:false))
	      (t (socket-error "accept")))))))

(macrolet ((name-call (who name)
	     `(defguile ,who (sock)
		(multiple-value-bind (r address)
		    (sockaddr-call ,who
				   (lambda (fd sap lsap)
				     (libc ,name sb-alien:int (sb-alien:int fd) (sb-sys:system-area-pointer sap)
					   (sb-sys:system-area-pointer lsap)))
				   (socket-fd ,who sock))
		  (if (minusp r) (socket-error ,who) address)))))
  (name-call "getsockname" "getsockname")
  (name-call "getpeername" "getpeername"))

(defguile "shutdown" (sock how)
  (when (minusp (libc "shutdown" sb-alien:int (sb-alien:int (socket-fd "shutdown" sock)) (sb-alien:int how)))
    (socket-error "shutdown"))
  *unspecified*)

;;; Options: an integer, or for SO_LINGER (on . seconds)

(defguile "setsockopt" (sock level option value)
  (let ((buf (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0)) (size 4))
    (cond ((and (= level (socket-constant "SOL_SOCKET")) (= option (socket-constant "SO_LINGER")))
	   (let ((on (if (consp value) (car value) value)) (secs (if (consp value) (cdr value) 0)))
	     (setf (sb-sys:sap-ref-32 (sb-sys:vector-sap buf) 0) on
		   (sb-sys:sap-ref-32 (sb-sys:vector-sap buf) 4) secs
		   size 8)))
	  ((integerp value) (setf (sb-sys:signed-sap-ref-32 (sb-sys:vector-sap buf) 0) value))
	  (t (wrong-type "setsockopt" 4 value)))
    (with-octets-sap (sap buf)
      (when (minusp (libc "setsockopt" sb-alien:int (sb-alien:int (socket-fd "setsockopt" sock))
			  (sb-alien:int level) (sb-alien:int option) (sb-sys:system-area-pointer sap)
			  (sb-alien:unsigned size)))
	(socket-error "setsockopt")))
    *unspecified*))

(defguile "getsockopt" (sock level option)
  (let ((buf (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0))
	(size (make-array 4 :element-type '(unsigned-byte 8))))
    (setf (sb-sys:sap-ref-32 (sb-sys:vector-sap size) 0) 8)
    (with-octets-sap (sap buf)
      (with-octets-sap (ssap size)
	(when (minusp (libc "getsockopt" sb-alien:int (sb-alien:int (socket-fd "getsockopt" sock))
			    (sb-alien:int level) (sb-alien:int option) (sb-sys:system-area-pointer sap)
			    (sb-sys:system-area-pointer ssap)))
	  (socket-error "getsockopt"))
	(if (and (= level (socket-constant "SOL_SOCKET")) (= option (socket-constant "SO_LINGER")))
	    (cons (sb-sys:signed-sap-ref-32 sap 0) (sb-sys:signed-sap-ref-32 sap 4))
	    (sb-sys:signed-sap-ref-32 sap 0))))))

;;; Sending and receiving bytevectors

(defun io-loop (who thunk)
  "Call THUNK, a system call, until it isn't interrupted: its result."
  (loop (let ((r (funcall thunk)))
	  (cond ((>= r 0) (return r))
		((= (sb-alien:get-errno) sb-unix:eintr))
		(t (socket-error who))))))

(defguile "send" (sock bv &optional (flags 0))
  (let ((fd (socket-fd "send" sock)) (octets (simple-octets bv)))
    (force-port-output sock)
    (with-octets-sap (sap octets)
      (io-loop "send" (lambda () (libc "send" sb-alien:long (sb-alien:int fd) (sb-sys:system-area-pointer sap)
				       (sb-alien:unsigned-long (length octets)) (sb-alien:int flags)))))))

(defguile "recv!" (sock bv &optional (flags 0))
  (let* ((fd (socket-fd "recv!" sock)) (octets (simple-octets bv))
	 (n (with-octets-sap (sap octets)
	      (io-loop "recv!" (lambda () (libc "recv" sb-alien:long (sb-alien:int fd) (sb-sys:system-area-pointer sap)
						(sb-alien:unsigned-long (length octets)) (sb-alien:int flags)))))))
    (unless (eq octets bv) (replace bv octets :end2 n))
    n))

(defguile "sendto" (sock bv family-or-address &rest args)
  ;; FLAGS follow the address
  (let* ((fd (socket-fd "sendto" sock)) (octets (simple-octets bv))
	 (parts (if (vectorp family-or-address) args
		    (subseq args 0 (min (length args) (if (= family-or-address +af-unix+) 1 2)))))
	 (flags (or (nth (length parts) args) 0)))
    (multiple-value-bind (addr length) (address-arguments "sendto" family-or-address parts)
      (with-octets-sap (sap octets)
	(with-octets-sap (asap addr)
	  (io-loop "sendto" (lambda () (libc "sendto" sb-alien:long (sb-alien:int fd) (sb-sys:system-area-pointer sap)
					     (sb-alien:unsigned-long (length octets)) (sb-alien:int flags)
					     (sb-sys:system-area-pointer asap) (sb-alien:unsigned length)))))))))

(defguile "recvfrom!" (sock bv &optional (flags 0) (start 0) end)
  (let* ((fd (socket-fd "recvfrom!" sock)) (end (or end (length bv)))
	 (octets (make-array (- end start) :element-type '(unsigned-byte 8))))
    (multiple-value-bind (n address)
	(with-octets-sap (sap octets)
	  (sockaddr-call "recvfrom!"
			 (lambda (fd asap lsap)
			   (io-loop "recvfrom!"
				    (lambda () (libc "recvfrom" sb-alien:long (sb-alien:int fd) (sb-sys:system-area-pointer sap)
						     (sb-alien:unsigned-long (length octets)) (sb-alien:int flags)
						     (sb-sys:system-area-pointer asap) (sb-sys:system-area-pointer lsap)))))
			 fd))
      (replace bv octets :start1 start :end2 n)
      (cons n address))))

(defun force-port-output (sock)
  "Write out what SOCK, if a port, has buffered."
  (when (gport-p sock) (funcall (root-value "force-output") sock)))

;;; The network database: getaddrinfo

(defun sap-c-string (sap)
  "The NUL-terminated UTF-8 string at SAP."
  (let* ((n (loop for i from 0 until (zerop (sb-sys:sap-ref-8 sap i)) finally (return i)))
	 (octets (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n) (setf (aref octets i) (sb-sys:sap-ref-8 sap i)))
    (sb-ext:octets-to-string octets :external-format :utf-8)))

(defun addrinfo-layout ()
  "Offsets in struct addrinfo of its canonical name and address."
  #+darwin (values 24 32) #-darwin (values 32 24))

(defguile "getaddrinfo" (name &optional (service qs:false) (flags 0) (family 0) (socktype 0) (protocol 0))
  (let ((hints (make-array 48 :element-type '(unsigned-byte 8) :initial-element 0))
	(result (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0)))
    (let ((h (sb-sys:vector-sap hints)))
      (setf (sb-sys:signed-sap-ref-32 h 0) (if (truthy flags) flags 0)
	    (sb-sys:signed-sap-ref-32 h 4) (if (truthy family) family 0)
	    (sb-sys:signed-sap-ref-32 h 8) (if (truthy socktype) socktype 0)
	    (sb-sys:signed-sap-ref-32 h 12) (if (truthy protocol) protocol 0)))
    (let ((code (with-octets-sap (hsap hints)
		  (with-octets-sap (rsap result)
		    (libc "getaddrinfo" sb-alien:int
			  (sb-alien:c-string (and (stringp name) name))
			  (sb-alien:c-string (and (stringp service) service))
			  (sb-sys:system-area-pointer hsap) (sb-sys:system-area-pointer rsap))))))
      (unless (zerop code)
	(funcall (root-value "throw") (ssym "getaddrinfo-error") code))
      (let ((head (sb-sys:sap-ref-sap (sb-sys:vector-sap result) 0)) (infos '()))
	(multiple-value-bind (canon-at addr-at) (addrinfo-layout)
	  (unwind-protect
	       (loop for ai = head then (sb-sys:sap-ref-sap ai 40)
		     until (zerop (sb-sys:sap-int ai))
		     do (let* ((addrlen (sb-sys:sap-ref-32 ai 16))
			       (addr (sb-sys:sap-ref-sap ai addr-at))
			       (canon (sb-sys:sap-ref-sap ai canon-at))
			       (octets (make-array (max addrlen 28) :element-type '(unsigned-byte 8) :initial-element 0)))
			  (dotimes (i addrlen) (setf (aref octets i) (sb-sys:sap-ref-8 addr i)))
			  (push (vector (sb-sys:signed-sap-ref-32 ai 0) (sb-sys:signed-sap-ref-32 ai 4)
					(sb-sys:signed-sap-ref-32 ai 8) (sb-sys:signed-sap-ref-32 ai 12)
					(octets-sockaddr octets)
					(if (zerop (sb-sys:sap-int canon))
					    qs:false
					    (sap-c-string canon)))
				infos)))
	    (libc "freeaddrinfo" sb-alien:void (sb-sys:system-area-pointer head))))
	(nreverse infos)))))

(defguile "gai-strerror" (code)
  (libc "gai_strerror" sb-alien:c-string (sb-alien:int code)))

(loop for (name . i) in '(("addrinfo:flags" . 0) ("addrinfo:fam" . 1) ("addrinfo:socktype" . 2)
			  ("addrinfo:protocol" . 3) ("addrinfo:addr" . 4) ("addrinfo:canonname" . 5))
      do (let ((i i)) (setf (gethash name *guile-primitives*) (lambda (ai) (svref ai i)))))

;;; poll(2), for (ice-9 poll): an array of struct pollfd (an int and two
;;; shorts), filled with the descriptors of the ports; a port with input
;;; buffered is ready to read without asking

(defextension "scm_init_poll"
  (list (cons "%sizeof-struct-pollfd" 8)
	(cons "POLLIN" 1) (cons "POLLPRI" 2) (cons "POLLOUT" 4)
	(cons "POLLERR" 8) (cons "POLLHUP" 16) (cons "POLLNVAL" 32)
	(cons "primitive-poll"
	      (lambda (pollfds nfds ports timeout)
		(let ((octets (simple-octets pollfds)) (ready 0))
		  (dotimes (i nfds)
		    (let ((port (svref ports i)) (at (* 8 i)))
		      (when (gport-p port)
			(setf (sb-sys:signed-sap-ref-32 (sb-sys:vector-sap octets) at) (or (port-fd port) -1))
			(when (and (< (port-rpos port) (port-rend port))
				   (logtest 1 (sb-sys:sap-ref-16 (sb-sys:vector-sap octets) (+ at 4))))
			  (incf ready)))))
		  (let ((n (with-octets-sap (sap octets)
			     (io-loop "poll" (lambda () (libc "poll" sb-alien:int (sb-sys:system-area-pointer sap)
							      (sb-alien:unsigned nfds)
							      (sb-alien:int (if (plusp ready) 0 timeout))))))))
		    ;; buffered input counts as POLLIN
		    (dotimes (i nfds)
		      (let ((port (svref ports i)) (at (* 8 i)))
			(when (and (gport-p port) (< (port-rpos port) (port-rend port)))
			  (let ((revents (sb-sys:sap-ref-16 (sb-sys:vector-sap octets) (+ at 6))))
			    (when (zerop revents) (incf n))
			    (setf (sb-sys:sap-ref-16 (sb-sys:vector-sap octets) (+ at 6)) (logior revents 1))))))
		    (unless (eq octets pollfds) (replace pollfds octets))
		    n))))))

;;; The constants, in the root module

(dolist (c (append +socket-constants+ +socket-common-constants+))
  (setf (gethash (first c) *guile-primitives*) (second c)))
