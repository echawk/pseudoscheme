; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-GUILE -*-

;;;; Guile's ports, written for Quasischeme: a port is a GPORT, a Gray
;;;; stream (trivial-gray-streams) that is binary and textual at once, as
;;;; Guile's are.  Bytes come from and go to a backend (a file descriptor,
;;;; a string or bytevector buffer, a custom port's procedures, or a Lisp
;;;; stream); characters are decoded and encoded in the port's encoding,
;;;; which can be changed at any time, with its conversion strategy for
;;;; what the encoding can't represent.  Unread characters and bytes go
;;;; back into the read buffer, as in Guile.
;;;;
;;;; The current ports are fluids whose values are the Lisp stream
;;;; variables (*standard-output* and so on), so that Quasischeme's own
;;;; procedures follow (current-output-port) and parameterize.  Guile code
;;;; runs with those variables bound to ports (CALL-WITH-GUILE-PORTS): a
;;;; Lisp stream there is wrapped in a port that writes to it.

(in-package "QUASISCHEME-GUILE")

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(defun make-octets (n) (make-array n :element-type '(unsigned-byte 8)))

(defconstant +buffer-size+ 1024 "As Guile's ports' (custom ports' read and write sizes show it).")
(defconstant +file-buffer-size+ 65536 "A regular file's port's buffers (FD-PORT).")

(defclass gport (trivial-gray-streams:fundamental-binary-input-stream
		 trivial-gray-streams:fundamental-binary-output-stream
		 trivial-gray-streams:fundamental-character-input-stream
		 trivial-gray-streams:fundamental-character-output-stream)
  ((kind :initarg :kind :initform "file" :accessor port-kind)
   (input :initarg :input :initform nil :accessor port-input-p)
   (output :initarg :output :initform nil :accessor port-output-p)
   ;; the backend: (octets start count) -> bytes moved (0 at end of file);
   ;; (offset whence) -> new position; () -> unspecified
   (read-fn :initarg :read :initform nil :accessor port-read-fn)
   (write-fn :initarg :write :initform nil :accessor port-write-fn)
   (seek-fn :initarg :seek :initform nil :accessor port-seek-fn)
   (close-fn :initarg :close :initform nil :accessor port-close-fn)
   (truncate-fn :initarg :truncate :initform nil :accessor port-truncate-fn)
   (fd :initarg :fd :initform nil :accessor port-fd)
   (data :initarg :data :initform nil :accessor port-data)
   ;; the buffers, as Guile's: #(bytevector cur end has-eof? position),
   ;; POSITION (line . column), the same for all three; (ice-9
   ;; suspendable-ports) works on them as this file does
   (rbuffer :accessor port-rbuffer)
   (wbuffer :accessor port-wbuffer)
   (aux-buffer :initform nil :accessor port-aux-buffer)
   (buffering :initarg :buffering :initform :full :accessor port-buffering) ; :none :line :full
   (encoding :initarg :encoding :initform "UTF-8" :accessor port-encoding-name)
   (codec :initform nil :accessor port-codec)
   (strategy :initarg :strategy :initform nil :accessor port-strategy)
   (bom-state :initform nil :accessor port-bom-state)	; for UTF-16 and UTF-32
   (filename :initarg :filename :initform qs:false :accessor port-filename*)
   (open :initform t :accessor port-open-p)
   (revealed :initform 0 :accessor port-revealed*)
   (properties :initform '() :accessor port-properties)
   ;; in *OPEN-OUTPUT-PORTS* (a weak table, locked: not asked per character)
   (registered :initform nil :accessor port-registered)))

(defun gport-p (x) (typep x 'gport))

(defun make-port-buffer (size position)
  (vector (make-octets size) 0 0 qs:false position))

(defmethod initialize-instance :after ((p gport) &key)
  (let ((position (cons 0 0)))
    (setf (port-rbuffer p) (make-port-buffer +buffer-size+ position)
	  (port-wbuffer p) (make-port-buffer (if (eq (port-buffering p) :none) 1 +buffer-size+) position))))

;;; An unbuffered port's write buffer holds a byte, as Guile's: what
;;; (ice-9 suspendable-ports) writes to it goes out at once.
(defmethod (setf port-buffering) :after (mode (p gport))
  (let ((size (if (eq mode :none) 1 +buffer-size+)))
    (when (and (slot-boundp p 'wbuffer) (/= size (length (port-wbuf p))))
      (flush-output p)
      (setf (port-wbuf p) (make-octets size)))))

(macrolet ((buffer-accessor (name buffer index &optional (get 'x) (set 'x))
	     `(progn
		(declaim (inline ,name (setf ,name)))
		(defun ,name (p) (let ((x (svref (,buffer p) ,index))) ,get))
		(defun (setf ,name) (x p) (setf (svref (,buffer p) ,index) ,set) x))))
  (buffer-accessor port-rbuf port-rbuffer 0)
  (buffer-accessor port-rpos port-rbuffer 1)
  (buffer-accessor port-rend port-rbuffer 2)
  ;; an end of file read, not yet returned twice
  (buffer-accessor port-eof-pending port-rbuffer 3 (truthy x) (bool x))
  (buffer-accessor port-wbuf port-wbuffer 0)
  (buffer-accessor port-wend port-wbuffer 2))

(declaim (inline port-line* (setf port-line*) port-column* (setf port-column*)))
(defun port-line* (p) (car (svref (port-rbuffer p) 4)))
(defun (setf port-line*) (x p) (setf (car (svref (port-rbuffer p) 4)) x))
(defun port-column* (p) (cdr (svref (port-rbuffer p) 4)))
(defun (setf port-column*) (x p) (setf (cdr (svref (port-rbuffer p) 4)) x))

(defmethod input-stream-p ((p gport)) (port-input-p p))
(defmethod output-stream-p ((p gport)) (port-output-p p))
(defmethod open-stream-p ((p gport)) (port-open-p p))
(defmethod stream-element-type ((p gport)) '(unsigned-byte 8))

(defmethod print-object ((p gport) stream)
  (format stream "#<~A: ~A ~A>"
	  (cond ((not (port-open-p p)) "closed")
		((and (port-input-p p) (port-output-p p)) "input-output")
		((port-input-p p) "input")
		(t "output"))
	  (if (stringp (port-filename* p)) (port-filename* p) (port-kind p))
	  (or (port-fd p) (format nil "~(~X~)" (logand (sb-kernel:get-lisp-obj-address p) #xffffffffff)))))

(defvar *open-output-ports* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "Output ports that may hold buffered bytes: flushed at exit.")

(defvar *fd-ports* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "Ports on file descriptors, for fdes->ports.")

(defun flush-all-ports ()
  (loop for p being the hash-keys of *open-output-ports*
	do (ignore-errors (flush-output p))))

(pushnew 'flush-all-ports sb-ext:*exit-hooks*)

;;; ------------------------------------------------------------------
;;; Encodings

(defun canonical-encoding (name)
  "Guile's name for the encoding NAME (a string or symbol), or NIL."
  (let ((name (string-upcase (if (symbolp name) (qs:scheme-symbol-name name) name))))
    (cond ((member name '("UTF-8" "UTF8") :test #'string=) "UTF-8")
	  ((member name '("ISO-8859-1" "ISO8859-1" "ISO_8859-1" "LATIN1" "LATIN-1" "L1") :test #'string=)
	   "ISO-8859-1")
	  ((member name '("ASCII" "US-ASCII" "ANSI_X3.4-1968" "646") :test #'string=) "US-ASCII")
	  ((member name '("UTF-16" "UTF16") :test #'string=) "UTF-16")
	  ((member name '("UTF-16LE" "UTF-16BE" "UTF-32" "UTF-32LE" "UTF-32BE") :test #'string=) name)
	  ((external-format-of name) name)
	  (t nil))))

(defun external-format-of (name)
  "An SBCL external format for the encoding NAME, or NIL."
  (let ((kw (find-symbol (string-upcase name) "KEYWORD")))
    (and kw (sb-impl::get-external-format kw) kw)))

(defun encoding-codec (encoding)
  (cond ((string= encoding "UTF-8") :utf-8)
	((string= encoding "ISO-8859-1") :latin-1)
	((string= encoding "US-ASCII") :ascii)
	((string= encoding "UTF-16") :utf-16)
	((string= encoding "UTF-16LE") :utf-16le)
	((string= encoding "UTF-16BE") :utf-16be)
	((string= encoding "UTF-32") :utf-32)
	((string= encoding "UTF-32LE") :utf-32le)
	((string= encoding "UTF-32BE") :utf-32be)
	(t (let ((format (external-format-of encoding))
		 (decode (make-array 256 :initial-element nil)))
	     (dotimes (b 256)
	       (setf (svref decode b)
		     (ignore-errors
		      (let ((s (sb-ext:octets-to-string (make-array 1 :element-type '(unsigned-byte 8)
								      :initial-element b)
							:external-format (list format :replacement #\Nul))))
			(and (= (length s) 1) (char/= (char s 0) #\Nul) (char s 0))))))
	     (list :table format decode)))))

(defun set-encoding (port encoding)
  (let ((name (canonical-encoding encoding)))
    (unless name
      (guile-error (ssym "misc-error") "set-port-encoding!" "unknown encoding: ~S" (list encoding)))
    (setf (port-encoding-name port) name
	  (port-codec port) (encoding-codec name)
	  (port-bom-state port) nil)))

(defun codec (port)
  (or (port-codec port)
      (setf (port-codec port) (encoding-codec (port-encoding-name port)))))

(defun strategy (port)
  (let ((s (or (port-strategy port)
	       (fluid-value (root-value "%default-port-conversion-strategy")))))
    (if (symbolp s) (qs:scheme-symbol-name s) "substitute")))

(defun default-encoding ()
  ;; #f is Latin-1, as Guile's: R6RS's binary ports are made so
  (let ((v (ignore-errors (fluid-value (root-value "%default-port-encoding")))))
    (cond ((stringp v)
	   (or (canonical-encoding v)
	       (guile-error (ssym "misc-error") "open-port" "unknown encoding: ~S" (list v))))
	  ((eq v qs:false) "ISO-8859-1")
	  (t "UTF-8"))))

;;; ------------------------------------------------------------------
;;; Bytes

(defun check-open (port who)
  (unless (port-open-p port)
    (guile-error (ssym "wrong-type-arg") who "Wrong type argument in position 1 (expecting open port): ~S"
		 (list port) (list port))))

(defun fill-input (port)
  "Make bytes available in PORT's read buffer: false at end of file."
  (or (< (port-rpos port) (port-rend port))
      (progn
	(check-open port "read")
	(unless (port-input-p port)
	  (wrong-type "read" 1 port))
	(when (plusp (port-wend port)) (flush-output port))
	;; an end of file just read stays until a read returns it (a peek
	;; doesn't): CONSUME-EOF
	(when (port-eof-pending port)
	  (return-from fill-input nil))
	;; the empty buffer is filled from its start, all of it asked for;
	;; at end of file it is left as it was, so a position in it (a bad
	;; sequence's start) still holds
	(let ((old-rpos (port-rpos port)) (old-rend (port-rend port)))
	  (setf (port-rpos port) 0 (port-rend port) 0)
	  (let ((n (funcall (port-read-fn port) (port-rbuf port) 0
			    ;; an unbuffered port reads a byte at a time
			    (if (eq (port-buffering port) :none) 1 (length (port-rbuf port))))))
	    (if (zerop n)
		(setf (port-eof-pending port) t
		      (port-rpos port) old-rpos (port-rend port) old-rend)
		(setf (port-rend port) n))
	    (plusp n))))))

(defun consume-eof (port)
  "A read returns the pending end of file: the next reads read again."
  (setf (port-eof-pending port) nil))

(defun port-read-byte (port)
  (if (fill-input port)
      (prog1 (aref (port-rbuf port) (port-rpos port)) (incf (port-rpos port)))
      (progn (consume-eof port) :eof)))

(defun port-peek-byte (port)
  (if (fill-input port) (aref (port-rbuf port) (port-rpos port)) :eof))

(defun unread-octets (port octets &optional (start 0) (end (length octets)))
  "Put OCTETS[START, END) back in front of PORT's unread input."
  (let ((n (- end start)) (rpos (port-rpos port)))
    (if (<= n rpos)
	(progn (replace (port-rbuf port) octets :start1 (- rpos n) :start2 start :end2 end)
	       (setf (port-rpos port) (- rpos n)))
	(let* ((rest (- (port-rend port) rpos))
	       (new (make-octets (max +buffer-size+ (+ n rest)))))
	  (replace new octets :start2 start :end2 end)
	  (replace new (port-rbuf port) :start1 n :start2 rpos :end2 (port-rend port))
	  (setf (port-rbuf port) new (port-rpos port) 0 (port-rend port) (+ n rest))))))

(defun write-octets (port octets &optional (start 0) (end (length octets)))
  (check-open port "write")
  (unless (port-output-p port) (wrong-type "write" 1 port))
  ;; writing discards buffered input, as in Guile (its position is the
  ;; file's), on a port with random access; a pipe's input stays
  (when (and (< (port-rpos port) (port-rend port)) (port-random-access-p port))
    (discard-input port))
  (unless (port-registered port)
    (setf (gethash port *open-output-ports*) t (port-registered port) t))
  (loop while (< start end)
	do (let* ((wbuf (port-wbuf port))
		  (n (min (- end start) (- (length wbuf) (port-wend port)))))
	     (replace wbuf octets :start1 (port-wend port) :start2 start :end2 (+ start n))
	     (incf (port-wend port) n)
	     (incf start n)
	     (when (= (port-wend port) (length wbuf)) (flush-output port))))
  (when (eq (port-buffering port) :none) (flush-output port)))

(defun flush-output (port)
  (let ((wend (port-wend port))
	(start (svref (port-wbuffer port) 1)))
    (when (< start wend)
      (setf (port-wend port) 0 (svref (port-wbuffer port) 1) 0)
      (progn
	(loop while (< start wend)
	      do (let ((n (funcall (port-write-fn port) (port-wbuf port) start (- wend start))))
		   (when (<= n 0) (return))
		   (incf start n)))))))

(defun port-random-access-p (port)
  (and (port-seek-fn port)
       (if (string= (port-kind port) "custom")
	   (let ((random-access (custom-dispatcher "custom-port-random-access?")))
	     (or (null random-access) (truthy (funcall random-access port (port-data port)))))
	   t)))

(defun discard-input (port)
  "Forget buffered input, moving the backend back to where it was read to."
  (let ((pending (- (port-rend port) (port-rpos port))))
    (setf (port-rpos port) 0 (port-rend port) 0)
    (when (and (plusp pending) (port-seek-fn port))
      (funcall (port-seek-fn port) (- pending) 1))))

;;; ------------------------------------------------------------------
;;; Characters

(defconstant +eilseq+ #+darwin 92 #-darwin 84)

(defvar *bad-input-start* nil
  "The read position before the character being decoded: an error puts
the bytes back, so that they can be read again (as U+FFFD, say).")

(defun decoding-error (port)
  ;; Guile's arguments: subr, message, errno, port
  (call-throw (ssym "decoding-error") (list "scm_getc" "input decoding error" +eilseq+ port)))

(defun bad-input (port)
  "A byte sequence that isn't a character: U+FFFD under the substitute
strategy, else (error and escape alike, as Guile's) an error, the
sequence left unread."
  (if (string= (strategy port) "substitute")
      (code-char #xfffd)
      (progn
	(when (and *bad-input-start* (<= *bad-input-start* (port-rpos port)))
	  (setf (port-rpos port) *bad-input-start*))
	(decoding-error port))))

(defun read-utf-8 (port b0)
  (let ((n (cond ((< b0 #x80) 0) ((< b0 #xc2) -1) ((< b0 #xe0) 1) ((< b0 #xf0) 2) ((< b0 #xf5) 3) (t -1))))
    (case n
      (0 (code-char b0))
      (-1 (bad-input port))
      (t (let ((code (logand b0 (case n (1 #x1f) (2 #x0f) (t #x07)))))
	   (dotimes (i n)
	     (let ((b (port-peek-byte port)))
	       (when (or (eq b :eof) (/= (logand b #xc0) #x80)
			 ;; overlong or surrogate or too large
			 (and (= i 0) (or (and (= b0 #xe0) (< b #xa0)) (and (= b0 #xed) (>= b #xa0))
					  (and (= b0 #xf0) (< b #x90)) (and (= b0 #xf4) (>= b #x90)))))
		 (return-from read-utf-8 (bad-input port)))
	       (port-read-byte port)
	       (setq code (logior (ash code 6) (logand b #x3f)))))
	   (code-char code))))))

(defun read-units (port size big-endian)
  "A SIZE-byte code unit, or :EOF."
  (let ((bytes (loop repeat size
		     for b = (port-read-byte port)
		     when (eq b :eof) do (return-from read-units :eof)
		     collect b)))
    (unless big-endian (setq bytes (reverse bytes)))
    (reduce (lambda (acc b) (logior (ash acc 8) b)) bytes :initial-value 0)))

(defun bom-endianness (port size)
  "For UTF-16 or UTF-32 without an order: read a byte order mark if
there is one, at the start; big-endian otherwise."
  (or (port-bom-state port)
      (setf (port-bom-state port)
	    (let ((unit (read-units port size t)))
	      (cond ((eq unit :eof) :big)
		    ((= unit (if (= size 2) #xfeff #x0000feff)) :big)
		    ((= unit (if (= size 2) #xfffe #xfffe0000)) :little)
		    (t (let ((bytes (make-octets size)))
			 (dotimes (i size) (setf (aref bytes (- size i 1)) (ldb (byte 8 (* 8 i)) unit)))
			 (unread-octets port bytes))
		       :big))))))

(defun read-wide (port size big-endian)
  (let ((u (read-units port size big-endian)))
    (cond ((eq u :eof) :eof)
	  ((and (= size 2) (<= #xd800 u #xdbff))
	   (let ((lo (read-units port 2 big-endian)))
	     (if (and (integerp lo) (<= #xdc00 lo #xdfff))
		 (code-char (+ #x10000 (ash (- u #xd800) 10) (- lo #xdc00)))
		 (bad-input port))))
	  ((or (<= #xd800 u #xdfff) (> u #x10ffff)) (bad-input port))
	  (t (code-char u)))))

(defun gport-slot-location (name)
  (let ((class (find-class 'gport)))
    (sb-mop:finalize-inheritance class)
    (sb-mop:slot-definition-location
     (find name (sb-mop:class-slots class) :key #'sb-mop:slot-definition-name))))

(defmacro gslot (port name)
  "PORT's slot NAME, read at its place (gport has no subclasses): the
per-character paths' slot reads, without PCL's."
  `(sb-mop:standard-instance-access ,port (load-time-value (gport-slot-location ',name) t)))

(defun buffered-ascii-line (port)
  "The rest of PORT's line, without its newline, which is consumed, if all
of it is ASCII in PORT's read buffer and PORT a UTF-8 port past its start;
else NIL (%read-line's common case: no character at a time)."
  (when (and (eq (class-of port) (load-time-value (find-class 'gport) t))
	     (eq (gslot port codec) :utf-8) (gslot port bom-state))
    (let* ((rb (gslot port rbuffer))
	   (buf (svref rb 0))
	   (rpos (svref rb 1))
	   (rend (svref rb 2)))
      (declare (simple-vector rb) (type (simple-array (unsigned-byte 8) (*)) buf) (fixnum rpos rend))
      (let ((nl (position 10 buf :start rpos :end rend)))
	(when (and nl (loop for i of-type fixnum from rpos below nl always (< (aref buf i) 128)))
	  (let ((s (make-string (- nl rpos)))
		(position (svref rb 4)))
	    (loop for i of-type fixnum from rpos below nl
		  for j of-type fixnum from 0
		  do (setf (schar s j) (code-char (aref buf i))))
	    (setf (svref rb 1) (1+ nl))
	    (incf (car position))
	    (setf (cdr position) 0)
	    s))))))

(defun buffered-ascii-char (port)
  "PORT's next character, consumed, if it is an ASCII byte in its read
buffer and PORT a UTF-8 port past its start (its byte-order mark looked
for); else NIL (read-char asks this per character)."
  (when (and (eq (class-of port) (load-time-value (find-class 'gport) t))
	     (eq (gslot port codec) :utf-8) (gslot port bom-state))
    (let* ((rb (gslot port rbuffer))
	   (rpos (svref rb 1)))
      (declare (simple-vector rb) (fixnum rpos))
      (when (< rpos (the fixnum (svref rb 2)))
	(let ((b (aref (the (simple-array (unsigned-byte 8) (*)) (svref rb 0)) rpos)))
	  (when (< b 128)
	    (setf (svref rb 1) (1+ rpos))
	    (code-char b)))))))

(defun buffer-ascii-char (port char)
  "Put ASCII CHAR in PORT's write buffer, if PORT is an open, buffered
UTF-8 output port with room there and no buffered input, its line and
column kept: true if so (write-char, per character)."
  (let ((code (char-code char)))
    (when (and (< code 128) (eq (class-of port) (load-time-value (find-class 'gport) t))
	       (eq (gslot port codec) :utf-8) (gslot port open)
	       (gslot port output) (gslot port registered))
      (let ((buffering (gslot port buffering))
	    (rb (gslot port rbuffer))
	    (wb (gslot port wbuffer)))
	(declare (simple-vector rb wb))
	(when (and (not (eq buffering :none)) (>= (the fixnum (svref rb 1)) (the fixnum (svref rb 2))))
	  (let ((wbuf (svref wb 0)) (wend (svref wb 2)))
	    (declare (type (simple-array (unsigned-byte 8) (*)) wbuf) (fixnum wend))
	    (when (< wend (length wbuf))
	      (setf (aref wbuf wend) code
		    (svref wb 2) (1+ wend))
	      (track-position (svref rb 4) char)
	      (when (and (eq buffering :line) (= code 10))
		(flush-output port))
	      t)))))))

(defun port-read-char (port)
  "The next character from PORT, or :EOF."
  ;; an ASCII byte already buffered, of a UTF-8 port past its start (its
  ;; byte-order mark looked for): the character at once
  (let ((c (buffered-ascii-char port)))
    (when c (return-from port-read-char c)))
  (fill-input port)
  (let ((codec (codec port))
	(*bad-input-start* (port-rpos port)))
    (case codec
      (:utf-16 (read-wide port 2 (not (eq (bom-endianness port 2) :little))))
      (:utf-32 (read-wide port 4 (not (eq (bom-endianness port 4) :little))))
      (:utf-16be (read-wide port 2 t))
      (:utf-16le (read-wide port 2 nil))
      (:utf-32be (read-wide port 4 t))
      (:utf-32le (read-wide port 4 nil))
      (t (when (and (eq codec :utf-8) (null (port-bom-state port)))
	   ;; a byte-order mark at the start of a stream is dropped, once
	   (setf (port-bom-state port) :done)
	   (when (and (fill-input port) (>= (- (port-rend port) (port-rpos port)) 3)
		      (= (aref (port-rbuf port) (port-rpos port)) #xef)
		      (= (aref (port-rbuf port) (+ 1 (port-rpos port))) #xbb)
		      (= (aref (port-rbuf port) (+ 2 (port-rpos port))) #xbf))
	     (incf (port-rpos port) 3)
	     (setq *bad-input-start* (port-rpos port))))
	 (let ((b (port-read-byte port)))
	   (cond ((eq b :eof) :eof)
		 ((eq codec :utf-8) (read-utf-8 port b))
		 ((eq codec :latin-1) (code-char b))
		 ((eq codec :ascii) (if (< b 128) (code-char b) (bad-input port)))
		 (t (or (svref (third codec) b) (bad-input port)))))))))

(defun encodable-p (char codec)
  (let ((code (char-code char)))
    (case codec
      (:latin-1 (< code 256))
      (:ascii (< code 128))
      ((:utf-8 :utf-16 :utf-16le :utf-16be :utf-32 :utf-32le :utf-32be) t)
      (t (ignore-errors (sb-ext:string-to-octets (string char) :external-format (second codec)))))))

(defun char-octets (char codec)
  "CHAR's bytes in CODEC, which can encode it."
  (let ((code (char-code char)))
    (flet ((units (size big values)
	     (let ((v (make-octets (* size (length values)))) (i 0))
	       (dolist (u values v)
		 (dotimes (k size)
		   (setf (aref v (+ i (if big k (- size k 1)))) (ldb (byte 8 (* 8 (- size k 1))) u)))
		 (incf i size))))
	   (utf-16-units ()
	     (if (< code #x10000)
		 (list code)
		 (let ((c (- code #x10000)))
		   (list (+ #xd800 (ash c -10)) (+ #xdc00 (logand c #x3ff)))))))
      (case codec
	((:latin-1 :ascii) (make-array 1 :element-type '(unsigned-byte 8) :initial-element code))
	(:utf-8 (sb-ext:string-to-octets (string char) :external-format :utf-8))
	((:utf-16 :utf-16be) (units 2 t (utf-16-units)))
	(:utf-16le (units 2 nil (utf-16-units)))
	((:utf-32 :utf-32be) (units 4 t (list code)))
	(:utf-32le (units 4 nil (list code)))
	(t (sb-ext:string-to-octets (string char) :external-format (second codec)))))))

(defun encode-char (port char)
  "CHAR's bytes in PORT's encoding, or its replacement as the
conversion strategy says."
  (let ((codec (codec port)))
    (if (encodable-p char codec)
	(char-octets char codec)
	(let ((strategy (strategy port)))
	  (cond ((string= strategy "substitute") (char-octets #\? codec))
		((string= strategy "escape")
		 (let ((code (char-code char)))
		   (sb-ext:string-to-octets (cond ((option-value *read-options* "r6rs-hex-escapes")
						   (format nil "\\x~(~X~);" code))
						  ((< code #x100)
						   (format nil "\\x~(~2,'0X~)" code))
						  ((< code #x10000)
						   (format nil "\\u~(~4,'0X~)" code))
						  (t (format nil "\\U~(~6,'0X~)" code)))
					    :external-format :latin-1)))
		;; Guile's arguments: subr, message, errno, port, character
		(t (call-throw (ssym "encoding-error")
			       (list "scm_putc" "conversion to port encoding failed" +eilseq+ port char))))))))

(defun track-position (position char)
  "POSITION, a port's (line . column), past CHAR."
  (case char
    (#\Newline (incf (car position)) (setf (cdr position) 0))
    (#\Tab (setf (cdr position) (* 8 (1+ (floor (cdr position) 8)))))
    (#\Backspace (when (plusp (cdr position)) (decf (cdr position))))
    (#\Return (setf (cdr position) 0))
    (#\Bel)				; the bell takes no column, as in Guile
    (t (incf (cdr position)))))

(defun track-char (port char)
  "PORT's line and column past CHAR."
  (track-position (svref (if (eq (class-of port) (load-time-value (find-class 'gport) t))
			     (gslot port rbuffer)
			     (port-rbuffer port))
			 4)
		  char))

(defun write-bom (port)
  "At the start of a UTF-16 or UTF-32 stream, a big-endian byte-order
mark, as Guile writes."
  (when (and (null (port-bom-state port)) (member (codec port) '(:utf-16 :utf-32)))
    (setf (port-bom-state port) :big)
    (write-octets port (if (eq (codec port) :utf-16)
			   (make-array 2 :element-type '(unsigned-byte 8) :initial-contents '(#xfe #xff))
			   (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(0 0 #xfe #xff))))))

(defun port-write-char (port char)
  ;; an ASCII character to an open, buffered UTF-8 output port with room
  ;; in its buffer and no buffered input: its byte there, at once
  (when (buffer-ascii-char port char)
    (return-from port-write-char))
  (write-bom port)
  (write-octets port (encode-char port char))
  (track-char port char)
  (when (and (eq (port-buffering port) :line) (char= char #\Newline))
    (flush-output port)))

(defun buffer-ascii-string (port string start end)
  "Put STRING[START,END) in PORT's write buffer, if all of it is printable
ASCII or newlines and PORT an open, buffered UTF-8 output port with room
there and no buffered input, its line and column kept: true if so."
  (when (and (simple-string-p string)
	     (eq (class-of port) (load-time-value (find-class 'gport) t))
	     (eq (gslot port codec) :utf-8) (gslot port open)
	     (gslot port output) (gslot port registered)
	     (not (eq (gslot port buffering) :none)))
    (let* ((rb (gslot port rbuffer))
	   (wb (gslot port wbuffer))
	   (wbuf (svref wb 0))
	   (wend (svref wb 2))
	   (n (- end start)))
      (declare (simple-vector rb wb) (type (simple-array (unsigned-byte 8) (*)) wbuf) (fixnum wend n))
      (when (and (>= (the fixnum (svref rb 1)) (the fixnum (svref rb 2)))
		 (<= (+ wend n) (length wbuf))
		 (loop for i of-type fixnum from start below end
		       always (let ((c (char-code (schar string i)))) (or (<= 32 c 126) (= c 10)))))
	(let ((last-newline nil) (newlines 0) (position (svref rb 4)))
	  (declare (fixnum newlines))
	  (loop for i of-type fixnum from start below end
		for j of-type fixnum from wend
		do (let ((c (char-code (schar string i))))
		     (setf (aref wbuf j) c)
		     (when (= c 10) (incf newlines) (setq last-newline i))))
	  (setf (svref wb 2) (+ wend n))
	  (if last-newline
	      (setf (car position) (+ (car position) newlines)
		    (cdr position) (- end last-newline 1))
	      (incf (cdr position) n))
	  (when (and (eq (gslot port buffering) :line) last-newline)
	    (flush-output port))
	  t)))))

(defun port-write-string (port string start end)
  (when (buffer-ascii-string port string start end)
    (return-from port-write-string))
  (if (and (eq (codec port) :utf-8) (not (eq (port-buffering port) :none)))
      (progn
	(write-octets port (sb-ext:string-to-octets string :external-format :utf-8 :start start :end end))
	(loop for i from start below end do (track-char port (char string i)))
	(when (and (eq (port-buffering port) :line) (find #\Newline string :start start :end end))
	  (flush-output port)))
      (loop for i from start below end do (port-write-char port (char string i)))))

(defun port-unread-char (port char)
  (unread-octets port (if (encodable-p char (codec port)) (char-octets char (codec port))
			  (char-octets char :utf-8)))
  (if (char= char #\Newline)
      (decf (port-line* port))
      (when (plusp (port-column* port)) (decf (port-column* port)))))

;;; The Gray stream protocol: what Quasischeme's and Lisp's own
;;; procedures use on a port

(defmethod trivial-gray-streams:stream-read-char ((p gport))
  (let ((c (port-read-char p)))
    (unless (eq c :eof) (track-char p c))
    c))

(defmethod trivial-gray-streams:stream-unread-char ((p gport) c)
  (port-unread-char p c)
  nil)

(defmethod trivial-gray-streams:stream-peek-char ((p gport))
  ;; read, then put the read position back where it was: the very bytes,
  ;; a bad sequence included
  (unless (fill-input p)
    ;; at end of file, which stays pending for the next read
    (return-from trivial-gray-streams:stream-peek-char :eof))
  (let* ((start (port-rpos p))
	 (buffer (port-rbuf p))
	 (c (port-read-char p)))
    (unless (eq c :eof)
      (if (and (eq buffer (port-rbuf p)) (<= start (port-rpos p)))
	  (setf (port-rpos p) start)
	  (unread-octets p (if (encodable-p c (codec p)) (char-octets c (codec p)) (char-octets c :utf-8)))))
    c))

(defmethod trivial-gray-streams:stream-read-char-no-hang ((p gport))
  (when (trivial-gray-streams:stream-listen p)
    (trivial-gray-streams:stream-read-char p)))

(defmethod trivial-gray-streams:stream-listen ((p gport))
  (or (< (port-rpos p) (port-rend p))
      (and (port-fd p) (sb-sys:wait-until-fd-usable (port-fd p) :input 0) t)
      (and (not (port-fd p)) (port-input-p p) (fill-input p))))

(defmethod trivial-gray-streams:stream-clear-input ((p gport))
  (setf (port-rpos p) 0 (port-rend p) 0)
  nil)

(defmethod trivial-gray-streams:stream-write-char ((p gport) c)
  (port-write-char p c)
  c)

(defmethod trivial-gray-streams:stream-write-string ((p gport) string &optional (start 0) end)
  (port-write-string p string start (or end (length string)))
  string)

(defmethod trivial-gray-streams:stream-line-column ((p gport)) (port-column* p))
(defmethod trivial-gray-streams:stream-start-line-p ((p gport)) (zerop (port-column* p)))

(defmethod trivial-gray-streams:stream-finish-output ((p gport)) (flush-output p) nil)
(defmethod trivial-gray-streams:stream-force-output ((p gport)) (flush-output p) nil)
(defmethod trivial-gray-streams:stream-clear-output ((p gport)) (setf (port-wend p) 0) nil)

(defmethod trivial-gray-streams:stream-read-byte ((p gport)) (port-read-byte p))
(defmethod trivial-gray-streams:stream-write-byte ((p gport) b)
  (write-octets p (make-array 1 :element-type '(unsigned-byte 8) :initial-element b))
  b)

(defmethod trivial-gray-streams:stream-read-sequence ((p gport) seq start end &key)
  (if (stringp seq)
      (loop for i from start below end
	    for c = (trivial-gray-streams:stream-read-char p)
	    until (eq c :eof) do (setf (char seq i) c)
	    finally (return i))
      (loop for i from start below end
	    for b = (port-read-byte p)
	    until (eq b :eof) do (setf (aref seq i) b)
	    finally (return i))))

(defmethod trivial-gray-streams:stream-write-sequence ((p gport) seq start end &key)
  (if (stringp seq)
      (port-write-string p seq start (or end (length seq)))
      (write-octets p (coerce seq 'octets) start (or end (length seq))))
  seq)

(defmethod trivial-gray-streams:stream-file-position ((p gport))
  (port-position p))

(defmethod (setf trivial-gray-streams:stream-file-position) (pos (p gport))
  (port-seek p pos 0)
  t)

(defmethod close ((p gport) &key abort)
  (declare (ignore abort))
  (when (port-open-p p)
    (when (port-output-p p) (ignore-errors (flush-output p)))
    (remhash p *open-output-ports*)
    (setf (port-registered p) nil (port-open-p p) nil)
    (let ((fd (port-fd p)))
      (when (port-close-fn p) (funcall (port-close-fn p)))
      (when fd (run-fdes-finalizers fd))))
  t)

;;; (ice-9 fdes-finalizers): procedures called with a file descriptor
;;; when a port closes it
(defvar *fdes-finalizers* (make-hash-table) "fd -> finalizers, the latest first.")

(defun run-fdes-finalizers (fd)
  (let ((finalizers (gethash fd *fdes-finalizers*)))
    (remhash fd *fdes-finalizers*)
    (dolist (f finalizers) (funcall f fd))))

(defextension "scm_init_fdes_finalizers"
  (list (cons "add-fdes-finalizer!"
	      (lambda (fd f) (push f (gethash fd *fdes-finalizers*)) *unspecified*))
	(cons "remove-fdes-finalizer!"
	      (lambda (fd f)
		(setf (gethash fd *fdes-finalizers*) (remove f (gethash fd *fdes-finalizers*) :count 1))
		*unspecified*))))

(defun port-position (port)
  (unless (port-seek-fn port)
    (guile-error (ssym "wrong-type-arg") "seek" "Wrong type argument in position 1: ~S" (list port) (list port)))
  (flush-output port)
  (- (funcall (port-seek-fn port) 0 1) (- (port-rend port) (port-rpos port))))

(defun port-seek (port offset whence)
  (unless (port-seek-fn port)
    (guile-error (ssym "wrong-type-arg") "seek" "Wrong type argument in position 1: ~S" (list port) (list port)))
  (if (and (= whence 1) (zerop offset))
      (port-position port)
      (progn
	(flush-output port)
	(consume-eof port)		; a seek clears a pending end of file
	(let ((pending (- (port-rend port) (port-rpos port))))
	  (setf (port-rpos port) 0 (port-rend port) 0)
	  (let ((pos (funcall (port-seek-fn port) (if (= whence 1) (- offset pending) offset) whence)))
	    ;; a byte-order mark is looked for again only at the start
	    (setf (port-bom-state port) (if (eql pos 0) nil :done))
	    ;; and the byte order it gave (specialize-port-encoding!) forgotten
	    (when (eql pos 0) (setf (port-codec port) nil))
	    pos)))))

;;; ------------------------------------------------------------------
;;; Backends

(defun make-port (&rest initargs &key encoding &allow-other-keys)
  (let ((p (apply #'make-instance 'gport :allow-other-keys t initargs)))
    (setf (port-encoding-name p) (or (and encoding (canonical-encoding encoding)) (default-encoding)))
    p))

;;; Bytes in memory: string and bytevector ports

(defstruct (memory (:constructor make-memory (octets &optional (length (length octets)))))
  octets length (position 0)
  (string-p nil))			; a string port's (setvbuf refuses those)

(defun memory-backend (memory)
  (list :read (lambda (buf start count)
		(let ((n (max 0 (min count (- (memory-length memory) (memory-position memory))))))
		  (replace buf (memory-octets memory) :start1 start :end1 (+ start n)
						      :start2 (memory-position memory))
		  (incf (memory-position memory) n)
		  n))
	:write (lambda (buf start count)
		 (let* ((pos (memory-position memory)) (end (+ pos count)))
		   (when (> end (length (memory-octets memory)))
		     (let ((new (make-octets (max end (* 2 (length (memory-octets memory)))))))
		       (replace new (memory-octets memory))
		       (setf (memory-octets memory) new)))
		   (replace (memory-octets memory) buf :start1 pos :start2 start :end2 (+ start count))
		   (setf (memory-position memory) end
			 (memory-length memory) (max end (memory-length memory)))
		   count))
	:seek (lambda (offset whence)
		(let ((pos (+ offset (case whence (0 0) (1 (memory-position memory)) (t (memory-length memory))))))
		  (when (or (minusp pos) (> pos (memory-length memory)))
		    (guile-error (ssym "out-of-range") "seek" "Value out of range: ~S" (list offset) (list offset)))
		  (setf (memory-position memory) pos)))
	:truncate (lambda (length) (setf (memory-length memory) (min length (memory-length memory))))))

(defun memory-contents (port)
  (flush-output port)
  (let ((m (port-data port)))
    (subseq (memory-octets m) 0 (memory-length m))))

(defun open-input-string* (string)
  (unless (stringp string) (wrong-type "open-input-string" 1 string))
  (let ((p (memory-port (sb-ext:string-to-octets string :external-format :utf-8) :input t)))
    (setf (memory-string-p (port-data p)) t)
    p))

(defun memory-port (octets &key input output (encoding "UTF-8"))
  "A port on OCTETS; an output-only one starts empty, OCTETS its space."
  (let ((m (make-memory octets (if input (length octets) 0))))
    (apply #'make-port :kind "file" :input input :output output :encoding encoding :data m
	   (memory-backend m))))

(defun open-output-string* ()
  (let ((p (memory-port (make-octets 64) :output t)))
    (setf (memory-string-p (port-data p)) t)
    p))

(defun port-output-string (port)
  (unless (and (gport-p port) (memory-p (port-data port)) (port-open-p port))
    (wrong-type "get-output-string" 1 port))
  (let ((octets (memory-contents port)))
    (if (string= (port-encoding-name port) "UTF-8")
	(sb-ext:octets-to-string octets :external-format (list :utf-8 :replacement (code-char #xfffd)))
	;; decode as the port does
	(let ((in (memory-port octets :input t :encoding (port-encoding-name port))))
	  (with-output-to-string (s)
	    (loop for c = (port-read-char in) until (eq c :eof) do (write-char c s)))))))

;;; File descriptors

(defun fd-backend (fd)
  (list :fd fd
	:read (lambda (buf start count)
		(loop
		  (multiple-value-bind (n errno)
		      (sb-sys:with-pinned-objects (buf)
			(sb-unix:unix-read fd (sb-sys:sap+ (sb-sys:vector-sap buf) start) count))
		    (cond (n (return n))
			  ((= errno sb-unix:eintr))
			  ((= errno sb-unix:eagain) (sb-sys:wait-until-fd-usable fd :input))
			  (t (port-system-error "fport_read" errno))))))
	:write (lambda (buf start count)
		 (loop
		   (multiple-value-bind (n errno) (sb-unix:unix-write fd buf start count)
		     (cond (n (return n))
			   ((= errno sb-unix:eintr))
			   ((= errno sb-unix:eagain) (sb-sys:wait-until-fd-usable fd :output))
			   (t (port-system-error "fport_write" errno))))))
	:seek (lambda (offset whence)
		(multiple-value-bind (pos errno) (sb-unix:unix-lseek fd offset whence)
		  (or pos (port-system-error "seek" errno))))
	:truncate (lambda (length) (sb-posix:ftruncate fd length))
	:close (lambda () (sb-unix:unix-close fd))))

(defun port-system-error (who errno &optional (args '()))
  (guile-error (ssym "system-error") who "~A" (cons (sb-int:strerror errno) args) (list errno)))

(defun fd-port (fd mode &key filename encoding)
  (let* ((input (or (find #\r mode) (find #\+ mode)))
	 (output (or (find #\w mode) (find #\a mode) (find #\+ mode)))
	 (port (apply #'make-port :kind "file" :input (and input t) :output (and output t)
				  :filename (or filename qs:false)
				  :encoding (cond ((find #\b mode) "ISO-8859-1") (encoding))
				  :buffering (if (find #\0 mode) :none (if (find #\l mode) :line :full))
				  (fd-backend fd))))
    (when (and (find #\0 mode) (find #\b mode))
      (setf (port-buffering port) :none))
    ;; a regular file's: buffers of +FILE-BUFFER-SIZE+, a system call per
    ;; that many bytes rather than per 1024 (cat's time was its writes)
    (when (and (eq (port-buffering port) :full)
	       (ignore-errors (sb-posix:s-isreg (sb-posix:stat-mode (sb-posix:fstat fd)))))
      (setf (port-rbuf port) (make-octets +file-buffer-size+)
	    (port-wbuf port) (make-octets +file-buffer-size+)))
    (setf (gethash port *fd-ports*) t)
    port))

(defun scan-for-coding (text)
  "The encoding a coding: (or coding=) declaration in TEXT names, if it
is in a comment (after a ; on its line, or in a #! ... !# block), as
Guile's scan; upper-cased; or NIL."
  (loop for at = (search "coding" text) then (search "coding" text :start2 (1+ at))
	while at
	do (let ((i (+ at 6)))
	     (when (and (< i (length text)) (member (char text i) '(#\: #\=)))
	       (incf i)
	       (loop while (and (< i (length text)) (member (char text i) '(#\Space #\Tab))) do (incf i))
	       (let* ((end (or (position-if-not (lambda (c) (or (alphanumericp c) (find c "-_.+"))) text :start i)
			       (length text)))
		      (line-start (let ((nl (position #\Newline text :end at :from-end t))) (if nl (1+ nl) 0)))
		      (block (let ((open (search "#!" text :end2 at)))
			       (and open (not (search "!#" text :start2 open :end2 at))))))
		 (when (and (> end i)
			    (or (find #\; text :start line-start :end at) block))
		   (return (string-upcase (subseq text i end)))))))))

(defun file-coding (path)
  "The encoding a file's coding: comment names in its first lines, or NIL."
  (with-open-file (in path :element-type '(unsigned-byte 8) :if-does-not-exist nil)
    (when in
      (let* ((buf (make-octets 1024))
	     (n (read-sequence buf in)))
	(scan-for-coding (sb-ext:octets-to-string buf :end n :external-format :latin-1))))))

(defun port-coding (port)
  "The encoding the coding: declaration at PORT's next input names: its
read buffer, unread."
  (let ((p (->port port "file-encoding")))
    (fill-input p)
    (let ((end (min (port-rend p) (+ (port-rpos p) 1024))))
      (or (scan-for-coding (sb-ext:octets-to-string (port-rbuf p) :start (port-rpos p) :end end
								   :external-format :latin-1))
	  qs:false))))

(defguile "file-encoding" (port) (port-coding port))
(defguile "%file-encoding" (port) (port-coding port))

(defun open-file (filename mode &key (encoding qs:false) (guess-encoding qs:false) (buffering qs:false))
  (declare (ignore buffering))
  (unless (stringp filename) (wrong-type "open-file" 1 filename))
  (let* ((mode (if (symbolp mode) (qs:scheme-symbol-name mode) mode))
	 (base (find-if (lambda (c) (find c "rwa")) mode))
	 (plus (find #\+ mode)))
    (when (some (lambda (c) (> (char-code c) 255)) mode)
      (guile-error (ssym "out-of-range") "open-file" "Value out of range: ~S" (list mode) (list mode)))
    (unless (and base (every (lambda (c) (find c "rwab+0l")) mode))
      (guile-error (ssym "misc-error") "open-file" "Invalid mode string: ~S" (list mode)))
    (let ((flags (logior (case base
			   (#\r (if plus sb-posix:o-rdwr sb-posix:o-rdonly))
			   (#\w (logior (if plus sb-posix:o-rdwr sb-posix:o-wronly) sb-posix:o-creat sb-posix:o-trunc))
			   (#\a (logior (if plus sb-posix:o-rdwr sb-posix:o-wronly) sb-posix:o-creat sb-posix:o-append))))))
      (multiple-value-bind (fd errno) (sb-unix:unix-open filename flags #o666)
	(unless fd
	  (guile-error (ssym "system-error") "open-file" "~A: ~S"
		       (list (sb-int:strerror errno) filename) (list errno)))
	(fd-port fd mode
		 :filename (port-file-name filename)
		 ;; a coding declaration, guessed, takes precedence
		 :encoding (or (and (truthy guess-encoding) (find #\r mode) (file-coding filename))
			       (and (stringp encoding) encoding)))))))

;;; The file name a file port records, as %file-port-name-canonicalization
;;; asks: as given (#f), canonical ('absolute), or relative to the
;;; %load-path entry it is under, the longest ('relative; as given if
;;; none).

(defun real-path (path)
  "PATH with symbolic links, . and .. resolved (realpath), or NIL."
  (let ((p (cffi:foreign-funcall "realpath" :string path :pointer (cffi:null-pointer) :pointer)))
    (unless (cffi:null-pointer-p p)
      (prog1 (cffi:foreign-string-to-lisp p)
	(cffi:foreign-funcall "free" :pointer p :void)))))

(defun port-file-name (filename)
  (let* ((fluid (root-value "%file-port-name-canonicalization"))
	 (mode (and fluid (fluid-p fluid) (fluid-value fluid)))
	 (mode (and (symbolp mode) mode (not (eq mode qs:false)) (qs:scheme-symbol-name mode))))
    (cond ((equal mode "absolute") (or (real-path filename) filename))
	  ((equal mode "relative")
	   (let ((canonical (real-path filename)) (best nil))
	     (when canonical
	       (dolist (dir (let ((v (root-value "%load-path"))) (if (listp v) v '())))
		 (let ((d (and (stringp dir) (plusp (length dir)) (real-path dir))))
		   (when d
		     (let ((prefix (if (char= (char d (1- (length d))) #\/) d (concatenate 'string d "/"))))
		       (when (and (> (length canonical) (length prefix))
				  (string= prefix canonical :end2 (length prefix))
				  (or (null best) (> (length prefix) (length best))))
			 (setq best prefix)))))))
	     (if best (subseq canonical (length best)) filename)))
	  (t filename))))

;;; Lisp streams as ports

(defun call-with-reading-port (stream function)
  "Call FUNCTION with a port reading Lisp STREAM a character at a time (a
reader extension's, reading the rest of a datum from the stream the
reader reads): what it peeked at and didn't take goes back to STREAM."
  (let ((port (make-port :kind "file" :input t :output nil :encoding "UTF-8" :buffering :none
			 :read (lambda (buf start count)
				 (declare (ignore count))
				 (let ((c (read-char stream nil nil)))
				   (if c
				       (let ((octets (sb-ext:string-to-octets (string c) :external-format :utf-8)))
					 (replace buf octets :start1 start)
					 (length octets))
				       0))))))
    (unwind-protect (funcall function port)
      (let ((left (- (port-rend port) (port-rpos port))))
	(when (plusp left)
	  (let ((chars (length (sb-ext:octets-to-string (port-rbuf port) :start (port-rpos port)
							 :end (port-rend port) :external-format :utf-8))))
	    (handler-case (file-position stream (- (file-position stream) chars))
	      (error () (when (= chars 1) (unread-char (char (sb-ext:octets-to-string (port-rbuf port) :start (port-rpos port) :end (port-rend port) :external-format :utf-8) 0) stream))))))))))

(defvar *stream-ports* (make-hash-table :test 'eq :weakness :key :synchronized t))

(defun stream-port (stream)
  "The port reading and writing the Lisp stream STREAM: one per stream."
  (cond ((gport-p stream) stream)
	((gethash stream *stream-ports*))
	(t (setf (gethash stream *stream-ports*)
		 (let ((pending (make-octets 0)))
		   (make-port
		    :kind "file" :input (input-stream-p stream) :output (output-stream-p stream)
		    :encoding "UTF-8"
		    :buffering (if (eq stream *error-output*) :none :line)
		    :fd (and (typep stream 'sb-sys:fd-stream) (sb-sys:fd-stream-fd stream))
		    :data stream
		    :read (lambda (buf start count)
			    ;; characters as UTF-8: a line at most, waiting only for the first
			    (when (zerop (length pending))
			      (let ((c (read-char stream nil nil)))
				(when c
				  (setq pending
					(sb-ext:string-to-octets
					 (with-output-to-string (s)
					   (write-char c s)
					   (loop repeat 1000
						 until (char= c #\Newline)
						 while (listen stream)
						 do (setq c (read-char stream nil nil))
						 while c do (write-char c s)))
					 :external-format :utf-8)))))
			    (let ((n (min count (length pending))))
			      (replace buf pending :start1 start :end2 n)
			      (setq pending (subseq pending n))
			      n))
		    :write (lambda (buf start count)
			     (write-string (sb-ext:octets-to-string buf :start start :end (+ start count)
									:external-format (list :utf-8 :replacement (code-char #xfffd)))
					   stream)
			     (force-output stream)
			     count)))))))

(defmacro with-guile-ports (&body body)
  `(call-with-guile-ports (lambda () ,@body)))

(defun call-with-guile-ports (thunk)
  "Call THUNK with the current Lisp streams as ports, flushed after."
  (if (and (gport-p *standard-output*) (gport-p *error-output*) (gport-p *standard-input*))
      (funcall thunk)
      (let ((*standard-output* (stream-port *standard-output*))
	    (*standard-input* (stream-port *standard-input*))
	    (*error-output* (stream-port *error-output*)))
	(unwind-protect (funcall thunk)
	  (ignore-errors (flush-output *standard-output*))
	  (ignore-errors (flush-output *error-output*))))))

;;; ------------------------------------------------------------------
;;; The procedures

(defun make-special-fluid (symbol)
  (let ((f (make-fluid* qs:false)))
    (setf (fluid-special f) symbol)
    f))

(defvar *current-input-port-fluid* (make-special-fluid '*standard-input*))
(defvar *current-output-port-fluid* (make-special-fluid '*standard-output*))
(defvar *current-error-port-fluid* (make-special-fluid '*error-output*))
(defvar *current-warning-port-fluid* (make-special-fluid '*error-output*))

(defun ->port (x who &optional (pos 1))
  "X as a port: a port, or a Lisp stream wrapped as one."
  (cond ((gport-p x) x)
	((streamp x) (stream-port x))
	(t (wrong-type who pos x))))

(defun in-port (p who) (->port (if (null p) *standard-input* p) who))
(defun out-port (p who) (->port (if (null p) *standard-output* p) who))

(defun port-column (port) (port-column* (->port port "port-column")))

(defun port-mode-string (port)
  (let ((p (->port port "port-mode")))
    (cond ((and (port-input-p p) (port-output-p p)) "r+")
	  ((port-input-p p) "r")
	  (t "w"))))

(defun read-char* (&optional port)
  (let* ((p (in-port port "read-char")) (c (port-read-char p)))
    (if (eq c :eof) qs:eof-object (progn (track-char p c) c))))

(defun peek-char* (&optional port)
  (let ((c (trivial-gray-streams:stream-peek-char (in-port port "peek-char"))))
    (if (eq c :eof) qs:eof-object c)))

(defun unread-string* (string &optional port)
  (let ((p (in-port port "unread-string")))
    (loop for i from (1- (length string)) downto 0 do (port-unread-char p (char string i))))
  *unspecified*)

(defun install-port-root-bindings ()
  (install-posix-constants)
  (flet ((def (name value) (obarray-define (ssym name) value)))
    (def "%current-input-port-fluid" *current-input-port-fluid*)
    (def "%current-output-port-fluid" *current-output-port-fluid*)
    (def "%current-error-port-fluid" *current-error-port-fluid*)
    (def "%current-warning-port-fluid" *current-warning-port-fluid*)
    (def "current-error-port" (lambda () *error-output*))
    (def "current-warning-port" (lambda () *error-output*))
    (def "current-load-port" (lambda () qs:false))
    (def "open-file" (lambda (filename mode &rest keys)
		       ;; Guile's keyword errors, rather than Lisp's
		       (check-keywords keys '(:encoding :guess-encoding :buffering) nil nil)
		       (apply #'open-file filename mode keys)))
    (def "open-input-file" (lambda (f &rest options)
			     (apply #'open-file f (if (getf (keywords options) :binary) "rb" "r")
				    (encoding-options options))))
    (def "open-output-file" (lambda (f &rest options)
			      (apply #'open-file f (if (getf (keywords options) :binary) "wb" "w")
				     (encoding-options options))))
    (def "open-input-string" #'open-input-string*)
    (def "open-output-string" #'open-output-string*)
    (def "get-output-string" #'port-output-string)
    (def "call-with-output-string"
	(lambda (proc) (let ((p (open-output-string*))) (funcall proc p) (port-output-string p))))
    (def "set-port-encoding!" (lambda (port enc) (set-encoding (->port port "set-port-encoding!") enc) *unspecified*))
    (def "force-output" (lambda (&optional port) (flush-output (out-port port "force-output")) *unspecified*))
    (def "close-port" (lambda (port)
			(let ((p (->port port "close-port")))
			  (bool (and (port-open-p p) (progn (close p) t))))))
    (def "read-char" #'read-char*)
    (def "peek-char" #'peek-char*)
    (def "unread-char" (lambda (c &optional port) (port-unread-char (in-port port "unread-char") c) c))
    (def "unread-string" #'unread-string*)
    (def "write-char" (lambda (c &optional port) (port-write-char (out-port port "write-char") c) *unspecified*))
    (def "newline" (lambda (&optional port) (port-write-char (out-port port "newline") #\Newline) *unspecified*))
    (def "char-ready?" (lambda (&optional port)
			 (bool (trivial-gray-streams:stream-listen (in-port port "char-ready?")))))
    (def "port?" (lambda (x) (bool (streamp x))))
    (def "input-port?" (lambda (x) (bool (and (streamp x) (input-stream-p x)))))
    (def "output-port?" (lambda (x) (bool (and (streamp x) (output-stream-p x)))))
    (def "%default-port-encoding" (make-fluid* "UTF-8"))
    (def "%default-port-conversion-strategy" (make-fluid* (ssym "substitute")))))

(defun keywords (options)
  "A plist of Guile keyword arguments, keys as Lisp keywords."
  (loop for (k v) on options by #'cddr when (keywordp k) append (list k v)))

(defun encoding-options (options)
  (let ((kw (keywords options)))
    (append (and (getf kw :encoding) (list :encoding (getf kw :encoding)))
	    (and (getf kw :guess-encoding) (list :guess-encoding (getf kw :guess-encoding))))))

(defun port-property (port key)
  (let ((e (assoc key (port-properties (->port port "%port-property")))))
    (if e (cdr e) qs:false)))

(defextension "scm_init_ice_9_ports"
  (append
   (list
   (cons "%port-property" #'port-property)
   (cons "%set-port-property!" (lambda (port key value)
				 (let* ((p (->port port "%set-port-property!"))
					(e (assoc key (port-properties p))))
				   (if e (setf (cdr e) value) (push (cons key value) (port-properties p))))
				 *unspecified*))
   (cons "set-current-input-port" (lambda (p) (prog1 *standard-input* (setq *standard-input* p))))
   (cons "set-current-output-port" (lambda (p) (prog1 *standard-output* (setq *standard-output* p))))
   (cons "set-current-error-port" (lambda (p) (prog1 *error-output* (setq *error-output* p))))
   (cons "port-mode" #'port-mode-string)
   (cons "port?" (lambda (x) (bool (streamp x))))
   (cons "input-port?" (lambda (x) (bool (and (streamp x) (input-stream-p x)))))
   (cons "output-port?" (lambda (x) (bool (and (streamp x) (output-stream-p x)))))
   (cons "port-closed?" (lambda (x) (bool (not (open-stream-p (->port x "port-closed?"))))))
   (cons "close-input-port" (lambda (p) (close (->port p "close-input-port")) *unspecified*))
   (cons "close-output-port" (lambda (p) (close (->port p "close-output-port")) *unspecified*))
   (cons "the-eof-object" qs:eof-object)
   (cons "%port-encoding" (lambda (p) (ssym (port-encoding-name (->port p "port-encoding")))))
   (cons "port-conversion-strategy"
	 (lambda (p)
	   (if (eq p qs:false)
	       (fluid-value (root-value "%default-port-conversion-strategy"))
	       (ssym (strategy (->port p "port-conversion-strategy"))))))
   (cons "set-port-conversion-strategy!"
	 (lambda (p s)
	   (unless (and (symbolp s) (member (qs:scheme-symbol-name s) '("error" "substitute" "escape")
					    :test #'string=))
	     (wrong-type "set-port-conversion-strategy!" 2 s))
	   (if (eq p qs:false)
	       (setf (fluid-value (root-value "%default-port-conversion-strategy")) s)
	       (setf (port-strategy (->port p "set-port-conversion-strategy!")) s))
	   *unspecified*))
   (cons "read-char" #'read-char*)
   (cons "peek-char" #'peek-char*)
   (cons "unread-char" (lambda (c &optional port) (port-unread-char (in-port port "unread-char") c) c))
   (cons "unread-string" #'unread-string*)
   (cons "setvbuf" (lambda (port mode &optional size)
		     (let ((p (->port port "setvbuf")))
		       ;; an open port with a descriptor or a custom one
		       (unless (and (port-open-p p)
				    (not (and (memory-p (port-data p)) (memory-string-p (port-data p)))))
			 (wrong-type "setvbuf" 1 port))
		       (flush-output p)
		       (setf (port-buffering p)
			     (let ((m (if (symbolp mode) (qs:scheme-symbol-name mode) mode)))
			       (cond ((member m '("none" 0) :test #'equal) :none)
				     ((member m '("line" 1) :test #'equal) :line)
				     (t :full))))
		       ;; buffers of SIZE (or the default), as libguile's
		       ;; setvbuf makes, replacing a custom port's natural sizes;
		       ;; a read buffer holding input is kept
		       (let ((n (cond ((eq (port-buffering p) :none) 1)
				      ((and (integerp size) (plusp size)) size)
				      (t +buffer-size+))))
			 (unless (= n (length (port-wbuf p)))
			   (setf (port-wbuf p) (make-octets n)))
			 (when (and (= (port-rpos p) (port-rend p)) (/= n (length (port-rbuf p))))
			   (setf (port-rbuf p) (make-octets n) (port-rpos p) 0 (port-rend p) 0))))
		     *unspecified*))
   (cons "drain-input" (lambda (port)
			 (let* ((p (->port port "drain-input"))
				(octets (subseq (port-rbuf p) (port-rpos p) (port-rend p))))
			   (setf (port-rpos p) 0 (port-rend p) 0)
			   (let ((in (memory-port octets :input t :encoding (port-encoding-name p))))
			     (with-output-to-string (s)
			       (loop for c = (port-read-char in) until (eq c :eof) do (write-char c s)))))))
   (cons "char-ready?" (lambda (&optional port)
			 (bool (trivial-gray-streams:stream-listen (in-port port "char-ready?")))))
   (cons "seek" (lambda (port offset whence)
		  (if (integerp port)
		      (multiple-value-bind (pos errno) (sb-unix:unix-lseek port offset whence)
			(or pos (port-system-error "seek" errno)))
		      (port-seek (->port port "seek") offset whence))))
   (cons "SEEK_SET" 0) (cons "SEEK_CUR" 1) (cons "SEEK_END" 2)
   (cons "truncate-file" (lambda (obj &optional length)
			   (cond
			     ((stringp obj)
			      (unless length
				(guile-error (ssym "misc-error") "truncate-file"
					     "must supply length if OBJECT is a filename" '()))
			      (sb-posix:truncate obj length))
			     ((integerp obj)
			      ;; to the descriptor's position, if no length
			      (sb-posix:ftruncate obj (or length (sb-posix:lseek obj 0 sb-posix:seek-cur))))
			     (t
			       (let ((p (->port obj "truncate-file")))
				 (when (memory-p (port-data p))
				   ;; a string or bytevector port: an output one, not cut
				   ;; before its position
				   (unless (port-output-p p) (wrong-type "truncate-file" 1 obj))
				   (when (and length (< length (port-position p)))
				     (out-of-range "truncate-file" length)))
				 (flush-output p)
				 (funcall (or (port-truncate-fn p) (wrong-type "truncate-file" 1 obj))
					  (or length (port-position p))))))
			   *unspecified*))
   (cons "port-line" (lambda (p) (port-line* (->port p "port-line"))))
   (cons "set-port-line!" (lambda (p n) (setf (port-line* (->port p "set-port-line!")) n) *unspecified*))
   (cons "port-column" #'port-column)
   (cons "set-port-column!" (lambda (p n) (setf (port-column* (->port p "set-port-column!")) n) *unspecified*))
   (cons "port-filename" (lambda (p) (port-filename* (->port p "port-filename"))))
   (cons "set-port-filename!" (lambda (p name) (setf (port-filename* (->port p "set-port-filename!")) name) *unspecified*))
   (cons "port-for-each" (lambda (proc)
			   (loop for p being the hash-keys of *open-output-ports* do (funcall proc p))
			   *unspecified*))
   (cons "flush-all-ports" (lambda () (flush-all-ports) *unspecified*))
   (cons "put-char" (lambda (p c) (port-write-char (->port p "put-char") c) *unspecified*))
   (cons "put-string" (lambda (p s &optional (start 0) (count (- (length s) start)))
			(port-write-string (->port p "put-string") s start (+ start count)) *unspecified*))
   (cons "%make-void-port" (lambda (mode)
			     (make-port :kind "void" :input (and (find #\r mode) t) :output (and (find #\w mode) t)
					:read (lambda (buf start count) (declare (ignore buf start count)) 0)
					:write (lambda (buf start count) (declare (ignore buf start)) count)))))
   ;; (ice-9 ports internal): the buffers themselves, which (ice-9
   ;; suspendable-ports) reads and writes as this file does, and what
   ;; that needs of a port's backend and encoding
   (list (cons "port-read-buffering"
	       (lambda (p) (let ((p (->port p "port-read-buffering")))
			     (if (eq (port-buffering p) :none) 1 (length (port-rbuf p))))))
	 (cons "port-read-buffer"
	       (lambda (p) (let ((p (->port p "port-read-buffer"))) (check-open p "read") (port-rbuffer p))))
	 (cons "port-write-buffer"
	       (lambda (p) (let ((p (->port p "port-write-buffer"))) (check-open p "write") (port-wbuffer p))))
	 (cons "port-auxiliary-write-buffer"
	       (lambda (p) (port-aux-buffer* (->port p "port-auxiliary-write-buffer"))))
	 (cons "port-line-buffered?" (lambda (p) (bool (eq (port-buffering (->port p "port-line-buffered?")) :line))))
	 (cons "expand-port-read-buffer!"
	       (lambda (p size)
		 (let ((p (->port p "expand-port-read-buffer!")))
		   (when (< (length (port-rbuf p)) size)
		     (let ((new (make-octets size)))
		       (replace new (port-rbuf p) :start2 (port-rpos p) :end2 (port-rend p))
		       (setf (port-rend p) (- (port-rend p) (port-rpos p)) (port-rpos p) 0
			     (port-rbuf p) new)))
		   *unspecified*)))
	 (cons "port-read"
	       (lambda (p)
		 (check-open (->port p "port-read") "port-read")
		 (lambda (port bv start count) (funcall (port-read-fn port) bv start count))))
	 (cons "port-write"
	       (lambda (p)
		 (check-open (->port p "port-write") "port-write")
		 (lambda (port bv start count) (funcall (port-write-fn port) bv start count))))
	 (cons "port-random-access?" (lambda (p) (bool (port-random-access-p (->port p "port-random-access?")))))
	 (cons "port-poll" (lambda (p mode &optional timeout) (declare (ignore p mode timeout)) qs:true))
	 (cons "port-read-wait-fd" (lambda (p) (or (port-fd (->port p "port-read-wait-fd")) qs:false)))
	 (cons "port-write-wait-fd" (lambda (p) (or (port-fd (->port p "port-write-wait-fd")) qs:false)))
	 (cons "port-clear-stream-start-for-bom-read"
	       (lambda (p)
		 (let ((p (->port p "port-clear-stream-start-for-bom-read")))
		   (bool (and (null (port-bom-state p))
			      (member (encoding-codec (port-encoding-name p)) '(:utf-8 :utf-16 :utf-32))
			      (setf (port-bom-state p) :done))))))
	 (cons "port-clear-stream-start-for-bom-write"
	       (lambda (p buf)
		 (let ((p (->port p "port-clear-stream-start-for-bom-write")))
		   (when (and (null (port-bom-state p)) (member (codec p) '(:utf-16 :utf-32)))
		     (setf (port-bom-state p) :big)
		     (buffer-put buf (if (eq (codec p) :utf-16) #(#xfe #xff) #(0 0 #xfe #xff))))
		   *unspecified*)))
	 (cons "specialize-port-encoding!"
	       ;; the byte order a byte-order mark gave: the port's encoding
	       ;; is still "UTF-16", its codec the specific one
	       (lambda (p encoding)
		 (let ((p (->port p "specialize-port-encoding!"))
		       (name (canonical-encoding encoding)))
		   (unless name (wrong-type "specialize-port-encoding!" 2 encoding))
		   (setf (port-codec p) (encoding-codec name))
		   *unspecified*)))
	 (cons "port-decode-char"
	       (lambda (p bv start count)
		 (let* ((p (->port p "port-decode-char"))
			(c (decode-octets-char (codec p) bv start count)))
		   (case c
		     (:incomplete qs:false)
		     (:invalid (if (string= (strategy p) "substitute") (code-char #xfffd) (decoding-error p)))
		     (t c)))))
	 (cons "port-encode-char"
	       (lambda (p buf char)
		 (let ((p (->port p "port-encode-char")))
		   (buffer-put buf (encode-char p char))
		   (track-char p char)
		   *unspecified*)))
	 (cons "port-encode-chars"
	       ;; as many of STRING's chars as fit in BUF: how many
	       (lambda (p buf string start count)
		 (let ((p (->port p "port-encode-chars")))
		   (loop for i from start below (+ start count)
			 for bytes = (encode-char p (char string i))
			 while (<= (+ (svref buf 2) (length bytes)) (length (svref buf 0)))
			 do (buffer-put buf bytes) (track-char p (char string i))
			 finally (return (- i start)))))))))

(defun port-aux-buffer* (p)
  (or (port-aux-buffer p)
      (setf (port-aux-buffer p) (make-port-buffer +buffer-size+ (svref (port-rbuffer p) 4)))))

(defun buffer-put (buf bytes)
  "Add BYTES at the end of a port buffer, growing it if need be."
  (let ((end (svref buf 2)))
    (when (> (+ end (length bytes)) (length (svref buf 0)))
      (let ((new (make-octets (max (* 2 (length (svref buf 0))) (+ end (length bytes))))))
	(replace new (svref buf 0))
	(setf (svref buf 0) new)))
    (replace (svref buf 0) bytes :start1 end)
    (setf (svref buf 2) (+ end (length bytes)))))

(defun decode-octets-char (codec bv start count)
  "The character BV[START, START+COUNT) is in CODEC, or :INCOMPLETE if
those bytes begin one, or :INVALID."
  (flet ((unit (size big at)
	   (let ((u 0))
	     (dotimes (k size u)
	       (setq u (logior (ash u 8) (aref bv (+ start at (if big k (- size k 1))))))))))
    (case codec
      ((:utf-16 :utf-16be :utf-16le)
       (let ((big (not (eq codec :utf-16le))))
	 (if (< count 2)
	     :incomplete
	     (let ((u (unit 2 big 0)))
	       (cond ((<= #xd800 u #xdbff)
		      (if (< count 4)
			  :incomplete
			  (let ((lo (unit 2 big 2)))
			    (if (and (= count 4) (<= #xdc00 lo #xdfff))
				(code-char (+ #x10000 (ash (- u #xd800) 10) (- lo #xdc00)))
				:invalid))))
		     ((or (<= #xdc00 u #xdfff) (/= count 2)) :invalid)
		     (t (code-char u)))))))
      ((:utf-32 :utf-32be :utf-32le)
       (if (< count 4)
	   :incomplete
	   (let ((u (unit 4 (not (eq codec :utf-32le)) 0)))
	     (if (or (/= count 4) (<= #xd800 u #xdfff) (> u #x10ffff)) :invalid (code-char u)))))
      (:utf-8
       (let* ((b0 (aref bv start))
	      (n (cond ((< b0 #x80) 1) ((< b0 #xc2) 0) ((< b0 #xe0) 2) ((< b0 #xf0) 3) ((< b0 #xf5) 4) (t 0))))
	 (cond ((= n 0) :invalid)
	       ((loop for i from 1 below (min n count)
		      for b = (aref bv (+ start i))
		      thereis (or (/= (logand b #xc0) #x80)
				  (and (= i 1) (or (and (= b0 #xe0) (< b #xa0)) (and (= b0 #xed) (>= b #xa0))
						   (and (= b0 #xf0) (< b #x90)) (and (= b0 #xf4) (>= b #x90))))))
		:invalid)
	       ((< count n) :incomplete)
	       ((> count n) :invalid)
	       (t (char (sb-ext:octets-to-string bv :external-format :utf-8 :start start :end (+ start n)) 0)))))
      (:latin-1 (if (= count 1) (code-char (aref bv start)) :invalid))
      (:ascii (if (and (= count 1) (< (aref bv start) 128)) (code-char (aref bv start)) :invalid))
      (t (if (= count 1) (or (svref (third codec) (aref bv start)) :invalid) :invalid)))))

(defextension "scm_init_ice_9_fports"
  (list (cons "file-port?" (lambda (x) (bool (and (gport-p x) (port-fd x)))))
	(cons "port-revealed" (lambda (p) (port-revealed* (->port p "port-revealed"))))
	(cons "set-port-revealed!" (lambda (p n) (setf (port-revealed* (->port p "set-port-revealed!")) n) *unspecified*))
	(cons "adjust-port-revealed!" (lambda (p n) (incf (port-revealed* (->port p "adjust-port-revealed!")) n) *unspecified*))))

(defun port-fdes (p who)
  (let ((p (->port p who)))
    (or (port-fd p) (wrong-type who 1 p))))

(defextension "scm_init_ice_9_ioext"
  (list (cons "ftell" (lambda (p) (port-position (->port p "ftell"))))
	(cons "redirect-port" (lambda (old new)
				(sb-posix:dup2 (port-fdes old "redirect-port") (port-fdes new "redirect-port"))
				*unspecified*))
	(cons "dup->fdes" (lambda (fd/port &optional new)
			    (let ((fd (if (integerp fd/port) fd/port (port-fdes fd/port "dup->fdes"))))
			      (if new (sb-posix:dup2 fd new) (sb-posix:dup fd)))))
	(cons "dup2" (lambda (old new) (sb-posix:dup2 old new)))
	(cons "fileno" (lambda (p) (port-fdes p "fileno")))
	(cons "isatty?" (lambda (p) (bool (and (streamp p) (port-fd (->port p "isatty?"))
					       (= 1 (sb-unix:unix-isatty (port-fd (->port p "isatty?"))))))))
	(cons "fdopen" (lambda (fd mode) (fd-port fd mode)))
	(cons "primitive-move->fdes" (lambda (p fd)
				       (let ((port (->port p "primitive-move->fdes")))
					 (if (eql (port-fd port) fd)
					     qs:false
					     (progn (sb-posix:dup2 (port-fd port) fd)
						    (sb-posix:close (port-fd port))
						    (setf (port-fd port) fd)
						    (let ((b (fd-backend fd)))
						      (setf (port-read-fn port) (getf b :read)
							    (port-write-fn port) (getf b :write)
							    (port-seek-fn port) (getf b :seek)
							    (port-close-fn port) (getf b :close)))
						    qs:true)))))
	(cons "fdes->ports" (lambda (fd)
			      (loop for p being the hash-keys of *fd-ports*
				    when (and (port-open-p p) (eql (port-fd p) fd)) collect p)))))

;;; (rnrs io ports) and (ice-9 binary-ports): scm_init_r6rs_ports

(defun get-bytes (port n)
  "Up to N bytes from PORT, as a bytevector: fewer at end of file."
  (when (and (eq (port-buffering port) :none) (= (port-rpos port) (port-rend port)) (plusp n)
	     (port-read-fn port))
    ;; unbuffered: reads of what is still asked for, straight into the
    ;; result, until it is all there or at end of file, as Guile's
    (when (plusp (port-wend port)) (flush-output port))
    (let ((out (make-octets n)) (got 0))
      (loop while (< got n)
	    do (let ((k (funcall (port-read-fn port) out got (- n got))))
		 (when (zerop k) (return))
		 (incf got k)))
      (return-from get-bytes (if (= got n) out (subseq out 0 got)))))
  (let ((out (make-octets n)) (i 0))
    (loop while (and (< i n) (fill-input port))
	  do (let ((k (min (- n i) (- (port-rend port) (port-rpos port)))))
	       (replace out (port-rbuf port) :start1 i :start2 (port-rpos port) :end2 (+ (port-rpos port) k))
	       (incf (port-rpos port) k)
	       (incf i k)))
    (when (zerop i) (consume-eof port))
    (if (= i n) out (subseq out 0 i))))

(defun binary-in (p who) (->port p who))

(defun transcoder-encoding (transcoder)
  "The encoding an R6RS transcoder (Guile's: a record) names."
  (if (and transcoder (not (eq transcoder qs:false)) (struct-p transcoder))
      (let ((codec (struct-field transcoder 0)))
	(if (stringp codec) codec "UTF-8"))
      "ISO-8859-1"))

(defextension "scm_init_r6rs_ports"
  (list
   (cons "eof-object" (lambda () qs:eof-object))
   (cons "open-bytevector-input-port"
	 (lambda (bv &optional transcoder)
	   (memory-port (copy-seq bv) :input t :encoding (transcoder-encoding transcoder))))
   (cons "open-bytevector-output-port"
	 (lambda (&optional transcoder)
	   (let ((p (memory-port (make-octets 64) :output t :encoding (transcoder-encoding transcoder))))
	     (values p (lambda ()
			 (prog1 (memory-contents p)
			   (let ((m (port-data p)))
			     (setf (memory-length m) 0 (memory-position m) 0))))))))
   (cons "get-u8" (lambda (p) (let ((b (port-read-byte (binary-in p "get-u8")))) (if (eq b :eof) qs:eof-object b))))
   (cons "lookahead-u8" (lambda (p) (let ((b (port-peek-byte (binary-in p "lookahead-u8")))) (if (eq b :eof) qs:eof-object b))))
   (cons "get-bytevector-n" (lambda (p n)
			      (unless (and (integerp n) (>= n 0)) (wrong-type "get-bytevector-n" 2 n))
			      (let ((bv (get-bytes (binary-in p "get-bytevector-n") n)))
				(if (and (zerop (length bv)) (plusp n)) qs:eof-object bv))))
   (cons "get-bytevector-n!" (lambda (p bv start count)
			       (let ((got (get-bytes (binary-in p "get-bytevector-n!") count)))
				 (replace bv got :start1 start)
				 (if (and (zerop (length got)) (plusp count)) qs:eof-object (length got)))))
   (cons "get-bytevector-some" (lambda (p)
				 (let ((port (binary-in p "get-bytevector-some")))
				   (if (fill-input port)
				       (get-bytes port (- (port-rend port) (port-rpos port)))
				       (progn (consume-eof port) qs:eof-object)))))
   (cons "get-bytevector-some!" (lambda (p bv start count)
				  (let ((port (binary-in p "get-bytevector-some!")))
				    (cond ((zerop count) 0)
					  ((fill-input port)
					   (let ((got (get-bytes port (min count (- (port-rend port) (port-rpos port))))))
					     (replace bv got :start1 start)
					     (length got)))
					  (t (consume-eof port) qs:eof-object)))))
   (cons "get-string-n!" (lambda (p string start count)
			   (let ((port (->port p "get-string-n!")))
			     (let ((n (loop for i from 0 below count
					    for c = (port-read-char port)
					    until (eq c :eof)
					    do (setf (char string (+ start i)) c) (track-char port c)
					    finally (return i))))
			       (if (and (zerop n) (plusp count)) qs:eof-object n)))))
   (cons "put-u8" (lambda (p b)
		    (unless (typep b '(unsigned-byte 8)) (wrong-type "put-u8" 2 b))
		    (write-octets (->port p "put-u8") (make-array 1 :element-type '(unsigned-byte 8) :initial-element b))
		    *unspecified*))
   (cons "put-bytevector" (lambda (p bv &optional (start 0) (count (- (length bv) start)))
			    (write-octets (->port p "put-bytevector") bv start (+ start count))
			    *unspecified*))
   (cons "unget-bytevector" (lambda (p bv &optional (start 0) (count (- (length bv) start)))
			      (unread-octets (->port p "unget-bytevector") bv start (+ start count))
			      *unspecified*))
   (cons "%make-transcoded-port"
	 (lambda (port)
	   ;; Guile's makes a new port on the same bytes; this is one port
	   ;; whose encoding (rnrs io ports) then sets
	   (->port port "transcoded-port")))))

;;; (ice-9 custom-ports): scm_init_custom_ports.  The port's methods are
;;; the module's dispatchers (custom-port-read and so on) applied to its
;;; data, as libguile's custom ports call them.

(defvar *custom-ports-module* nil)

(defun custom-dispatcher (name)
  (let ((v (module-variable* *custom-ports-module* (ssym name))))
    (and v (gvariable-value v))))

(defun make-custom-port* (input output data encoding strategy &optional close-on-gc)
  (declare (ignore close-on-gc))
  (let ((port nil))
    (setq port
	  (make-port :kind "custom" :input (truthy input) :output (truthy output)
		     :encoding (cond ((stringp encoding) encoding)
				     ((and (symbolp encoding) (not (eq encoding qs:false)))
				      (qs:scheme-symbol-name encoding))
				     (t "UTF-8"))
		     :data data
		     :buffering :full
		     :read (lambda (buf start count)
			     (funcall (custom-dispatcher "custom-port-read") port buf start count))
		     :write (lambda (buf start count)
			      (funcall (custom-dispatcher "custom-port-write") port buf start count))
		     :seek (lambda (offset whence)
			     (funcall (custom-dispatcher "custom-port-seek") port data offset whence))
		     :close (lambda ()
			      (funcall (custom-dispatcher "custom-port-close") port data))
		     :truncate (lambda (length)
				 (funcall (custom-dispatcher "custom-port-truncate") port data length))))
    (when (and (symbolp strategy) (not (eq strategy qs:false))) (setf (port-strategy port) strategy))
    ;; the buffer sizes the port's get-natural-buffer-sizes asks for (a soft
    ;; port's write size is 1: each write reaches it at once)
    (let ((sizes (custom-dispatcher "custom-port-get-natural-buffer-sizes")))
      (when sizes
	(multiple-value-bind (read-size write-size)
	    (funcall sizes port data +buffer-size+ +buffer-size+)
	  (when (and (integerp read-size) (plusp read-size))
	    (setf (port-rbuf port) (make-octets read-size)))
	  (when (and (integerp write-size) (plusp write-size))
	    (setf (port-wbuf port) (make-octets write-size))))))
    port))

(defextension "scm_init_custom_ports"
  (setq *custom-ports-module* *current-module*)
  (list (cons "%make-custom-port" #'make-custom-port*)
	(cons "%custom-port-data" (lambda (p) (port-data (->port p "%custom-port-data"))))))

;;; (ice-9 rdelim)'s and (ice-9 rw)'s C halves

(defun read-delimited-into (delims buf gobble port start end)
  "Read characters into BUF from START until one in DELIMS (a string),
end of file, or END: (delimiter-or-eof . count)."
  (loop for i from start below end
	do (let ((c (port-read-char port)))
	     (cond ((eq c :eof) (return-from read-delimited-into (cons qs:eof-object (- i start))))
		   ((find c delims)
		    (if (truthy gobble) (track-char port c) (port-unread-char port c))
		    (return-from read-delimited-into (cons c (- i start))))
		   (t (track-char port c) (setf (char buf i) c)))))
  (cons qs:false (- end start)))

(defguile "%init-rdelim-builtins" ()
  (flet ((def (name value) (define-in-module *current-module* (ssym name) value)))
    (def "%read-delimited!"
	(lambda (delims buf gobble &optional port (start 0) (end (length buf)))
	  (read-delimited-into delims buf gobble (in-port port "%read-delimited!") start end)))
    (def "%read-line"
	(lambda (&optional port)
	 (block %read-line-block
	  (let ((p (in-port port "%read-line"))
		(out (make-string-output-stream)))
	    (let ((line (buffered-ascii-line p)))
	      (when line (return-from %read-line-block (cons line #\Newline))))
	    (loop (let ((c (port-read-char p)))
		    (cond ((eq c :eof)
			   (let ((s (get-output-stream-string out)))
			     (return (cons (if (zerop (length s)) qs:eof-object s) qs:eof-object))))
			  (t (track-char p c)
			     (if (char= c #\Newline)
				 (return (cons (get-output-stream-string out) c))
				 (write-char c out))))))))))
    (def "write-line"
	(lambda (obj &optional port)
	  (let ((p (out-port port "write-line")))
	    (guile-display obj p) (port-write-char p #\Newline))
	  *unspecified*)))
  *unspecified*)

(defguile "%init-rw-builtins" ()
  (flet ((def (name value) (define-in-module *current-module* (ssym name) value)))
    (def "read-string!/partial"
	(lambda (buf &optional port (start 0) (end (length buf)))
	  (let* ((p (in-port port "read-string!/partial"))
		 (n (loop for i from start below end
			  for c = (if (or (= i start) (< (port-rpos p) (port-rend p))) (port-read-char p) :eof)
			  until (eq c :eof)
			  do (setf (char buf i) c)
			  finally (return (- i start)))))
	    (if (and (zerop n) (< start end)) qs:false n))))
    (def "write-string/partial"
	(lambda (s &optional port (start 0) (end (length s)))
	  (port-write-string (out-port port "write-string/partial") s start end) (- end start))))
  *unspecified*)

;;; Source properties: with the positions read option, where the reader
;;; read each datum that can have properties (not fixnums, characters,
;;; symbols, keywords, booleans or ()), from a port that counts lines

(defun recordable-p (x)
  (or (consp x) (vectorp x) (garray-p x)
      (and (numberp x)
	   (not (and (integerp x) (<= (- -1 +guile-fixnum-max+) x +guile-fixnum-max+))))))

(defun record-source (port read)
  (let ((p (and (gport-p port) (option-value *read-options* "positions") port)))
    (if (null p)
	(if (and *load-text* (eq port (first *load-text*)))
	    ;; a file being loaded: where in its text
	    (progn
	      (skip-whitespace-and-comments port)
	      (multiple-value-bind (line column) (text-line-column (fourth *load-text*) (file-position port))
		(let ((x (funcall read port)))
		  (when (and (recordable-p x) (not (gethash x *source-properties*)))
		    (setf (gethash x *source-properties*)
			  (list (cons (ssym "filename") (third *load-text*))
				(cons (ssym "line") line) (cons (ssym "column") column))))
		  x)))
	    (funcall read port))
	(progn
	  (skip-whitespace-and-comments p)
	  (let* ((line (port-line* p)) (column (port-column* p))
		 (x (funcall read p)))
	    (when (or (consp x) (vectorp x) (garray-p x)
		      (and (numberp x)
			   (not (and (integerp x) (<= (- -1 +guile-fixnum-max+) x +guile-fixnum-max+)))))
	      (setf (gethash x *source-properties*)
		    (append (when (stringp (port-filename* p)) (list (cons (ssym "filename") (port-filename* p))))
			    (list (cons (ssym "line") line) (cons (ssym "column") column)))))
	    x)))))

(setq *source-recorder* 'record-source)
