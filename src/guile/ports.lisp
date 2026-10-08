; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-GUILE -*-

;;;; Guile's ports, written for Pseudoscheme: a port is a GPORT, a Gray
;;;; stream (trivial-gray-streams) that is binary and textual at once, as
;;;; Guile's are.  Bytes come from and go to a backend (a file descriptor,
;;;; a string or bytevector buffer, a custom port's procedures, or a Lisp
;;;; stream); characters are decoded and encoded in the port's encoding,
;;;; which can be changed at any time, with its conversion strategy for
;;;; what the encoding can't represent.  Unread characters and bytes go
;;;; back into the read buffer, as in Guile.
;;;;
;;;; The current ports are fluids whose values are the Lisp stream
;;;; variables (*standard-output* and so on), so that Pseudoscheme's own
;;;; procedures follow (current-output-port) and parameterize.  Guile code
;;;; runs with those variables bound to ports (CALL-WITH-GUILE-PORTS): a
;;;; Lisp stream there is wrapped in a port that writes to it.

(in-package "PSEUDOSCHEME-GUILE")

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(defun make-octets (n) (make-array n :element-type '(unsigned-byte 8)))

(defconstant +buffer-size+ 4096)

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
   (rbuf :initform (make-octets +buffer-size+) :accessor port-rbuf)
   (rpos :initform 0 :accessor port-rpos)
   (rend :initform 0 :accessor port-rend)
   (wbuf :initform (make-octets +buffer-size+) :accessor port-wbuf)
   (wend :initform 0 :accessor port-wend)
   (buffering :initarg :buffering :initform :full :accessor port-buffering) ; :none :line :full
   (encoding :initarg :encoding :initform "UTF-8" :accessor port-encoding-name)
   (codec :initform nil :accessor port-codec)
   (strategy :initarg :strategy :initform nil :accessor port-strategy)
   (bom-state :initform nil :accessor port-bom-state)	; for UTF-16 and UTF-32
   (line :initform 0 :accessor port-line*)
   (column :initform 0 :accessor port-column*)
   (filename :initarg :filename :initform ps:false :accessor port-filename*)
   (open :initform t :accessor port-open-p)
   (revealed :initform 0 :accessor port-revealed*)
   (properties :initform '() :accessor port-properties)))

(defun gport-p (x) (typep x 'gport))

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

(defun flush-all-ports ()
  (loop for p being the hash-keys of *open-output-ports*
	do (ignore-errors (flush-output p))))

(pushnew 'flush-all-ports sb-ext:*exit-hooks*)

;;; ------------------------------------------------------------------
;;; Encodings

(defun canonical-encoding (name)
  "Guile's name for the encoding NAME (a string or symbol), or NIL."
  (let ((name (string-upcase (if (symbolp name) (ps:scheme-symbol-name name) name))))
    (cond ((member name '("UTF-8" "UTF8") :test #'string=) "UTF-8")
	  ((member name '("ISO-8859-1" "ISO8859-1" "ISO_8859-1" "LATIN1" "LATIN-1" "L1") :test #'string=)
	   "ISO-8859-1")
	  ((member name '("ASCII" "US-ASCII" "ANSI_X3.4-1968" "646") :test #'string=) "US-ASCII")
	  ((member name '("UTF-16" "UTF16") :test #'string=) "UTF-16")
	  ((member name '("UTF-16LE" "UTF-16BE" "UTF-32" "UTF-32LE" "UTF-32BE") :test #'string=) name)
	  ((external-format-of name) name)
	  (t nil))))

(defun external-format-of (name)
  "An SBCL external format for the single-byte encoding NAME, or NIL."
  (let ((kw (intern (string-upcase name) "KEYWORD")))
    (and (ignore-errors (sb-ext:octets-to-string (make-octets 1) :external-format kw))
	 kw)))

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
    (if (symbolp s) (ps:scheme-symbol-name s) "substitute")))

(defun default-encoding ()
  (let ((v (ignore-errors (fluid-value (root-value "%default-port-encoding")))))
    (if (stringp v) (or (canonical-encoding v) "UTF-8") "UTF-8")))

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
	(let ((n (funcall (port-read-fn port) (port-rbuf port) 0 (length (port-rbuf port)))))
	  (setf (port-rpos port) 0 (port-rend port) n)
	  (plusp n)))))

(defun port-read-byte (port)
  (if (fill-input port)
      (prog1 (aref (port-rbuf port) (port-rpos port)) (incf (port-rpos port)))
      :eof))

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
  ;; writing discards buffered input, as in Guile (its position is the file's)
  (when (and (< (port-rpos port) (port-rend port)) (port-seek-fn port))
    (discard-input port))
  (setf (gethash port *open-output-ports*) t)
  (loop while (< start end)
	do (let* ((wbuf (port-wbuf port))
		  (n (min (- end start) (- (length wbuf) (port-wend port)))))
	     (replace wbuf octets :start1 (port-wend port) :start2 start :end2 (+ start n))
	     (incf (port-wend port) n)
	     (incf start n)
	     (when (= (port-wend port) (length wbuf)) (flush-output port))))
  (when (eq (port-buffering port) :none) (flush-output port)))

(defun flush-output (port)
  (let ((wend (port-wend port)))
    (when (plusp wend)
      (setf (port-wend port) 0)
      (let ((start 0))
	(loop while (< start wend)
	      do (let ((n (funcall (port-write-fn port) (port-wbuf port) start (- wend start))))
		   (when (<= n 0) (return))
		   (incf start n)))))))

(defun discard-input (port)
  "Forget buffered input, moving the backend back to where it was read to."
  (let ((pending (- (port-rend port) (port-rpos port))))
    (setf (port-rpos port) 0 (port-rend port) 0)
    (when (and (plusp pending) (port-seek-fn port))
      (funcall (port-seek-fn port) (- pending) 1))))

;;; ------------------------------------------------------------------
;;; Characters

(defun decoding-error (port)
  (guile-error (ssym "decoding-error") "scm_getc" "input decoding error" '() (list port)))

(defun bad-input (port)
  "A byte sequence that isn't a character: U+FFFD, or an error."
  (if (string= (strategy port) "error") (decoding-error port) (code-char #xfffd)))

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

(defun port-read-char (port)
  "The next character from PORT, or :EOF."
  (let ((codec (codec port)))
    (case codec
      (:utf-16 (read-wide port 2 (eq (bom-endianness port 2) :big)))
      (:utf-32 (read-wide port 4 (eq (bom-endianness port 4) :big)))
      (:utf-16be (read-wide port 2 t))
      (:utf-16le (read-wide port 2 nil))
      (:utf-32be (read-wide port 4 t))
      (:utf-32le (read-wide port 4 nil))
      (t (let ((b (port-read-byte port)))
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
						  ((< code #x10000)
						   (format nil "\\u~(~4,'0X~)" code))
						  (t (format nil "\\U~(~6,'0X~)" code)))
					    :external-format :latin-1)))
		(t (guile-error (ssym "encoding-error") "scm_putc"
				"conversion to port encoding failed" '() (list port char))))))))

(defun track-char (port char)
  (case char
    (#\Newline (incf (port-line* port)) (setf (port-column* port) 0))
    (#\Tab (setf (port-column* port) (* 8 (1+ (floor (port-column* port) 8)))))
    (#\Backspace (when (plusp (port-column* port)) (decf (port-column* port))))
    (#\Return (setf (port-column* port) 0))
    (t (incf (port-column* port)))))

(defun port-write-char (port char)
  (write-octets port (encode-char port char))
  (track-char port char)
  (when (and (eq (port-buffering port) :line) (char= char #\Newline))
    (flush-output port)))

(defun port-write-string (port string start end)
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

;;; The Gray stream protocol: what Pseudoscheme's and Lisp's own
;;; procedures use on a port

(defmethod trivial-gray-streams:stream-read-char ((p gport))
  (let ((c (port-read-char p)))
    (unless (eq c :eof) (track-char p c))
    c))

(defmethod trivial-gray-streams:stream-unread-char ((p gport) c)
  (port-unread-char p c)
  nil)

(defmethod trivial-gray-streams:stream-peek-char ((p gport))
  (let ((c (port-read-char p)))
    (unless (eq c :eof)
      (unread-octets p (if (encodable-p c (codec p)) (char-octets c (codec p)) (char-octets c :utf-8))))
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
    (setf (port-open-p p) nil)
    (when (port-close-fn p) (funcall (port-close-fn p))))
  t)

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
	(let ((pending (- (port-rend port) (port-rpos port))))
	  (setf (port-rpos port) 0 (port-rend port) 0)
	  (prog1 (funcall (port-seek-fn port) (if (= whence 1) (- offset pending) offset) whence)
	    (setf (port-bom-state port) nil))))))

;;; ------------------------------------------------------------------
;;; Backends

(defun make-port (&rest initargs &key encoding &allow-other-keys)
  (let ((p (apply #'make-instance 'gport :allow-other-keys t initargs)))
    (setf (port-encoding-name p) (or (and encoding (canonical-encoding encoding)) (default-encoding)))
    p))

;;; Bytes in memory: string and bytevector ports

(defstruct (memory (:constructor make-memory (octets &optional (length (length octets)))))
  octets length (position 0))

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
  (memory-port (sb-ext:string-to-octets string :external-format :utf-8) :input t))

(defun memory-port (octets &key input output (encoding "UTF-8"))
  "A port on OCTETS; an output-only one starts empty, OCTETS its space."
  (let ((m (make-memory octets (if input (length octets) 0))))
    (apply #'make-port :kind "file" :input input :output output :encoding encoding :data m
	   (memory-backend m))))

(defun open-output-string* ()
  (memory-port (make-octets 64) :output t))

(defun port-output-string (port)
  (unless (and (gport-p port) (memory-p (port-data port)))
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
				  :filename (or filename ps:false)
				  :encoding (cond ((find #\b mode) "ISO-8859-1") (encoding))
				  :buffering (if (find #\0 mode) :none (if (find #\l mode) :line :full))
				  (fd-backend fd))))
    (when (and (find #\0 mode) (find #\b mode))
      (setf (port-buffering port) :none))
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
	  ps:false))))

(defguile "file-encoding" (port) (port-coding port))
(defguile "%file-encoding" (port) (port-coding port))

(defun open-file (filename mode &key (encoding ps:false) (guess-encoding ps:false) (buffering ps:false))
  (declare (ignore buffering))
  (unless (stringp filename) (wrong-type "open-file" 1 filename))
  (let* ((mode (if (symbolp mode) (ps:scheme-symbol-name mode) mode))
	 (base (find-if (lambda (c) (find c "rwa")) mode))
	 (plus (find #\+ mode)))
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
		 :filename filename
		 :encoding (cond ((stringp encoding) encoding)
				 ((and (truthy guess-encoding) (find #\r mode)) (file-coding filename))))))))

;;; Lisp streams as ports

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
  (let ((f (make-fluid* ps:false)))
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
    (if (eq c :eof) ps:eof-object (progn (track-char p c) c))))

(defun peek-char* (&optional port)
  (let ((c (trivial-gray-streams:stream-peek-char (in-port port "peek-char"))))
    (if (eq c :eof) ps:eof-object c)))

(defun unread-string* (string port)
  (let ((p (in-port port "unread-string")))
    (loop for i from (1- (length string)) downto 0 do (port-unread-char p (char string i))))
  *unspecified*)

(defun install-port-root-bindings ()
  (flet ((def (name value) (obarray-define (ssym name) value)))
    (def "%current-input-port-fluid" *current-input-port-fluid*)
    (def "%current-output-port-fluid" *current-output-port-fluid*)
    (def "%current-error-port-fluid" *current-error-port-fluid*)
    (def "%current-warning-port-fluid" *current-warning-port-fluid*)
    (def "current-error-port" (lambda () *error-output*))
    (def "current-warning-port" (lambda () *error-output*))
    (def "current-load-port" (lambda () ps:false))
    (def "open-file" #'open-file)
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
    (if e (cdr e) ps:false)))

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
   (cons "%port-encoding" (lambda (p) (ssym (port-encoding-name (->port p "port-encoding")))))
   (cons "port-conversion-strategy"
	 (lambda (p)
	   (if (eq p ps:false)
	       (fluid-value (root-value "%default-port-conversion-strategy"))
	       (ssym (strategy (->port p "port-conversion-strategy"))))))
   (cons "set-port-conversion-strategy!"
	 (lambda (p s)
	   (unless (and (symbolp s) (member (ps:scheme-symbol-name s) '("error" "substitute" "escape")
					    :test #'string=))
	     (wrong-type "set-port-conversion-strategy!" 2 s))
	   (if (eq p ps:false)
	       (setf (fluid-value (root-value "%default-port-conversion-strategy")) s)
	       (setf (port-strategy (->port p "set-port-conversion-strategy!")) s))
	   *unspecified*))
   (cons "read-char" #'read-char*)
   (cons "peek-char" #'peek-char*)
   (cons "unread-char" (lambda (c &optional port) (port-unread-char (in-port port "unread-char") c) c))
   (cons "unread-string" #'unread-string*)
   (cons "setvbuf" (lambda (port mode &optional size)
		     (declare (ignore size))
		     (let ((p (->port port "setvbuf")))
		       (flush-output p)
		       (setf (port-buffering p)
			     (let ((m (if (symbolp mode) (ps:scheme-symbol-name mode) mode)))
			       (cond ((member m '("none" 0) :test #'equal) :none)
				     ((member m '("line" 1) :test #'equal) :line)
				     (t :full)))))
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
   (cons "seek" (lambda (port offset whence) (port-seek (->port port "seek") offset whence)))
   (cons "SEEK_SET" 0) (cons "SEEK_CUR" 1) (cons "SEEK_END" 2)
   (cons "truncate-file" (lambda (obj &optional length)
			   (if (stringp obj)
			       (sb-posix:truncate obj length)
			       (let ((p (->port obj "truncate-file")))
				 (flush-output p)
				 (funcall (or (port-truncate-fn p) (wrong-type "truncate-file" 1 obj))
					  (or length (port-position p)))))
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
   ;; (ice-9 ports internal): ports' buffers, for (ice-9 suspendable-ports)
   (mapcar (lambda (name)
	     (cons name (lambda (&rest args)
			  (declare (ignore args))
			  (error "~A: port buffers aren't available here" name))))
	   '("port-read-buffer" "port-write-buffer" "port-auxiliary-write-buffer"
	     "port-line-buffered?" "expand-port-read-buffer!" "port-read" "port-write"
	     "port-clear-stream-start-for-bom-read" "port-clear-stream-start-for-bom-write"
	     "specialize-port-encoding!" "port-decode-char" "port-encode-char"
	     "port-encode-chars" "port-random-access?" "port-read-buffering" "port-poll"
	     "port-read-wait-fd" "port-write-wait-fd"))))

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
					     ps:false
					     (progn (sb-posix:dup2 (port-fd port) fd)
						    (sb-posix:close (port-fd port))
						    (setf (port-fd port) fd)
						    (let ((b (fd-backend fd)))
						      (setf (port-read-fn port) (getf b :read)
							    (port-write-fn port) (getf b :write)
							    (port-seek-fn port) (getf b :seek)
							    (port-close-fn port) (getf b :close)))
						    ps:true)))))
	(cons "fdes->ports" (lambda (fd)
			      (loop for p being the hash-keys of *open-output-ports*
				    when (eql (port-fd p) fd) collect p)))))

;;; (rnrs io ports) and (ice-9 binary-ports): scm_init_r6rs_ports

(defun get-bytes (port n)
  "Up to N bytes from PORT, as a bytevector: fewer at end of file."
  (let ((out (make-octets n)) (i 0))
    (loop while (and (< i n) (fill-input port))
	  do (let ((k (min (- n i) (- (port-rend port) (port-rpos port)))))
	       (replace out (port-rbuf port) :start1 i :start2 (port-rpos port) :end2 (+ (port-rpos port) k))
	       (incf (port-rpos port) k)
	       (incf i k)))
    (if (= i n) out (subseq out 0 i))))

(defun binary-in (p who) (->port p who))

(defun transcoder-encoding (transcoder)
  "The encoding an R6RS transcoder (Guile's: a record) names."
  (if (and transcoder (not (eq transcoder ps:false)) (struct-p transcoder))
      (let ((codec (svref (struct-slots-of transcoder) 0)))
	(if (stringp codec) codec "UTF-8"))
      "ISO-8859-1"))

(defextension "scm_init_r6rs_ports"
  (list
   (cons "eof-object" (lambda () ps:eof-object))
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
   (cons "get-u8" (lambda (p) (let ((b (port-read-byte (binary-in p "get-u8")))) (if (eq b :eof) ps:eof-object b))))
   (cons "lookahead-u8" (lambda (p) (let ((b (port-peek-byte (binary-in p "lookahead-u8")))) (if (eq b :eof) ps:eof-object b))))
   (cons "get-bytevector-n" (lambda (p n)
			      (unless (and (integerp n) (>= n 0)) (wrong-type "get-bytevector-n" 2 n))
			      (let ((bv (get-bytes (binary-in p "get-bytevector-n") n)))
				(if (and (zerop (length bv)) (plusp n)) ps:eof-object bv))))
   (cons "get-bytevector-n!" (lambda (p bv start count)
			       (let ((got (get-bytes (binary-in p "get-bytevector-n!") count)))
				 (replace bv got :start1 start)
				 (if (and (zerop (length got)) (plusp count)) ps:eof-object (length got)))))
   (cons "get-bytevector-some" (lambda (p)
				 (let ((port (binary-in p "get-bytevector-some")))
				   (if (fill-input port)
				       (get-bytes port (- (port-rend port) (port-rpos port)))
				       ps:eof-object))))
   (cons "get-bytevector-some!" (lambda (p bv start count)
				  (let ((port (binary-in p "get-bytevector-some!")))
				    (cond ((zerop count) 0)
					  ((fill-input port)
					   (let ((got (get-bytes port (min count (- (port-rend port) (port-rpos port))))))
					     (replace bv got :start1 start)
					     (length got)))
					  (t ps:eof-object)))))
   (cons "get-string-n!" (lambda (p string start count)
			   (let ((port (->port p "get-string-n!")))
			     (let ((n (loop for i from 0 below count
					    for c = (port-read-char port)
					    until (eq c :eof)
					    do (setf (char string (+ start i)) c) (track-char port c)
					    finally (return i))))
			       (if (and (zerop n) (plusp count)) ps:eof-object n)))))
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
		     :encoding (if (stringp encoding) encoding "UTF-8")
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
    (when (and (symbolp strategy) (not (eq strategy ps:false))) (setf (port-strategy port) strategy))
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
	     (cond ((eq c :eof) (return-from read-delimited-into (cons ps:eof-object (- i start))))
		   ((find c delims)
		    (if (truthy gobble) (track-char port c) (port-unread-char port c))
		    (return-from read-delimited-into (cons c (- i start))))
		   (t (track-char port c) (setf (char buf i) c)))))
  (cons ps:false (- end start)))

(defguile "%init-rdelim-builtins" ()
  (flet ((def (name value) (define-in-module *current-module* (ssym name) value)))
    (def "%read-delimited!"
	(lambda (delims buf gobble &optional port (start 0) (end (length buf)))
	  (read-delimited-into delims buf gobble (in-port port "%read-delimited!") start end)))
    (def "%read-line"
	(lambda (&optional port)
	  (let ((p (in-port port "%read-line"))
		(out (make-string-output-stream)))
	    (loop (let ((c (port-read-char p)))
		    (cond ((eq c :eof)
			   (let ((s (get-output-stream-string out)))
			     (return (cons (if (zerop (length s)) ps:eof-object s) ps:eof-object))))
			  (t (track-char p c)
			     (if (char= c #\Newline)
				 (return (cons (get-output-stream-string out) c))
				 (write-char c out)))))))))
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
	    (if (and (zerop n) (< start end)) ps:false n))))
    (def "write-string/partial"
	(lambda (s &optional port (start 0) (end (length s)))
	  (port-write-string (out-port port "write-string/partial") s start end) (- end start))))
  *unspecified*)

;;; Source properties: with the positions read option, where the reader
;;; read each datum that can have properties (not fixnums, characters,
;;; symbols, keywords, booleans or ()), from a port that counts lines

(defun record-source (port read)
  (let ((p (and (gport-p port) (option-value *read-options* "positions") port)))
    (if (null p)
	(funcall read port)
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
