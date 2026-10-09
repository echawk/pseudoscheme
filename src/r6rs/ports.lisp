; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-R6RS -*-

;;;; (rnrs io ports (6)) and (rnrs io simple (6)), library report
;;;; chapter 8.  Ports are CL streams.
;;;;
;;;; Textual file ports are CL character streams (UTF-8).  Everything
;;;; else -- binary ports (which need lookahead-u8), bytevector ports,
;;;; custom ports, and transcoded ports -- is a Gray stream class below.
;;;; Gray streams come from trivial-gray-streams.

(in-package "PSEUDOSCHEME-R6RS")

(defun io-error-filename (who name kind)
  (ps-r7rs:raise-object
   (funcall (prim "condition")
	    (funcall (prim kind) name)
	    (funcall (prim "make-who-condition") who)
	    (funcall (prim "make-message-condition") "cannot open file")
	    (funcall (prim "make-irritants-condition") (list name)))
   nil))

;;; ------------------------------------------------------------------
;;; Transcoders (8.2.4)

(defstruct (codec (:constructor make-codec (name))) name)
(defstruct (transcoder (:constructor %make-transcoder (codec eol-style handling)))
  codec eol-style handling)

(defmethod print-object ((c codec) s) (print-unreadable-object (c s) (format s "Codec ~A" (codec-name c))))
(defmethod print-object ((c transcoder) s)
  (print-unreadable-object (c s) (format s "Transcoder ~A" (codec-name (transcoder-codec c)))))

(defvar *utf-8-codec* (make-codec "utf-8"))
(defvar *latin-1-codec* (make-codec "latin-1"))
(defvar *utf-16-codec* (make-codec "utf-16"))

(defprim "utf-8-codec" () *utf-8-codec*)
(defprim "latin-1-codec" () *latin-1-codec*)
(defprim "utf-16-codec" () *utf-16-codec*)
(defprim "native-eol-style" () (ps:intern-scheme-symbol "lf"))
(defprim "make-transcoder" (codec &optional eol handling)
  (%make-transcoder codec
		    (or eol (ps:intern-scheme-symbol "lf"))
		    (or handling (ps:intern-scheme-symbol "replace"))))
(defprim "native-transcoder" ()
  (%make-transcoder *utf-8-codec* (ps:intern-scheme-symbol "lf") (ps:intern-scheme-symbol "replace")))
(defprim "transcoder-codec" (tc) (transcoder-codec tc))
(defprim "transcoder-eol-style" (tc) (transcoder-eol-style tc))
(defprim "transcoder-error-handling-mode" (tc) (transcoder-handling tc))

(defun symbol-is (symbol name)
  (and (symbolp symbol) (string= (ps:scheme-symbol-name symbol) name)))

;;; ------------------------------------------------------------------
;;; Gray stream classes

(defclass scheme-port ()
  ((transcoder :initform nil :initarg :transcoder :accessor port-transcoder*)))

(defclass binary-input-port (scheme-port trivial-gray-streams:fundamental-binary-input-stream)
  ((peeked :initform nil :accessor peeked)))

(defclass bytevector-input-port (binary-input-port)
  ((bytes :initarg :bytes :reader bytes)
   (index :initform 0 :accessor index)))

(defmethod stream-element-type ((s binary-input-port)) '(unsigned-byte 8))

(defgeneric next-byte (stream)
  (:documentation "The next byte, or :EOF, without lookahead handling."))

(defmethod next-byte ((s bytevector-input-port))
  (if (< (index s) (length (bytes s)))
      (prog1 (aref (bytes s) (index s)) (incf (index s)))
      :eof))

(defmethod trivial-gray-streams:stream-read-byte ((s binary-input-port))
  (let ((p (peeked s)))
    (if p (progn (setf (peeked s) nil) p) (next-byte s))))

(defun peek-byte (s)
  (or (peeked s) (setf (peeked s) (next-byte s))))

(defmethod trivial-gray-streams:stream-file-position ((s bytevector-input-port))
  (- (index s) (if (peeked s) (if (eq (peeked s) :eof) 0 1) 0)))
(defmethod (setf trivial-gray-streams:stream-file-position) (position (s bytevector-input-port))
  (setf (index s) position (peeked s) nil)
  t)

;; A binary port over a CL octet stream (files): adds the lookahead.
(defclass octet-stream-input-port (binary-input-port)
  ((stream :initarg :stream :reader underlying)))

(defmethod next-byte ((s octet-stream-input-port))
  (read-byte (underlying s) nil :eof))

(defmethod trivial-gray-streams:stream-file-position ((s octet-stream-input-port))
  (- (file-position (underlying s)) (if (and (peeked s) (not (eq (peeked s) :eof))) 1 0)))
(defmethod (setf trivial-gray-streams:stream-file-position) (position (s octet-stream-input-port))
  (setf (peeked s) nil)
  (file-position (underlying s) position))

(defmethod close ((s octet-stream-input-port) &key abort)
  (close (underlying s) :abort abort) (call-next-method))

(defclass binary-output-port (scheme-port trivial-gray-streams:fundamental-binary-output-stream) ())
(defmethod stream-element-type ((s binary-output-port)) '(unsigned-byte 8))

(defclass bytevector-output-port (binary-output-port)
  ((buffer :initform (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer t)
	   :accessor buffer)))

(defmethod trivial-gray-streams:stream-write-byte ((s bytevector-output-port) byte)
  (vector-push-extend byte (buffer s)) byte)

(defun take-bytes (s)
  (prog1 (coerce (buffer s) '(simple-array (unsigned-byte 8) (*))) (setf (fill-pointer (buffer s)) 0)))

;;; Custom ports (8.2.10)

(defclass custom-binary-input-port (binary-input-port)
  ((id :initarg :id) (read! :initarg :read!) (get-position :initarg :get-position)
   (set-position! :initarg :set-position!) (closer :initarg :closer)))

(defmethod next-byte ((s custom-binary-input-port))
  (let* ((bv (make-array 1 :element-type '(unsigned-byte 8)))
	 (n (funcall (slot-value s 'read!) bv 0 1)))
    (if (zerop n) :eof (aref bv 0))))

(defclass custom-binary-output-port (binary-output-port)
  ((id :initarg :id) (write! :initarg :write!) (get-position :initarg :get-position)
   (set-position! :initarg :set-position!) (closer :initarg :closer)))

(defmethod trivial-gray-streams:stream-write-byte ((s custom-binary-output-port) byte)
  (let ((bv (make-array 1 :element-type '(unsigned-byte 8) :initial-element byte)))
    (funcall (slot-value s 'write!) bv 0 1)
    byte))

(defclass custom-textual-input-port (scheme-port trivial-gray-streams:fundamental-character-input-stream)
  ((id :initarg :id) (read! :initarg :read!) (get-position :initarg :get-position)
   (set-position! :initarg :set-position!) (closer :initarg :closer)
   (unread :initform nil :accessor unread)))

(defmethod trivial-gray-streams:stream-read-char ((s custom-textual-input-port))
  (let ((u (unread s)))
    (if u
	(progn (setf (unread s) nil) u)
	(let* ((str (make-string 1)) (n (funcall (slot-value s 'read!) str 0 1)))
	  (if (zerop n) :eof (char str 0))))))
(defmethod trivial-gray-streams:stream-unread-char ((s custom-textual-input-port) c) (setf (unread s) c) nil)

(defclass custom-textual-output-port (scheme-port trivial-gray-streams:fundamental-character-output-stream)
  ((id :initarg :id) (write! :initarg :write!) (get-position :initarg :get-position)
   (set-position! :initarg :set-position!) (closer :initarg :closer)
   (column :initform 0 :accessor column)))

(defmethod trivial-gray-streams:stream-write-char ((s custom-textual-output-port) c)
  (funcall (slot-value s 'write!) (string c) 0 1)
  (setf (column s) (if (char= c #\Newline) 0 (1+ (column s))))
  c)
(defmethod trivial-gray-streams:stream-line-column ((s custom-textual-output-port)) (column s))

(defmethod close ((s scheme-port) &key abort)
  (declare (ignore abort))
  (when (and (slot-exists-p s 'closer) (slot-boundp s 'closer)
	     (functionp (slot-value s 'closer)))
    (funcall (slot-value s 'closer)))
  (call-next-method))

(defun proc-or-nil (x) (if (functionp x) x nil))

(defprim "make-custom-binary-input-port" (id read! get-position set-position! close)
  (make-instance 'custom-binary-input-port :id id :read! read! :get-position (proc-or-nil get-position)
		 :set-position! (proc-or-nil set-position!) :closer (proc-or-nil close)))
(defprim "make-custom-binary-output-port" (id write! get-position set-position! close)
  (make-instance 'custom-binary-output-port :id id :write! write! :get-position (proc-or-nil get-position)
		 :set-position! (proc-or-nil set-position!) :closer (proc-or-nil close)))
(defprim "make-custom-textual-input-port" (id read! get-position set-position! close)
  (make-instance 'custom-textual-input-port :id id :read! read! :get-position (proc-or-nil get-position)
		 :set-position! (proc-or-nil set-position!) :closer (proc-or-nil close)))
(defprim "make-custom-textual-output-port" (id write! get-position set-position! close)
  (make-instance 'custom-textual-output-port :id id :write! write! :get-position (proc-or-nil get-position)
		 :set-position! (proc-or-nil set-position!) :closer (proc-or-nil close)))

;;; Custom input/output ports (8.2.13): both directions over the
;;; same procedures.

(defclass custom-binary-io-port (custom-binary-input-port trivial-gray-streams:fundamental-binary-output-stream)
  ((write! :initarg :write!)))

(defmethod trivial-gray-streams:stream-write-byte ((s custom-binary-io-port) byte)
  (let ((bv (make-array 1 :element-type '(unsigned-byte 8) :initial-element byte)))
    (funcall (slot-value s 'write!) bv 0 1)
    byte))

(defclass custom-textual-io-port (custom-textual-input-port trivial-gray-streams:fundamental-character-output-stream)
  ((write! :initarg :write!)
   (column :initform 0 :accessor column)))

(defmethod trivial-gray-streams:stream-write-char ((s custom-textual-io-port) c)
  (funcall (slot-value s 'write!) (string c) 0 1)
  (setf (column s) (if (char= c #\Newline) 0 (1+ (column s))))
  c)
(defmethod trivial-gray-streams:stream-line-column ((s custom-textual-io-port)) (column s))

(defprim "make-custom-binary-input/output-port" (id read! write! get-position set-position! close)
  (make-instance 'custom-binary-io-port :id id :read! read! :write! write!
		 :get-position (proc-or-nil get-position)
		 :set-position! (proc-or-nil set-position!) :closer (proc-or-nil close)))
(defprim "make-custom-textual-input/output-port" (id read! write! get-position set-position! close)
  (make-instance 'custom-textual-io-port :id id :read! read! :write! write!
		 :get-position (proc-or-nil get-position)
		 :set-position! (proc-or-nil set-position!) :closer (proc-or-nil close)))

;;; Transcoded ports (8.2.6): a textual view of a binary port.  The
;;; transcoder's codec, eol style and error-handling mode all apply
;;; here, and bytevector->string and string->bytevector are these
;;; ports over bytevector ports.

(defclass transcoded-port (scheme-port)
  ((binary :initarg :binary :reader binary)))

(defmethod close ((s transcoded-port) &key abort)
  (close (binary s) :abort abort)
  (call-next-method))

(defmethod trivial-gray-streams:stream-file-position ((s transcoded-port))
  (file-position (binary s)))

(defclass transcoded-input-port (transcoded-port trivial-gray-streams:fundamental-character-input-stream)
  ((unread :initform nil :accessor unread)	; given back by unread-char
   (pending :initform nil :accessor pending)	; decoded past a CR, or :eof
   (endian :initform nil :accessor endian)))	; UTF-16's, once the BOM is read

(defmethod (setf trivial-gray-streams:stream-file-position) (position (s transcoded-port))
  (when (typep s 'transcoded-input-port)
    (setf (unread s) nil (pending s) nil))
  (file-position (binary s) position))

(defun raise-coding-error (port constructor &rest args)
  (ps-r7rs:raise-object
   (funcall (prim "condition")
	    (apply (prim constructor) port args)
	    (funcall (prim "make-message-condition")
		     (if (string= constructor "make-i/o-decoding-error")
			 "cannot decode"
			 "cannot encode")))
   nil))

(defun decoding-error (s)
  ;; An undecodable byte sequence: by the error-handling mode, a
  ;; replacement character, nothing (:skip), or &i/o-decoding.
  (let ((mode (transcoder-handling (port-transcoder* s))))
    (cond ((symbol-is mode "ignore") :skip)
	  ((symbol-is mode "raise") (raise-coding-error s "make-i/o-decoding-error"))
	  (t (code-char #xFFFD)))))

(defun take-byte-if (bin predicate)
  ;; The next byte, consumed, if it satisfies PREDICATE.  Otherwise NIL,
  ;; and the byte stays unread where the binary port has lookahead.
  (let ((b (if (typep bin 'binary-input-port) (peek-byte bin) (read-byte bin nil :eof))))
    (and (integerp b) (funcall predicate b)
	 (progn (when (typep bin 'binary-input-port) (read-byte bin)) b))))

(defun decode-utf-8 (s bin)
  (let ((b (read-byte bin nil :eof)))
    (cond ((eq b :eof) :eof)
	  ((< b #x80) (code-char b))
	  (t (multiple-value-bind (n least)
		 (cond ((<= #xC2 b #xDF) (values 1 #x80))
		       ((<= #xE0 b #xEF) (values 2 #x800))
		       ((<= #xF0 b #xF4) (values 3 #x10000))
		       (t (values 0 nil)))
	       (if (null least)
		   (decoding-error s)
		   (let ((code (logand b (ash #x3F (- n)))))
		     (dotimes (i n)
		       (let ((c (take-byte-if bin (lambda (c) (= (logand c #xC0) #x80)))))
			 (unless c (return-from decode-utf-8 (decoding-error s)))
			 (setq code (logior (ash code 6) (logand c #x3F)))))
		     (if (or (< code least) (<= #xD800 code #xDFFF) (> code #x10FFFF))
			 (decoding-error s)
			 (code-char code)))))))))

(defun decode-utf-16 (s bin)
  ;; Big-endian, unless a byte-order mark at the start says otherwise.
  (flet ((unit ()
	   (let ((b1 (read-byte bin nil :eof)))
	     (if (eq b1 :eof)
		 :eof
		 (let ((b2 (read-byte bin nil :eof)))
		   (cond ((eq b2 :eof) :odd)
			 ((eq (endian s) :little) (logior b1 (ash b2 8)))
			 (t (logior (ash b1 8) b2))))))))
    (let ((u (unit)))
      (unless (endian s)
	(setf (endian s) :big)
	(case u
	  (#xFEFF (setq u (unit)))
	  (#xFFFE (setf (endian s) :little) (setq u (unit)))))
      (cond ((eq u :eof) :eof)
	    ((eq u :odd) (decoding-error s))
	    ((<= #xD800 u #xDBFF)
	     (let ((low (unit)))
	       (if (and (integerp low) (<= #xDC00 low #xDFFF))
		   (code-char (+ #x10000 (ash (- u #xD800) 10) (- low #xDC00)))
		   (decoding-error s))))
	    ((<= #xDC00 u #xDFFF) (decoding-error s))
	    (t (code-char u))))))

(defun decode-char (s)
  (let ((bin (binary s))
	(codec (transcoder-codec (port-transcoder* s))))
    (loop
      (let ((c (cond ((pending s) (shiftf (pending s) nil))
		     ((eq codec *latin-1-codec*)
		      (let ((b (read-byte bin nil :eof))) (if (eq b :eof) :eof (code-char b))))
		     ((eq codec *utf-16-codec*) (decode-utf-16 s bin))
		     (t (decode-utf-8 s bin)))))
	(unless (eq c :skip) (return c))))))

(defmethod trivial-gray-streams:stream-read-char ((s transcoded-input-port))
  (if (unread s)
      (shiftf (unread s) nil)
      (let ((c (decode-char s)))
	;; Any eol style but none reads every line ending as a linefeed.
	(cond ((symbol-is (transcoder-eol-style (port-transcoder* s)) "none") c)
	      ((eql c #\Return)
	       (let ((next (decode-char s)))
		 (unless (member next '(#\Newline #.(code-char #x85)))
		   (setf (pending s) next))
		 #\Newline))
	      ((member c '(#.(code-char #x85) #.(code-char #x2028))) #\Newline)
	      (t c)))))

(defmethod trivial-gray-streams:stream-unread-char ((s transcoded-input-port) c) (setf (unread s) c) nil)

(defclass transcoding-output ()
  ((column :initform 0 :accessor column)))

(defclass transcoded-output-port (transcoded-port transcoding-output
				  trivial-gray-streams:fundamental-character-output-stream)
  ())

(defclass transcoded-io-port (transcoded-input-port transcoding-output
			      trivial-gray-streams:fundamental-character-output-stream)
  ())

(defun encode-char (c codec emit)
  ;; Calls EMIT on each byte of C in CODEC; NIL if CODEC can't encode C.
  (let ((code (char-code c)))
    (cond ((eq codec *latin-1-codec*)
	   (and (< code 256) (progn (funcall emit code) t)))
	  ((eq codec *utf-16-codec*)
	   (flet ((unit (u) (funcall emit (ash u -8)) (funcall emit (logand u #xFF))))
	     (if (< code #x10000)
		 (unit code)
		 (let ((v (- code #x10000)))
		   (unit (+ #xD800 (ash v -10)))
		   (unit (+ #xDC00 (logand v #x3FF))))))
	   t)
	  (t
	   (flet ((more (shift) (funcall emit (logior #x80 (logand (ash code (- shift)) #x3F)))))
	     (cond ((< code #x80) (funcall emit code))
		   ((< code #x800) (funcall emit (logior #xC0 (ash code -6))) (more 0))
		   ((< code #x10000) (funcall emit (logior #xE0 (ash code -12))) (more 6) (more 0))
		   (t (funcall emit (logior #xF0 (ash code -18))) (more 12) (more 6) (more 0))))
	   t))))

(defun eol-chars (style)
  (cond ((symbol-is style "cr") '(#\Return))
	((symbol-is style "crlf") '(#\Return #\Newline))
	((symbol-is style "nel") '(#.(code-char #x85)))
	((symbol-is style "crnel") '(#\Return #.(code-char #x85)))
	((symbol-is style "ls") '(#.(code-char #x2028)))
	(t '(#\Newline))))

(defun transcode-char (s c)
  (let* ((tc (port-transcoder* s))
	 (codec (transcoder-codec tc))
	 (bin (binary s))
	 (emit (lambda (b) (write-byte b bin))))
    (unless (encode-char c codec emit)
      ;; Only latin-1 can fail; its replacement character is ?.
      (let ((mode (transcoder-handling tc)))
	(cond ((symbol-is mode "ignore"))
	      ((symbol-is mode "raise") (raise-coding-error s "make-i/o-encoding-error" c))
	      (t (encode-char #\? codec emit)))))))

(defmethod trivial-gray-streams:stream-write-char ((s transcoding-output) c)
  (if (char= c #\Newline)
      (dolist (e (eol-chars (transcoder-eol-style (port-transcoder* s))))
	(transcode-char s e))
      (transcode-char s c))
  (setf (column s) (if (char= c #\Newline) 0 (1+ (column s))))
  c)
(defmethod trivial-gray-streams:stream-line-column ((s transcoding-output)) (column s))
(defmethod trivial-gray-streams:stream-force-output ((s transcoding-output)) (force-output (binary s)))
(defmethod trivial-gray-streams:stream-finish-output ((s transcoding-output)) (finish-output (binary s)))

(defprim "transcoded-port" (binary tc)
  (make-instance (cond ((and (input-stream-p binary) (output-stream-p binary)) 'transcoded-io-port)
		       ((input-stream-p binary) 'transcoded-input-port)
		       (t 'transcoded-output-port))
		 :binary binary :transcoder tc))

(defprim "bytevector->string" (bv tc)
  (let ((in (make-instance 'transcoded-input-port :transcoder tc
			   :binary (make-instance 'bytevector-input-port
						  :bytes (check-bv "bytevector->string" bv))))
	(out (make-string-output-stream)))
    (loop for c = (read-char in nil :eof)
	  until (eq c :eof)
	  do (write-char c out))
    (coerce (get-output-stream-string out) 'simple-string)))

(defprim "string->bytevector" (string tc)
  (let* ((bin (make-instance 'bytevector-output-port))
	 (out (make-instance 'transcoded-output-port :binary bin :transcoder tc)))
    (write-string string out)
    (take-bytes bin)))

(defprim "port-transcoder" (port)
  (or (and (typep port 'scheme-port) (port-transcoder* port))
      (if (subtypep (stream-element-type port) 'character)
	  (funcall (prim "native-transcoder"))
	  ps:false)))

;;; ------------------------------------------------------------------
;;; Opening ports (8.2.7 - 8.2.13)

(defun options-include (options name)
  (let ((options (if (enum-set-p options) (enum-members options) options)))
    (and (listp options) (member name options :key #'ps:scheme-symbol-name :test #'string=))))

(defprim "open-bytevector-input-port" (bv &optional tc)
  (let ((port (make-instance 'bytevector-input-port :bytes (check-bv "open-bytevector-input-port" bv))))
    (if (and tc (transcoder-p tc)) (funcall (prim "transcoded-port") port tc) port)))

(defprim "open-bytevector-output-port" (&optional tc)
  (let* ((port (make-instance 'bytevector-output-port))
	 (view (if (and tc (transcoder-p tc)) (funcall (prim "transcoded-port") port tc) port)))
    (values view (lambda () (finish-output view) (take-bytes port)))))

(defprim "call-with-bytevector-output-port" (proc &optional tc)
  (multiple-value-bind (port extract) (funcall (prim "open-bytevector-output-port") tc)
    (funcall proc port)
    (funcall extract)))

(defun open-file (who name direction options tc)
  ;; R6RS 8.2.2: with no options, opening an existing file for output
  ;; is an error; no-create (only open existing files) and no-fail both
  ;; lift that.  A missing file is created unless no-create; an existing
  ;; one is truncated unless no-truncate.
  (let ((exists (probe-file name)))
    (when (and (eq direction :input) (not exists))
      (io-error-filename who name "make-i/o-file-does-not-exist-error"))
    (when (member direction '(:output :io))
      (when (and exists (not (options-include options "no-fail"))
		 (not (options-include options "no-create")))
	(io-error-filename who name "make-i/o-file-already-exists-error"))
      (when (and (not exists) (options-include options "no-create"))
	(io-error-filename who name "make-i/o-file-does-not-exist-error")))
    (let* ((stream (open name :direction direction :element-type '(unsigned-byte 8)
			      :if-exists (if (options-include options "no-truncate") :overwrite :supersede)
			      :if-does-not-exist :create))
	   (binary (if (eq direction :input)
		       (make-instance 'octet-stream-input-port :stream stream)
		       stream)))
      (if (and tc (transcoder-p tc))
	  (funcall (prim "transcoded-port") binary tc)
	  binary))))

(defvar *buffer-modes* (trivial-garbage:make-weak-hash-table :weakness :key)
  "The buffer mode each file port was opened with.")

(defun with-buffer-mode (port mode)
  (when (symbolp mode) (setf (gethash port *buffer-modes*) mode))
  port)

(defprim "open-file-input-port" (name &optional options buffer-mode tc)
  (with-buffer-mode (open-file "open-file-input-port" name :input options tc) buffer-mode))
(defprim "open-file-output-port" (name &optional options buffer-mode tc)
  (with-buffer-mode (open-file "open-file-output-port" name :output options tc) buffer-mode))
(defprim "open-file-input/output-port" (name &optional options buffer-mode tc)
  (with-buffer-mode (open-file "open-file-input/output-port" name :io options tc) buffer-mode))

;;; Fresh binary ports on the process's standard streams (8.2.7).
;;; Closing one must not close the file descriptor underneath, which
;;; the textual (current-output-port) etc. still use, so they wrap a
;;; shared fd stream and closing only closes the wrapper.

(defvar *fd-streams* (make-hash-table))

(defun fd-stream (fd direction)
  (or (gethash fd *fd-streams*)
      (setf (gethash fd *fd-streams*)
	    #+sbcl (sb-sys:make-fd-stream fd :element-type '(unsigned-byte 8)
					     :input (eq direction :input) :output (eq direction :output)
					     :buffering :full)
	    ;; Elsewhere, reopen the descriptor (Unix).
	    #-sbcl (open (format nil "/dev/fd/~D" fd) :element-type '(unsigned-byte 8)
			 :direction direction :if-exists :append))))

(defclass standard-binary-output-port (binary-output-port)
  ((stream :initarg :stream :reader underlying)
   (open :initform t :accessor port-open)))

(defmethod trivial-gray-streams:stream-write-byte ((s standard-binary-output-port) byte)
  (write-byte byte (underlying s)))
(defmethod trivial-gray-streams:stream-force-output ((s standard-binary-output-port))
  (finish-output (underlying s)))
(defmethod trivial-gray-streams:stream-finish-output ((s standard-binary-output-port))
  (finish-output (underlying s)))
(defmethod close ((s standard-binary-output-port) &key abort)
  (declare (ignore abort))
  (finish-output (underlying s))
  (setf (port-open s) nil)
  t)
(defmethod open-stream-p ((s standard-binary-output-port)) (port-open s))

(defclass standard-binary-input-port (octet-stream-input-port) ())
(defmethod close ((s standard-binary-input-port) &key abort)
  (declare (ignore abort))
  t)

(defprim "standard-input-port" ()
  (make-instance 'standard-binary-input-port :stream (fd-stream 0 :input)))
(defprim "standard-output-port" ()
  (make-instance 'standard-binary-output-port :stream (fd-stream 1 :output)))
(defprim "standard-error-port" ()
  (make-instance 'standard-binary-output-port :stream (fd-stream 2 :output)))

(defprim "buffer-mode?" (x)
  (bool (and (symbolp x) (member (ps:scheme-symbol-name x) '("none" "line" "block") :test #'string=))))

(defprim "output-port-buffer-mode" (port)
  (values (gethash port *buffer-modes* (ps:intern-scheme-symbol "block"))))

;;; ------------------------------------------------------------------
;;; Port positions, eof

(defun has-position-p (port)
  (or (typep port 'bytevector-input-port) (typep port 'octet-stream-input-port)
      (typep port 'file-stream)
      (and (typep port 'transcoded-port) (has-position-p (binary port)))
      (and (typep port '(or custom-binary-input-port custom-binary-output-port
			  custom-textual-input-port custom-textual-output-port))
	   (slot-value port 'get-position))))

(defprim "port-has-port-position?" (port) (bool (has-position-p port)))
(defprim "port-has-set-port-position!?" (port)
  (bool (or (typep port 'bytevector-input-port) (typep port 'octet-stream-input-port)
		(typep port 'file-stream)
		(and (typep port 'transcoded-port) (has-position-p (binary port)))
		(and (typep port '(or custom-binary-input-port custom-binary-output-port
				    custom-textual-input-port custom-textual-output-port))
		     (slot-value port 'set-position!)))))

(defprim "port-position" (port)
  (if (and (typep port '(or custom-binary-input-port custom-binary-output-port
			  custom-textual-input-port custom-textual-output-port))
	   (slot-value port 'get-position))
      ;; less a byte read ahead by lookahead-u8
      (- (funcall (slot-value port 'get-position))
	 (if (and (typep port 'binary-input-port) (integerp (peeked port))) 1 0))
      (or (file-position port) (r6rs-assertion-violation "port-position" "port has no position" port))))

(defprim "set-port-position!" (port pos)
  (if (and (typep port '(or custom-binary-input-port custom-binary-output-port
			  custom-textual-input-port custom-textual-output-port))
	   (slot-value port 'set-position!))
      (progn
	(typecase port
	  (binary-input-port (setf (peeked port) nil))
	  (custom-textual-input-port (setf (unread port) nil)))
	(funcall (slot-value port 'set-position!) pos))
      (file-position port pos))
  ps:unspecific)

(defprim "port-eof?" (port)
  (bool (if (typep port 'binary-input-port)
		(eq (peek-byte port) :eof)
		(eq (peek-char nil port nil :eof) :eof))))

;;; ------------------------------------------------------------------
;;; Binary input/output (8.2.8, 8.2.11)

(defun check-binary-in (who port)
  (unless (and (input-stream-p port) (not (subtypep (stream-element-type port) 'character)))
    (r6rs-assertion-violation who "not a binary input port" port)))

(defprim "get-u8" (port)
  (check-binary-in "get-u8" port)
  (let ((b (read-byte port nil :eof))) (if (eq b :eof) ps:eof-object b)))

(defprim "lookahead-u8" (port)
  (check-binary-in "lookahead-u8" port)
  (let ((b (if (typep port 'binary-input-port) (peek-byte port) (read-byte port nil :eof))))
    (if (eq b :eof) ps:eof-object b)))

(defun read-bytes (port count)
  (let ((out '()))
    (loop repeat count
	  for b = (read-byte port nil :eof)
	  until (eq b :eof)
	  do (push b out))
    (coerce (nreverse out) 'octets)))

(defprim "get-bytevector-n" (port count)
  (check-binary-in "get-bytevector-n" port)
  (let ((bv (read-bytes port count)))
    (if (and (zerop (length bv)) (plusp count)) ps:eof-object bv)))

(defprim "get-bytevector-n!" (port bv start count)
  (check-binary-in "get-bytevector-n!" port)
  (let ((got (read-bytes port count)))
    (if (and (zerop (length got)) (plusp count))
	ps:eof-object
	(progn (replace bv got :start1 start) (length got)))))

(defun byte-ready-p (port)
  "Can a byte (or EOF) be read from PORT without waiting?  As far as is
known: a file port's buffer or descriptor, a bytevector port always."
  (typecase port
    (bytevector-input-port t)
    (octet-stream-input-port (or (peeked port) (listen (underlying port))))
    (t nil)))

(defun read-some-bytes (port bv start count &optional nonblocking)
  "Read what is available from PORT into BV at START, at most COUNT bytes,
waiting for the first unless NONBLOCKING; the number read, or :EOF."
  (let ((n 0))
    (flet ((take ()
	     (let ((b (read-byte port nil :eof)))
	       (if (eq b :eof)
		   (return-from read-some-bytes (if (zerop n) :eof n))
		   (progn (setf (aref bv (+ start n)) b) (incf n))))))
      (when (plusp count)
	(cond ((byte-ready-p port) (take))
	      ((not nonblocking) (take))
	      ;; nothing buffered and none ready: at EOF, the descriptor
	      ;; polls readable all the same
	      ((let ((fd (port-descriptor port)))
		 (and fd #+sbcl (sb-unix:unix-simple-poll fd :input 0)))
	       (take))
	      (t (return-from read-some-bytes 0))))
      (loop while (and (< n count) (byte-ready-p port)) do (take))
      n)))

(defun port-descriptor (port)
  "The file descriptor under PORT, or NIL."
  (loop for s = port then (and (typep s 'standard-object) (slot-exists-p s 'stream)
			       (slot-boundp s 'stream) (slot-value s 'stream))
	repeat 6
	while s
	do #+sbcl (when (typep s 'sb-sys:fd-stream) (return (sb-sys:fd-stream-fd s)))
	   (unless (typep s 'standard-object) (return nil))))

(defprim "get-bytevector-some" (port)
  (check-binary-in "get-bytevector-some" port)
  (let* ((bv (make-array 4096 :element-type '(unsigned-byte 8)))
	 (n (read-some-bytes port bv 0 4096)))
    (if (eq n :eof) ps:eof-object (subseq bv 0 n))))

(defprim "get-bytevector-all" (port)
  (check-binary-in "get-bytevector-all" port)
  (let ((bv (read-bytes port most-positive-fixnum)))
    (if (zerop (length bv)) ps:eof-object bv)))

(defprim "put-u8" (port byte) (write-byte byte port) ps:unspecific)

(defprim "put-bytevector" (port bv &optional (start 0) (count nil))
  (let ((end (if count (+ start count) (length bv))))
    (loop for i from start below end do (write-byte (aref bv i) port)))
  ps:unspecific)

;;; Textual input/output (8.2.9, 8.2.12)

(defprim "get-char" (port)
  (let ((c (read-char port nil :eof))) (if (eq c :eof) ps:eof-object c)))

(defprim "lookahead-char" (port)
  (let ((c (peek-char nil port nil :eof))) (if (eq c :eof) ps:eof-object c)))

(defprim "get-string-n" (port count)
  (let* ((s (make-string count))
	 (n (loop for i from 0 below count
		  for c = (read-char port nil :eof)
		  until (eq c :eof)
		  do (setf (char s i) c)
		  finally (return i))))
    (if (and (zerop n) (plusp count)) ps:eof-object (coerce (subseq s 0 n) 'simple-string))))

(defprim "get-string-n!" (port string start count)
  (let ((n (loop for i from 0 below count
		 for c = (read-char port nil :eof)
		 until (eq c :eof)
		 do (setf (char string (+ start i)) c)
		 finally (return i))))
    (if (and (zerop n) (plusp count)) ps:eof-object n)))

(defprim "get-string-all" (port)
  (let ((out (make-string-output-stream)))
    (loop for c = (read-char port nil :eof)
	  until (eq c :eof)
	  do (write-char c out))
    (let ((s (get-output-stream-string out)))
      (if (zerop (length s)) ps:eof-object (coerce s 'simple-string)))))

(defprim "get-line" (port)
  (let ((line (read-line port nil :eof)))
    (if (eq line :eof) ps:eof-object (coerce line 'simple-string))))

(defprim "get-datum" (port) (funcall ps:*scheme-read* port))

(defprim "put-char" (port c) (write-char c port) ps:unspecific)

(defprim "put-string" (port string &optional (start 0) (count nil))
  (write-string string port :start start :end (if count (+ start count) nil))
  ps:unspecific)

(defprim "put-datum" (port datum) (funcall ps:*scheme-write* datum port) ps:unspecific)

;;; String ports

(defprim "open-string-output-port" ()
  (let ((s (make-string-output-stream)))
    (values s (lambda () (coerce (get-output-stream-string s) 'simple-string)))))

(defprim "open-string-input-port" (string)
  (make-string-input-stream string))

(defprim "call-with-string-output-port" (proc)
  (let ((s (make-string-output-stream)))
    (funcall proc s)
    (coerce (get-output-stream-string s) 'simple-string)))

;;; (rnrs io simple) port predicates over Gray ports

(defprim "textual-port?" (x)
  (bool (and (streamp x) (subtypep (stream-element-type x) 'character))))
(defprim "binary-port?" (x)
  (bool (and (streamp x) (not (subtypep (stream-element-type x) 'character)))))
