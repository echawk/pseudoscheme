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

(defun encode (string codec)
  (cond ((eq codec *latin-1-codec*)
	 (map '(vector (unsigned-byte 8)) (lambda (c) (min 255 (char-code c))) string))
	((eq codec *utf-16-codec*) (funcall (prim "string->utf16") string))
	(t (funcall (ps-r7rs::primitive "string->utf8") string))))

(defun decode (bytes codec)
  (let ((bytes (coerce bytes 'octets)))
    (cond ((eq codec *latin-1-codec*) (map 'simple-string #'code-char bytes))
	  ((eq codec *utf-16-codec*) (funcall (prim "utf16->string") bytes (ps:intern-scheme-symbol "big")))
	  (t (funcall (ps-r7rs::primitive "utf8->string") bytes)))))

(defprim "string->bytevector" (string tc) (coerce (encode string (transcoder-codec tc)) 'octets))
(defprim "bytevector->string" (bv tc) (decode bv (transcoder-codec tc)))

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
  (prog1 (coerce (buffer s) 'octets) (setf (fill-pointer (buffer s)) 0)))

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

;;; Transcoded ports (8.2.6): a textual view of a binary port.

(defclass transcoded-input-port (scheme-port trivial-gray-streams:fundamental-character-input-stream)
  ((binary :initarg :binary :reader binary)
   (unread :initform nil :accessor unread)))

(defun read-utf8-char (bin)
  (let ((b (read-byte bin nil :eof)))
    (cond ((eq b :eof) :eof)
	  ((< b #x80) (code-char b))
	  (t (let* ((n (cond ((< b #xE0) 1) ((< b #xF0) 2) (t 3)))
		    (code (logand b (ash #x3F (- n)))))
	       (dotimes (i n)
		 (let ((c (read-byte bin nil 0)))
		   (setq code (logior (ash code 6) (logand c #x3F)))))
	       (code-char code))))))

(defmethod trivial-gray-streams:stream-read-char ((s transcoded-input-port))
  (let ((u (unread s)))
    (if u
	(progn (setf (unread s) nil) u)
	(let ((codec (transcoder-codec (port-transcoder* s))))
	  (if (eq codec *latin-1-codec*)
	      (let ((b (read-byte (binary s) nil :eof))) (if (eq b :eof) :eof (code-char b)))
	      (read-utf8-char (binary s)))))))
(defmethod trivial-gray-streams:stream-unread-char ((s transcoded-input-port) c) (setf (unread s) c) nil)

(defclass transcoded-output-port (scheme-port trivial-gray-streams:fundamental-character-output-stream)
  ((binary :initarg :binary :reader binary)
   (column :initform 0 :accessor column)))

(defmethod trivial-gray-streams:stream-write-char ((s transcoded-output-port) c)
  (loop for b across (encode (string c) (transcoder-codec (port-transcoder* s)))
	do (write-byte b (binary s)))
  (setf (column s) (if (char= c #\Newline) 0 (1+ (column s))))
  c)
(defmethod trivial-gray-streams:stream-line-column ((s transcoded-output-port)) (column s))

(defprim "transcoded-port" (binary tc)
  (if (input-stream-p binary)
      (make-instance 'transcoded-input-port :binary binary :transcoder tc)
      (make-instance 'transcoded-output-port :binary binary :transcoder tc)))

(defprim "port-transcoder" (port)
  (or (and (typep port 'scheme-port) (port-transcoder* port))
      (if (subtypep (stream-element-type port) 'character)
	  (funcall (prim "native-transcoder"))
	  ps:false)))

;;; ------------------------------------------------------------------
;;; Opening ports (8.2.7 - 8.2.13)

(defun options-include (options name)
  (and (listp options) (member name options :key #'ps:scheme-symbol-name :test #'string=)))

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
    (let* ((textual (and tc (transcoder-p tc)))
	   (stream (open name :direction direction
			      :element-type (if textual 'character '(unsigned-byte 8))
			      :external-format :utf-8
			      :if-exists (if (options-include options "no-truncate") :overwrite :supersede)
			      :if-does-not-exist :create)))
      (cond (textual stream)
	    ((eq direction :input) (make-instance 'octet-stream-input-port :stream stream))
	    (t stream)))))

(defprim "open-file-input-port" (name &optional options buffer-mode tc)
  (declare (ignore buffer-mode))
  (open-file "open-file-input-port" name :input options tc))
(defprim "open-file-output-port" (name &optional options buffer-mode tc)
  (declare (ignore buffer-mode))
  (open-file "open-file-output-port" name :output options tc))
(defprim "open-file-input/output-port" (name &optional options buffer-mode tc)
  (declare (ignore buffer-mode))
  (open-file "open-file-input/output-port" name :io options tc))

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
  (ps:true? (and (symbolp x) (member (ps:scheme-symbol-name x) '("none" "line" "block") :test #'string=))))

(defprim "output-port-buffer-mode" (port) (declare (ignore port)) (ps:intern-scheme-symbol "block"))

;;; ------------------------------------------------------------------
;;; Port positions, eof

(defun has-position-p (port)
  (or (typep port 'bytevector-input-port) (typep port 'octet-stream-input-port)
      (typep port 'file-stream)
      (and (typep port '(or custom-binary-input-port custom-binary-output-port
			  custom-textual-input-port custom-textual-output-port))
	   (slot-value port 'get-position))))

(defprim "port-has-port-position?" (port) (ps:true? (has-position-p port)))
(defprim "port-has-set-port-position!?" (port)
  (ps:true? (or (typep port 'bytevector-input-port) (typep port 'octet-stream-input-port)
		(typep port 'file-stream)
		(and (typep port '(or custom-binary-input-port custom-binary-output-port
				    custom-textual-input-port custom-textual-output-port))
		     (slot-value port 'set-position!)))))

(defprim "port-position" (port)
  (if (and (typep port '(or custom-binary-input-port custom-binary-output-port
			  custom-textual-input-port custom-textual-output-port))
	   (slot-value port 'get-position))
      (funcall (slot-value port 'get-position))
      (or (file-position port) (r6rs-assertion-violation "port-position" "port has no position" port))))

(defprim "set-port-position!" (port pos)
  (if (and (typep port '(or custom-binary-input-port custom-binary-output-port
			  custom-textual-input-port custom-textual-output-port))
	   (slot-value port 'set-position!))
      (funcall (slot-value port 'set-position!) pos)
      (file-position port pos))
  ps:unspecific)

(defprim "port-eof?" (port)
  (ps:true? (if (typep port 'binary-input-port)
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

(defprim "get-bytevector-some" (port)
  (check-binary-in "get-bytevector-some" port)
  (let ((b (read-byte port nil :eof)))
    (if (eq b :eof) ps:eof-object (coerce (list b) 'octets))))

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
  (ps:true? (and (streamp x) (subtypep (stream-element-type x) 'character))))
(defprim "binary-port?" (x)
  (ps:true? (and (streamp x) (not (subtypep (stream-element-type x) 'character)))))
