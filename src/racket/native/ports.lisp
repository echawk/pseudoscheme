; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

;;;; Ports, natively: strategy B's stand-in for the io layer's ports,
;;;; enough for the expander to read modules (docs/racket.md, "How Racket
;;;; is layered").
;;;;
;;;; An input port (RINPORT) is bytes and a position: a file is read
;;;; whole when opened.  Characters are decoded from UTF-8 as they are
;;;; read; with line counting on (port-count-lines!), the port keeps the
;;;; line, column and position of the next character, as Racket's does.
;;;; An output port (ROUTPORT) collects bytes, for open-output-bytes and
;;;; open-output-string.  The operations take these, or the host's ports
;;;; (current-input-port and the like are the host's), and pass the
;;;; host's to the host's procedures.
;;;; Not yet: custom ports, pipes, file output ports, specials, peeking
;;;; progress events, encodings other than UTF-8.

(in-package "PSEUDOSCHEME-RACKET")

(defstruct (rinport (:constructor make-rinport (bytes name)))
  (bytes #() :type (simple-array (unsigned-byte 8) (*)))
  (pos 0 :type fixnum)
  name
  (closed nil)
  (count-lines nil)
  (line 1) (column 0) (position 1))	; of the next character, when counting

(defstruct (routport (:constructor make-routport (name)))
  (buffer (make-array 64 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
  name
  (closed nil))

(defmethod print-object ((p rinport) stream) (format stream "#<input-port:~A>" (rinport-name p)))
(defmethod print-object ((p routport) stream) (format stream "#<output-port:~A>" (routport-name p)))

(defun bytes-of (s) (sb-ext:string-to-octets s :external-format :utf-8))

;;; ------------------------------------------------------------------
;;; Input

(defun utf-8-char-at (bytes pos)
  "The character whose UTF-8 encoding starts at POS in BYTES, and how
many bytes it takes; a bad sequence is #\\uFFFD, one byte."
  (let* ((b (aref bytes pos))
	 (n (cond ((< b #x80) 1) ((< b #xC0) 0) ((< b #xE0) 2) ((< b #xF0) 3) ((< b #xF8) 4) (t 0))))
    (if (or (= n 0) (> (+ pos n) (length bytes)))
	(values (code-char #xFFFD) 1)
	(if (= n 1)
	    (values (code-char b) 1)
	    (let ((code (logand b (ash #xFF (- (1+ n))))))
	      (loop for i from 1 below n
		    for c = (aref bytes (+ pos i))
		    do (if (= (logand c #xC0) #x80)
			   (setq code (logior (ash code 6) (logand c #x3F)))
			   (return-from utf-8-char-at (values (code-char #xFFFD) 1))))
	      (values (if (or (> code #x10FFFF) (<= #xD800 code #xDFFF)) (code-char #xFFFD) (code-char code))
		      n))))))

(defun inport-peek-char (p skip)
  "The character SKIP bytes after P's position, and its byte length, or NIL."
  (let ((pos (+ (rinport-pos p) skip)))
    (if (>= pos (length (rinport-bytes p)))
	nil
	(utf-8-char-at (rinport-bytes p) pos))))

(defun note-char (p c)
  (when (rinport-count-lines p)
    (incf (rinport-position p))
    (cond ((char= c #\Newline) (incf (rinport-line p)) (setf (rinport-column p) 0))
	  ((char= c #\Tab) (setf (rinport-column p) (* 8 (1+ (floor (rinport-column p) 8)))))
	  (t (incf (rinport-column p))))))

(defun inport-read-char (p)
  (multiple-value-bind (c n) (inport-peek-char p 0)
    (cond ((null c) ps:eof-object)
	  (t (incf (rinport-pos p) n) (note-char p c) c))))

(defun inport-read-byte (p)
  (if (>= (rinport-pos p) (length (rinport-bytes p)))
      ps:eof-object
      (prog1 (aref (rinport-bytes p) (rinport-pos p))
	(incf (rinport-pos p))
	(when (rinport-count-lines p) (incf (rinport-position p))))))

(defun input-port (who port)
  (let ((port (or port (funcall (psx:host-ref "current-input-port")))))
    (when (and (rinport-p port) (rinport-closed port))
      (racket-error who "input port is closed~%  port: ~A" (written port)))
    port))

(defnative "open-input-file" (path &rest mode) (:leaf t)
  (declare (ignore mode))
  (let ((name (path-string "open-input-file" path)))
    (handler-case
	(with-open-file (in name :element-type '(unsigned-byte 8))
	  (let ((bytes (make-array (file-length in) :element-type '(unsigned-byte 8))))
	    (read-sequence bytes in)
	    (make-rinport bytes (make-rpath name))))
      (file-error ()
	(do-raise (make-exn "exn:fail:filesystem"
			    (format nil "open-input-file: cannot open input file~%  path: ~A" name)))))))
(defnative "open-input-string" (s &optional (name (rsym "string"))) (:leaf t)
  (make-rinport (bytes-of s) name))
(defnative "open-input-bytes" (b &optional (name (rsym "bytes"))) (:leaf t)
  (make-rinport (copy-seq b) name))

(defnative "read-char" (&optional port) (:leaf t)
  (let ((p (input-port "read-char" port)))
    (if (rinport-p p) (inport-read-char p) (funcall (psx:host-ref "read-char") p))))
(defnative "peek-char" (&optional port (skip 0)) (:leaf t)
  (let ((p (input-port "peek-char" port)))
    (if (rinport-p p)
	(or (inport-peek-char p skip) ps:eof-object)
	(funcall (psx:host-ref "peek-char") p))))
(defnative "read-char-or-special" (&optional port special-wrap source-name) (:leaf t)
  (declare (ignore special-wrap source-name))
  (let ((p (input-port "read-char-or-special" port)))
    (if (rinport-p p) (inport-read-char p) (funcall (psx:host-ref "read-char") p))))
(defnative "peek-char-or-special" (&optional port (skip 0) special-wrap source-name) (:leaf t)
  (declare (ignore special-wrap source-name))
  (let ((p (input-port "peek-char-or-special" port)))
    (if (rinport-p p)
	(or (inport-peek-char p skip) ps:eof-object)
	(funcall (psx:host-ref "peek-char") p))))
(defnative "read-byte" (&optional port) (:leaf t)
  (let ((p (input-port "read-byte" port)))
    (if (rinport-p p) (inport-read-byte p) (funcall (psx:host-ref "read-u8") p))))
(defnative "peek-byte" (&optional port (skip 0)) (:leaf t)
  (let ((p (input-port "peek-byte" port)))
    (if (rinport-p p)
	(let ((pos (+ (rinport-pos p) skip)))
	  (if (< pos (length (rinport-bytes p))) (aref (rinport-bytes p) pos) ps:eof-object))
	(funcall (psx:host-ref "peek-u8") p))))
(defnative "read-bytes" (n &optional port) (:leaf t)
  (let* ((p (input-port "read-bytes" port))
	 (start (rinport-pos p))
	 (end (min (length (rinport-bytes p)) (+ start n))))
    (if (and (= start end) (plusp n))
	ps:eof-object
	(progn (setf (rinport-pos p) end)
	       (when (rinport-count-lines p) (incf (rinport-position p) (- end start)))
	       (subseq (rinport-bytes p) start end)))))
(defnative "peek-bytes" (n skip &optional port) (:leaf t)
  (let* ((p (input-port "peek-bytes" port))
	 (start (+ (rinport-pos p) skip))
	 (end (min (length (rinport-bytes p)) (+ start n))))
    (if (and (>= start end) (plusp n)) ps:eof-object (subseq (rinport-bytes p) start end))))
(defnative "read-string" (n &optional port) (:leaf t)
  (let ((p (input-port "read-string" port)))
    (let ((chars (loop repeat n
		       for c = (inport-read-char p)
		       until (eq c ps:eof-object) collect c)))
      (if (and (null chars) (plusp n)) ps:eof-object (coerce chars 'string)))))
(defnative "read-line" (&optional port mode) (:leaf t)
  (declare (ignore mode))
  (let ((p (input-port "read-line" port)))
    (if (not (rinport-p p))
	(funcall (psx:host-ref "read-line") p)
	(let ((first (inport-read-char p)))
	  (if (eq first ps:eof-object)
	      ps:eof-object
	      (with-output-to-string (out)
		(loop for c = first then (inport-read-char p)
		      until (or (eq c ps:eof-object) (char= c #\Newline))
		      do (write-char c out))))))))

(defnative "port-count-lines!" (p) (:leaf t)
  (when (and (rinport-p p) (not (rinport-count-lines p)))
    (setf (rinport-count-lines p) t))
  *void*)
(defnative "port-counts-lines?" (p) (:leaf t) (bool (and (rinport-p p) (rinport-count-lines p))))
(defnative "port-next-location" (p) (:leaf t)
  (if (and (rinport-p p) (rinport-count-lines p))
      (values (rinport-line p) (rinport-column p) (rinport-position p))
      (values ps:false ps:false (if (rinport-p p) (1+ (rinport-pos p)) ps:false))))
(defnative "file-position" (p &optional (pos nil pos-p)) (:leaf t)
  (cond ((and pos-p (rinport-p p)) (setf (rinport-pos p) (if (eq pos ps:eof-object) (length (rinport-bytes p)) pos)) *void*)
	((rinport-p p) (rinport-pos p))
	((routport-p p) (fill-pointer (routport-buffer p)))
	(t 0)))
(defnative "file-position*" (p) (:leaf t)
  (if (rinport-p p) (rinport-pos p) ps:false))
(defnative "close-input-port" (p) (:leaf t)
  (if (rinport-p p) (setf (rinport-closed p) t) (funcall (psx:host-ref "close-input-port") p))
  *void*)
(defnative "char-ready?" (&optional port) (:leaf t)
  (let ((p (input-port "char-ready?" port)))
    (if (rinport-p p) ps:true (funcall (psx:host-ref "char-ready?") p))))
(defnative "byte-ready?" (&optional port) (:leaf t) (declare (ignore port)) ps:true)

;;; ------------------------------------------------------------------
;;; Output

(defun output-port (port)
  (or port (funcall (psx:host-ref "current-output-port"))))

(defun out-bytes (port bytes)
  (let ((buffer (routport-buffer port)))
    (loop for b across bytes do (vector-push-extend b buffer))))

(defun port-write-string (string port)
  "STRING to PORT, ours or the host's."
  (let ((p (output-port port)))
    (if (routport-p p)
	(out-bytes p (bytes-of string))
	(funcall (psx:host-ref "write-string") string p))))

(defnative "open-output-bytes" (&optional (name (rsym "bytes"))) (:leaf t) (make-routport name))
(defnative "open-output-string" (&optional (name (rsym "string"))) (:leaf t) (make-routport name))
(defnative "get-output-bytes" (p &optional reset? (start 0) end) (:leaf t)
  (let ((b (subseq (coerce (routport-buffer p) '(simple-array (unsigned-byte 8) (*))) start end)))
    (when (and reset? (truthy reset?)) (setf (fill-pointer (routport-buffer p)) 0))
    b))
(defnative "get-output-string" (p) (:leaf t)
  (sb-ext:octets-to-string (coerce (routport-buffer p) '(simple-array (unsigned-byte 8) (*)))
			   :external-format :utf-8))
(defnative "write-string" (s &optional port (start 0) end) (:leaf t)
  (port-write-string (subseq s start end) port)
  (- (or end (length s)) start))
(defnative "write-char" (c &optional port) (:leaf t)
  (port-write-string (string c) port)
  *void*)
(defnative "newline" (&optional port) (:leaf t)
  (port-write-string (string #\Newline) port)
  *void*)
(defnative "write-bytes" (b &optional port (start 0) end) (:leaf t)
  (let ((p (output-port port)) (b (subseq b start end)))
    (if (routport-p p)
	(out-bytes p b)
	(funcall (psx:host-ref "write-string") (sb-ext:octets-to-string b :external-format :utf-8) p))
    (length b)))
(defnative "write-byte" (byte &optional port) (:leaf t)
  (let ((p (output-port port)))
    (if (routport-p p)
	(vector-push-extend byte (routport-buffer p))
	(funcall (psx:host-ref "write-char") (code-char byte) p))
    *void*))
(defnative "flush-output" (&optional port) (:leaf t)
  (let ((p (output-port port)))
    (unless (routport-p p) (funcall (psx:host-ref "flush-output-port") p))
    *void*))
(defnative "close-output-port" (p) (:leaf t)
  (if (routport-p p) (setf (routport-closed p) t) (funcall (psx:host-ref "close-output-port") p))
  *void*)

;;; ------------------------------------------------------------------
;;; Ports in general

(defnative "input-port?" (x) (:leaf t)
  (bool (or (rinport-p x) (truthy (funcall (psx:host-ref "input-port?") x)))))
(defnative "output-port?" (x) (:leaf t)
  (bool (or (routport-p x) (truthy (funcall (psx:host-ref "output-port?") x)))))
(defnative "port?" (x) (:leaf t)
  (bool (or (rinport-p x) (routport-p x) (truthy (funcall (psx:host-ref "port?") x)))))
(defnative "port-closed?" (p) (:leaf t)
  (bool (cond ((rinport-p p) (rinport-closed p)) ((routport-p p) (routport-closed p)))))
(defnative "file-stream-port?" (p) (:leaf t) (bool (and (rinport-p p) (rpath-p (rinport-name p)))))
(defnative "terminal-port?" (p) (:leaf t) (declare (ignore p)) ps:false)
