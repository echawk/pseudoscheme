; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-RACKET -*-

;;;; Ports, natively: strategy B's stand-in for the io layer's ports,
;;;; enough for the expander to read modules (docs/racket.md, "How Racket
;;;; is layered"), and the rest of Racket's port procedures that a
;;;; program meets (port-ops.lisp has the less common ones).
;;;;
;;;; An input port (RINPORT) is a buffer of bytes and a position.  A
;;;; port of a string, bytes or a file has all its bytes when opened; the
;;;; others (pipes, custom ports, the host's) fill the buffer as bytes
;;;; are asked for (RINPORT-FILL), and drop what has been read now and
;;;; then.  A custom port with a peek procedure has no buffer: its peeks
;;;; and reads call its procedures.  Characters are decoded from UTF-8
;;;; as they are read; with line counting on (port-count-lines!), the
;;;; port keeps the line, column and position of the next character, as
;;;; Racket's does.
;;;; An output port (ROUTPORT) is one of: bytes collected, for
;;;; open-output-bytes and open-output-string; a file; the output end of
;;;; a pipe; a custom port.  The operations take these, or the host's
;;;; ports (current-input-port and the like are the host's), and pass
;;;; the host's to the host's procedures.
;;;; There is one thread, so a read that would block forever is an
;;;; error, as sync's is.
;;;; Not yet: encodings other than UTF-8, progress events of the host's ports.

(in-package "QUASISCHEME-RACKET")

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(defun make-octets (n) (make-array n :element-type '(unsigned-byte 8)))

;;; port-count-lines-enabled: new ports count lines when it is true
(defvar *port-count-lines-enabled* (make-parameter* qs:false))
(define-primitive "#%kernel" "port-count-lines-enabled" *port-count-lines-enabled*)

(defun auto-count-lines () (truthy (funcall *port-count-lines-enabled*)))

;;; What a custom input port is made of (make-input-port, port-ops.lisp)
(defstruct custom-in
  read-in peek-in close progress commit location count-lines init-position buffer-mode
  (pipe nil)				; a pipe the read procedure redirected to
  (closed nil))

(defstruct (rinport (:constructor make-rinport (bytes name &key (kind :static) (end nil) (fill nil) (custom nil))))
  (bytes #() :type (simple-array (unsigned-byte 8) (*)))
  (pos 0 :type fixnum)
  name
  (closed nil)
  (count-lines nil)
  (line 1) (column 0) (position 1)	; of the next character, when counting
  end					; a lazy port's bytes end here (NIL: at the end of BYTES)
  (base 0)				; bytes dropped from the front of BYTES
  kind					; :static :string :pipe :custom :host
  fill					; (port need blocking) -> :data, :eof or :block, after appending bytes
  custom
  (progress nil)			; the semaphore of the port's progress event, until the port progresses
  (closed-sema nil)
  (peer nil)				; a pipe's output port
  (limit nil)				; a pipe's capacity
  (writer-closed nil)			; a pipe's output end is closed
  (host nil)				; the host port a :host port reads
  (buffer-mode nil))

(defstruct custom-out
  write-out close write-out-special get-write-evt get-write-special-evt
  location count-lines init-position buffer-mode)

(defstruct (routport (:constructor make-routport (name)))
  (buffer (make-array 64 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
  name
  (closed nil)
  (kind :bytes)				; :bytes :file :pipe :custom
  (stream nil)				; a file's
  (peer nil)				; a pipe's input port
  (custom nil)
  (written 0)				; bytes written, for file-position
  (closed-sema nil)
  (count-lines nil)
  (line 1) (column 0) (position 1)
  (buffer-mode nil)
  (path nil)
  (obuf nil)				; a file's bytes not yet written to its stream
  (opos 0 :type fixnum)
  (cbuf nil))				; a character's bytes, for write-char

(defun name-string (name)
  (cond ((rpath-p name) (rpath-string name))
	((stringp name) name)
	(t (with-output-to-string (s) (print-value name s :display)))))

(defun port-desc (p)
  "P as Racket's error messages show it."
  (typecase p
    (rinport (format nil "#<input-port:~A>" (name-string (rinport-name p))))
    (routport (format nil "#<output-port:~A>" (name-string (routport-name p))))
    (t (written p))))

(defmethod print-object ((p rinport) stream) (write-string (port-desc p) stream))
(defmethod print-object ((p routport) stream) (write-string (port-desc p) stream))

(defun bytes-of (s) (sb-ext:string-to-octets s :external-format :utf-8))

(defun decode-utf-8 (bytes &optional (start 0) end)
  (let ((end (or end (length bytes))))
    (if (and (typep bytes '(simple-array (unsigned-byte 8) (*)))
	     (locally (declare (type (simple-array (unsigned-byte 8) (*)) bytes) (type fixnum start end))
	       (loop for i of-type fixnum from start below end always (< (aref bytes i) #x80))))
	;; ASCII, as most lines are: a character per byte
	(locally (declare (type (simple-array (unsigned-byte 8) (*)) bytes) (type fixnum start end)
			  (optimize speed))
	  (let ((s (make-string (- end start))))
	    (loop for i of-type fixnum from start below end
		  for j of-type fixnum from 0
		  do (setf (schar s j) (code-char (aref bytes i))))
	    s))
	(sb-ext:octets-to-string bytes :start start :end end
				       :external-format (list :utf-8 :replacement (code-char #xFFFD))))))

(defun finish-inport (p)
  "P, counting lines if the parameter says so."
  (when (auto-count-lines) (setf (rinport-count-lines p) t))
  p)

(defun finish-outport (p)
  (when (auto-count-lines) (setf (routport-count-lines p) t))
  p)

(defun new-lazy-inport (name kind fill &key custom)
  (finish-inport (make-rinport (make-octets 0) name :kind kind :end 0 :fill fill :custom custom)))

;; ports are evts too, as in Racket
(defun evt-p (x) (or (revt-p x) (rsemaphore-p x) (rinport-p x) (routport-p x)))

;;; ------------------------------------------------------------------
;;; Where the next character is

(defmacro bump (place &optional (n 1))
  `(when (integerp ,place) (incf ,place ,n)))

(defmacro define-location-noter (name count line column position)
  `(defun ,name (p c)
     (when (,count p)
       (bump (,position p))
       (cond ((char= c #\Newline) (bump (,line p)) (setf (,column p) (if (integerp (,column p)) 0 (,column p))))
	     ((char= c #\Tab) (when (integerp (,column p)) (setf (,column p) (* 8 (1+ (floor (,column p) 8))))))
	     (t (bump (,column p)))))))

(define-location-noter note-in-char rinport-count-lines rinport-line rinport-column rinport-position)
(define-location-noter note-out-char routport-count-lines routport-line routport-column routport-position)

(defun note-char (p c)
  (if (rinport-p p) (note-in-char p c) (note-out-char p c)))

(defun counting-p (p)
  (if (rinport-p p) (rinport-count-lines p) (routport-count-lines p)))

(defun note-bytes (p vec start end)
  "Bytes VEC[START,END) went through P: move its location past the characters they start."
  (when (counting-p p)
    (loop for i from start below end
	  for b = (aref vec i)
	  do (unless (= (logand b #xC0) #x80)
	       (note-char p (if (< b #x80) (code-char b) #\?))))))

(defun note-special (p)
  (when (counting-p p) (note-char p #\?)))

;;; ------------------------------------------------------------------
;;; Input: the buffer

(defun utf-8-lead-length (b)
  (cond ((< b #x80) 1) ((< b #xC0) 0) ((< b #xE0) 2) ((< b #xF0) 3) ((< b #xF8) 4) (t 0)))

(defun utf-8-char-at (bytes pos)
  "The character whose UTF-8 encoding starts at POS in BYTES, and how
many bytes it takes; a bad sequence is #\\uFFFD, one byte."
  (let* ((b (aref bytes pos))
	 (n (utf-8-lead-length b)))
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

(defun ip-end (p) (or (rinport-end p) (length (rinport-bytes p))))

(defun ip-append (p vec start end)
  "Add VEC[START,END) to the end of lazy port P's buffer, first dropping what's been read."
  (let* ((n (- end start))
	 (e (ip-end p))
	 (bytes (rinport-bytes p)))
    (when (and (> (rinport-pos p) 4096) (> (* 2 (rinport-pos p)) e))
      (let ((k (rinport-pos p)))
	(replace bytes bytes :start1 0 :start2 k :end2 e)
	(decf e k)
	(incf (rinport-base p) k)
	(setf (rinport-pos p) 0)))
    (when (> (+ e n) (length bytes))
      (let ((new (make-octets (max (* 2 (length bytes)) (+ e n) 256))))
	(replace new bytes :end2 e)
	(setf bytes new (rinport-bytes p) new)))
    (replace bytes vec :start1 e :start2 start :end2 end)
    (setf (rinport-end p) (+ e n))))

(defun ip-fill (p need blocking)
  (let ((f (rinport-fill p)))
    (if f (funcall f p need blocking) :eof)))

(defun ip-avail (p skip want blocking)
  "How many bytes, up to WANT, are SKIP bytes past P's position (filling
a lazy port if there are none), and, if none, :eof or :block."
  (if (<= want 0)
      (values 0 nil)
      (loop
	(let ((have (- (ip-end p) (rinport-pos p) skip)))
	  (when (plusp have) (return (values (min have want) nil)))
	  (case (ip-fill p (- (+ skip want) (- (ip-end p) (rinport-pos p))) blocking)
	    (:data nil)
	    (:eof (return (values 0 :eof)))
	    (t (if blocking
		   (racket-error "read" "would block forever (one thread)")
		   (return (values 0 :block)))))))))

(defun ip-progress (p)
  "P read something: its progress event is ready."
  (let ((s (rinport-progress p)))
    (when s
      (incf (rsemaphore-count s))
      (setf (rinport-progress p) nil))))

(defun ip-advance (p n)
  (incf (rinport-pos p) n)
  (ip-progress p))

(defun pipe-content (p) (- (ip-end p) (rinport-pos p)))

(defun peek-proc-port-p (p)
  (let ((c (rinport-custom p))) (and c (custom-in-peek-in c))))

;;; ------------------------------------------------------------------
;;; Custom input ports: the user's procedures

(defun custom-call (p buf skip blocking progress-evt)
  "Call P's read procedure (SKIP NIL) or peek procedure with BUF (a fresh
byte string), for some of the bytes the caller asked for: the count
(positive), :eof, :block, :progress, (:special . procedure) or (:pipe . input-port)."
  (let* ((c (rinport-custom p))
	 (peek (not (null skip)))
	 (proc (if peek (custom-in-peek-in c) (custom-in-read-in c)))
	 (who (if peek "user port peek" "user port read")))
    (loop repeat 100000
	  do (let ((r (cond ((rinport-p proc)
			     (let ((n (if peek
					  (ip-peek-bytes proc skip buf 0 (length buf) blocking)
					  (ip-read-bytes proc buf 0 (length buf) blocking))))
			       (if (functionp n) (return (cons :special n)) n)))
			    (peek (funcall proc buf skip (or progress-evt qs:false)))
			    (t (funcall proc buf)))))
	       (cond ((integerp r)
		      (when (> r (length buf))
			(racket-error who "result integer is larger than the supplied byte string~%  result: ~D~%  byte-string length: ~D"
				      r (length buf)))
		      (when (plusp r) (return r))
		      (unless blocking (return :block)))
		     ((eq r qs:eof-object) (return :eof))
		     ((member r '(:eof :block :progress)) (return r))
		     ((rinport-p r)
		      (unless (eq (rinport-kind r) :pipe)
			(racket-error who "result is not an accepted value~%  result: ~A" (written r)))
		      (return (cons :pipe r)))
		     ((evt-p r)
		      (if blocking
			  (unless (nth-value 1 (evt-ready r))
			    (racket-error "sync" "would block forever (one thread)"))
			  (return :block)))
		     ((eq r qs:false)
		      (if progress-evt
			  (return :progress)
			  (racket-error who "returned #f when no progress evt was supplied")))
		     ((functionp r)
		      (unless (custom-in-peek-in c)
			(racket-error who "the port has no specific peek procedure, so a special read result is not allowed~%  special result: ~A"
				      (written r)))
		      (return (cons :special r)))
		     (t (racket-error who "result is not an accepted value~%  result: ~A" (written r)))))
	  finally (racket-error "read" "would block forever (one thread)"))))

(defun custom-peek (p skip dest dstart dend blocking &optional progress-evt)
  "Peek P's peek procedure for the bytes of DEST[DSTART,DEND): see IP-PEEK-BYTES."
  (let ((c (rinport-custom p)))
    (loop
      (let ((pipe (custom-in-pipe c)))
	(if (and pipe (> (pipe-content pipe) skip))
	    (return (ip-peek-bytes pipe skip dest dstart dend blocking))
	    (setf (custom-in-pipe c) nil)))
      (let* ((buf (make-octets (min (- dend dstart) 8192)))
	     (r (custom-call p buf skip blocking progress-evt)))
	(cond ((integerp r) (replace dest buf :start1 dstart :end2 r) (return r))
	      ((and (consp r) (eq (car r) :pipe)) (setf (custom-in-pipe c) (cdr r)))
	      ((consp r) (return (cdr r)))
	      (t (return r)))))))

(defun custom-read (p dest dstart dend blocking)
  "Read P's read procedure for the bytes of DEST[DSTART,DEND): see IP-READ-BYTES."
  (let ((c (rinport-custom p)))
    (loop
      (let ((pipe (custom-in-pipe c)))
	(if (and pipe (plusp (pipe-content pipe)))
	    (return (ip-read-bytes pipe dest dstart dend blocking))
	    (setf (custom-in-pipe c) nil)))
      (let* ((buf (make-octets (min (- dend dstart) 8192)))
	     (r (custom-call p buf nil blocking nil)))
	(cond ((integerp r) (replace dest buf :start1 dstart :end2 r) (return r))
	      ((and (consp r) (eq (car r) :pipe)) (setf (custom-in-pipe c) (cdr r)))
	      ((consp r) (return (cdr r)))
	      (t (return r)))))))

(defun custom-fill (p need blocking)
  "The fill procedure of a custom port without a peek procedure: ask its read procedure."
  (let ((c (rinport-custom p)))
    (loop
      (let ((pipe (custom-in-pipe c)))
	(if (and pipe (plusp (pipe-content pipe)))
	    (let* ((n (min (pipe-content pipe) 8192))
		   (buf (make-octets n)))
	      (ip-read-bytes pipe buf 0 n nil)
	      (ip-append p buf 0 n)
	      (return :data))
	    (setf (custom-in-pipe c) nil)))
      (let* ((buf (make-octets (max 1 (min need 8192))))
	     (r (custom-call p buf nil blocking nil)))
	(cond ((integerp r) (ip-append p buf 0 r) (return :data))
	      ((and (consp r) (eq (car r) :pipe)) (setf (custom-in-pipe c) (cdr r)))
	      (t (return r)))))))

;;; ------------------------------------------------------------------
;;; Input: peeking and reading bytes

(defun ip-peek-bytes (p skip dest dstart dend blocking)
  "Up to DEND-DSTART bytes of P, SKIP bytes after its position, into DEST
from DSTART, not consumed: the count (at least one unless asked for
none), :eof, :block (nothing now, and not BLOCKING), :progress, or a
procedure, a special."
  (let ((n (- dend dstart)))
    (cond ((<= n 0) 0)
	  ((peek-proc-port-p p) (custom-peek p skip dest dstart dend blocking))
	  (t (multiple-value-bind (m status) (ip-avail p skip n blocking)
	       (if (plusp m)
		   (let ((from (+ (rinport-pos p) skip)))
		     (replace dest (rinport-bytes p) :start1 dstart :start2 from :end2 (+ from m))
		     m)
		   status))))))

(defun ip-read-bytes (p dest dstart dend blocking)
  "Like IP-PEEK-BYTES, and consumes what it returns."
  (let ((n (- dend dstart)))
    (cond ((<= n 0) 0)
	  ((peek-proc-port-p p)
	   (let ((r (custom-read p dest dstart dend blocking)))
	     (cond ((integerp r) (ip-progress p) (note-bytes p dest dstart (+ dstart r)))
		   ((functionp r) (ip-progress p) (note-special p)))
	     r))
	  (t (let ((r (ip-peek-bytes p 0 dest dstart dend blocking)))
	       (when (integerp r)
		 (ip-advance p r)
		 (note-bytes p dest dstart (+ dstart r)))
	       r)))))

(defun ip-skip (p n)
  "Consume N bytes that were peeked."
  (if (peek-proc-port-p p)
      (let ((tmp (make-octets n)) (got 0))
	(loop while (< got n)
	      do (let ((r (custom-read p tmp got n t)))
		   (if (integerp r) (incf got r) (return))))
	(ip-progress p))
      (ip-advance p n)))

(defun special-error (who p)
  (racket-error who "non-character in an unsupported context~%  port: ~A" (port-desc p)))

(defun inport-peek-byte (p skip &optional (who "peek-byte") special-ok)
  "The byte SKIP bytes after P's position, or eof (or a special)."
  (if (and (null (rinport-fill p)) (null (rinport-custom p)))
      (let ((pos (+ (rinport-pos p) skip)))
	(if (< pos (ip-end p)) (aref (rinport-bytes p) pos) qs:eof-object))
      (let* ((tmp (make-octets 1))
	     (r (ip-peek-bytes p skip tmp 0 1 t)))
	(cond ((eql r 1) (aref tmp 0))
	      ((functionp r) (if special-ok r (special-error who p)))
	      (t qs:eof-object)))))

(defun inport-read-byte (p &optional (who "read-byte") special-ok)
  (if (and (null (rinport-fill p)) (null (rinport-custom p)))
      (if (>= (rinport-pos p) (ip-end p))
	  qs:eof-object
	  (let ((b (aref (rinport-bytes p) (rinport-pos p))))
	    (ip-advance p 1)
	    (when (rinport-count-lines p) (note-in-char p (if (< b #x80) (code-char b) #\?)))
	    b))
      (let* ((tmp (make-octets 1))
	     (r (ip-read-bytes p tmp 0 1 t)))
	(cond ((eql r 1) (aref tmp 0))
	      ((functionp r) (if special-ok r (special-error who p)))
	      (t qs:eof-object)))))

;;; ------------------------------------------------------------------
;;; Input: characters

(defun inport-peek-char (p skip &optional (who "peek-char") special-ok)
  "The character SKIP bytes after P's position, and its byte length; NIL
at eof (or a special, and 1)."
  ;; the character's bytes already in P's buffer (a file's, a pipe's, a
  ;; custom port's without a peek procedure): read there, with no
  ;; temporary byte string
  (let* ((pos (+ (rinport-pos p) skip))
	 (end (ip-end p)))
    (when (and (< pos end) (not (peek-proc-port-p p)))
      (let ((n (utf-8-lead-length (aref (rinport-bytes p) pos))))
	(when (and (<= 1 n) (<= (+ pos n) end))
	  (return-from inport-peek-char (utf-8-char-at (rinport-bytes p) pos))))))
  (if (and (null (rinport-fill p)) (null (rinport-custom p)))
      (let ((pos (+ (rinport-pos p) skip)))
	(if (>= pos (ip-end p))
	    nil
	    (utf-8-char-at (rinport-bytes p) pos)))
      (let* ((tmp (make-octets 4))
	     (r (ip-peek-bytes p skip tmp 0 1 t)))
	(cond ((eql r 1)
	       (let ((n (utf-8-lead-length (aref tmp 0))))
		 (if (<= n 1)
		     (utf-8-char-at tmp 0)
		     (let ((got 1))
		       (loop while (< got n)
			     do (let ((m (ip-peek-bytes p (+ skip got) tmp got n t)))
				  (if (integerp m) (incf got m) (return))))
		       (utf-8-char-at (subseq tmp 0 got) 0)))))
	      ((functionp r) (if special-ok (values r 1) (special-error who p)))
	      (t nil)))))

(defun inport-read-char (p &optional (who "read-char") special-ok)
  ;; an ASCII byte already in the buffer, of a port not counting lines
  (let ((pos (rinport-pos p)))
    (when (and (< pos (ip-end p)) (not (rinport-count-lines p)) (not (peek-proc-port-p p)))
      (let ((b (aref (rinport-bytes p) pos)))
	(when (< b #x80)
	  (setf (rinport-pos p) (1+ pos))
	  (when (rinport-progress p) (ip-progress p))
	  (return-from inport-read-char (code-char b))))))
  (multiple-value-bind (c n) (inport-peek-char p 0 who special-ok)
    (cond ((null c) qs:eof-object)
	  ((functionp c) (ip-skip p 1) (note-special p) c)
	  (t (ip-skip p n) (note-in-char p c) c))))

(defun read-chars (p n who)
  "Up to N characters of P, a string, or eof if there were none."
  (let ((out (make-string-output-stream)) (count 0))
    (loop while (< count n)
	  do (let ((c (inport-read-char p who)))
	       (when (eq c qs:eof-object) (return))
	       (write-char c out)
	       (incf count)))
    (if (and (zerop count) (plusp n)) qs:eof-object (get-output-stream-string out))))

(defun buffered-line (p mode)
  "For P's bytes all in its buffer (a file's, a string's), not counting
lines: the start and end in RINPORT-BYTES of its next line, consumed
with the line separator of MODE; :EOF at the end; NIL if the slow way
must do (a return byte before the linefeed, in a mode other than
linefeed's)."
  (when (and (null (rinport-fill p)) (null (rinport-custom p)) (not (rinport-count-lines p))
	     (member mode '("linefeed" "any" "any-one") :test #'string=))
    (let* ((bytes (rinport-bytes p))
	   (pos (rinport-pos p))
	   (end (ip-end p))
	   (nl (position 10 bytes :start pos :end end))
	   (stop (or nl end)))
      (cond ((and (not (string= mode "linefeed")) (position 13 bytes :start pos :end stop)) nil)
	    ((and (null nl) (= pos end)) :eof)
	    (t (ip-advance p (- (if nl (1+ nl) end) pos))
	       (values pos stop))))))

(defun read-line-bytes (p mode who)
  "The bytes of P up to the next line separator of MODE (a string), which is consumed; or eof."
  (multiple-value-bind (start stop) (buffered-line p mode)
    (case start
      ((nil))
      (:eof (return-from read-line-bytes qs:eof-object))
      (t (return-from read-line-bytes (subseq (rinport-bytes p) start stop)))))
  (let ((out (make-array 80 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
	(seen nil))
    (flet ((result () (coerce out 'octets)))
      (loop
	(let ((b (inport-read-byte p who)))
	  (cond ((eq b qs:eof-object) (return (if seen (result) qs:eof-object)))
		(t (setq seen t)
		   (cond ((and (= b 10) (member mode '("linefeed" "any" "any-one") :test #'string=))
			  (return (result)))
			 ((and (= b 13) (string= mode "return")) (return (result)))
			 ((= b 13)
			  (cond ((string= mode "return-linefeed")
				 (if (eql (inport-peek-byte p 0 who) 10)
				     (progn (inport-read-byte p who) (return (result)))
				     (vector-push-extend b out)))
				((string= mode "any")
				 (when (eql (inport-peek-byte p 0 who) 10) (inport-read-byte p who))
				 (return (result)))
				((string= mode "any-one") (return (result)))
				(t (vector-push-extend b out))))
			 (t (vector-push-extend b out))))))))))

(defun line-mode-name (who mode)
  (let ((name (and (symbolp mode) (not (eq mode qs:false)) (not (eq mode qs:true)) (rname mode))))
    (unless (member name '("linefeed" "return" "return-linefeed" "any" "any-one") :test #'equal)
      (racket-error who "contract violation~%  expected: (or/c 'linefeed 'return 'return-linefeed 'any 'any-one)~%  given: ~A"
		    (written mode)))
    name))

;;; ------------------------------------------------------------------
;;; Opening input ports

(defun raise-errno (who what path errno &optional (type "exn:fail:filesystem:errno"))
  (do-raise (make-exn type
		      (format nil "~A: ~A~%  path: ~A~%  system error: ~A; errno=~D"
			      who what path (sb-int:strerror errno) errno)
		      (cons errno (rsym "posix")))))

(defun slurp-stream (stream size)
  "All the bytes of STREAM, a file of SIZE bytes (maybe not known)."
  (if (plusp size)
      (let* ((bytes (make-octets size))
	     (n (read-sequence bytes stream)))
	(if (= n size) bytes (subseq bytes 0 n)))
      (let ((out (make-array 256 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
	    (buf (make-octets 4096)))
	(loop for n = (read-sequence buf stream)
	      while (plusp n)
	      do (loop for i below n do (vector-push-extend (aref buf i) out)))
	(coerce out 'octets))))

(defun open-file-for-input (who name)
  "The bytes of the file NAME, or Racket's exception."
  (let ((fd (handler-case (sb-posix:open name sb-posix:o-rdonly)
	      (sb-posix:syscall-error (e)
		(raise-errno who "cannot open input file" name (sb-posix:syscall-errno e))))))
    (let ((st (sb-posix:fstat fd)))
      (when (sb-posix:s-isdir (sb-posix:stat-mode st))
	(sb-posix:close fd)
	(raise-errno who "cannot open input file" name 21))
      (or (read-regular-file fd st)
	  (let ((stream (sb-sys:make-fd-stream fd :input t :element-type '(unsigned-byte 8)
						     :buffering :full :file name)))
	    (unwind-protect (slurp-stream stream (sb-posix:stat-size st))
	      (close stream)))))))

(defun read-regular-file (fd st)
  "A regular file's bytes, read from FD straight into a vector of its size
(no stream, with its buffer and finalizer, for one read), and FD closed;
NIL, FD open, for anything else."
  (let ((size (sb-posix:stat-size st)))
    (when (and (sb-posix:s-isreg (sb-posix:stat-mode st)) (plusp size))
      (let ((bytes (make-octets size)) (got 0))
	(declare (fixnum got))
	(sb-sys:with-pinned-objects (bytes)
	  (loop while (< got size)
		do (let ((n (sb-unix:unix-read fd (sb-sys:sap+ (sb-sys:vector-sap bytes) got) (- size got))))
		     (if (and n (plusp n)) (incf got n) (return)))))
	(sb-posix:close fd)
	(if (= got size) bytes (subseq bytes 0 got))))))

(defnative "open-input-file" (path &rest mode) ()
  (declare (ignore mode))
  (let* ((name (native-path "open-input-file" path))
	 (bytes (open-file-for-input "open-input-file" name)))
    (finish-inport (make-rinport bytes (make-rpath name)))))
(defnative "open-input-string" (s &optional (name (rsym "string"))) ()
  (finish-inport (make-rinport (bytes-of s) name :kind :string)))
(defnative "open-input-bytes" (b &optional (name (rsym "string"))) ()
  (finish-inport (make-rinport (copy-seq b) name :kind :string)))

(defvar *host-adapters* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "A host input port -> the lazy input port of ours that reads it.")

(defun host-fill (p need blocking)
  (declare (ignore need))
  (let ((host (rinport-host p)))
    (if (and (not blocking) (not (truthy (funcall (qsx:host-ref "char-ready?") host))))
	:block
	(let ((b (funcall (qsx:host-ref "read-u8") host)))
	  (if (eq b qs:eof-object)
	      :eof
	      (let ((v (make-octets 1)))
		(setf (aref v 0) b)
		(ip-append p v 0 1)
		:data))))))

(defun host-adapter (who port)
  (or (gethash port *host-adapters*)
      (progn
	(unless (truthy (funcall (qsx:host-ref "input-port?") port))
	  (racket-error who "contract violation~%  expected: input-port?~%  given: ~A" (written port)))
	(let ((p (new-lazy-inport (rsym "host") :host #'host-fill)))
	  (setf (rinport-host p) port)
	  (setf (gethash port *host-adapters*) p)))))

(defun port-struct-target (x property)
  "The port a struct instance X stands for through prop:input-port or
prop:output-port PROPERTY (a port, or the index of a field holding one), or NIL."
  (multiple-value-bind (value found) (property-value x property)
    (when found
      (let ((target
	      (if (integerp value)
		  (let ((owner (loop for ty = (struct-of-type x) then (struct-type-parent ty)
				     while (and (struct-type-parent ty)
						(eql (property-value (struct-type-parent ty) property) value))
				     finally (return ty))))
		    (svref (struct-fields x) (+ value (- (struct-type-total owner) (struct-type-field-count owner)))))
		  value)))
	(and target (not (eq target qs:false)) target)))))

(defun unwrap-port (x property)
  "X, or the port it stands for if it is a struct with PROPERTY."
  (when (or (rinport-p x) (routport-p x))
    (return-from unwrap-port x))
  (loop repeat 100
	while (and x (struct-instance-p x))
	do (let ((target (port-struct-target x property)))
	     (if target (setf x target) (return))))
  x)

(defun unwrap-in (x) (unwrap-port x *prop-input-port*))
(defun unwrap-out (x) (unwrap-port x *prop-output-port*))

(defun input-port (who port)
  (let ((port (unwrap-in (or port (current-port :input)))))
    (when (and (rinport-p port) (rinport-closed port))
      (racket-error who "input port is closed~%  input port: ~A" (port-desc port)))
    port))

(defun in-port (who port)
  "PORT, or the current input port, as one of ours (the host's through an adapter)."
  (let ((p (input-port who port)))
    (if (rinport-p p) p (host-adapter who p))))

;;; ------------------------------------------------------------------
;;; Reading

(defmacro buffered-ascii-read (p)
  "The next character of rinport P, consumed, if it is an ASCII byte in
its buffer, P open, not counting lines, not custom, and no progress
event waiting; else NIL."
  `(let ((p ,p))
     (when (and (rinport-p p) (not (rinport-closed p)) (null (rinport-custom p))
		(not (rinport-count-lines p)) (null (rinport-progress p)))
       (let* ((bytes (rinport-bytes p))
	      (pos (rinport-pos p))
	      (end (or (rinport-end p) (length bytes))))
	 (declare (type (simple-array (unsigned-byte 8) (*)) bytes) (fixnum pos end))
	 (when (< pos end)
	   (let ((b (aref bytes pos)))
	     (when (< b #x80)
	       (setf (rinport-pos p) (1+ pos))
	       (code-char b))))))))

(defnative "read-char" (&optional port) ()
  (or (buffered-ascii-read port)
      (let ((p (input-port "read-char" port)))
	(if (rinport-p p) (inport-read-char p "read-char") (funcall (qsx:host-ref "read-char") p)))))
(defnative "peek-char" (&optional port (skip 0)) ()
  (let ((p (input-port "peek-char" port)))
    (if (rinport-p p)
	(or (inport-peek-char p skip "peek-char") qs:eof-object)
	(funcall (qsx:host-ref "peek-char") p))))
(defun wrap-special (p special source-name delta special-wrap)
  "A special SPECIAL that P produced, as read-char-or-special and the like return it."
  (if (eq special-wrap (rsym "special"))
      (rsym "special")
      (multiple-value-bind (line col pos) (funcall (native-function (gethash "port-next-location" *natives*)) p)
	(let ((v (funcall special source-name line (if (integerp col) (+ col delta) col)
			  (if (integerp pos) (+ pos delta) pos))))
	  (if (and special-wrap (truthy special-wrap)) (funcall special-wrap v) v)))))
(defnative "read-char-or-special" (&optional port special-wrap (source-name qs:false)) ()
  (let ((p (input-port "read-char-or-special" port)))
    (if (rinport-p p)
	(let ((c (inport-read-char p "read-char-or-special" t)))
	  (if (functionp c) (wrap-special p c source-name -1 special-wrap) c))
	(funcall (qsx:host-ref "read-char") p))))
(defnative "peek-char-or-special" (&optional port (skip 0) special-wrap (source-name qs:false)) ()
  (let ((p (input-port "peek-char-or-special" port)))
    (if (rinport-p p)
	(multiple-value-bind (c n) (inport-peek-char p skip "peek-char-or-special" t)
	  (declare (ignore n))
	  (cond ((null c) qs:eof-object)
		((functionp c) (wrap-special p c source-name skip special-wrap))
		(t c)))
	(funcall (qsx:host-ref "peek-char") p))))
(defnative "read-byte" (&optional port) ()
  (let ((p (input-port "read-byte" port)))
    (if (rinport-p p) (inport-read-byte p "read-byte") (funcall (qsx:host-ref "read-u8") p))))
(defnative "peek-byte" (&optional port (skip 0)) ()
  (let ((p (input-port "peek-byte" port)))
    (if (rinport-p p)
	(inport-peek-byte p skip "peek-byte")
	(funcall (qsx:host-ref "peek-u8") p))))

(defun check-count (who n)
  (unless (and (integerp n) (>= n 0))
    (racket-error who "contract violation~%  expected: exact-nonnegative-integer?~%  given: ~A" (written n))))

(defnative "read-bytes" (n &optional port) ()
  (check-count "read-bytes" n)
  (let* ((p (in-port "read-bytes" port))
	 (buf (make-octets n))
	 (got 0))
    (loop while (< got n)
	  do (let ((r (ip-read-bytes p buf got n t)))
	       (cond ((integerp r) (incf got r))
		     ((functionp r) (special-error "read-bytes" p))
		     (t (return)))))
    (cond ((and (zerop got) (plusp n)) qs:eof-object)
	  ((= got n) buf)
	  (t (subseq buf 0 got)))))
(defnative "peek-bytes" (n skip &optional port) ()
  (check-count "peek-bytes" n)
  (let* ((p (in-port "peek-bytes" port))
	 (buf (make-octets n))
	 (got 0))
    (loop while (< got n)
	  do (let ((r (ip-peek-bytes p (+ skip got) buf got n t)))
	       (cond ((integerp r) (incf got r))
		     ((functionp r) (special-error "peek-bytes" p))
		     (t (return)))))
    (cond ((and (zerop got) (plusp n)) qs:eof-object)
	  ((= got n) buf)
	  (t (subseq buf 0 got)))))
(defnative "read-string" (n &optional port) ()
  (check-count "read-string" n)
  (read-chars (in-port "read-string" port) n "read-string"))
(defnative "read-line" (&optional port (mode (rsym "linefeed"))) ()
  (let ((p (input-port "read-line" port)))
    (if (not (rinport-p p))
	(funcall (qsx:host-ref "read-line") p)
	(let ((mode (line-mode-name "read-line" mode)))
	  (multiple-value-bind (start stop) (buffered-line p mode)
	    (case start
	      ((nil) (let ((line (read-line-bytes p mode "read-line")))
		       (if (eq line qs:eof-object) line (decode-utf-8 line))))
	      (:eof qs:eof-object)
	      (t (decode-utf-8 (rinport-bytes p) start stop))))))))
(defnative "read-bytes-line" (&optional port (mode (rsym "linefeed"))) ()
  (read-line-bytes (in-port "read-bytes-line" port) (line-mode-name "read-bytes-line" mode) "read-bytes-line"))

(defun inport-location (p)
  (if (and (rinport-count-lines p)
	   (rinport-custom p)
	   (custom-in-location (rinport-custom p)))
      (funcall (custom-in-location (rinport-custom p)))
      (if (rinport-count-lines p)
	  (values (rinport-line p) (rinport-column p) (rinport-position p))
	  (values qs:false qs:false (1+ (+ (rinport-base p) (rinport-pos p)))))))

(defun outport-location (p)
  (if (and (routport-count-lines p)
	   (routport-custom p)
	   (custom-out-location (routport-custom p)))
      (funcall (custom-out-location (routport-custom p)))
      (if (routport-count-lines p)
	  (values (routport-line p) (routport-column p) (routport-position p))
	  (values qs:false qs:false (1+ (out-position p))))))

(defnative "port-count-lines!" (p) ()
  (setq p (unwrap-out (unwrap-in p)))
  (cond ((rinport-p p)
	 (unless (rinport-count-lines p)
	   (setf (rinport-count-lines p) t)
	   (let ((c (rinport-custom p)))
	     (when (and c (custom-in-count-lines c)) (funcall (custom-in-count-lines c))))))
	((routport-p p)
	 (unless (routport-count-lines p)
	   (setf (routport-count-lines p) t)
	   (let ((c (routport-custom p)))
	     (when (and c (custom-out-count-lines c)) (funcall (custom-out-count-lines c)))))))
  *void*)
(defnative "port-counts-lines?" (p) (:leaf t)
  (setq p (unwrap-out (unwrap-in p)))
  (bool (cond ((rinport-p p) (rinport-count-lines p)) ((routport-p p) (routport-count-lines p)))))
(defnative "port-next-location" (p) ()
  (setq p (unwrap-out (unwrap-in p)))
  (cond ((rinport-p p) (inport-location p))
	((routport-p p) (outport-location p))
	(t (values qs:false qs:false qs:false))))

(defun init-position-offset (init)
  "The bytes already read or written, for a custom port made with init-position INIT."
  (cond ((integerp init) (1- init))
	(t 0)))

(defun init-position-value (init base)
  "file-position of a custom port with init-position INIT after BASE more bytes."
  (cond ((functionp init)
	 (let ((v (funcall init))) (if (integerp v) (1- v) base)))
	((rinport-p init) (funcall (native-function (gethash "file-position" *natives*)) init))
	((routport-p init) (funcall (native-function (gethash "file-position" *natives*)) init))
	(t base)))

(defnative "file-position" (p &optional (pos nil pos-p)) ()
  (setq p (unwrap-out (unwrap-in p)))
  (cond ((and pos-p (rinport-p p))
	 (setf (rinport-pos p) (if (eq pos qs:eof-object) (ip-end p) (- pos (rinport-base p))))
	 *void*)
	((and pos-p (routport-p p))
	 (when (eq (routport-kind p) :file)
	   (finish-output (file-outport-stream p))
	   (file-position (file-outport-stream p) (if (eq pos qs:eof-object) (file-length (file-outport-stream p)) pos)))
	 *void*)
	((rinport-p p)
	 (let ((c (rinport-custom p)))
	   (if (and c (custom-in-init-position c) (not (integerp (custom-in-init-position c))))
	       (init-position-value (custom-in-init-position c) (+ (rinport-base p) (rinport-pos p)))
	       (+ (rinport-base p) (rinport-pos p)))))
	((routport-p p) (out-position p))
	(t 0)))
(defnative "file-position*" (p) ()
  (setq p (unwrap-out (unwrap-in p)))
  (cond ((rinport-p p) (+ (rinport-base p) (rinport-pos p)))
	((routport-p p) (out-position p))
	(t qs:false)))

(defun ip-ready (p)
  (let ((tmp (make-octets 1)))
    (not (eq (ip-peek-bytes p 0 tmp 0 1 nil) :block))))

(defnative "char-ready?" (&optional port) ()
  (let ((p (input-port "char-ready?" port)))
    (if (rinport-p p) (bool (ip-ready p)) (funcall (qsx:host-ref "char-ready?") p))))
(defnative "byte-ready?" (&optional port) ()
  (let ((p (input-port "byte-ready?" port)))
    (if (rinport-p p) (bool (ip-ready p)) (funcall (qsx:host-ref "char-ready?") p))))

;;; ------------------------------------------------------------------
;;; Closing

(defun post-closed (p)
  (let ((s (if (rinport-p p) (rinport-closed-sema p) (routport-closed-sema p))))
    (when s (incf (rsemaphore-count s)))))

(defun close-inport (p)
  (unless (rinport-closed p)
    (let ((c (rinport-custom p)))
      (when (and c (not (custom-in-closed c)))
	(setf (custom-in-closed c) t)
	(funcall (custom-in-close c))))
    (when (eq (rinport-kind p) :pipe)
      (setf (rinport-closed p) t))
    (setf (rinport-closed p) t)
    (ip-progress p)			; closing is progress
    (post-closed p)))

(defnative "close-input-port" (p) ()
  (setq p (unwrap-in p))
  (unless (or (rinport-p p) (truthy (funcall (qsx:host-ref "input-port?") p)))
    (contract-error "close-input-port" "input-port?" p))
  (if (rinport-p p) (close-inport p) (funcall (qsx:host-ref "close-input-port") p))
  *void*)

;;; ------------------------------------------------------------------
;;; Output

(defun output-port (port &optional (who "write"))
  (let ((p (unwrap-out (or port (current-port :output)))))
    (when (and (routport-p p) (routport-closed p))
      (racket-error who "output port is closed~%  output port: ~A" (port-desc p)))
    p))

(defun out-position (p)
  "The bytes written to P, for file-position."
  (ecase (routport-kind p)
    (:bytes (fill-pointer (routport-buffer p)))
    (:file (finish-output (file-outport-stream p)) (file-position (file-outport-stream p)))
    (:pipe (routport-written p))
    (:custom (let* ((c (routport-custom p)) (init (custom-out-init-position c)))
	       (if (and init (not (integerp init)))
		   (init-position-value init (routport-written p))
		   (+ (init-position-offset init) (routport-written p)))))))

(defun pipe-write (p vec start end)
  (let* ((in (routport-peer p))
	 (n (- end start)))
    (if (rinport-closed in)
	n
	(let* ((limit (rinport-limit in))
	       (room (if limit (- limit (pipe-content in)) n)))
	  (if (<= room 0)
	      :block
	      (let ((m (min n room)))
		(ip-append in vec start (+ start m))
		m))))))

(defun custom-write (p vec start end nonblock)
  (let* ((c (routport-custom p))
	 (w (custom-out-write-out c))
	 (n (- end start)))
    (cond
      ((routport-p w) (out-write-some w vec start end nonblock))
      ((streamp w)			; a host port to forward to
       (host-write-bytes w vec start end)
       n)
      (t
	(loop repeat 100000
	      do (let ((r (funcall w (subseq vec start end) 0 n (bool nonblock) qs:false)))
		   (cond ((integerp r)
			  (when (> r n)
			    (racket-error "user port write" "result integer is larger than the supplied byte string~%  result: ~D~%  byte string length: ~D" r n))
			  (if (plusp r)
			      (return r)
			      (racket-error "user port write" "bad result for non-flush write~%  result: ~A" r)))
			 ((eq r qs:false) (when nonblock (return :block)))
			 ((routport-p r)
			  (return (out-write-some r vec start end nonblock)))
			 ((evt-p r)
			  (if nonblock
			      (return :block)
			      (unless (nth-value 1 (evt-ready r))
				(racket-error "sync" "would block forever (one thread)"))))
			 (t (racket-error "user port write" "result is not (or/c exact-nonnegative-integer? #f evt?)~%  result: ~A"
					  (written r)))))
	      finally (racket-error "write" "would block forever (one thread)"))))))

;;; A file output port's bytes go to a buffer of its own, written to its
;;; stream 64 KB at a time: an SBCL fd-stream written a byte or a
;;; character at a time costs a call through its buffering each time.
;;; Whatever uses the stream gets it with FILE-OUTPORT-STREAM, which
;;; writes the buffer out first.

(defconstant +file-outport-buffer-size+ 65536)

(defun file-outport-write (p vec start end)
  (let ((buf (or (routport-obuf p)
		 (setf (routport-obuf p) (make-octets +file-outport-buffer-size+))))
	(n (- end start)))
    (declare (type (simple-array (unsigned-byte 8) (*)) buf) (fixnum n))
    (when (> (+ (routport-opos p) n) +file-outport-buffer-size+)
      (file-outport-drain p))
    (if (> n +file-outport-buffer-size+)
	(write-sequence vec (routport-stream p) :start start :end end)
	(let ((pos (routport-opos p)))
	  (declare (fixnum pos))
	  (if (< n 16)
	      (loop for i fixnum from start below end
		    for j fixnum from pos
		    do (setf (aref buf j) (aref vec i)))
	      (replace buf vec :start1 pos :start2 start :end2 end))
	  (setf (routport-opos p) (+ pos n))))))

(defun file-outport-drain (p)
  (when (plusp (routport-opos p))
    (write-sequence (routport-obuf p) (routport-stream p) :end (routport-opos p))
    (setf (routport-opos p) 0)))

(defun file-outport-stream (p)
  "File output port P's stream, its buffered bytes written to it."
  (file-outport-drain p)
  (routport-stream p))

(defun out-write-some (p vec start end nonblock)
  "Write some of VEC[START,END) to our output port P: how many bytes, or :block."
  (if (>= start end)
      0
      (let* ((n (- end start))
	     (r (ecase (routport-kind p)
		  (:bytes (let ((buffer (routport-buffer p)))
			    (loop for i from start below end do (vector-push-extend (aref vec i) buffer)))
			  n)
		  ;; into the stream's buffer, flushed when it fills, by
		  ;; flush-output or closing, and at exit (FLUSH-FILE-OUTPORTS),
		  ;; as Racket's block-buffered file ports are; unbuffered only
		  ;; in buffer mode none
		  (:file (file-outport-write p vec start end)
			 (when (eq (routport-buffer-mode p) (load-time-value (rsym "none")))
			   (finish-output (file-outport-stream p)))
			 n)
		  (:pipe (pipe-write p vec start end))
		  (:custom (custom-write p vec start end nonblock)))))
	(when (integerp r)
	  (incf (routport-written p) r)
	  (note-bytes p vec start (+ start r)))
	r)))

(defun out-emit (p vec &optional (start 0) (end (length vec)))
  "All of VEC[START,END) to our output port P."
  (let ((i start))
    (loop for tries from 0
	  while (< i end)
	  do (let ((r (out-write-some p vec i end nil)))
	       (cond ((integerp r) (incf i r) (setq tries 0))
		     ((> tries 100000) (racket-error "write" "would block forever (one thread)")))))))

(defun out-bytes (port bytes)
  (out-emit port bytes))

(defun host-write-bytes (p bytes start end)
  (funcall (qsx:host-ref "write-string") (decode-utf-8 bytes start end) p))

(defun port-write-string (string port &optional (who "write-string"))
  "STRING to PORT, ours or the host's."
  (let ((p (output-port port who)))
    (if (routport-p p)
	(out-emit p (bytes-of string))
	(funcall (qsx:host-ref "write-string") string p))))

(defnative "open-output-bytes" (&optional (name (rsym "string"))) ()
  (finish-outport (make-routport name)))
(defnative "open-output-string" (&optional (name (rsym "string"))) ()
  (finish-outport (make-routport name)))
(defnative "get-output-bytes" (p &optional reset? (start 0) end) ()
  (let ((b (subseq (coerce (routport-buffer p) 'octets) start end)))
    (when (and reset? (truthy reset?)) (setf (fill-pointer (routport-buffer p)) 0))
    b))
(defnative "get-output-string" (p) ()
  (decode-utf-8 (coerce (routport-buffer p) 'octets)))
(defnative "write-string" (s &optional port (start 0) end) ()
  (port-write-string (subseq s start end) port "write-string")
  (- (or end (length s)) start))
(defun encode-utf-8-char (c buf)
  "C's UTF-8 bytes into BUF, at least 4 long: how many."
  (let ((code (char-code c)))
    (cond ((< code #x80) (setf (aref buf 0) code) 1)
	  ((< code #x800)
	   (setf (aref buf 0) (logior #xC0 (ash code -6))
		 (aref buf 1) (logior #x80 (logand code #x3F)))
	   2)
	  ((< code #x10000)
	   (setf (aref buf 0) (logior #xE0 (ash code -12))
		 (aref buf 1) (logior #x80 (logand (ash code -6) #x3F))
		 (aref buf 2) (logior #x80 (logand code #x3F)))
	   3)
	  (t (setf (aref buf 0) (logior #xF0 (ash code -18))
		   (aref buf 1) (logior #x80 (logand (ash code -12) #x3F))
		   (aref buf 2) (logior #x80 (logand (ash code -6) #x3F))
		   (aref buf 3) (logior #x80 (logand code #x3F)))
	     4))))

(defun port-write-char (c port who)
  "Character C to PORT: its bytes from the port's own 4-byte buffer, which
every kind of port copies (Racket threads run one at a time here), not a
string and then its bytes, as WRITE-STRING would."
  (let ((p (output-port port who)))
    (cond ((not (routport-p p)) (funcall (qsx:host-ref "write-char") c p))
	  ;; an ASCII character to a file port that isn't counting lines:
	  ;; the byte into its buffer, directly
	  ((and (eq (routport-kind p) :file) (< (char-code c) #x80)
		(not (routport-count-lines p)) (routport-obuf p)
		(< (routport-opos p) +file-outport-buffer-size+)
		(not (eq (routport-buffer-mode p) (load-time-value (rsym "none")))))
	   (let ((buf (routport-obuf p)))
	     (declare (type (simple-array (unsigned-byte 8) (*)) buf))
	     (setf (aref buf (routport-opos p)) (char-code c))
	     (incf (routport-opos p))
	     (incf (routport-written p))))
	  (t (let ((buf (or (routport-cbuf p) (setf (routport-cbuf p) (make-octets 4)))))
	       (out-emit p buf 0 (encode-utf-8-char c buf)))))))

(defnative "write-char" (c &optional port) ()
  ;; an ASCII character to an open file port's buffer, not counting
  ;; lines, block or line buffered: as PORT-WRITE-CHAR's own fast path,
  ;; without the port's unwrapping
  (let ((p port))
    (if (and (routport-p p) (characterp c) (< (char-code c) #x80)
	     (eq (routport-kind p) :file) (not (routport-closed p))
	     (not (routport-count-lines p)) (routport-obuf p)
	     (< (routport-opos p) +file-outport-buffer-size+)
	     (not (eq (routport-buffer-mode p) (load-time-value (rsym "none")))))
	(let ((buf (routport-obuf p)))
	  (declare (type (simple-array (unsigned-byte 8) (*)) buf))
	  (setf (aref buf (routport-opos p)) (char-code c))
	  (incf (routport-opos p))
	  (incf (routport-written p)))
	(port-write-char c port "write-char")))
  *void*)
(defnative "newline" (&optional port) ()
  (port-write-char #\Newline port "newline")
  *void*)
(defnative "write-bytes" (b &optional port (start 0) end) ()
  (let* ((p (output-port port "write-bytes")) (end (or end (length b))))
    (if (routport-p p)
	(out-emit p b start end)
	(host-write-bytes p b start end))
    (- end start)))
(defnative "write-byte" (byte &optional port) ()
  (let ((p (output-port port "write-byte")))
    (if (routport-p p)
	(out-emit p (make-array 1 :element-type '(unsigned-byte 8) :initial-element byte))
	(funcall (qsx:host-ref "write-char") (code-char byte) p))
    *void*))

(defun flush-outport (p)
  (when (routport-closed p)
    (racket-error "flush-output" "output port is closed~%  output port: ~A" (port-desc p)))
  (case (routport-kind p)
    (:file (finish-output (file-outport-stream p)))
    (:custom
     (let ((w (custom-out-write-out (routport-custom p))))
       (cond
	 ((routport-p w) (flush-outport w))
	 ((streamp w) (funcall (qsx:host-ref "flush-output-port") w))
	 (t
	   (loop repeat 100000
		 do (let ((r (funcall w (make-octets 0) 0 0 qs:false qs:false)))
		      (cond ((eql r 0) (return))
			    ((evt-p r) (unless (nth-value 1 (evt-ready r))
					 (racket-error "sync" "would block forever (one thread)")))
			    ((eq r qs:false))
			    (t (racket-error "flush-output" "weird result"))))
		 finally (racket-error "flush-output" "would block forever (one thread)"))))))))

(defnative "flush-output" (&optional port) ()
  (let ((p (output-port port "flush-output")))
    (if (routport-p p)
	(flush-outport p)
	(funcall (qsx:host-ref "flush-output-port") p))
    *void*))

(defun close-outport (p)
  (unless (routport-closed p)
    (ecase (routport-kind p)
      (:bytes nil)
      (:file (finish-output (file-outport-stream p)) (close (file-outport-stream p)))
      (:pipe (setf (rinport-writer-closed (routport-peer p)) t))
      (:custom (funcall (custom-out-close (routport-custom p)))))
    (setf (routport-closed p) t)
    (post-closed p)))

(defnative "close-output-port" (p) ()
  (setq p (unwrap-out p))
  (unless (or (routport-p p) (truthy (funcall (qsx:host-ref "output-port?") p)))
    (contract-error "close-output-port" "output-port?" p))
  (if (routport-p p) (close-outport p) (funcall (qsx:host-ref "close-output-port") p))
  *void*)

;;; ------------------------------------------------------------------
;;; Ports in general

(defnative "input-port?" (x) (:leaf t)
  (setq x (unwrap-in x))
  (bool (or (rinport-p x) (truthy (funcall (qsx:host-ref "input-port?") x)))))
(defnative "output-port?" (x) (:leaf t)
  (setq x (unwrap-out x))
  (bool (or (routport-p x) (truthy (funcall (qsx:host-ref "output-port?") x)))))
(defnative "port?" (x) (:leaf t)
  (setq x (unwrap-out (unwrap-in x)))
  (bool (or (rinport-p x) (routport-p x) (truthy (funcall (qsx:host-ref "port?") x)))))
(defnative "port-closed?" (p) (:leaf t)
  (setq p (unwrap-out (unwrap-in p)))
  (bool (cond ((rinport-p p) (rinport-closed p)) ((routport-p p) (routport-closed p)))))
(defnative "file-stream-port?" (p) (:leaf t)
  (bool (or (and (rinport-p p) (rpath-p (rinport-name p)))
	    (and (routport-p p) (eq (routport-kind p) :file)))))
