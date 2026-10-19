; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-RACKET -*-

;;;; More of Racket's ports, natively (ports.lisp has the ports and the
;;;; common operations): custom ports (make-input-port,
;;;; make-output-port), pipes, peeking and reading into byte strings,
;;;; specials, progress events and commits, file output ports and the
;;;; file-stream operations, file identity and permissions.
;;;; Semantics are those of Racket's io layer (racket/src/io/port/).
;;;; With one thread, an event the user's procedures return is "try
;;;; again" if it isn't ready (a blocking operation on it is an error),
;;;; and a file lock always succeeds.

(in-package "QUASISCHEME-RACKET")

;;; ------------------------------------------------------------------
;;; Checking arguments

(defun contract-error (who expected value)
  (racket-error who "contract violation~%  expected: ~A~%  given: ~A" expected (written value)))

(defun arity-includes-p (f k)
  (and (functionp f)
       (truthy (funcall (native-function (gethash "procedure-arity-includes?" *natives*)) f k))))

(defun check-proc (who f k contract &optional or-false)
  (unless (or (and or-false (eq f qs:false)) (arity-includes-p f k))
    (contract-error who (if or-false (format nil "(or/c ~A #f)" contract) contract) f)))

(defun false-to-nil (x) (if (eq x qs:false) nil x))

(defun check-range (who start end len what value)
  (when (> start len)
    (racket-error who "starting index is out of range~%  starting index: ~D~%  valid range: [0, ~D]~%  ~A: ~A"
		  start len what (written value)))
  (when (> end len)
    (racket-error who "ending index is out of range~%  ending index: ~D~%  valid range: [0, ~D]~%  ~A: ~A"
		  end len what (written value)))
  (when (< end start)
    (racket-error who "ending index is smaller than starting index~%  ending index: ~D~%  starting index: ~D~%  valid range: [0, ~D]~%  ~A: ~A"
		  end start len what (written value))))

(defun check-skip (who skip)
  (unless (and (integerp skip) (>= skip 0))
    (contract-error who "exact-nonnegative-integer?" skip)))

(defun evt-ready-p (e)
  "Whether event E is ready, without taking a semaphore."
  (cond ((rsemaphore-p e) (plusp (rsemaphore-count e)))
	((revt-p e) (nth-value 1 (evt-ready e)))
	(t t)))

(defun need-in (who port)
  "PORT, which must be an input port, as one of ours."
  (setq port (unwrap-in port))
  (unless (or (null port) (rinport-p port) (truthy (funcall (qsx:host-ref "input-port?") port)))
    (contract-error who "input-port?" port))
  (in-port who port))

(defun need-out (who port)
  (setq port (unwrap-out port))
  (unless (or (null port) (routport-p port) (truthy (funcall (qsx:host-ref "output-port?") port)))
    (contract-error who "output-port?" port))
  (output-port port who))

(defun call-with-parameter (parameter value thunk)
  "THUNK called with PARAMETER (a parameter) parameterized to VALUE."
  (funcall (native-function (gethash "call-with-parameterization" *natives*))
	   (funcall (native-function (gethash "extend-parameterization" *natives*))
		    (current-parameterization) parameter value)
	   thunk))

;;; ------------------------------------------------------------------
;;; Custom ports

(defun check-init-position (who x)
  (unless (or (and (integerp x) (plusp x)) (rinport-p x) (routport-p x) (eq x qs:false)
	      (arity-includes-p x 0))
    (contract-error who "(or/c exact-positive-integer? port? #f (procedure-arity-includes/c 0))" x)))

(defun check-buffer-mode-proc (who x)
  (unless (or (eq x qs:false) (and (arity-includes-p x 0) (arity-includes-p x 1)))
    (contract-error who "(or/c #f (and/c (procedure-arity-includes/c 0)\n                (procedure-arity-includes/c 1)))" x)))

(defun void-procedure () (lambda (&rest args) (declare (ignore args)) *void*))

(defnative "make-input-port" (name read-in peek-in close &optional (progress qs:false) (commit qs:false)
				   (location qs:false) (count-lines nil) (init-position 1) (buffer-mode qs:false)) ()
  (let ((who "make-input-port")
	(count-lines (if (null count-lines) (void-procedure) count-lines)))
    (unless (or (rinport-p read-in) (arity-includes-p read-in 1))
      (contract-error who "(or/c (procedure-arity-includes/c 1) input-port?)" read-in))
    (unless (or (eq peek-in qs:false) (rinport-p peek-in) (arity-includes-p peek-in 3))
      (contract-error who "(or/c (procedure-arity-includes/c 3) input-port? #f)" peek-in))
    (check-proc who close 0 "(procedure-arity-includes/c 0)")
    (check-proc who progress 0 "(procedure-arity-includes/c 0)" t)
    (check-proc who commit 3 "(procedure-arity-includes/c 3)" t)
    (check-proc who location 0 "(procedure-arity-includes/c 0)" t)
    (check-proc who count-lines 0 "(procedure-arity-includes/c 0)" t)
    (check-init-position who init-position)
    (check-buffer-mode-proc who buffer-mode)
    (unless (eq (rinport-p read-in) (rinport-p peek-in))
      (racket-error who "~A~%  read argument: ~A~%  peek argument: ~A"
		    (if (rinport-p read-in)
			"read argument is an input port, but peek argument is not a port"
			"read argument is not an input port, but peek argument is a port")
		    (written read-in) (written peek-in)))
    (when (and (eq peek-in qs:false) (truthy progress))
      (racket-error who "peek argument is #f, but progress-evt argument is not~%  progress-evt argument: ~A"
		    (written progress)))
    (when (and (eq progress qs:false) (truthy commit))
      (racket-error who "progress-evt argument is #f, but commit argument is not~%  commit argument: ~A"
		    (written commit)))
    (when (and (eq commit qs:false) (truthy progress))
      (racket-error who "commit argument is #f, but progress-evt argument is not~%  progress-evt argument: ~A"
		    (written progress)))
    (let* ((init (false-to-nil init-position))
	   (c (make-custom-in :read-in read-in :peek-in (false-to-nil peek-in) :close close
			      :progress (false-to-nil progress) :commit (false-to-nil commit)
			      :location (false-to-nil location) :count-lines (false-to-nil count-lines)
			      :init-position init :buffer-mode (false-to-nil buffer-mode)))
	   (p (new-lazy-inport name :custom (if (eq peek-in qs:false) #'custom-fill nil) :custom c)))
      (setf (rinport-base p) (init-position-offset init))
      (when (integerp init) (setf (rinport-position p) init))
      p)))

(defnative "make-output-port" (name evt write-out close &optional (write-out-special qs:false)
				    (get-write-evt qs:false) (get-write-special-evt qs:false)
				    (location qs:false) (count-lines nil) (init-position 1) (buffer-mode qs:false)) ()
  (let ((who "make-output-port")
	(count-lines (if (null count-lines) (void-procedure) count-lines)))
    (unless (evt-value-p evt) (contract-error who "evt?" evt))
    (setq write-out (unwrap-out write-out))
    (unless (or (routport-p write-out) (and (streamp write-out) (output-stream-p write-out))
		(arity-includes-p write-out 5))
      (contract-error who "(or/c output-port? (procedure-arity-includes/c 5))" write-out))
    (check-proc who close 0 "(procedure-arity-includes/c 0)")
    (unless (or (eq write-out-special qs:false) (routport-p write-out-special) (arity-includes-p write-out-special 3))
      (contract-error who "(or/c #f output-port? (procedure-arity-includes/c 3))" write-out-special))
    (check-proc who get-write-evt 3 "(procedure-arity-includes/c 3)" t)
    (check-proc who get-write-special-evt 1 "(procedure-arity-includes/c 1)" t)
    (check-proc who location 0 "(procedure-arity-includes/c 0)" t)
    (check-proc who count-lines 0 "(procedure-arity-includes/c 0)" t)
    (check-init-position who init-position)
    (check-buffer-mode-proc who buffer-mode)
    (when (and (eq write-out-special qs:false) (truthy get-write-special-evt))
      (racket-error who "write-special argument is #f, but get-write-special-evt argument is not~%  get-write-special-evt argument: ~A"
		    (written get-write-special-evt)))
    (when (and (eq get-write-evt qs:false) (truthy get-write-special-evt))
      (racket-error who "get-write-evt argument is #f, but get-write-special-evt argument is not~%  get-write-special-evt argument: ~A"
		    (written get-write-special-evt)))
    (when (and (eq get-write-special-evt qs:false) (truthy get-write-evt) (truthy write-out-special))
      (racket-error who "get-write-special-evt argument is #f, but get-write-evt argument is not, and write-special argument is not~%  get-write-evt argument: ~A~%  get-write-special-evt argument: ~A"
		    (written get-write-evt) (written get-write-special-evt)))
    (let* ((init (false-to-nil init-position))
	   (c (make-custom-out :write-out write-out :close close
			       :write-out-special (false-to-nil write-out-special)
			       :get-write-evt (false-to-nil get-write-evt)
			       :get-write-special-evt (false-to-nil get-write-special-evt)
			       :location (false-to-nil location) :count-lines (false-to-nil count-lines)
			       :init-position init :buffer-mode (false-to-nil buffer-mode)))
	   (p (finish-outport (make-routport name))))
      (setf (routport-kind p) :custom (routport-custom p) c)
      (when (integerp init) (setf (routport-position p) init))
      p)))

;;; ------------------------------------------------------------------
;;; Pipes

(defun pipe-fill (p need blocking)
  (declare (ignore need blocking))
  (if (rinport-writer-closed p) :eof :block))

(defnative "make-pipe" (&optional (limit qs:false) (in-name (rsym "pipe")) (out-name (rsym "pipe"))) ()
  (unless (or (eq limit qs:false) (and (integerp limit) (plusp limit)))
    (contract-error "make-pipe" "(or/c exact-positive-integer? #f)" limit))
  (let ((in (new-lazy-inport (if (eq in-name qs:false) (rsym "pipe") in-name) :pipe #'pipe-fill))
	(out (finish-outport (make-routport (if (eq out-name qs:false) (rsym "pipe") out-name)))))
    (setf (rinport-limit in) (false-to-nil limit)
	  (rinport-peer in) out
	  (routport-kind out) :pipe
	  (routport-peer out) in)
    (values in out)))

(defun pipe-input-of (p)
  (cond ((and (rinport-p p) (eq (rinport-kind p) :pipe)) p)
	((and (routport-p p) (eq (routport-kind p) :pipe)) (routport-peer p))))

(defnative "pipe-port?" (x) (:leaf t) (bool (pipe-input-of x)))
(defnative "pipe-content-length" (p) (:leaf t)
  (let ((in (pipe-input-of p)))
    (unless in (contract-error "pipe-content-length" "pipe-port?" p))
    (pipe-content in)))

;;; ------------------------------------------------------------------
;;; Reading and peeking into byte strings and strings

(defun check-bytes (who x)
  (unless (typep x 'octets) (contract-error who "bytes?" x)))

(defun transfer-exact (who bstr port start end skip)
  "read-bytes! (SKIP NIL) and peek-bytes!: fill BSTR[START,END) from PORT, waiting
for all of it; the count, or eof if there was none."
  (check-bytes who bstr)
  (let ((end (or end (length bstr))))
    (check-range who start end (length bstr) "byte string" bstr)
    (let* ((p (need-in who port)) (n (- end start)) (got 0))
      (loop while (< got n)
	    do (let ((r (if skip
			    (ip-peek-bytes p (+ skip got) bstr (+ start got) end t)
			    (ip-read-bytes p bstr (+ start got) end t))))
		 (cond ((integerp r) (incf got r))
		       ((functionp r) (special-error who p))
		       (t (return)))))
      (if (and (zerop got) (plusp n)) qs:eof-object got))))

(defnative "read-bytes!" (bstr &optional port (start 0) end) ()
  (transfer-exact "read-bytes!" bstr port start end nil))
(defnative "peek-bytes!" (bstr skip &optional port (start 0) end) ()
  (check-skip "peek-bytes!" skip)
  (transfer-exact "peek-bytes!" bstr port start end skip))

(defun transfer-avail (who bstr port start end skip blocking &optional progress)
  "The *-avail! operations: some bytes, at least one if BLOCKING (a
count, eof or a special); 0 for none yet."
  (check-bytes who bstr)
  (let ((end (or end (length bstr))))
    (check-range who start end (length bstr) "byte string" bstr)
    (let ((p (if (and progress (rinport-p port) (rinport-closed port) (evt-ready-p progress))
		 port			; a closed port's progress evt is ready: 0, no error
		 (need-in who port))))
      (cond ((= start end) 0)
	    ((and progress (evt-ready-p progress)) 0)
	    (t (let ((r (cond ((and skip progress (peek-proc-port-p p))
			       (custom-peek p skip bstr start end blocking progress))
			      (skip (ip-peek-bytes p skip bstr start end blocking))
			      (t (ip-read-bytes p bstr start end blocking)))))
		 (cond ((eq r :eof) qs:eof-object)
		       ((member r '(:block :progress)) 0)
		       (t r))))))))

(defnative "read-bytes-avail!" (bstr &optional port (start 0) end) ()
  (transfer-avail "read-bytes-avail!" bstr port start end nil t))
(defnative "read-bytes-avail!*" (bstr &optional port (start 0) end) ()
  (transfer-avail "read-bytes-avail!*" bstr port start end nil nil))
(defnative "read-bytes-avail!/enable-break" (bstr &optional port (start 0) end) ()
  (transfer-avail "read-bytes-avail!/enable-break" bstr port start end nil t))

(defun progress-arg (progress)
  (and progress (truthy progress) progress))

(defnative "peek-bytes-avail!" (bstr skip &optional progress port (start 0) end) ()
  (check-skip "peek-bytes-avail!" skip)
  (transfer-avail "peek-bytes-avail!" bstr port start end skip t (progress-arg progress)))
(defnative "peek-bytes-avail!*" (bstr skip &optional progress port (start 0) end) ()
  (check-skip "peek-bytes-avail!*" skip)
  (transfer-avail "peek-bytes-avail!*" bstr port start end skip nil (progress-arg progress)))
(defnative "peek-bytes-avail!/enable-break" (bstr skip &optional progress port (start 0) end) ()
  (check-skip "peek-bytes-avail!/enable-break" skip)
  (transfer-avail "peek-bytes-avail!/enable-break" bstr port start end skip t (progress-arg progress)))

(defun chars-into (who str port start end skip)
  "read-string! (SKIP NIL) and peek-string!."
  (unless (stringp str) (contract-error who "string?" str))
  (let ((end (or end (length str))))
    (check-range who start end (length str) "string" str)
    (let* ((p (need-in who port)) (n (- end start)) (got 0) (offset (or skip 0)))
      (loop while (< got n)
	    do (if skip
		   (multiple-value-bind (c len) (inport-peek-char p offset who)
		     (when (null c) (return))
		     (setf (char str (+ start got)) c)
		     (incf offset len)
		     (incf got))
		   (let ((c (inport-read-char p who)))
		     (when (eq c qs:eof-object) (return))
		     (setf (char str (+ start got)) c)
		     (incf got))))
      (if (and (zerop got) (plusp n)) qs:eof-object got))))

(defnative "read-string!" (str &optional port (start 0) end) ()
  (chars-into "read-string!" str port start end nil))
(defnative "peek-string!" (str skip &optional port (start 0) end) ()
  (check-skip "peek-string!" skip)
  (chars-into "peek-string!" str port start end skip))
(defnative "peek-string" (amt skip &optional port) ()
  (check-count "peek-string" amt)
  (check-skip "peek-string" skip)
  (let* ((p (need-in "peek-string" port))
	 (out (make-string-output-stream))
	 (got 0) (offset skip))
    (loop while (< got amt)
	  do (multiple-value-bind (c len) (inport-peek-char p offset "peek-string")
	       (when (null c) (return))
	       (write-char c out)
	       (incf offset len)
	       (incf got)))
    (if (and (zerop got) (plusp amt)) qs:eof-object (get-output-stream-string out))))

(defnative "read-byte-or-special" (&optional port special-wrap (source-name qs:false)) ()
  (let* ((p (need-in "read-byte-or-special" port))
	 (b (inport-read-byte p "read-byte-or-special" t)))
    (if (functionp b) (wrap-special p b source-name -1 special-wrap) b)))

(defnative "peek-byte-or-special" (&optional port (skip 0) progress special-wrap (source-name qs:false)) ()
  (check-skip "peek-byte-or-special" skip)
  (let ((p (need-in "peek-byte-or-special" port)))
    (if (and progress (truthy progress) (evt-ready-p progress))
	qs:false
	(let ((b (inport-peek-byte p skip "peek-byte-or-special" t)))
	  (if (functionp b) (wrap-special p b source-name skip special-wrap) b)))))

;;; ------------------------------------------------------------------
;;; Progress events and commits

(defvar *progress-evts* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "A progress event -> the input port it is for.")

(defnative "port-provides-progress-evts?" (p) (:leaf t)
  (bool (and (rinport-p p)
	     (not (eq (rinport-kind p) :host))
	     (let ((c (rinport-custom p)))
	       (or (null c) (custom-in-progress c) (null (custom-in-peek-in c)))))))

(defnative "port-progress-evt" (&optional port) ()
  (let* ((p (or port (current-port :input)))
	 (c (and (rinport-p p) (rinport-custom p))))
    (cond ((and (rinport-p p) (rinport-closed p)) ; ready from the start
	   (let ((e (make-revt :peek (make-rsemaphore 1))))
	     (setf (gethash e *progress-evts*) p)
	     e))
	  ((not (rinport-p p))
	   (racket-error "port-progress-evt" "port does not provide progress evts~%  port: ~A" (written p)))
	  ((eq (rinport-kind p) :host)
	   (racket-error "port-progress-evt" "port does not provide progress evts~%  port: ~A" (port-desc p)))
	  ((and c (custom-in-progress c))
	   (let ((e (funcall (custom-in-progress c))))
	     (unless (evt-value-p e)
	       (racket-error "user port progress-evt" "contract violation~%  expected: evt?~%  result: ~A" (written e)))
	     (setf (gethash e *progress-evts*) p)
	     e))
	  ((and c (custom-in-peek-in c))
	   (racket-error "port-progress-evt" "port does not provide progress evts~%  port: ~A" (port-desc p)))
	  (t (let ((e (make-revt :peek (or (rinport-progress p)
					   (setf (rinport-progress p) (make-rsemaphore 0))))))
	       (setf (gethash e *progress-evts*) p)
	       e)))))

(defnative "port-commit-peeked" (amt progress evt &optional port) ()
  (check-count "port-commit-peeked" amt)
  (let ((p (input-port "port-commit-peeked" port)))
    (unless (and (rinport-p p) (eq (gethash progress *progress-evts*) p))
      (racket-error "port-commit-peeked" "evt is not a progress evt for the given port~%  evt: ~A~%  port: ~A"
		    (written progress) (written p)))
    (let ((c (rinport-custom p)))
      (cond ((and c (custom-in-commit c))
	     (let ((r (funcall (custom-in-commit c) amt progress evt)))
	       (cond ((eq r qs:false) qs:false)
		     (t (let ((vec (if (typep r 'octets) r (make-array amt :element-type '(unsigned-byte 8)
									 :initial-element (char-code #\x)))))
			  (note-bytes p vec 0 (length vec))
			  (ip-progress p)
			  qs:true)))))
	    ((peek-proc-port-p p) qs:false)
	    ((not (eq (rinport-progress p) (revt-target progress))) qs:false)
	    ((not (evt-ready-p evt)) qs:false)
	    (t (let ((n (min amt (- (ip-end p) (rinport-pos p)))))
		 (note-bytes p (rinport-bytes p) (rinport-pos p) (+ (rinport-pos p) n))
		 (ip-advance p n)
		 qs:true))))))

(defnative "port-closed-evt" (p) ()
  (setq p (unwrap-out (unwrap-in p)))
  (cond ((rinport-p p)
	 (make-revt :peek (or (rinport-closed-sema p)
			      (setf (rinport-closed-sema p) (make-rsemaphore (if (rinport-closed p) 1 0))))))
	((routport-p p)
	 (make-revt :peek (or (routport-closed-sema p)
			      (setf (routport-closed-sema p) (make-rsemaphore (if (routport-closed p) 1 0))))))
	((or (truthy (funcall (qsx:host-ref "input-port?") p)) (truthy (funcall (qsx:host-ref "output-port?") p)))
	 *never-evt*)
	(t (contract-error "port-closed-evt" "port?" p))))

(defnative "port-waiting-peer?" (p) (:leaf t) (declare (ignore p)) qs:false)

;;; ------------------------------------------------------------------
;;; Writing

(defun write-some-to (who bstr port start end star)
  "write-bytes-avail and the rest: some bytes, at least one unless STAR."
  (check-bytes who bstr)
  (let ((end (or end (length bstr))))
    (check-range who start end (length bstr) "byte string" bstr)
    (let ((p (need-out who port)))
      (cond ((= start end) 0)
	    ((routport-p p)
	     (loop for tries from 0
		   do (let ((r (out-write-some p bstr start end t)))
			(cond ((integerp r) (return r))
			      (star (return 0))
			      ((> tries 100000) (racket-error who "would block forever (one thread)"))))))
	    (t (host-write-bytes p bstr start end)
	       (- end start))))))

(defnative "write-bytes-avail" (bstr &optional port (start 0) end) ()
  (write-some-to "write-bytes-avail" bstr port start end nil))
(defnative "write-bytes-avail*" (bstr &optional port (start 0) end) ()
  (write-some-to "write-bytes-avail*" bstr port start end t))
(defnative "write-bytes-avail/enable-break" (bstr &optional port (start 0) end) ()
  (write-some-to "write-bytes-avail/enable-break" bstr port start end nil))

(defun write-evt-for (thunk)
  "An event that is ready at once, whose result is what THUNK returns
(sync takes the :thunk kind of revt)."
  (make-revt :thunk thunk))

(defnative "write-bytes-avail-evt" (bstr &optional port (start 0) end) ()
  (check-bytes "write-bytes-avail-evt" bstr)
  (let* ((end (or end (length bstr)))
	 (p (need-out "write-bytes-avail-evt" port))
	 (c (and (routport-p p) (routport-custom p))))
    (check-range "write-bytes-avail-evt" start end (length bstr) "byte string" bstr)
    (cond ((and c (custom-out-get-write-evt c))
	   (funcall (custom-out-get-write-evt c) (subseq bstr start end) 0 (- end start)))
	  ((and c (not (or (routport-p (custom-out-write-out c)) (streamp (custom-out-write-out c)))))
	   (racket-error "write-bytes-avail-evt" "port does not support output events~%  port: ~A" (port-desc p)))
	  (t (write-evt-for (lambda () (write-some-to "write-bytes-avail-evt" bstr p start end nil)))))))

(defun write-special-to (who v port nonblock)
  (let* ((p (need-out who port))
	 (c (and (routport-p p) (routport-custom p)))
	 (proc (and c (custom-out-write-out-special c))))
    (cond ((null proc)
	   (racket-error who "port does not support special values~%  port: ~A" (port-desc p)))
	  ((routport-p proc) (write-special-to who v proc nonblock))
	  (t (loop repeat 100000
		   do (let ((r (funcall proc v (bool nonblock) qs:false)))
			(cond ((eq r qs:false) (when nonblock (return qs:false)))
			      ((evt-p r)
			       (if nonblock
				   (return qs:false)
				   (unless (nth-value 1 (evt-ready r))
				     (racket-error "sync" "would block forever (one thread)"))))
			      (t (return qs:true))))
		   finally (racket-error who "would block forever (one thread)"))))))

(defnative "write-special" (v &optional port) ()
  (write-special-to "write-special" v port nil))
(defnative "write-special-avail*" (v &optional port) ()
  (write-special-to "write-special-avail*" v port t))
(defnative "write-special-evt" (v &optional port) ()
  (let* ((p (need-out "write-special-evt" port))
	 (c (and (routport-p p) (routport-custom p))))
    (if (and c (custom-out-get-write-special-evt c))
	(funcall (custom-out-get-write-special-evt c) v)
	(racket-error "write-special-evt" "port does not support special values~%  port: ~A" (port-desc p)))))

(defnative "port-writes-atomic?" (p) (:leaf t)
  (setq p (unwrap-out p))
  (cond ((routport-p p)
	 (let ((c (routport-custom p)))
	   (bool (or (null c) (custom-out-get-write-evt c)))))
	((truthy (funcall (qsx:host-ref "output-port?") p)) qs:true)
	(t (contract-error "port-writes-atomic?" "output-port?" p))))
(defnative "port-writes-special?" (p) (:leaf t)
  (setq p (unwrap-out p))
  (cond ((routport-p p)
	 (let ((c (routport-custom p)))
	   (bool (and c (custom-out-write-out-special c)))))
	((truthy (funcall (qsx:host-ref "output-port?") p)) qs:false)
	(t (contract-error "port-writes-special?" "output-port?" p))))

;;; ------------------------------------------------------------------
;;; Files

(defun mode-name (m)
  (and m (symbolp m) (not (eq m qs:false)) (not (eq m qs:true)) (rname m)))

(defun exists-mode (m1 m2)
  (dolist (m (list m1 m2) "error")
    (let ((n (mode-name m)))
      (when (member n '("error" "replace" "truncate" "must-truncate" "truncate/replace" "update"
			"must-update" "can-update" "append")
		    :test #'equal)
	(return n)))))

(defun open-output-error (who name errno)
  (cond ((= errno 17)
	 (do-raise (make-exn "exn:fail:filesystem:exists" (format nil "~A: file exists~%  path: ~A" who name))))
	((= errno 21) (raise-errno who "path is a directory" name errno))
	(t (raise-errno who "error opening file" name errno))))

(defun open-output-fd (who name exists perms)
  "A file descriptor for writing NAME as EXISTS (a mode's name) says."
  (let* ((flags (cond ((member exists '("truncate" "truncate/replace") :test #'string=)
		       (logior sb-posix:o-creat sb-posix:o-trunc))
		      ((string= exists "must-truncate") sb-posix:o-trunc)
		      ((string= exists "can-update") sb-posix:o-creat)
		      ((member exists '("update" "must-update") :test #'string=) 0)
		      ((string= exists "append") (logior sb-posix:o-creat sb-posix:o-append))
		      (t (logior sb-posix:o-creat sb-posix:o-excl))))
	 (flags (logior sb-posix:o-wronly flags)))
    (handler-case (sb-posix:open name flags perms)
      (sb-posix:syscall-error (e)
	(let ((errno (sb-posix:syscall-errno e)))
	  (if (and (member errno '(17 13))
		   (member exists '("replace" "truncate/replace") :test #'string=))
	      (handler-case (progn (sb-posix:unlink name) (sb-posix:open name flags perms))
		(sb-posix:syscall-error (e2) (open-output-error who name (sb-posix:syscall-errno e2))))
	      (open-output-error who name errno)))))))

(defun make-file-outport (name fd)
  (let ((p (finish-outport (make-routport (make-rpath name)))))
    (setf (routport-kind p) :file
	  (routport-path p) name
	  (routport-stream p) (sb-sys:make-fd-stream fd :output t :element-type '(unsigned-byte 8)
							:buffering :full :file name))
    p))

(defun file-follower (name)
  "The fill function of an input port that follows the file NAME as it grows."
  (let ((stream (sb-sys:make-fd-stream (sb-posix:open name sb-posix:o-rdonly)
				       :input t :element-type '(unsigned-byte 8) :buffering :full :file name)))
    (lambda (p need blocking)
      (declare (ignore need blocking))
      (let* ((buf (make-octets 4096))
	     (n (read-sequence buf stream)))
	(if (zerop n)
	    :eof
	    (progn (ip-append p buf 0 n) :data))))))

(defun open-output (who path m1 m2 perms plus-input)
  (let* ((name (path-string who path))
	 (exists (exists-mode m1 m2))
	 (perms (if (integerp perms) perms #o666))
	 (fd (open-output-fd who name exists perms))
	 (out (make-file-outport name fd)))
    (if plus-input
	(let ((in (new-lazy-inport (make-rpath name) :static (file-follower name))))
	  (values in out))
	out)))

(defnative "open-output-file" (path &optional m1 m2 perms replace-perms?) ()
  (declare (ignore replace-perms?))
  (open-output "open-output-file" path m1 m2 perms nil))
(defnative "open-input-output-file" (path &optional m1 m2 perms replace-perms?) ()
  (declare (ignore replace-perms?))
  (open-output "open-input-output-file" path m1 m2 perms t))

(defnative "call-with-input-file" (path proc &optional mode) ()
  (path-string "call-with-input-file" path)
  (check-proc "call-with-input-file" proc 1 "(procedure-arity-includes/c 1)")
  (let ((in (funcall (native-function (gethash "open-input-file" *natives*)) path mode)))
    (multiple-value-prog1 (funcall proc in)
      (close-inport in))))

(defnative "call-with-output-file" (path proc &optional m1 m2 perms replace-perms?) ()
  (declare (ignore replace-perms?))
  (path-string "call-with-output-file" path)
  (check-proc "call-with-output-file" proc 1 "(procedure-arity-includes/c 1)")
  (let ((out (open-output "call-with-output-file" path m1 m2 perms nil)))
    (multiple-value-prog1 (funcall proc out)
      (close-outport out))))

(defnative "with-input-from-file" (path proc &optional mode) ()
  (path-string "with-input-from-file" path)
  (check-proc "with-input-from-file" proc 0 "(procedure-arity-includes/c 0)")
  (let ((in (funcall (native-function (gethash "open-input-file" *natives*)) path mode)))
    (funcall (qsx:host-ref "dynamic-wind")
	     (lambda () *void*)
	     (lambda () (call-with-parameter *current-input-port* in proc))
	     (lambda () (close-inport in)))))

(defnative "with-output-to-file" (path proc &optional m1 m2 perms replace-perms?) ()
  (declare (ignore replace-perms?))
  (path-string "with-output-to-file" path)
  (check-proc "with-output-to-file" proc 0 "(procedure-arity-includes/c 0)")
  (let ((out (open-output "with-output-to-file" path m1 m2 perms nil)))
    (funcall (qsx:host-ref "dynamic-wind")
	     (lambda () *void*)
	     (lambda () (call-with-parameter *current-output-port* out proc))
	     (lambda () (close-outport out)))))

(defun file-stream-fd (p)
  "The descriptor of file output port P."
  (sb-sys:fd-stream-fd (routport-stream p)))

(defnative "file-stream-buffer-mode" (port &optional (mode nil mode-p)) ()
  (let ((c (cond ((rinport-p port) (rinport-custom port))
		 ((routport-p port) (routport-custom port)))))
    (cond ((or (and (rinport-p port) (rpath-p (rinport-name port)))
	       (and (routport-p port) (eq (routport-kind port) :file)))
	   (if mode-p
	       (progn (if (rinport-p port) (setf (rinport-buffer-mode port) mode) (setf (routport-buffer-mode port) mode))
		      *void*)
	       (or (if (rinport-p port) (rinport-buffer-mode port) (routport-buffer-mode port)) (rsym "block"))))
	  ((and c (rinport-p port) (custom-in-buffer-mode c))
	   (if mode-p (funcall (custom-in-buffer-mode c) mode) (funcall (custom-in-buffer-mode c))))
	  ((and c (routport-p port) (custom-out-buffer-mode c))
	   (if mode-p (funcall (custom-out-buffer-mode c) mode) (funcall (custom-out-buffer-mode c))))
	  ((or (rinport-p port) (routport-p port))
	   (if mode-p
	       (racket-error "file-stream-buffer-mode" "buffering not supported for ~A port~%  mode: ~A~%  ~A port: ~A"
			     (if (rinport-p port) "input" "output") (written mode)
			     (if (rinport-p port) "input" "output") (port-desc port))
	       qs:false))
	  ((or (truthy (funcall (qsx:host-ref "input-port?") port))
	       (truthy (funcall (qsx:host-ref "output-port?") port)))
	   (if mode-p *void* (rsym "block")))
	  (t (contract-error "file-stream-buffer-mode" "port?" port)))))

(defnative "file-truncate" (port pos) ()
  (unless (and (routport-p port) (eq (routport-kind port) :file))
    (contract-error "file-truncate" "(and/c output-port? file-stream-port?)" port))
  (check-count "file-truncate" pos)
  (when (routport-closed port)
    (racket-error "file-truncate" "output port is closed~%  output port: ~A" (port-desc port)))
  (finish-output (routport-stream port))
  (sb-posix:ftruncate (file-stream-fd port) pos)
  *void*)

(defnative "string-port?" (x) (:leaf t)
  (bool (or (and (rinport-p x) (eq (rinport-kind x) :string))
	    (and (routport-p x) (eq (routport-kind x) :bytes)))))

(defnative "set-port-next-location!" (port line col pos) (:leaf t)
  (cond ((and (rinport-p port) (rinport-count-lines port))
	 (setf (rinport-line port) line (rinport-column port) col (rinport-position port) pos))
	((and (routport-p port) (routport-count-lines port))
	 (setf (routport-line port) line (routport-column port) col (routport-position port) pos))
	((not (or (rinport-p port) (routport-p port)))
	 (contract-error "set-port-next-location!" "port?" port)))
  *void*)

(defnative "terminal-file-position" () (:leaf t) 0)

;;; ------------------------------------------------------------------
;;; File status, identity, permissions, links

(defun stat-of (who what path as-link)
  (handler-case (if as-link (sb-posix:lstat path) (sb-posix:stat path))
    (sb-posix:syscall-error (e)
      (raise-errno who what path (sb-posix:syscall-errno e)))))

(defun stat-field (name st default)
  (let ((f (find-symbol name "SB-POSIX")))
    (if (and f (fboundp f)) (funcall f st) default)))

(defun stat-identity (st)
  (+ (ash (sb-posix:stat-dev st) 64) (sb-posix:stat-ino st)))

(defun stat-hash (st)
  (flet ((pair (name value) (cons (rsym name) value)))
    (ihash-from-assocs
     :eq
     (list (pair "device-id" (sb-posix:stat-dev st))
	   (pair "inode" (sb-posix:stat-ino st))
	   (pair "mode" (sb-posix:stat-mode st))
	   (pair "hardlink-count" (sb-posix:stat-nlink st))
	   (pair "user-id" (sb-posix:stat-uid st))
	   (pair "group-id" (sb-posix:stat-gid st))
	   (pair "device-id-for-special-file" (sb-posix:stat-rdev st))
	   (pair "size" (sb-posix:stat-size st))
	   (pair "block-size" (stat-field "STAT-BLKSIZE" st 4096))
	   (pair "block-count" (stat-field "STAT-BLOCKS" st 0))
	   (pair "access-time-seconds" (sb-posix:stat-atime st))
	   (pair "access-time-nanoseconds" (* 1000000000 (sb-posix:stat-atime st)))
	   (pair "modify-time-seconds" (sb-posix:stat-mtime st))
	   (pair "modify-time-nanoseconds" (* 1000000000 (sb-posix:stat-mtime st)))
	   (pair "change-time-seconds" (sb-posix:stat-ctime st))
	   (pair "change-time-nanoseconds" (* 1000000000 (sb-posix:stat-ctime st)))
	   (pair "creation-time-seconds" 0)
	   (pair "creation-time-nanoseconds" 0)))))

(defnative "file-or-directory-identity" (path &optional as-link?) ()
  (let ((name (path-string "file-or-directory-identity" path)))
    (stat-identity (stat-of "file-or-directory-identity" "error obtaining identity for path" name
			    (and as-link? (truthy as-link?))))))

(defnative "file-or-directory-stat" (path &optional as-link?) ()
  (let ((name (path-string "file-or-directory-stat" path)))
    (stat-hash (stat-of "file-or-directory-stat" "cannot get stat result" name
			(and as-link? (truthy as-link?))))))

(defun accessible-p (name flag)
  (handler-case (progn (sb-posix:access name flag) t)
    (sb-posix:syscall-error () nil)))

(defnative "file-or-directory-permissions" (path &optional (mode qs:false)) ()
  (let* ((name (path-string "file-or-directory-permissions" path))
	 (st (stat-of "file-or-directory-permissions" "access failed" name nil))
	 (m (mode-name mode)))
    (cond ((null m)
	   (append (and (accessible-p name sb-posix:x-ok) (list (rsym "execute")))
		   (and (accessible-p name sb-posix:w-ok) (list (rsym "write")))
		   (and (accessible-p name sb-posix:r-ok) (list (rsym "read")))))
	  ((string= m "bits") (logand (sb-posix:stat-mode st) #o7777))
	  ((string= m "read") (bool (accessible-p name sb-posix:r-ok)))
	  ((string= m "write") (bool (accessible-p name sb-posix:w-ok)))
	  ((string= m "execute") (bool (accessible-p name sb-posix:x-ok)))
	  (t (contract-error "file-or-directory-permissions" "(or/c 'read 'write 'execute 'bits #f)" mode)))))

(defun file-port-stat (who port)
  "The stat of the file PORT (a file-stream port) is on."
  (cond ((and (routport-p port) (eq (routport-kind port) :file))
	 (when (routport-closed port)
	   (racket-error who "output port is closed~%  output port: ~A" (port-desc port)))
	 (finish-output (routport-stream port))
	 (sb-posix:fstat (file-stream-fd port)))
	((and (rinport-p port) (rpath-p (rinport-name port)))
	 (when (rinport-closed port)
	   (racket-error who "input port is closed~%  input port: ~A" (port-desc port)))
	 (stat-of who "cannot get stat result" (rpath-string (rinport-name port)) nil))
	(t (contract-error who "file-stream-port?" port))))

(defnative "port-file-identity" (port) ()
  (stat-identity (file-port-stat "port-file-identity" port)))
(defnative "port-file-stat" (port) ()
  (stat-hash (file-port-stat "port-file-stat" port)))
(defnative "port-try-file-lock?" (port mode) ()
  (declare (ignore mode))
  (file-port-stat "port-try-file-lock?" port)
  qs:true)
(defnative "port-file-unlock" (port) ()
  (file-port-stat "port-file-unlock" port)
  *void*)

(defnative "make-file-or-directory-link" (to path) ()
  (let ((to-name (path-string "make-file-or-directory-link" to))
	(name (path-string "make-file-or-directory-link" path)))
    (handler-case (sb-posix:symlink to-name name)
      (sb-posix:syscall-error (e)
	(if (= (sb-posix:syscall-errno e) 17)
	    (do-raise (make-exn "exn:fail:filesystem:exists"
				(format nil "make-file-or-directory-link: cannot make link;~% the path already exists~%  path: ~A" name)))
	    (raise-errno "make-file-or-directory-link" "cannot make link;" name (sb-posix:syscall-errno e)))))
    *void*))

;;; Filesystem change events: with no other process to change anything
;;; while we wait, such an event never becomes ready.
(defnative "filesystem-change-evt" (path &optional (fail qs:false)) ()
  (let ((name (path-string "filesystem-change-evt" path)))
    (handler-case (progn (sb-posix:stat name) (make-revt :never (list :fs name)))
      (sb-posix:syscall-error (e)
	(if (truthy fail)
	    (funcall fail)
	    (raise-errno "filesystem-change-evt" "error generating event" name (sb-posix:syscall-errno e)))))))

(defun fs-evt-p (x)
  (and (revt-p x) (consp (revt-target x)) (eq (car (revt-target x)) :fs)))

(defnative "filesystem-change-evt?" (x) (:leaf t) (bool (fs-evt-p x)))
(defnative "filesystem-change-evt-cancel" (e) (:leaf t)
  (unless (fs-evt-p e) (contract-error "filesystem-change-evt-cancel" "filesystem-change-evt?" e))
  *void*)
(defnative "filesystem-change-evt-ready?" (e) (:leaf t)
  (unless (fs-evt-p e) (contract-error "filesystem-change-evt-ready?" "filesystem-change-evt?" e))
  qs:false)
