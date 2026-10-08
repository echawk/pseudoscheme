; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-GUILE -*-

;;;; Ports: Guile's ports are Pseudoscheme's, Lisp streams.  The current
;;;; ports are fluids whose values are the Lisp stream variables
;;;; (*standard-output* and so on), so that Pseudoscheme's own
;;;; procedures, which write to *standard-output*, follow
;;;; (current-output-port) and parameterize.  (ice-9 ports) loads the
;;;; rest with load-extension.

(in-package "PSEUDOSCHEME-GUILE")


(defun make-special-fluid (symbol)
  (let ((f (make-fluid* ps:false)))
    (setf (fluid-special f) symbol)
    f))

(defvar *current-input-port-fluid* (make-special-fluid '*standard-input*))
(defvar *current-output-port-fluid* (make-special-fluid '*standard-output*))
(defvar *current-error-port-fluid* (make-special-fluid '*error-output*))
(defvar *current-warning-port-fluid* (make-special-fluid '*error-output*))

(defvar *port-properties* (trivial-garbage:make-weak-hash-table :weakness :key :test 'eq))
(defvar *port-lines* (trivial-garbage:make-weak-hash-table :weakness :key :test 'eq))
(defvar *port-filenames* (trivial-garbage:make-weak-hash-table :weakness :key :test 'eq))

(defun port-column (port)
  (or (ignore-errors (sb-impl::charpos port)) 0))

(defun open-file (filename mode &key (encoding ps:false) (guess-encoding ps:false))
  (declare (ignore guess-encoding))
  (let* ((input (find #\r mode))
	 (plus (find #\+ mode))
	 (direction (cond ((and input plus) :io) (input :input) (t :output)))
	 (external-format (if (and (stringp encoding) (search "8859" encoding)) :latin-1 :utf-8))
	 (stream (handler-case
		     (open filename :direction direction :external-format external-format
				    :if-exists (cond ((find #\a mode) :append)
						     ((eq direction :io) :overwrite)
						     (t :supersede))
				    :if-does-not-exist (if (eq direction :input) :error :create))
		   (file-error ()
		     (guile-error (ssym "system-error") "open-file" "~A: ~S"
				  (list "No such file or directory" filename) (list 2))))))
    (setf (gethash stream *port-filenames*) filename)
    stream))

(defun port-mode-string (port)
  (cond ((and (input-stream-p port) (output-stream-p port)) "r+")
	((input-stream-p port) "r")
	(t "w")))

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
    (def "open-input-file" (lambda (f &rest options) (declare (ignore options)) (open-file f "r")))
    (def "set-port-encoding!" (lambda (port enc) (declare (ignore port enc)) *unspecified*))
    (def "force-output" (lambda (&optional (port *standard-output*)) (force-output port) *unspecified*))
    (def "close-port" (lambda (port) (bool (and (open-stream-p port) (progn (close port) t)))))
    (def "call-with-output-string"
	(lambda (proc) (with-output-to-string (s) (funcall proc s))))
    (def "%default-port-encoding" (make-fluid* "UTF-8"))
    (def "%default-port-conversion-strategy" (make-fluid* (ssym "substitute")))))

(defextension "scm_init_ice_9_ports"
  (append
   (list
   (cons "%port-property" (lambda (port key)
			    (let ((e (assoc key (gethash port *port-properties*))))
			      (if e (cdr e) ps:false))))
   (cons "%set-port-property!" (lambda (port key value)
				 (let ((e (assoc key (gethash port *port-properties*))))
				   (if e (setf (cdr e) value)
				       (push (cons key value) (gethash port *port-properties*))))
				 *unspecified*))
   (cons "set-current-input-port" (lambda (p) (prog1 *standard-input* (setq *standard-input* p))))
   (cons "set-current-output-port" (lambda (p) (prog1 *standard-output* (setq *standard-output* p))))
   (cons "set-current-error-port" (lambda (p) (prog1 *error-output* (setq *error-output* p))))
   (cons "port-mode" #'port-mode-string)
   (cons "port?" (lambda (x) (bool (streamp x))))
   (cons "input-port?" (lambda (x) (bool (and (streamp x) (input-stream-p x)))))
   (cons "output-port?" (lambda (x) (bool (and (streamp x) (output-stream-p x)))))
   (cons "port-closed?" (lambda (x) (bool (not (open-stream-p x)))))
   (cons "close-input-port" (lambda (p) (close p) *unspecified*))
   (cons "close-output-port" (lambda (p) (close p) *unspecified*))
   (cons "%port-encoding" (lambda (p) (declare (ignore p)) (ssym "UTF-8")))
   (cons "port-conversion-strategy" (lambda (p) (declare (ignore p)) (ssym "substitute")))
   (cons "set-port-conversion-strategy!" (lambda (p s) (declare (ignore p s)) *unspecified*))
   (cons "read-char" (lambda (&optional (p *standard-input*))
		       (let ((c (read-char p nil nil)))
			 (cond ((null c) ps:eof-object)
			       (t (when (char= c #\Newline) (incf (gethash p *port-lines* 0))) c)))))
   (cons "peek-char" (lambda (&optional (p *standard-input*)) (peek-char nil p nil ps:eof-object)))
   (cons "unread-char" (lambda (c &optional (p *standard-input*)) (unread-char c p) *unspecified*))
   (cons "unread-string" (lambda (s &optional (p *standard-input*))
			   (declare (ignore s p))
			   (error "unread-string isn't supported here")))
   (cons "setvbuf" (lambda (&rest args) (declare (ignore args)) *unspecified*))
   (cons "drain-input" (lambda (p) (declare (ignore p)) ""))
   (cons "char-ready?" (lambda (&optional (p *standard-input*)) (bool (listen p))))
   (cons "seek" (lambda (p offset whence)
		  (let ((pos (case whence
			       (0 offset)
			       (1 (+ (file-position p) offset))
			       (t (+ (file-length p) offset)))))
		    (file-position p pos)
		    pos)))
   (cons "SEEK_SET" 0) (cons "SEEK_CUR" 1) (cons "SEEK_END" 2)
   (cons "truncate-file" (lambda (p &optional length)
			   (sb-posix:ftruncate (sb-sys:fd-stream-fd p) (or length (file-position p)))
			   *unspecified*))
   (cons "port-line" (lambda (p) (gethash p *port-lines* 0)))
   (cons "set-port-line!" (lambda (p n) (setf (gethash p *port-lines*) n) *unspecified*))
   (cons "port-column" #'port-column)
   (cons "set-port-column!" (lambda (p n) (declare (ignore p n)) *unspecified*))
   (cons "port-filename" (lambda (p) (gethash p *port-filenames* ps:false)))
   (cons "set-port-filename!" (lambda (p name) (setf (gethash p *port-filenames*) name) *unspecified*))
   (cons "port-for-each" (lambda (proc) (declare (ignore proc)) *unspecified*))
   (cons "flush-all-ports" (lambda () (finish-output *standard-output*) (finish-output *error-output*)
				   *unspecified*))
   (cons "put-char" (lambda (p c) (write-char c p) *unspecified*))
   (cons "put-string" (lambda (p s &optional (start 0) (count (- (length s) start)))
			(write-string s p :start start :end (+ start count)) *unspecified*))
   (cons "%make-void-port" (lambda (mode)
			     (if (find #\r mode)
				 (make-string-input-stream "")
				 (make-broadcast-stream)))))
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
  (list (cons "file-port?" (lambda (x) (bool (typep x 'sb-sys:fd-stream))))
	(cons "port-revealed" (lambda (p) (declare (ignore p)) 0))
	(cons "set-port-revealed!" (lambda (p n) (declare (ignore p n)) *unspecified*))
	(cons "adjust-port-revealed!" (lambda (p n) (declare (ignore p n)) *unspecified*))))

(defextension "scm_init_ice_9_ioext"
  (list (cons "ftell" (lambda (p) (file-position p)))
	(cons "redirect-port" (lambda (old new) (declare (ignore old new)) *unspecified*))
	(cons "dup->fdes" (lambda (fd/port &optional new)
			    (let ((fd (if (integerp fd/port) fd/port (sb-sys:fd-stream-fd fd/port))))
			      (if new (sb-posix:dup2 fd new) (sb-posix:dup fd)))))
	(cons "dup2" (lambda (old new) (sb-posix:dup2 old new)))
	(cons "fileno" (lambda (p) (sb-sys:fd-stream-fd p)))
	(cons "isatty?" (lambda (p) (bool (and (typep p 'sb-sys:fd-stream)
					       (= 1 (sb-unix:unix-isatty (sb-sys:fd-stream-fd p)))))))
	(cons "fdopen" (lambda (fd mode)
			 (sb-sys:make-fd-stream fd :input (bool (find #\r mode)) :output (not (find #\r mode))
						   :external-format :utf-8 :buffering :full)))
	(cons "primitive-move->fdes" (lambda (p fd) (declare (ignore p fd)) ps:false))
	(cons "fdes->ports" (lambda (fd) (declare (ignore fd)) '()))))

;;; (ice-9 rdelim)'s and (ice-9 rw)'s C halves

(defun read-delimited-into (delims buf gobble port start end)
  "Read characters into BUF from START until one in DELIMS (a string),
end of file, or END: (delimiter-or-eof . count)."
  (loop for i from start below end
	do (let ((c (read-char port nil nil)))
	     (cond ((null c) (return-from read-delimited-into (cons ps:eof-object (- i start))))
		   ((find c delims)
		    (unless (truthy gobble) (unread-char c port))
		    (return-from read-delimited-into (cons c (- i start))))
		   (t (setf (char buf i) c)))))
  (cons ps:false (- end start)))

(defguile "%init-rdelim-builtins" ()
  (flet ((def (name value) (define-in-module *current-module* (ssym name) value)))
    (def "%read-delimited!"
	(lambda (delims buf gobble &optional (port *standard-input*) (start 0) (end (length buf)))
	  (read-delimited-into delims buf gobble port start end)))
    (def "%read-line"
	(lambda (&optional (port *standard-input*))
	  (let ((out (make-string-output-stream)))
	    (loop (let ((c (read-char port nil nil)))
		    (cond ((null c)
			   (let ((s (get-output-stream-string out)))
			     (return (cons (if (zerop (length s)) ps:eof-object s) ps:eof-object))))
			  ((char= c #\Newline)
			   (return (cons (get-output-stream-string out) c)))
			  (t (write-char c out))))))))
    (def "write-line"
	(lambda (obj &optional (port *standard-output*))
	  (funcall ps:*scheme-display* obj port) (terpri port) *unspecified*)))
  *unspecified*)

(defguile "%init-rw-builtins" ()
  (flet ((def (name value) (define-in-module *current-module* (ssym name) value)))
    (def "read-string!/partial"
	(lambda (buf &optional (port *standard-input*) (start 0) (end (length buf)))
	  (let ((n (- (read-sequence buf port :start start :end end) start)))
	    (if (and (zerop n) (< start end)) ps:false n))))
    (def "write-string/partial"
	(lambda (s &optional (port *standard-output*) (start 0) (end (length s)))
	  (write-string s port :start start :end end) (- end start))))
  *unspecified*)

;;; Guile's ports are binary and textual at once: characters read from
;;; and written to Pseudoscheme's binary ports, as UTF-8.

(defvar *unread-chars* (trivial-garbage:make-weak-hash-table :weakness :key :test 'eq))

(defmethod trivial-gray-streams:stream-read-char ((s ps-r6rs::binary-input-port))
  (let ((c (gethash s *unread-chars*)))
    (when c
      (remhash s *unread-chars*)
      (return-from trivial-gray-streams:stream-read-char c)))
  (let ((b (read-byte s nil :eof)))
    (if (eq b :eof)
	:eof
	(let ((n (cond ((< b #x80) 0) ((< b #xe0) 1) ((< b #xf0) 2) (t 3))))
	  (if (zerop n)
	      (code-char b)
	      (let ((bytes (make-array (1+ n) :element-type '(unsigned-byte 8))))
		(setf (aref bytes 0) b)
		(loop for i from 1 to n do (setf (aref bytes i) (let ((x (read-byte s nil 0))) x)))
		(char (sb-ext:octets-to-string bytes :external-format :utf-8) 0)))))))

(defmethod trivial-gray-streams:stream-unread-char ((s ps-r6rs::binary-input-port) c)
  (setf (gethash s *unread-chars*) c)
  nil)

(defmethod trivial-gray-streams:stream-write-char ((s ps-r6rs::binary-output-port) c)
  (loop for b across (sb-ext:string-to-octets (string c) :external-format :utf-8)
	do (write-byte b s))
  c)

(defmethod trivial-gray-streams:stream-line-column ((s ps-r6rs::binary-output-port)) nil)
