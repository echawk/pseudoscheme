; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

;;;; rktio: Racket's portable operating-system layer (files, sockets,
;;;; processes, polling), in C, which the io layer calls through its
;;;; rktio_* primitives (racket/src/io/, for console ports:
;;;; terminal-port? is rktio_fd_is_terminal on a std fd, a read is
;;;; rktio_poll_read_ready then rktio_read).  It is built from Racket's
;;;; source (racket/src/rktio/) as a shared library, by
;;;; utils/racket/rktio.sh into build/external/rktio/, and bound here
;;;; with CFFI, loaded the first time it's used, so that what the REPL
;;;; asks of the terminal is Racket's own answer.  Without the library
;;;; (no C compiler when setting up), RKTIO-AVAILABLE-P is false and the
;;;; callers fall back to the host Lisp's streams.
;;;;
;;;; The std fds are rktio's own descriptors for 0, 1 and 2, never made
;;;; non-blocking (rktio_std_fd does not), so the host's streams on the
;;;; same descriptors still work beside them.

(in-package "PSEUDOSCHEME-RACKET")

(defconstant +rktio-stdin+ 0)
(defconstant +rktio-stdout+ 1)
(defconstant +rktio-stderr+ 2)
(defconstant +rktio-read-eof+ -1)
(defconstant +rktio-read-error+ -2)
(defconstant +rktio-poll-error+ -2)

(defvar *rktio* nil "The rktio_t, once rktio_init has made it.")
(defvar *rktio-std-fds* (make-array 3 :initial-element nil)
  "rktio's fds for stdin, stdout and stderr, made when first asked for.")
(defvar *rktio-library-loaded* nil)

(defun rktio-library ()
  "The built librktio under build/external/rktio/ (utils/racket/rktio.sh),
or NIL."
  (let ((dir (or (uiop:getenv "PSEUDOSCHEME_RKTIO_DIRECTORY")
		 (namestring (asdf:system-relative-pathname :pseudoscheme "build/external/rktio/")))))
    (loop for type in '("dylib" "so")
	  for path = (probe-file (merge-pathnames (format nil "librktio.~A" type)
						  (uiop:ensure-directory-pathname dir)))
	  when path return path)))

(defun rktio-available-p ()
  (and (or *rktio-library-loaded* (rktio-library)) t))

(defun rktio-load-library ()
  (unless *rktio-library-loaded*
    (let ((path (rktio-library)))
      (unless path
	(error 'racket-error :who "rktio" :message "librktio isn't built: run sh utils/racket/rktio.sh"))
      (cffi:load-foreign-library path)
      (setq *rktio-library-loaded* t))))

(defun rktio ()
  "The rktio_t, made by rktio_init the first time."
  (or *rktio*
      (progn
	(rktio-load-library)
	(let ((r (cffi:foreign-funcall "rktio_init" :pointer)))
	  (when (cffi:null-pointer-p r)
	    (error 'racket-error :who "rktio" :message "rktio_init failed"))
	  ;; rktio_init blocks SIGCHLD in this thread (it waits for children
	  ;; on a thread of its own, once a process starts), which SBCL, whose
	  ;; own signals include it, won't run with
	  #+sbcl (sb-unix::unblock-deferrable-signals)
	  (setq *rktio* r)))))

(defun rktio-reset ()
  "Forget the rktio_t and the fds, as a saved image starts: they were
made by another process."
  (setq *rktio* nil
	*rktio-library-loaded* nil
	*rktio-std-fds* (make-array 3 :initial-element nil)))

#+sbcl (pushnew 'rktio-reset sb-ext:*init-hooks*)
#+sbcl (pushnew 'rktio-reset sb-ext:*save-hooks*)

(defun rktio-std-fd (which)
  "rktio's fd for stdin (0), stdout (1) or stderr (2)."
  (or (svref *rktio-std-fds* which)
      (setf (svref *rktio-std-fds* which)
	    (cffi:foreign-funcall "rktio_std_fd" :pointer (rktio) :int which :pointer))))

(defun rktio-fd-terminal-p (fd)
  "Is the rktio fd FD a terminal?"
  (not (zerop (cffi:foreign-funcall "rktio_fd_is_terminal" :pointer (rktio) :pointer fd :int))))

(defun rktio-std-terminal-p (which)
  "Is stdin (0), stdout (1) or stderr (2) a terminal?  What Racket's
terminal-port? says of the console ports; false if there is no librktio."
  (and (rktio-available-p)
       (rktio-fd-terminal-p (rktio-std-fd which))))

(defun rktio-fd-regular-file-p (fd)
  (not (zerop (cffi:foreign-funcall "rktio_fd_is_regular_file" :pointer (rktio) :pointer fd :int))))

(defun rktio-poll-read-ready (fd)
  "Is there input to read from FD, or an end of file, without blocking?"
  (let ((r (cffi:foreign-funcall "rktio_poll_read_ready" :pointer (rktio) :pointer fd :int)))
    (cond ((= r +rktio-poll-error+) (error 'racket-error :who "rktio" :message "rktio_poll_read_ready failed"))
	  (t (not (zerop r))))))

(defun rktio-read (fd count)
  "Up to COUNT bytes read from FD (blocking, if nothing is ready: poll
first): an octet vector, or :EOF."
  (cffi:with-foreign-object (buffer :uint8 count)
    (let ((n (cffi:foreign-funcall "rktio_read" :pointer (rktio) :pointer fd :pointer buffer
					       :intptr count :intptr)))
      (cond ((= n +rktio-read-eof+) :eof)
	    ((= n +rktio-read-error+) (error 'racket-error :who "rktio" :message "rktio_read failed"))
	    (t (let ((octets (make-octets n)))
		 (dotimes (i n octets)
		   (setf (aref octets i) (cffi:mem-aref buffer :uint8 i)))))))))

(defun rktio-write (fd octets &key (start 0) (end (length octets)))
  "OCTETS from START to END written to FD, all of them; the number
written."
  (let ((total 0))
    (cffi:with-foreign-object (buffer :uint8 (max 1 (- end start)))
      (loop for i from start below end for j from 0
	    do (setf (cffi:mem-aref buffer :uint8 j) (aref octets i)))
      (loop while (< total (- end start))
	    do (let ((n (cffi:foreign-funcall "rktio_write" :pointer (rktio) :pointer fd
							  :pointer (cffi:inc-pointer buffer total)
							  :intptr (- (- end start) total) :intptr)))
		 (when (= n +rktio-read-error+)
		   (error 'racket-error :who "rktio" :message "rktio_write failed"))
		 (incf total n))))
    total))

;;; ------------------------------------------------------------------
;;; Console ports
;;;
;;; As the io layer makes them: current-input-port, -output-port and
;;; -error-port are ports on the std fds, whose reads and writes are
;;; rktio's.  They are custom ports (make-input-port, make-output-port)
;;; on the rktio fds, so they are Racket ports through and through (an
;;; event, for sync and pretty-print's tentative ports; port-count-lines!;
;;; no host stream in sight).  A write goes straight to the fd.  A read
;;; polls (rktio_poll_read_ready) and, if nothing is there, hands back an
;;; alarm event for the port to wait on, so other threads run meanwhile,
;;; and a char-ready? doesn't block.  REPL.LISP installs them.

(defvar *console-ports* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "A console port -> the std fd (0, 1 or 2) it is on.")

(defun native-call (name &rest args)
  (apply (native-function (gethash name *natives*)) args))

(defun make-console-input-port (&optional (which +rktio-stdin+) (name "stdin"))
  (let* ((fd (rktio-std-fd which))
	 (port (native-call
		"make-input-port" (rsym name)
		(lambda (bytes)
		  (if (rktio-poll-read-ready fd)
		      (let ((chunk (rktio-read fd (length bytes))))
			(if (eq chunk :eof)
			    ps:eof-object
			    (progn (replace bytes chunk) (length chunk))))
		      (native-call "alarm-evt" (+ (epoch-ms) 10))))
		ps:false
		(lambda () *void*))))
    (setf (gethash port *console-ports*) which)
    port))

(defun make-console-output-port (which name)
  (let ((port (native-call
	       "make-output-port" (rsym name) *always-evt*
	       (lambda (bytes start end non-block breakable)
		 (declare (ignore non-block breakable))
		 (if (= start end)
		     0
		     (rktio-write (rktio-std-fd which) bytes :start start :end end)))
	       (lambda () *void*))))
    (setf (gethash port *console-ports*) which)
    port))

(defun install-console-ports ()
  "Make the console ports the current ports, in this thread: rktio's
stdin, stdout and stderr."
  (funcall *current-input-port* (make-console-input-port))
  (funcall *current-output-port* (make-console-output-port +rktio-stdout+ "stdout"))
  (funcall *current-error-port* (make-console-output-port +rktio-stderr+ "stderr")))

;;; ------------------------------------------------------------------
;;; terminal-port?
;;;
;;; The io layer's answer for a console port is rktio_fd_is_terminal of
;;; its std fd.  The console ports above know theirs; the host's streams,
;;; SBCL's fd-streams on the same descriptors, are mapped back to the std
;;; fds.  (This replaces the stub in ports.lisp, which said false.)

(defun host-stream-std-fd (stream)
  "0, 1 or 2 if the host stream STREAM (or a synonym or two-way stream
for one) is on that descriptor, else NIL."
  (loop
    (typecase stream
      (synonym-stream (setq stream (symbol-value (synonym-stream-symbol stream))))
      (two-way-stream (setq stream (if (input-stream-p stream)
				       (two-way-stream-input-stream stream)
				       (two-way-stream-output-stream stream))))
      (sb-sys:fd-stream (let ((fd (sb-sys:fd-stream-fd stream)))
			  (return (and (<= 0 fd 2) fd))))
      (t (return nil)))))

(defun rktio-terminal-port-p (port)
  "Racket's terminal-port?: is PORT, a console port or one of the host's,
on a terminal?"
  (let ((fd (or (gethash port *console-ports*)
		(and (rinport-p port) (eq (rinport-kind port) :host)
		     (host-stream-std-fd (rinport-host port)))
		(and (not (rinport-p port)) (not (routport-p port))
		     (host-stream-std-fd port)))))
    (and fd (rktio-std-terminal-p fd))))

(defnative "terminal-port?" (p) (:leaf t)
  (bool (rktio-terminal-port-p p)))
