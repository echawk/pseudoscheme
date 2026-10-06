;;; (srfi private srfi-181-ports): what SRFIs 181 and 192 need of
;;; Pseudoscheme's R6RS custom ports (src/r6rs/ports.lisp) beyond
;;; (rnrs io ports).  Written for Pseudoscheme; it subclasses that file's
;;; Gray stream classes, so it must follow it.
;;;
;;; - SRFI 181's custom output ports take an optional flush procedure,
;;;   called when the port is flushed; R6RS's have none.  The output
;;;   port constructors here make subclasses of the R6RS custom port
;;;   classes whose force-output and finish-output call it.
;;; - Pseudoscheme's port-position on a custom textual input port
;;;   returns the get-position procedure's value even when a character
;;;   has been read ahead (by peek-char) and given back, so that
;;;   position is one character too far.  The textual input ports made
;;;   here remember the position before each call of read!, and
;;;   custom-port-position returns it while a character is given back.
;;; - Likewise a write to a custom binary input/output port after
;;;   peek-u8 goes to the position after the peeked byte; the ports made
;;;   here first set the position back to the peeked byte's, when they
;;;   have get-position and set-position! procedures.
(define-library (srfi private srfi-181-ports)
  (export make-custom-textual-input-port
          make-custom-binary-output-port
          make-custom-textual-output-port
          make-custom-binary-input/output-port
          custom-port-position)
  (import (scheme base) (pseudoscheme lisp))
  (begin
    ;; The Lisp definitions, read in Pseudoscheme's R6RS package.  (The
    ;; code must contain no double quotes or backslashes.)
    (define lisp-code "
(progn

(defclass srfi-181-flush-mixin ()
  ((flush :initarg :flush :initform nil)))

(defun srfi-181-flush (s)
  (let ((f (slot-value s 'flush)))
    (when (functionp f) (funcall f)))
  nil)

(defmethod trivial-gray-streams:stream-force-output ((s srfi-181-flush-mixin))
  (srfi-181-flush s))
(defmethod trivial-gray-streams:stream-finish-output ((s srfi-181-flush-mixin))
  (srfi-181-flush s))

(defclass srfi-181-binary-output-port (srfi-181-flush-mixin custom-binary-output-port) ())
(defclass srfi-181-textual-output-port (srfi-181-flush-mixin custom-textual-output-port) ())
(defclass srfi-181-binary-io-port (srfi-181-flush-mixin custom-binary-io-port) ())

;; a write after a peek-u8 goes where the peeked byte was
(defmethod trivial-gray-streams:stream-write-byte :before ((s srfi-181-binary-io-port) byte)
  (declare (ignore byte))
  (let ((p (peeked s)))
    (when p
      (setf (peeked s) nil)
      (when (and (integerp p)
                 (slot-value s 'get-position)
                 (slot-value s 'set-position!))
        (funcall (slot-value s 'set-position!)
                 (- (funcall (slot-value s 'get-position)) 1))))))

(defclass srfi-181-textual-input-port (custom-textual-input-port)
  ((before :initform nil :accessor srfi-181-before)))

(defmethod trivial-gray-streams:stream-read-char ((s srfi-181-textual-input-port))
  (unless (unread s)
    (let ((g (slot-value s 'get-position)))
      (when g (setf (srfi-181-before s) (funcall g)))))
  (call-next-method))

(defun srfi-181-make-port (kind id read! write! get-position set-position! close flush)
  (let ((common (list :id id
                      :get-position (proc-or-nil get-position)
                      :set-position! (proc-or-nil set-position!)
                      :closer (proc-or-nil close))))
    (ecase kind
      (:textual-input
       (apply #'make-instance 'srfi-181-textual-input-port :read! read! common))
      (:binary-output
       (apply #'make-instance 'srfi-181-binary-output-port :write! write!
              :flush (proc-or-nil flush) common))
      (:textual-output
       (apply #'make-instance 'srfi-181-textual-output-port :write! write!
              :flush (proc-or-nil flush) common))
      (:binary-io
       (apply #'make-instance 'srfi-181-binary-io-port :read! read! :write! write!
              :flush (proc-or-nil flush) common)))))

(defun srfi-181-port-position (port)
  (if (and (typep port 'srfi-181-textual-input-port)
           (unread port)
           (slot-value port 'get-position))
      (srfi-181-before port)
      (funcall (prim (string-downcase (symbol-name 'port-position))) port))))
")

    (lisp-eval-string
     (string-append
      "(let ((*package* (find-package :pseudoscheme-r6rs))) "
      "(eval (read-from-string \"" lisp-code "\")))"))

    (define %make (lisp-function "srfi-181-make-port" "pseudoscheme-r6rs"))
    (define %position (lisp-function "srfi-181-port-position" "pseudoscheme-r6rs"))

    (define (flush-argument rest) (if (pair? rest) (car rest) #f))

    (define (make-custom-textual-input-port id read! get-position set-position! close)
      (lisp-funcall %make (lisp-keyword "textual-input") id read! #f
                    get-position set-position! close #f))

    (define (make-custom-binary-output-port id write! get-position set-position! close . flush)
      (lisp-funcall %make (lisp-keyword "binary-output") id #f write!
                    get-position set-position! close (flush-argument flush)))

    (define (make-custom-textual-output-port id write! get-position set-position! close . flush)
      (lisp-funcall %make (lisp-keyword "textual-output") id #f write!
                    get-position set-position! close (flush-argument flush)))

    (define (make-custom-binary-input/output-port id read! write! get-position set-position! close . flush)
      (lisp-funcall %make (lisp-keyword "binary-io") id read! write!
                    get-position set-position! close (flush-argument flush)))

    (define (custom-port-position port)
      (lisp-funcall %position port))))
