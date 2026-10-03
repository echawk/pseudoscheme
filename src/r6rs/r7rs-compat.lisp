; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-R6RS -*-

;;;; Host procedures for R7RS that R6RS doesn't have, or has with a
;;;; different meaning under the same name.
;;;;
;;;; The (scheme ...) libraries (src/r7rs/front.lisp) export these under
;;;; their R7RS names; a different meaning gets a host name of its own
;;;; (r7rs:error, r7rs:bytevector-copy!, ...), since each host global has
;;;; exactly one value.

(in-package "PSEUDOSCHEME-R6RS")

;;; error: (error message irritant ...), raising the same kind of
;;; condition as R6RS's (error who message irritant ...) minus the who.

(defprim "r7rs:error" (message &rest irritants)
  (ps-r7rs:raise-object (make-standard-condition "make-error" ps:false message irritants) nil))

;;; (bytevector-copy! to at from [start [end]]), not R6RS's
;;; (bytevector-copy! from from-start to to-start count).

(defprim "r7rs:bytevector-copy!" (to at from &optional start end)
  (apply (ps-r7rs::primitive "bytevector-copy!") to at from
	 (append (and start (list start)) (and end (list end)))))

;;; ------------------------------------------------------------------
;;; Binary ports (R7RS 6.13), on the R6RS port machinery of ports.lisp

(defun default-in (port) (or port *standard-input*))
(defun default-out (port) (or port *standard-output*))

(defprim "read-u8" (&optional port) (funcall (prim "get-u8") (default-in port)))
(defprim "peek-u8" (&optional port) (funcall (prim "lookahead-u8") (default-in port)))
(defprim "u8-ready?" (&optional port) (declare (ignore port)) t)
(defprim "write-u8" (byte &optional port) (funcall (prim "put-u8") (default-out port) byte))

(defprim "read-bytevector" (k &optional port)
  (funcall (prim "get-bytevector-n") (default-in port) k))

(defprim "read-bytevector!" (bv &optional port (start 0) (end nil))
  (let* ((end (or end (length bv)))
	 (n (funcall (prim "get-bytevector-n!") (default-in port) bv start (- end start))))
    n))

(defprim "write-bytevector" (bv &optional port (start 0) (end nil))
  (funcall (prim "put-bytevector") (default-out port) bv start (- (or end (length bv)) start)))

(defprim "open-input-bytevector" (bv)
  (funcall (prim "open-bytevector-input-port") bv))

(defprim "open-output-bytevector" ()
  (make-instance 'bytevector-output-port))

(defprim "get-output-bytevector" (port)
  (unless (typep port 'bytevector-output-port)
    (r6rs-assertion-violation "get-output-bytevector" "not a bytevector output port" port))
  (prog1 (coerce (buffer port) 'octets)))

(defprim "open-binary-input-file" (name)
  (open-file "open-binary-input-file" name :input '() nil))

(defprim "open-binary-output-file" (name)
  ;; R7RS doesn't say what happens to an existing file; supersede it,
  ;; as open-output-file does.
  (open-file "open-binary-output-file" name :output
	     (list (ps:intern-scheme-symbol "no-fail")) nil))
