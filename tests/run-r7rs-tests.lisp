; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER -*-

;;;; Runs chibi's R7RS suite (tests/chibi/r7rs-tests.scm) against the
;;;; R7RS skeleton and reports the pass count.  Expect a lot of failures
;;;; for now: this is the acceptance test the skeleton is growing toward,
;;;; not a gate.
;;;;
;;;; Usage:  sbcl --script tests/run-r7rs-tests.lisp [-v]

(require :asdf)

(let* ((here (make-pathname :name nil :type nil
			    :defaults (or *load-truename* *load-pathname*)))
       (root (merge-pathnames (make-pathname :directory '(:relative :up)) here)))
  (pushnew (truename root) asdf:*central-registry* :test #'equal))

(let ((*standard-output* (make-broadcast-stream))
      (*error-output* (make-broadcast-stream)))
  (handler-bind ((warning #'muffle-warning))
    (asdf:load-system :pseudoscheme/r7rs)))

(defparameter *verbose* (member "-v" sb-ext:*posix-argv* :test #'string=))
(defparameter *trace* (member "-vv" sb-ext:*posix-argv* :test #'string=))
(defvar *here* (or *load-truename* *load-pathname*))

(defun read-all (path)
  (with-open-file (in path)
    (loop for form = (funcall ps:*scheme-read* in)
	  until (eq form ps:eof-object)
	  collect form)))

;; (chibi test), the shim next to the tests.
(dolist (form (read-all (merge-pathnames "chibi/chibi-test.scm" *here*)))
  (psl:define-library-form form))

;;; Split the test file into top-level data textually (honoring strings,
;;; comments and #\x characters) so that one unreadable datum -- say a
;;; complex literal the reader doesn't know -- loses only that form
;;; instead of everything after it.

(defun split-toplevel (text)
  (let ((chunks '()) (i 0) (n (length text)))
    (labels ((peek (&optional (k 0)) (and (< (+ i k) n) (char text (+ i k))))
	     (skip-ws-and-comments ()
	       (loop
		 (let ((c (peek)))
		   (cond ((null c) (return))
			 ((member c '(#\Space #\Tab #\Newline #\Return)) (incf i))
			 ((char= c #\;) (loop while (and (peek) (char/= (peek) #\Newline)) do (incf i)))
			 ((and (char= c #\#) (eql (peek 1) #\|))
			  (incf i 2)
			  (let ((depth 1))
			    (loop while (and (plusp depth) (peek))
				  do (cond ((and (eql (peek) #\|) (eql (peek 1) #\#)) (decf depth) (incf i 2))
					   ((and (eql (peek) #\#) (eql (peek 1) #\|)) (incf depth) (incf i 2))
					   (t (incf i))))))
			 (t (return))))))
	     (scan-datum ()
	       ;; Advance over one datum.
	       (let ((c (peek)))
		 (cond ((null c))
		       ((char= c #\() (incf i)
			(loop (skip-ws-and-comments)
			      (cond ((null (peek)) (return))
				    ((char= (peek) #\)) (incf i) (return))
				    (t (scan-datum)))))
		       ((char= c #\") (incf i)
			(loop (let ((d (peek)))
				(cond ((null d) (return))
				      ((char= d #\\) (incf i 2))
				      ((char= d #\") (incf i) (return))
				      (t (incf i))))))
		       ((member c '(#\' #\` #\,)) (incf i) (when (eql (peek) #\@) (incf i))
			(skip-ws-and-comments) (scan-datum))
		       ((and (char= c #\#) (eql (peek 1) #\\)) (incf i 3)
			(loop while (and (peek) (not (member (peek) '(#\Space #\Tab #\Newline #\( #\)))))
			      do (incf i)))
		       ((and (char= c #\#) (eql (peek 1) #\;)) (incf i 2) (skip-ws-and-comments) (scan-datum))
		       ((and (char= c #\#) (member (peek 1) '(#\( ))) (incf i) (scan-datum))
		       (t (loop while (and (peek) (not (member (peek) '(#\Space #\Tab #\Newline #\Return #\( #\) #\" #\;))))
				do (incf i)))))))
      (loop (skip-ws-and-comments)
	    (when (>= i n) (return))
	    (let ((start i))
	      (scan-datum)
	      (when (= i start) (incf i))
	      (push (subseq text start i) chunks))))
    (nreverse chunks)))

(defun file-text (path)
  (with-open-file (in path)
    (let ((s (make-string (file-length in))))
      (subseq s 0 (read-sequence s in)))))

(defvar *harness-errors* 0)
(defvar *unreadable* 0)

(defun chunk-form (chunk)
  (handler-case (with-input-from-string (in chunk) (funcall ps:*scheme-read* in))
    (error (e)
      (incf *unreadable*)
      (when *verbose*
	(format t "~&[UNREADABLE] ~A~%  ~A~%" e (subseq chunk 0 (min 70 (length chunk)))))
      nil)))

(let* ((chunks (split-toplevel (file-text (merge-pathnames "chibi/r7rs-tests.scm" *here*))))
       (env (psl:program-environment (chunk-form (car chunks)))))
  (dolist (chunk (cdr chunks))
    (let ((form (chunk-form chunk)))
      (when form
	(when *trace*
	  (format t "~&>> ~A~%" (subseq chunk 0 (min 100 (length chunk))))
	  (finish-output))
	(handler-case (ps:scheme-eval form env)
	  ;; SERIOUS-CONDITION, not ERROR: a runaway WRITE of a circular
	  ;; structure (no datum labels yet) ends in heap exhaustion.
	  (serious-condition (e)
	    (incf *harness-errors*)
	    (when *verbose*
	      (format t "~&[ERROR] ~A~%  in ~A~%"
		      (remove #\Newline (princ-to-string e))
		      (subseq chunk 0 (min 90 (length chunk))))))))))
  (let* ((counts (ps:scheme-eval (read-from-string "(scheme::test-results)") env))
	 (run (car counts)) (passed (cdr counts)))
    (format t "~&~%R7RS: ~A of ~A tests passed (~A forms raised errors, ~A unreadable).~%"
	    passed run *harness-errors* *unreadable*)))
