; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

;;;; Strategy B's linklets from Racket's source tree.
;;;;
;;;; The expander: Racket BC's racket/src/bc/src/startup.inc holds the
;;;; extracted, flattened expander linklet, as C string literals, for
;;;; BC's own linklet compiler.  It is the linklet strategy B needs,
;;;; with no Racket to extract it (racket/src/expander/extract).  The
;;;; thread, io and regexp layers aren't there (BC has them in C):
;;;; extracting them needs a Racket (docs/racket-linklet-compiler.md,
;;;; "Status").

(in-package "PSEUDOSCHEME-RACKET")

(defun c-string-literals (stream)
  "The concatenation of the C string literals in STREAM, unescaped."
  (with-output-to-string (out)
    (loop for c = (read-char stream nil)
	  while c
	  do (case c
	       (#\/ (when (eql (peek-char nil stream nil) #\*) ; a comment
		      (loop for p = nil then d
			    for d = (read-char stream nil)
			    until (or (null d) (and (eql p #\*) (eql d #\/))))))
	       (#\" (loop for d = (read-char stream)
			  until (char= d #\")
			  do (if (char= d #\\)
				 (let ((e (read-char stream)))
				   (case e
				     (#\n (write-char #\Newline out))
				     (#\t (write-char #\Tab out))
				     (#\r (write-char #\Return out))
				     ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
				      (let ((n (digit-char-p e 8)))
					(loop repeat 2
					      while (digit-char-p (peek-char nil stream) 8)
					      do (setq n (+ (* n 8) (digit-char-p (read-char stream) 8))))
					(write-char (code-char n) out)))
				     (t (write-char e out))))
				 (write-char d out))))))))

(defun expander-linklet-text ()
  "The expander linklet's source, from BC's startup.inc.  The C string is
bytes (UTF-8): decoded as such."
  (let ((latin (with-open-file (in (racket-source "racket/src/bc/src/startup.inc")
				   :external-format :latin-1)
		 (c-string-literals in))))
    (sb-ext:octets-to-string (map '(vector (unsigned-byte 8)) #'char-code latin)
			     :external-format :utf-8)))

(defun read-expander-linklet ()
  (with-input-from-string (in (expander-linklet-text))
    (read-racket in)))

(defun unknown-primitives (core)
  "The names CORE (a lowered linklet) refers to as primitives that no
instance has, with how many times each."
  (let ((counts (make-hash-table :test 'eq)))
    (labels ((walk (x)
	       (when (consp x)
		 (if (and (eq (car x) (core "racket:unknown-primitive")) (consp (cdr x)))
		     (incf (gethash (cadr (cadr x)) counts 0))
		     (progn (walk (car x)) (walk (cdr x)))))))
      (walk core))
    (sort (loop for k being the hash-keys of counts using (hash-value n) collect (cons (rname k) n))
	  #'> :key #'cdr)))

(defun expander-report (&key (stream *standard-output*) (missing nil))
  "Lower the expander linklet (passes 1 to 4, no SBCL compilation): say
whether it lowers, and which primitives it uses that we lack."
  (let* ((form (read-expander-linklet))
	 (compiled (compile-linklet-form form :name (rsym "expander")))
	 (unknown (unknown-primitives (compiled-linklet-form compiled))))
    (format stream "~&expander linklet: ~D body forms, ~D exports; lowered; ~D primitives it uses are missing~%"
	    (length (cdddr form)) (length (compiled-linklet-exports compiled)) (length unknown))
    (when missing
      (format stream "~&  missing (uses): ~{~A (~D)~^, ~}~%"
	      (loop for (name . n) in unknown append (list name n))))
    unknown))
