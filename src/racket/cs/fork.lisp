; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

;;;; Where an addition goes: --chez stays faithful to upstream Chez.  A
;;;; name upstream Chez also has is implemented in src/chez/, as upstream
;;;; defines it; one only Racket's fork has is defined here (fork.scm),
;;;; for --racket alone, never in the Chez mode.
;;;;
;;;; Strategy A (and C): the additions Racket's fork of Chez Scheme makes,
;;;; which rumble uses, provided on --chez (docs/racket.md, "The Chez
;;;; fork's additions").  fork.scm defines the Scheme side; this file
;;;; keeps the list, so that milestone 0's inventory (FORK-INVENTORY) can
;;;; say what's still missing.

(in-package "PSEUDOSCHEME-RACKET")

(defparameter *fork-additions*
  '((:attachments "$call-setting-continuation-attachment" "$call-getting-continuation-attachment"
     "$call-consuming-continuation-attachment" "$current-attachments" "$continuation-attachments"
     "$call-in-continuation" "$null-continuation")
    (:engines "set-timer" "timer-interrupt-handler" "$current-winders"
     "current-exception-state" "create-exception-state")
    (:wrapper-procedures "make-wrapper-procedure" "make-arity-wrapper-procedure"
     "wrapper-procedure?" "wrapper-procedure-data" "$app" "$app/no-inline" "$app/no-return"
     "$app/value")
    (:records "$record-ref" "$record-set!" "$sealed-record?" "$record-type-act-sealed?"
     "$record-type-act-sealed!" "$record-cas!" "$record-type-field-count")
    (:stencil-vectors "$system-stencil-vector" "$system-stencil-vector-update" "$stencil-vector-mask")
    (:memory "make-ephemeron-eq-hashtable" "make-phantom-bytevector" "make-guardian")
    (:ffi "foreign-procedure" "foreign-callable" "make-ftype-pointer"))
  "What rumble needs beyond Chez Scheme, by kind.  At the pinned Racket
(build/external/racket) the fork's primdata.ss is upstream's but for two
flags, so all of them are upstream's too, and src/chez/ (system.scm,
host.lisp) has them: build/agents/chez-fork-inventory.txt.")

(defun source-tokens (path)
  "The tokens of Scheme source PATH: what lies between delimiters,
comments and strings skipped.  Enough to find identifiers by shape."
  (let ((tokens '()))
    (with-open-file (in path :external-format :utf-8)
      (let ((token (make-string-output-stream)))
	(flet ((flush ()
		 (let ((s (get-output-stream-string token)))
		   (when (plusp (length s)) (push s tokens)))))
	  (loop for c = (read-char in nil)
		while c
		do (cond ((char= c #\;) (flush) (read-line in nil))
			 ((char= c #\") (flush)
			  (loop for d = (read-char in nil)
				until (or (null d) (char= d #\"))
				do (when (char= d #\\) (read-char in nil))))
			 ((or (member c '(#\( #\) #\[ #\] #\' #\` #\,)) (sb-unicode:whitespace-p c))
			  (flush))
			 (t (write-char c token))))
	  (flush))))
    tokens))

(defun chez-internal-name (token)
  "The Chez name TOKEN refers to, if it's an internal one: $name, or a
primitive reference #%name, #2%name, #3%name."
  (let ((name (cond ((and (> (length token) 2) (string= "#%" token :end2 2)) (subseq token 2))
		    ((and (> (length token) 3) (char= (char token 0) #\#)
			  (digit-char-p (char token 1)) (char= (char token 2) #\%))
		     (subseq token 3))
		    ((and (> (length token) 1) (char= (char token 0) #\$)
			  (alpha-char-p (char token 1)))
		     token))))
    (and name (plusp (length name)) (not (find #\. name)) name)))

(defun fork-sources ()
  (append (directory (racket-source "racket/src/cs/rumble/*.ss"))
	  (list (racket-source "racket/src/cs/rumble.sls"))
	  (directory (racket-source "racket/src/cs/schemified/*.scm"))))

(defun chez-bound-p (name)
  "Is NAME bound at the Chez top level, or, as #%NAME says it, a name of
the (chezscheme) library's body (the $-names it doesn't export)?"
  (pseudoscheme-api::ensure-psyntax)
  (or (truthy (ps-r7rs::eval-at-chez-top-level
	       (list (ps:intern-scheme-symbol "top-level-bound?")
		     (list (ps:intern-scheme-symbol "quote") (ps:intern-scheme-symbol name)))))
      (handler-case (progn (ps-r7rs::eval-at-chez-top-level
			    (list (ps:intern-scheme-symbol "$primitive") (ps:intern-scheme-symbol name)))
			   t)
	(error () nil))))

(defun upstream-chez-names ()
  "The names upstream Chez Scheme has (build/external/chezscheme/, as
utils/chez/setup.sh fetches it): those s/primdata.ss declares, and those
its s/*.ss files define; or NIL without the source."
  (let ((dir (asdf:system-relative-pathname :pseudoscheme "build/external/chezscheme/s/")))
    (when (probe-file dir)
      (let ((names (make-hash-table :test 'equal)))
	(dolist (file (directory (merge-pathnames "*.ss" dir)) names)
	  (let ((tokens (reverse (source-tokens file)))
		(primdata (string= (pathname-name file) "primdata")))
	    (loop for (a b) on tokens
		  do (when (and b (member a '("define" "define-who" "set-who!" "define-inline"
					      "$define" "define-library-entry")
					  :test #'string=))
		       (setf (gethash b names) t))
		     (when (and primdata (plusp (length a)) (not (find #\[ a)))
		       (setf (gethash a names) t)))))))))

(defun fork-inventory (&key (stream *standard-output*))
  "Milestone 0: the Chez internals ($name, #%name, #3%name) that rumble
and the schemified layers refer to and --chez doesn't define, with how
many references each has; and the names *FORK-ADDITIONS* lists, the same
way.  Names rumble defines itself are left out."
  (let ((counts (make-hash-table :test 'equal))
	(defined (make-hash-table :test 'equal)))
    (dolist (file (fork-sources))
      (let ((tokens (reverse (source-tokens file))))
	(loop for (a b) on tokens
	      do (when (and b (member a '("define" "define-syntax" "define-record-type" "define-values"
					  "define-inline" "define-primitive")
				      :test #'string=))
		   (setf (gethash b defined) t))
		 (let ((name (chez-internal-name a)))
		   (when name (incf (gethash name counts 0)))))))
    (loop for (nil . names) in *fork-additions*
	  do (dolist (n names) (unless (gethash n counts) (setf (gethash n counts) 0))))
    (let ((missing (sort (loop for name being the hash-keys of counts using (hash-value n)
			       unless (or (gethash name defined) (chez-bound-p name))
				 collect (cons name n))
			 #'> :key #'cdr)))
      (format stream "~&--chez lacks ~D of the ~D Chez internals rumble and the schemified layers use~%"
	      (length missing) (hash-table-count counts))
      (let ((upstream (upstream-chez-names)))
	(if (null upstream)
	    (format stream "~{  ~A (~D)~%~}" (loop for (name . n) in missing append (list name n)))
	    ;; where each goes (docs/racket.md, "The Chez fork's additions")
	    (flet ((show (title list)
		     (format stream "~&~A: ~D~%~{  ~A (~D)~%~}" title (length list)
			     (loop for (name . n) in list append (list name n)))))
	      (show "in upstream Chez too (src/chez/)"
		    (remove-if-not (lambda (e) (gethash (car e) upstream)) missing))
	      (show "only in Racket's fork (src/racket/cs/)"
		    (remove-if (lambda (e) (gethash (car e) upstream)) missing)))))
      missing)))
