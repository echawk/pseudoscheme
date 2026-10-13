;;;; Write scheme.lisp, the table's DEFS entries, from GNU Guile's
;;;; reference manual (its doc/ref/*.texi), which documents each C
;;;; function that is a Scheme procedure next to it:
;;;;
;;;;   @deffn {Scheme Procedure} hashq-ref table key [dflt]
;;;;   @deffnx {C Function} scm_hashq_ref (table, key, dflt)
;;;;
;;;;   sbcl --script extract-manual.lisp GUILE-SOURCE/doc/ref EXPORTS > scheme.lisp
;;;;
;;;; EXPORTS is a file of the function names libguile exports, one per
;;;; line (`nm -D --defined-only libguile-3.0.so | awk '$2 == "T" {print $3}'`);
;;;; only those are written.  A C function whose argument count doesn't
;;;; match its procedure's is left out, with a comment.

(defun strip-texinfo (s)
  "S without @var{...} and the like: their contents stay."
  (with-output-to-string (out)
    (let ((i 0) (n (length s)))
      (loop while (< i n)
	    do (let ((c (char s i)))
		 (cond ((and (char= c #\@) (< (1+ i) n) (alpha-char-p (char s (1+ i))))
			(let ((brace (position #\{ s :start i))
			      (space (or (position-if-not #'alpha-char-p s :start (1+ i)) n)))
			  (cond ((and brace (= brace space))
				 (when (string= (subseq s (1+ i) brace) "dots")
				   (write-string "..." out))
				 (setq i (1+ brace)))
				(t (setq i space)))))
		       ((char= c #\}) (incf i))
		       (t (write-char c out) (incf i))))))))

(defun words (s)
  (let ((words '()) (start nil))
    (loop for i from 0 to (length s)
	  for c = (if (< i (length s)) (char s i) #\Space)
	  do (cond ((member c '(#\Space #\Tab))
		    (when start (push (subseq s start i) words) (setq start nil)))
		   ((member c '(#\[ #\]))
		    (when start (push (subseq s start i) words) (setq start nil))
		    (push (string c) words))
		   ((null start) (setq start i))))
    (nreverse words)))

(defun scheme-signature (line)
  "(name req opt rest) of a {Scheme Procedure} line."
  (let* ((text (strip-texinfo (subseq line (+ (search "}" line) 1))))
	 (ws (words text))
	 (name (first ws)) (req 0) (opt 0) (rest nil) (depth 0) (after-dot nil))
    (dolist (w (rest ws))
      (cond ((string= w "[") (incf depth))
	    ((string= w "]") (decf depth))
	    ((string= w ".") (setq after-dot t))
	    ((or (string= w "...") (search "..." w)) (setq rest t))
	    (after-dot (setq rest t after-dot nil))
	    ((> depth 0) (incf opt))
	    (t (incf req))))
    ;; "arg ..." counts arg as the rest, not a required argument
    (when (and rest (search "..." text) (not (search ". " text)) (> (+ req opt) 0))
      (if (> opt 0) (decf opt) (decf req)))
    (list name req opt rest)))

(defun c-signature (line)
  "(name nargs) of a {C Function} line."
  (let* ((text (strip-texinfo (subseq line (+ (search "}" line) 1))))
	 (open (position #\( text))
	 (close (position #\) text :from-end t)))
    (unless (and open close) (return-from c-signature nil))
    (let* ((name (string-trim " " (subseq text 0 open)))
	 (inside (string-trim " " (subseq text (1+ open) close))))
      (list name (if (or (string= inside "") (string= inside "void"))
		     0
		     (1+ (count #\, inside)))))))

(defun replace-all (s old new)
  (with-output-to-string (out)
    (loop with i = 0
	  for j = (search old s :start2 i)
	  do (write-string s out :start i :end (or j (length s)))
	     (if j (progn (write-string new out) (setq i (+ j (length old)))) (return)))))

(defun c-name (scheme-name)
  "The C name Guile's convention gives SCHEME-NAME: list->string is
scm_list_to_string, set-car! scm_set_car_x, pair? scm_pair_p."
  (let ((s scheme-name))
    (loop for (old new) in '(("->" "_to_") ("!" "_x") ("?" "_p") ("*" "_star")
			      ("<=" "_leq") (">=" "_geq") ("<" "_less") (">" "_gr")
			      ("=" "_num_eq") ("/" "_") ("-" "_") ("+" "_"))
	  do (setq s (replace-all s old new)))
    (concatenate 'string "scm_" (string-trim "_" s))))

(defun read-lines (path)
  "PATH's lines, a line ending in @ (texinfo's continuation) joined to the next."
  (with-open-file (in path :external-format :latin-1)
    (let ((lines '()) (pending nil))
      (loop for line = (read-line in nil) while line
	    do (when pending (setq line (concatenate 'string pending " " line) pending nil))
	       (if (and (plusp (length line)) (char= (char line (1- (length line))) #\@))
		   (setq pending (subseq line 0 (1- (length line))))
		   (push line lines)))
      (nreverse lines))))

(defun prefixp (prefix s) (and (>= (length s) (length prefix)) (string= prefix s :end2 (length prefix))))

(defun extract (directory exports-file)
  (let ((exports (make-hash-table :test 'equal))
	(seen (make-hash-table :test 'equal))
	(entries '()) (mismatches '()))
    (dolist (name (read-lines exports-file)) (setf (gethash (string-trim " " name) exports) t))
    (dolist (file (sort (directory (merge-pathnames "*.texi" directory)) #'string< :key #'namestring))
      (let ((block '()))
	(dolist (line (read-lines file))
	  (cond ((prefixp "@end deffn" line) (setq block '()))
		((or (prefixp "@deffn {Scheme Procedure}" line) (prefixp "@deffnx {Scheme Procedure}" line))
		 (push (scheme-signature line) block))
		((prefixp "@deffnx {C Function}" line)
		 (destructuring-bind (&optional c nargs) (c-signature line)
		   (unless (or (null c) (gethash c seen))
		     (let* ((fits (remove-if-not (lambda (s) (destructuring-bind (name req opt rest) s
							       (declare (ignore name))
							       (= nargs (+ req opt (if rest 1 0)))))
						 block))
			    ;; the procedure whose name gives C's, else the only one
			    ;; that fits
			    (match (or (find c fits :key (lambda (s) (c-name (first s))) :test #'string=)
				       (and (= (length fits) 1) (first fits)))))
		       (cond ((not (gethash c exports)))
			     (match (setf (gethash c seen) t)
				    (push (cons c match) entries))
			     (block (push (list c nargs (first (first block))) mismatches)))))))))))
    (format t ";;;; Generated by src/libguile/extract-manual.lisp from GNU Guile 3.0.11's~%")
    (format t ";;;; reference manual: don't edit.  libguile's C functions that are~%")
    (format t ";;;; Scheme procedures (table.lisp, DEFS): the C name, the procedure,~%")
    (format t ";;;; its required and optional arguments, and whether it has a rest list.~%~%")
    (format t "(in-package \"PSEUDOSCHEME-LIBGUILE\")~%~%")
    (dolist (e (sort entries #'string< :key #'car))
      (destructuring-bind (c name req opt rest) e
	(format t "(defs ~S ~S ~D ~D ~:[nil~;t~])~%" c name req opt rest)))
    (when mismatches
      (format t "~%;;; Left out: argument counts that don't match the procedure's~%")
      (dolist (m (sort mismatches #'string< :key #'first))
	(format t ";;;   ~A (~D arguments), ~A~%" (first m) (second m) (third m))))))

(extract (pathname (let ((d (second sb-ext:*posix-argv*)))
		     (if (char= (char d (1- (length d))) #\/) d (concatenate 'string d "/"))))
	 (third sb-ext:*posix-argv*))
