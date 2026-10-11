;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-REPL -*-

;;;; The REPL's line editor, in Lisp over the raw terminal
;;;; (terminal.lisp): multi-line input that Enter submits once its
;;;; parentheses close, automatic indentation, highlighting and
;;;; matching parentheses (highlight.lisp), history with incremental
;;;; search, completion, Emacs's editing keys, undo, and bracketed
;;;; paste.  READ-INPUT is the entry point.

(in-package "PSEUDOSCHEME-REPL")

;;; ------------------------------------------------------------------
;;; Options, shared with the REPL (,option)

(defvar *options*
  (list :color t :rainbow t :paredit nil :history-length 1000
	:print-numbered nil :indent t)
  "The REPL's options, a plist; ,option shows and sets them.")

(defun option (name) (getf *options* name))
(defun (setf option) (value name) (setf (getf *options* name) value))

;;; ------------------------------------------------------------------
;;; History

(defclass history ()
  ((entries :initform (make-array 0 :adjustable t :fill-pointer t) :reader history-entries)
   (file :initarg :file :initform nil :reader history-file)))

(defun history-directory ()
  (merge-pathnames "pseudoscheme/"
		   (uiop:ensure-directory-pathname
		    (or (uiop:getenv-absolute-directory "XDG_STATE_HOME")
			(merge-pathnames ".local/state/" (user-homedir-pathname))))))

(defun escape-entry (s)
  (with-output-to-string (out)
    (loop for c across s
	  do (case c (#\\ (write-string "\\\\" out)) (#\Newline (write-string "\\n" out))
		   (t (write-char c out))))))

(defun unescape-entry (s)
  (with-output-to-string (out)
    (loop with i = 0 while (< i (length s))
	  do (let ((c (char s i)))
	       (if (and (char= c #\\) (< (1+ i) (length s)))
		   (progn (write-char (if (char= (char s (1+ i)) #\n) #\Newline (char s (1+ i))) out)
			  (incf i 2))
		   (progn (write-char c out) (incf i)))))))

(defun load-history (name)
  "The history saved for language NAME (a file in
$XDG_STATE_HOME/pseudoscheme/), or an empty one."
  (let* ((file (merge-pathnames (format nil "history-~(~A~)" name) (history-directory)))
	 (h (make-instance 'history :file file)))
    (ignore-errors
     (with-open-file (in file :if-does-not-exist nil :external-format :utf-8)
       (when in
	 (loop for line = (read-line in nil)
	       while line
	       do (vector-push-extend (unescape-entry line) (history-entries h))))))
    h))

(defun add-history (h text)
  (let ((entries (history-entries h)))
    (unless (or (blank-input-p text)
		(and (> (length entries) 0) (string= text (aref entries (1- (length entries))))))
      (vector-push-extend text entries)
      (when (history-file h)
	(ignore-errors
	 (ensure-directories-exist (history-file h))
	 (let ((limit (option :history-length)))
	   (when (> (length entries) (* 2 limit))
	     ;; trim the file now and then
	     (let ((keep (subseq entries (- (length entries) limit))))
	       (setf (fill-pointer entries) 0)
	       (loop for e across keep do (vector-push-extend e entries))
	       (with-open-file (out (history-file h) :direction :output :if-exists :supersede
						     :external-format :utf-8)
		 (loop for e across entries do (write-line (escape-entry e) out)))
	       (return-from add-history))))
	 (with-open-file (out (history-file h) :direction :output :if-exists :append
					       :if-does-not-exist :create :external-format :utf-8)
	   (write-line (escape-entry text) out)))))))

;;; ------------------------------------------------------------------
;;; The editor's state

(defstruct (editor (:conc-name ed-))
  (buffer (make-array 0 :element-type 'character :adjustable t :fill-pointer t))
  (point 0)
  (prompt "> ")
  (continuation "")
  history
  (history-index 0)
  (saved nil)				; the input being edited, while in history
  completer
  (cursor-row 0)			; the cursor's row below the input's first, as drawn
  (kill "")
  (undo '())
  (last-command nil))

(defun text (ed) (coerce (ed-buffer ed) 'simple-string))

(defun set-text (ed string &optional (point (length string)))
  (let ((b (ed-buffer ed)))
    (setf (fill-pointer b) 0)
    (loop for c across string do (vector-push-extend c b))
    (setf (ed-point ed) (min point (length string)))))

(defun save-undo (ed)
  (push (cons (text ed) (ed-point ed)) (ed-undo ed)))

(defun insert (ed string)
  (let* ((s (text ed)) (p (ed-point ed)))
    (set-text ed (concatenate 'string (subseq s 0 p) string (subseq s p)) (+ p (length string)))))

(defun delete-range (ed start end)
  (let ((s (text ed)))
    (set-text ed (concatenate 'string (subseq s 0 start) (subseq s end)) start)))

;;; ------------------------------------------------------------------
;;; Drawing

(defun visible-columns (s)
  "S's width on the terminal, its escape sequences not counted."
  (let ((n 0) (i 0))
    (loop while (< i (length s))
	  do (if (char= (char s i) #\Esc)
		 (progn (incf i)
			(when (and (< i (length s)) (char= (char s i) #\[))
			  (incf i)
			  (loop while (and (< i (length s)) (not (alpha-char-p (char s i))))
				do (incf i))
			  (incf i)))
		 (progn (incf n (char-columns (char s i))) (incf i))))
    n))

(defun line-prefix (ed k)
  (if (= k 0) (ed-prompt ed) (ed-continuation ed)))

(defun render (ed)
  "Redraw the input from its first row, the cursor left at the point."
  (let* ((width (terminal-width))
	 (s (text ed))
	 (point (ed-point ed))
	 (mark (and (option :color)
		    (multiple-value-bind (a b) (matching-paren s point)
		      (and a (list a b)))))
	 (lines (uiop:split-string (highlight s :mark mark) :separator '(#\Newline)))
	 (row 0) (col 0) (point-row 0) (point-col 0) (index 0))
    ;; back to the first row
    (emit (if (> (ed-cursor-row ed) 0) (csi (ed-cursor-row ed) "A") "") (string #\Return) (csi "J"))
    (flet ((advance (w)
	     (when (> (+ col w) width) (incf row) (setq col 0))
	     (incf col w)))
      (loop for k from 0
	    for line in lines
	    for plain-line in (uiop:split-string s :separator '(#\Newline))
	    do (when (> k 0) (emit (string #\Newline)) (incf row) (setq col 0) (incf index))
	       (let ((prefix (line-prefix ed k)))
		 (emit prefix line)
		 (advance (visible-columns prefix))
		 (loop for c across plain-line
		       do (when (= index point)
			    (when (>= col width) (incf row) (setq col 0))
			    (setq point-row row point-col col))
			  (advance (char-columns c))
			  (incf index))
		 (when (= index point)
		   (when (>= col width) (incf row) (setq col 0))
		   (setq point-row row point-col col)))))
    ;; a full last row leaves the cursor waiting to wrap: make it wrap
    (when (>= col width)
      (emit " " (string #\Return) (csi "K"))
      (incf row) (setq col 0))
    (let ((up (- row point-row)))
      (emit (if (> up 0) (csi up "A") "") (string #\Return) (if (> point-col 0) (csi point-col "C") "")))
    (setf (ed-cursor-row ed) point-row)))

(defun message-below (ed text)
  "Show TEXT below the input, then redraw the input under it."
  (let ((s (text ed)) (point (ed-point ed)))
    (setf (ed-point ed) (length s))
    (render ed)
    (emit (string #\Newline) text (string #\Newline))
    (setf (ed-cursor-row ed) 0 (ed-point ed) point)
    (render ed)))

(defun finish-input (ed)
  "Leave the cursor below the input, for the output."
  (setf (ed-point ed) (length (ed-buffer ed)))
  (render ed)
  (emit (string #\Newline)))

;;; ------------------------------------------------------------------
;;; Indentation

(defun column-at (s index)
  (let ((line-start (1+ (or (position #\Newline s :end index :from-end t) -1))))
    (string-columns s :start line-start :end index)))

(defun line-start (s index) (1+ (or (position #\Newline s :end index :from-end t) -1)))

(defun indentation (s)
  "The column a new line after S should start at: under the first
argument of the innermost open form, or two in from its parenthesis for
a defining or binding form."
  (let ((frames '()))			; (open-index head-token arg-token)
    (dolist (tok (lex s))
      (flet ((element ()
	       (let ((f (car frames)))
		 (when f
		   (cond ((null (second f)) (setf (second f) tok))
			 ((null (third f)) (setf (third f) tok)))))))
	(case (token-kind tok)
	  (:open (element) (push (list (1- (token-end tok)) nil nil) frames))
	  (:close (pop frames))
	  (:comment)
	  (t (element)))))
    (let ((f (car frames)))
      (if (null f)
	  0
	  (destructuring-bind (open head arg) f
	    (let ((open-col (column-at s open))
		  (name (and head (member (token-kind head) '(:symbol :special))
			     (subseq s (token-start head) (token-end head)))))
	      (cond ((and name (or (special-form-p name)
				   (and (> (length name) 3) (string= "def" name :end2 3))
				   (and (> (length name) 4) (string= "with" name :end2 4))))
		     (+ open-col 2))
		    ((and head arg (member (token-kind head) '(:symbol :special))
			  (= (line-start s (token-start head)) (line-start s (token-start arg))))
		     (column-at s (token-start arg)))
		    (head (+ open-col 1))
		    (t (+ open-col 1)))))))))

(defun newline-and-indent (ed)
  (let ((before (subseq (text ed) 0 (ed-point ed))))
    (insert ed (format nil "~%~A" (make-string (if (option :indent) (indentation before) 0)
					       :initial-element #\Space)))))

(defun indent-line (ed)
  "Reindent the point's line."
  (let* ((s (text ed)) (start (line-start s (ed-point ed)))
	 (ws-end (or (position-if-not (lambda (c) (member c '(#\Space #\Tab))) s :start start) (length s)))
	 (target (indentation (subseq s 0 start)))
	 (offset (- (ed-point ed) ws-end)))
    (set-text ed (concatenate 'string (subseq s 0 start) (make-string target :initial-element #\Space)
			      (subseq s ws-end))
	      (+ start target (max offset 0)))))

;;; ------------------------------------------------------------------
;;; Completion

(defun word-start (s point)
  (let ((i point))
    (loop while (and (> i 0) (not (delimiter-p (char s (1- i))))) do (decf i))
    i))

(defun common-prefix (strings)
  (reduce (lambda (a b) (subseq a 0 (or (mismatch a b) (length a)))) strings))

(defun columns-text (items)
  (let* ((width (terminal-width))
	 (w (+ 2 (reduce #'max items :key #'length :initial-value 0)))
	 (per-row (max 1 (floor width w))))
    (with-output-to-string (out)
      (loop for item in items
	    for i from 1
	    do (format out "~VA" w item)
	       (when (and (zerop (mod i per-row)) (< i (length items))) (terpri out))))))

(defun complete (ed)
  (let* ((s (text ed)) (point (ed-point ed)) (start (word-start s point))
	 (prefix (subseq s start point))
	 (candidates (and (ed-completer ed)
			  (sort (remove-duplicates (funcall (ed-completer ed) prefix) :test #'string=)
				#'string<))))
    (cond ((null candidates) (emit (string (code-char 7))))
	  ((null (cdr candidates))
	   (insert ed (subseq (car candidates) (length prefix)))
	   (let ((s (text ed)) (p (ed-point ed)))
	     (when (or (= p (length s)) (not (delimiter-p (char s p))))
	       (insert ed " "))))
	  (t
	   (let ((common (common-prefix candidates)))
	     (if (> (length common) (length prefix))
		 (insert ed (subseq common (length prefix)))
		 (message-below ed (if (> (length candidates) 200)
				       (format nil "(~D possibilities)" (length candidates))
				       (columns-text candidates)))))))))

;;; ------------------------------------------------------------------
;;; Moving

(defun line-end (s index) (or (position #\Newline s :start index) (length s)))

(defun move-vertically (ed delta)
  "Move the point DELTA lines; NIL if there's no such line."
  (let* ((s (text ed)) (p (ed-point ed)) (start (line-start s p)) (col (- p start)))
    (cond ((and (< delta 0) (> start 0))
	   (let ((prev-start (line-start s (1- start))))
	     (setf (ed-point ed) (min (+ prev-start col) (1- start)))
	     t))
	  ((and (> delta 0) (< (line-end s p) (length s)))
	   (let* ((next-start (1+ (line-end s p))))
	     (setf (ed-point ed) (min (+ next-start col) (line-end s next-start)))
	     t))
	  (t nil))))

(defun word-constituent-p (c) (or (alphanumericp c) (find c "-_!?*<>=/+:.$%&~^")))

(defun forward-word (s p)
  (let ((n (length s)))
    (loop while (and (< p n) (not (word-constituent-p (char s p)))) do (incf p))
    (loop while (and (< p n) (word-constituent-p (char s p))) do (incf p))
    p))

(defun backward-word (s p)
  (loop while (and (> p 0) (not (word-constituent-p (char s (1- p))))) do (decf p))
  (loop while (and (> p 0) (word-constituent-p (char s (1- p)))) do (decf p))
  p)

(defun history-move (ed delta)
  (let* ((entries (history-entries (ed-history ed)))
	 (i (+ (ed-history-index ed) delta)))
    (when (<= 0 i (length entries))
      (when (= (ed-history-index ed) (length entries))
	(setf (ed-saved ed) (text ed)))
      (setf (ed-history-index ed) i)
      (set-text ed (if (= i (length entries)) (or (ed-saved ed) "") (aref entries i))))))

;;; ------------------------------------------------------------------
;;; Incremental search (Ctrl-R)

(defun search-history (ed)
  "Ctrl-R: search the history backward as a query is typed.  Returns a
key that ended the search for the caller to act on, or NIL."
  (let* ((entries (history-entries (ed-history ed)))
	 (query "") (from (length entries)) (found nil)
	 (original (text ed)) (prompt (ed-prompt ed)))
    (labels ((find-match (start)
	       (loop for i from (1- start) downto 0
		     when (search query (aref entries i)) return i))
	     (show ()
	       (setf (ed-prompt ed) (format nil "(search)`~A': " query))
	       (when found (set-text ed (aref entries found) (or (search query (aref entries found)) 0)))
	       (render ed)))
      (show)
      (unwind-protect
	   (loop
	     (let ((key (read-key)))
	       (cond ((eql key (code-char 18))	; Ctrl-R again: older
		      (let ((i (find-match (or found from)))) (when i (setq found i))))
		     ((or (eql key (code-char 7)) (eq key :escape)) ; Ctrl-G: cancel
		      (set-text ed original) (return nil))
		     ((member key (list (code-char 127) (code-char 8)))
		      (when (> (length query) 0)
			(setq query (subseq query 0 (1- (length query)))
			      found (find-match from))))
		     ((and (characterp key) (graphic-char-p key))
		      (setq query (concatenate 'string query (string key)))
		      (let ((i (find-match (if found (1+ found) from)))) (setq found i)))
		     (t (return key))))
	     (show))
	(setf (ed-prompt ed) prompt)))))

;;; ------------------------------------------------------------------
;;; The keys

(defun ctrl (c) (code-char (- (char-code (char-upcase c)) 64)))

(defun paredit-insert (ed c)
  (let* ((s (text ed)) (p (ed-point ed)) (next (and (< p (length s)) (char s p))))
    (cond ((member c '(#\( #\[ #\{))
	   (insert ed (coerce (list c (ecase c (#\( #\)) (#\[ #\]) (#\{ #\}))) 'string))
	   (decf (ed-point ed)))
	  ((and (member c '(#\) #\] #\})) (eql next c)) (incf (ed-point ed)))
	  ((and (char= c #\") (eql next #\")) (incf (ed-point ed)))
	  ((char= c #\") (insert ed "\"\"") (decf (ed-point ed)))
	  (t (insert ed (string c))))))

(defun handle-key (ed key complete-p)
  "Act on KEY.  Returns :submit, :eof, :interrupt, or NIL to read on."
  (let* ((s (text ed)) (p (ed-point ed)) (n (length s)))
    (flet ((kill (start end)
	     (save-undo ed)
	     (setf (ed-kill ed) (if (eq (ed-last-command ed) :kill)
				    (if (< start p) (concatenate 'string (subseq s start end) (ed-kill ed))
					(concatenate 'string (ed-kill ed) (subseq s start end)))
				    (subseq s start end)))
	     (delete-range ed start end)
	     (setf (ed-last-command ed) :kill)
	     (return-from handle-key nil)))
      (setf (ed-last-command ed)
	    (if (and (characterp key) (graphic-char-p key)) :insert nil))
      (cond
	((eq key :eof) :eof)
	((eql key #\Return)
	 (if (or (= n 0) (funcall complete-p s))
	     :submit
	     (progn (save-undo ed) (newline-and-indent ed) nil)))
	((or (eql key #\Newline) (eq key :alt-return))
	 (save-undo ed) (newline-and-indent ed) nil)
	((and (consp key) (eq (car key) :paste))
	 (save-undo ed) (insert ed (substitute #\Space #\Tab (cdr key))) nil)
	((and (characterp key) (graphic-char-p key))
	 (unless (eq (ed-last-command ed) :insert) (save-undo ed))
	 (setf (ed-last-command ed) :insert)
	 (if (option :paredit) (paredit-insert ed key) (insert ed (string key)))
	 nil)
	((eql key #\Tab)
	 (let ((start (word-start s p)))
	   (if (or (= start p) (char= (char s (1- p)) #\Newline))
	       (progn (save-undo ed) (indent-line ed))
	       (complete ed)))
	 nil)
	((member key (list (code-char 127) (code-char 8)))
	 (when (> p 0)
	   (save-undo ed)
	   (if (and (option :paredit) (< p n)
		    (member (cons (char s (1- p)) (char s p))
			    '((#\( . #\)) (#\[ . #\]) (#\{ . #\}) (#\" . #\")) :test #'equal))
	       (delete-range ed (1- p) (1+ p))
	       (delete-range ed (1- p) p)))
	 nil)
	((or (eq key :delete) (and (eql key (ctrl #\D)) (> n 0)))
	 (when (< p n) (save-undo ed) (delete-range ed p (1+ p))) nil)
	((eql key (ctrl #\D)) :eof)
	((eql key (ctrl #\C)) :interrupt)
	((or (eq key :left) (eql key (ctrl #\B))) (setf (ed-point ed) (max 0 (1- p))) nil)
	((or (eq key :right) (eql key (ctrl #\F))) (setf (ed-point ed) (min n (1+ p))) nil)
	((or (eq key :home) (eql key (ctrl #\A))) (setf (ed-point ed) (line-start s p)) nil)
	((or (eq key :end) (eql key (ctrl #\E))) (setf (ed-point ed) (line-end s p)) nil)
	((member key '(:alt-b :ctrl-left)) (setf (ed-point ed) (backward-word s p)) nil)
	((member key '(:alt-f :ctrl-right)) (setf (ed-point ed) (forward-word s p)) nil)
	((eq key :up) (unless (move-vertically ed -1) (history-move ed -1)) nil)
	((eq key :down) (unless (move-vertically ed 1) (history-move ed 1)) nil)
	((eql key (ctrl #\P)) (history-move ed -1) nil)
	((eql key (ctrl #\N)) (history-move ed 1) nil)
	((eq key :page-up) (history-move ed (- (ed-history-index ed))) nil)
	((eql key (ctrl #\K))
	 (let ((end (line-end s p))) (kill p (if (= end p) (min n (1+ p)) end))))
	((eql key (ctrl #\U)) (kill (line-start s p) p))
	((member key (list (ctrl #\W) :alt-backspace)) (kill (backward-word s p) p))
	((eq key :alt-d) (kill p (forward-word s p)))
	((eql key (ctrl #\Y)) (save-undo ed) (insert ed (ed-kill ed)) nil)
	((eql key (ctrl #\T))
	 (when (and (> p 0) (< p n))
	   (save-undo ed)
	   (let ((a (char s (1- p))) (b (char s p)))
	     (set-text ed (concatenate 'string (subseq s 0 (1- p)) (string b) (string a) (subseq s (1+ p)))
		       (1+ p))))
	 nil)
	((or (eql key (code-char 31)) (eql key (ctrl #\Z))) ; Ctrl-_: undo
	 (let ((u (pop (ed-undo ed))))
	   (when u (set-text ed (car u) (cdr u))))
	 nil)
	((eql key (ctrl #\L))
	 (emit (csi "H") (csi "2J")) (setf (ed-cursor-row ed) 0) nil)
	((eql key (ctrl #\R))
	 (let ((next (search-history ed)))
	   (and next (handle-key ed next complete-p))))
	(t nil)))))

(defun read-input (prompt &key (continuation nil) history completer (complete-p #'input-complete-p))
  "Read an input at the terminal, editing it: a string, or :EOF (Ctrl-D
on an empty input), or :INTERRUPT (Ctrl-C).  PROMPT may contain escape
sequences; continuation lines get CONTINUATION, by default spaces as wide
as the prompt.  COMPLETER, a function of a prefix, gives the completions."
  (let ((ed (make-editor :prompt prompt
			 :continuation (or continuation
					   (make-string (visible-columns prompt) :initial-element #\Space))
			 :history (or history (make-instance 'history))
			 :completer completer)))
    (setf (ed-history-index ed) (length (history-entries (ed-history ed))))
    (with-raw-terminal ()
      (render ed)
      (loop
	(let ((result (handle-key ed (read-key) complete-p)))
	  (case result
	    (:submit (finish-input ed)
	     (let ((s (text ed))) (add-history (ed-history ed) s) (return s)))
	    (:eof (if (zerop (length (ed-buffer ed)))
		      (progn (emit (string #\Newline)) (return :eof))
		      (render ed)))
	    (:interrupt (finish-input ed) (return :interrupt))
	    (t (render ed))))))))
