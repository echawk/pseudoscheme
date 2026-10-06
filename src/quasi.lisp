; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PS -*-

;;;; Quasi-literals: SRFI 109's &{text}, SRFI 108's &name[args]{text}
;;;; and SRFI 107's #<tag attr="value">content</tag>
;;;;
;;;; read.scm calls READ-QUASI-LITERAL after a token & or &name followed
;;;; by { or [, and READ-XML-LITERAL after #<.  They return the forms the
;;;; SRFIs specify:
;;;;
;;;;   &{a&[x]b}            ($string$ "a" $<<$ x $>>$ "b")
;;;;   &name[i ...]{text}   ($construct$:name i ... $>>$ text ...)
;;;;   &name{text}          ($construct$:name text ...)
;;;;   #<a k="v">t</a>      ($xml-element$ () ($resolve-qname$ a)
;;;;                          ($xml-attribute$ 'k "v") "t")
;;;;
;;;; In text: &[e ...] and &(e) are enclosed expressions, &name; an entity
;;;; (amp, lt, gt, quot, apos, lbrace, rbrace, R7RS's character names and
;;;; a few XML ones are its text; any other is the variable
;;;; $entity$:name), &#N; and &#xN; characters, &- a line continuation,
;;;; &| an indentation marker, &#|...|# a comment, and &name{...} a
;;;; nested constructor.  The libraries (srfi 107), (srfi 108) and (srfi
;;;; 109) give the forms their meanings.

(in-package "PS")

(defun quasi-error (port message &rest irritants)
  (apply #'scheme-reading-error port message irritants))

(defun qsym (name) (intern-scheme-symbol name))

(defparameter *quasi-entities*
  '(("amp" . "&") ("lt" . "<") ("gt" . ">") ("quot" . "\"") ("apos" . "'")
    ("lbrace" . "{") ("rbrace" . "}") ("lbracket" . "[") ("rbracket" . "]")
    ("null" . #.(string (code-char 0))) ("alarm" . #.(string (code-char 7)))
    ("backspace" . #.(string (code-char 8))) ("tab" . #.(string (code-char 9)))
    ("newline" . #.(string (code-char 10))) ("return" . #.(string (code-char 13)))
    ("escape" . #.(string (code-char 27))) ("space" . " ")
    ("delete" . #.(string (code-char 127))) ("nbsp" . #.(string (code-char 160)))
    ("copy" . #.(string (code-char 169))) ("reg" . #.(string (code-char 174)))
    ("aelig" . #.(string (code-char 230))) ("AElig" . #.(string (code-char 198)))
    ("oslash" . #.(string (code-char 248))) ("Oslash" . #.(string (code-char 216)))
    ("aring" . #.(string (code-char 229))) ("Aring" . #.(string (code-char 197)))
    ("eacute" . #.(string (code-char 233))) ("egrave" . #.(string (code-char 232)))
    ("auml" . #.(string (code-char 228))) ("ouml" . #.(string (code-char 246)))
    ("uuml" . #.(string (code-char 252))) ("szlig" . #.(string (code-char 223)))
    ("hellip" . #.(string (code-char 8230))) ("mdash" . #.(string (code-char 8212)))
    ("ndash" . #.(string (code-char 8211))))
  "Entity names whose text the reader puts in place.")

(defun name-char-p (c)
  (and (characterp c)
       (or (alphanumericp c) (find c "-_.:+*!?$%^~"))))

(defun read-quasi-name (port)
  (with-output-to-string (s)
    (loop for c = (peek-char nil port nil nil)
	  while (and c (name-char-p c))
	  do (write-char (read-char port) s))))

(defun skip-quasi-whitespace (port)
  "Whitespace and ; comments."
  (loop for c = (peek-char nil port nil nil)
	do (cond ((null c) (return))
		 ((member c '(#\Space #\Tab #\Newline #\Return #\Page)) (read-char port))
		 ((char= c #\;) (read-line port nil))
		 (t (return)))))

(defun read-enclosed (port close)
  "Data up to the character CLOSE, which is consumed."
  (loop with forms = '()
	do (skip-quasi-whitespace port)
	   (let ((c (peek-char nil port nil nil)))
	     (cond ((null c) (quasi-error port "end of file in an enclosed expression"))
		   ((char= c close) (read-char port) (return (nreverse forms)))
		   (t (push (funcall *scheme-read* port) forms))))))

;;; Text: a list of parts, strings and forms.

(defun read-quasi-text (port mode)
  "Read text up to its end -- MODE :BRACE, an unmatched }; :XML, a < (left
unread); #\\\" or #\\', that quote -- and return its parts."
  (let ((parts '())
	(text (make-string-output-stream))
	(depth 0)
	(seen-marker nil)
	(start t))		; nothing but whitespace yet
    (labels ((flush ()
	       (let ((s (get-output-stream-string text)))
		 (when (plusp (length s)) (push s parts))))
	     (add (part) (flush) (push part parts))
	     (pending () (let ((s (get-output-stream-string text)))
			   ;; peek: put it back
			   (write-string s text)
			   s)))
      (loop
	(let ((c (read-char port nil nil)))
	  (cond
	    ((null c) (quasi-error port "end of file in a quasi-literal"))
	    ((and (eq mode :brace) (char= c #\{))
	     (incf depth) (write-char c text) (setq start nil))
	    ((and (eq mode :brace) (char= c #\}))
	     (if (zerop depth)
		 (progn (flush) (return (nreverse parts)))
		 (progn (decf depth) (write-char c text))))
	    ((and (eq mode :xml) (char= c #\<))
	     (unread-char c port) (flush) (return (nreverse parts)))
	    ((and (characterp mode) (char= c mode))
	     (flush) (return (nreverse parts)))
	    ((char= c #\Return)
	     (when (eql (peek-char nil port nil nil) #\Newline) (read-char port))
	     (write-char #\Newline text))
	    ((char= c #\&)
	     (let ((d (peek-char nil port nil nil)))
	       (cond
		 ((null d) (quasi-error port "end of file after &"))
		 ((char= d #\[)
		  (read-char port)
		  (add (qsym "$<<$"))
		  (dolist (e (read-enclosed port #\])) (add e))
		  (add (qsym "$>>$")))
		 ((char= d #\()
		  (add (qsym "$<<$")) (add (funcall *scheme-read* port)) (add (qsym "$>>$")))
		 ((char= d #\-)		; line continuation
		  (read-char port)
		  (loop for e = (read-char port nil nil)
			until (or (null e) (char= e #\Newline))))
		 ((char= d #\|)		; indentation marker
		  (read-char port)
		  (let* ((s (pending))
			 (nl (position #\Newline s :from-end t))
			 (tail (subseq s (if nl (1+ nl) 0))))
		    (unless (every (lambda (x) (member x '(#\Space #\Tab))) tail)
		      (quasi-error port "text before an indentation marker &|"))
		    (let ((kept (cond ((and (not seen-marker) start) "")
				      (nl (subseq s 0 (1+ nl)))
				      (t ""))))
		      (get-output-stream-string text)
		      (write-string kept text))
		    (setq seen-marker t)))
		 ((char= d #\#)
		  (read-char port)
		  (let ((e (peek-char nil port nil nil)))
		    (cond ((eql e #\|)	; &#| comment |#
			   (read-char port)
			   (loop with prev = nil
				 for x = (read-char port nil nil)
				 do (cond ((null x) (quasi-error port "end of file in a comment"))
					  ((and (eql prev #\|) (char= x #\#)) (return)))
				    (setq prev x)))
			  (t		; &#N; or &#xN;
			   (let* ((hex (when (member e '(#\x #\X)) (read-char port) t))
				  (digits (with-output-to-string (s)
					    (loop for x = (read-char port nil nil)
						  until (or (null x) (char= x #\;))
						  do (write-char x s))))
				  (n (ignore-errors (parse-integer digits :radix (if hex 16 10)))))
			     (unless n (quasi-error port "bad character reference" digits))
			     (write-char (code-char n) text) (setq start nil))))))
		 ((name-char-p d)
		  (let* ((name (read-quasi-name port))
			 (e (peek-char nil port nil nil)))
		    (cond ((eql e #\;)
			   (read-char port)
			   (let ((entity (assoc name *quasi-entities* :test #'string=)))
			     (if entity
				 (write-string (cdr entity) text)
				 (add (qsym (concatenate 'string "$entity$:" name))))
			     (setq start nil)))
			  ((member e '(#\{ #\[))
			   (add (read-quasi-literal name port)))
			  (t (quasi-error port "& must be followed by an entity, a constructor or an escape"
					  name)))))
		 (t (quasi-error port "unknown & escape" d)))))
	    (t (write-char c text)
	       (unless (member c '(#\Space #\Tab #\Newline)) (setq start nil)))))))))

(defun read-quasi-literal (name port)
  "After & or &NAME, at { or [: SRFI 109's string or SRFI 108's
constructor."
  (let ((initial (when (eql (peek-char nil port nil nil) #\[)
		   (read-char port)
		   (read-enclosed port #\]))))
    (unless (eql (read-char port nil nil) #\{)
      (quasi-error port "&name[...] must be followed by {...}" name))
    (let ((text (read-quasi-text port :brace)))
      (if (string= name "")
	  (cons (qsym "$string$") text)
	  (cons (qsym (concatenate 'string "$construct$:" name))
		(append initial (when initial (list (qsym "$>>$"))) text))))))

;;; SRFI 107

(defun xml-name-form (port element)
  "A name: prefix:local, [expression] or (expression)."
  (let ((c (peek-char nil port nil nil)))
    (cond ((eql c #\[) (read-char port) (car (read-enclosed port #\])))
	  ((eql c #\() (funcall *scheme-read* port))
	  (t (let* ((name (read-quasi-name port))
		    (colon (position #\: name)))
	       (when (string= name "") (quasi-error port "missing XML name"))
	       (cond (colon (list (qsym "$resolve-qname$") (qsym (subseq name (1+ colon)))
				  (qsym (subseq name 0 colon))))
		     (element (list (qsym "$resolve-qname$") (qsym name)))
		     (t (list (qsym "quote") (qsym name)))))))))

(defun read-xml-until (port end)
  (with-output-to-string (s)
    (loop with n = (length end)
	  with buffer = '()
	  for c = (read-char port nil nil)
	  do (unless c (quasi-error port "end of file in an XML literal"))
	     (write-char c s)
	     (push c buffer)
	     (when (and (>= (length buffer) n)
			(string= end (coerce (reverse (subseq buffer 0 n)) 'string)))
	       (return)))))

(defun read-xml-literal (port)
  "After #<: an XML element, comment, CDATA section or processing
instruction."
  (let ((c (peek-char nil port nil nil)))
    (cond
      ((eql c #\!)
       (read-char port)
       (cond ((eql (peek-char nil port nil nil) #\-)
	      (read-char port)
	      (unless (eql (read-char port nil nil) #\-) (quasi-error port "bad XML comment"))
	      (let ((s (read-xml-until port "-->")))
		(list (qsym "$xml-comment$") (subseq s 0 (- (length s) 3)))))
	     (t (let ((head (read-xml-until port "[CDATA[")))
		  (unless (string= head "[CDATA[") (quasi-error port "bad XML <!" head))
		  (let ((s (read-xml-until port "]]>")))
		    (list (qsym "$xml-CDATA$") (subseq s 0 (- (length s) 3))))))))
      ((eql c #\?)
       (read-char port)
       (let* ((target (read-quasi-name port))
	      (s (read-xml-until port "?>")))
	 (list (qsym "$xml-processing-instruction$") target
	       (string-left-trim " " (subseq s 0 (- (length s) 2))))))
      (t (read-xml-element port)))))

(defun read-xml-element (port)
  (let ((name (xml-name-form port t))
	(namespaces '())
	(attributes '()))
    ;; attributes
    (loop
      (skip-quasi-whitespace port)
      (let ((c (peek-char nil port nil nil)))
	(cond
	  ((null c) (quasi-error port "end of file in an XML tag"))
	  ((char= c #\>) (read-char port) (return))
	  ((char= c #\/)
	   (read-char port)
	   (unless (eql (read-char port nil nil) #\>) (quasi-error port "bad XML tag end"))
	   (return-from read-xml-element
	     (list* (qsym "$xml-element$") (nreverse namespaces) name (nreverse attributes))))
	  ((char= c #\[)		; an enclosed expression
	   (read-char port)
	   (dolist (e (read-enclosed port #\])) (push e attributes)))
	  (t
	   (let* ((attribute-name (read-quasi-name port))
		  (value (progn
			   (skip-quasi-whitespace port)
			   (unless (eql (read-char port nil nil) #\=)
			     (quasi-error port "XML attribute without a value" attribute-name))
			   (skip-quasi-whitespace port)
			   (let ((q (read-char port nil nil)))
			     (case q
			       ((#\" #\') (read-quasi-text port q))
			       (#\[ (read-enclosed port #\]))
			       (#\( (unread-char q port) (list (funcall *scheme-read* port)))
			       (t (quasi-error port "bad XML attribute value" attribute-name)))))))
	     (cond ((string= attribute-name "xmlns")
		    (push (cons (qsym "") value) namespaces))
		   ((and (> (length attribute-name) 6) (string= "xmlns:" attribute-name :end2 6))
		    (push (cons (qsym (subseq attribute-name 6)) value) namespaces))
		   (t
		    (let ((colon (position #\: attribute-name)))
		      (push (list* (qsym "$xml-attribute$")
				   (if colon
				       (list (qsym "$resolve-qname$") (qsym (subseq attribute-name (1+ colon)))
					     (qsym (subseq attribute-name 0 colon)))
				       (list (qsym "quote") (qsym attribute-name)))
				   value)
			    attributes)))))))))
    ;; content, up to </name> or </>
    (let ((content '()))
      (loop
	(dolist (part (read-quasi-text port :xml)) (push part content))
	(read-char port)		; the <
	(if (eql (peek-char nil port nil nil) #\/)
	    (progn
	      (read-char port)
	      (read-quasi-name port)
	      (skip-quasi-whitespace port)
	      (unless (eql (read-char port nil nil) #\>) (quasi-error port "bad XML end tag"))
	      (return))
	    (push (read-xml-literal port) content)))
      (list* (qsym "$xml-element$") (nreverse namespaces) name
	     (append (nreverse attributes) (nreverse content))))))
