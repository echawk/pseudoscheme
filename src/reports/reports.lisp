; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-R7RS -*-

;;;; The older Revised Reports as languages: the Revised Report on
;;;; Scheme (1978), R2RS (the "Revised Revised Report", 1985), R3RS,
;;;; IEEE 1178-1990 and R4RS, each a library of exactly the names its
;;;; report has, and a top level whose bindings are that library's, the
;;;; way R5RS's is (pseudoscheme r5rs).  docs/reports.md.
;;;;
;;;; R4RS and IEEE 1178 are subsets of (scheme r5rs).  R2RS and R3RS
;;;; differ in that the empty list is false, and have their own numeric
;;;; procedures, a few names R5RS lacks and #!true, #!false and #!null
;;;; (R2RS); the Revised Report is another language, MacLISP's Scheme.
;;;; Those are in reports.sls and rrs.sls, which this file installs.

(in-package "PSEUDOSCHEME-R7RS")

(defparameter *car-cdr-names*
  "caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr
   caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr cdaaar cdaadr
   cdadar cdaddr cddaar cddadr cdddar cddddr")

(defparameter *syntax-common*
  "quote lambda if set! cond case and or let let* letrec begin do quasiquote
   unquote unquote-splicing define else")

(defparameter *r4rs-names*
  (concatenate 'string
   *syntax-common* " delay
   not boolean? eqv? eq? equal? pair? cons car cdr set-car! set-cdr! " *car-cdr-names* "
   null? list? list length append reverse list-tail list-ref memq memv member
   assq assv assoc symbol? symbol->string string->symbol
   number? complex? real? rational? integer? exact? inexact? = < > <= >=
   zero? positive? negative? odd? even? max min + * - / abs quotient remainder
   modulo gcd lcm numerator denominator floor ceiling truncate round
   rationalize exp log sin cos tan asin acos atan sqrt expt make-rectangular
   make-polar real-part imag-part magnitude angle exact->inexact
   inexact->exact number->string string->number
   char? char=? char<? char>? char<=? char>=? char-ci=? char-ci<? char-ci>?
   char-ci<=? char-ci>=? char-alphabetic? char-numeric? char-whitespace?
   char-upper-case? char-lower-case? char->integer integer->char char-upcase
   char-downcase
   string? make-string string string-length string-ref string-set! string=?
   string-ci=? string<? string>? string<=? string>=? string-ci<? string-ci>?
   string-ci<=? string-ci>=? substring string-append string->list
   list->string string-copy string-fill!
   vector? make-vector vector vector-length vector-ref vector-set!
   vector->list list->vector vector-fill!
   procedure? apply map for-each force call-with-current-continuation
   call-with-input-file call-with-output-file input-port? output-port?
   current-input-port current-output-port with-input-from-file
   with-output-to-file open-input-file open-output-file close-input-port
   close-output-port read read-char peek-char eof-object? char-ready? write
   display newline write-char load transcript-on transcript-off")
  "R4RS (1991), Chapter 6 and the syntax of Chapters 4 and 5 (the c[ad]*r
compositions, which the SchemeXref list leaves out, are in the report).")

(defparameter *ieee-names*
  (let ((out (split-names *r4rs-names*)))
    (dolist (n '("with-input-from-file" "with-output-to-file" "load" "transcript-on"
		 "transcript-off"))
      (setq out (remove n out :test #'string=)))
    (format nil "~{~A~^ ~}" out))
  "IEEE 1178-1990: R4RS without what SchemeXref's list of the standard
leaves out (the two with-...-file procedures, load and the transcripts).")

(defparameter *r3rs-names*
  (let ((out (split-names *r4rs-names*)))
    (dolist (n '("list?" "string" "peek-char"))
      (setq out (remove n out :test #'string=)))
    (format nil "~{~A~^ ~} last-pair nil t" out))
  "R3RS (1986): R4RS less list?, string and peek-char, plus last-pair and
the crutches nil and t.")

(defparameter *r2rs-names*
  (concatenate 'string
   "quote lambda if cond case and or let let* letrec rec named-lambda define
   set! begin sequence do quasiquote unquote unquote-splicing else =>
   not eq? eqv? equal? pair? cons car cdr set-car! set-cdr! " *car-cdr-names* "
   null? list length append append! reverse list-ref list-tail last-pair
   memq memv member assq assv assoc symbol? symbol->string string->symbol
   number? complex? real? rational? integer? zero? positive? negative? odd?
   even? exact? inexact? = =? < <? > >? <= <=? >= >=? max min + * - / 1+ -1+
   abs quotient remainder modulo gcd lcm floor ceiling truncate round
   rationalize exp log expt sqrt sin cos tan asin acos atan make-rectangular
   make-polar real-part imag-part magnitude angle exact->inexact
   inexact->exact number->string string->number
   char? char=? char<? char>? char<=? char>=? char-ci=? char-ci<? char-ci>?
   char-ci<=? char-ci>=? char-upper-case? char-lower-case? char-alphabetic?
   char-numeric? char-whitespace? char->integer integer->char char-upcase
   char-downcase
   string? string-null? string=? string-ci=? string<? string>? string<=?
   string>=? string-ci<? string-ci>? string-ci<=? string-ci>=? make-string
   string-length string-ref substring string-append string->list list->string
   string-set! string-fill! string-copy substring-fill! substring-move-right!
   substring-move-left!
   vector? make-vector vector vector-length vector-ref vector-set!
   vector->list list->vector vector-fill!
   object-hash object-unhash
   apply map for-each call-with-current-continuation
   call-with-input-file call-with-output-file input-port? output-port?
   current-input-port current-output-port with-input-from-file
   with-output-to-file open-input-file open-output-file close-input-port
   close-output-port eof-object? read read-char char-ready? load write display
   newline write-char transcript-on transcript-off nil t")
  "R2RS (1985), whose Part II is a catalog of exactly these.")

(defparameter *reports*
  '((:r4rs "r4rs" "R4RS" :r5rs-only)
    (:ieee "ieee" "IEEE 1178-1990" :r5rs-only)
    (:r3rs "r3rs" "R3RS" :empty-list-false)
    (:r2rs "r2rs" "R2RS" :empty-list-false)
    (:rrs "rrs" "the Revised Report" :own))
  "(keyword library-name title kind) of each report.")

(defun report-names (report)
  (ecase report
    (:r4rs *r4rs-names*) (:ieee *ieee-names*) (:r3rs *r3rs-names*) (:r2rs *r2rs-names*)))

(defparameter *extras-names*
  (split-names "define named-lambda rec sequence =? <? >? <=? >=? 1+ -1+ append! last-pair
    string-null? substring-fill! substring-move-left! substring-move-right! object-hash
    object-unhash transcript-on transcript-off number->string string->number rationalize")
  "What (pseudoscheme reports extras) exports.")

(defparameter *truth-names* (split-names "if cond and or do not")
  "What (pseudoscheme reports truth) exports.")

(defun report-library-form (report)
  "The library form for REPORT (not the Revised Report, which rrs.sls has)."
  (destructuring-bind (key name title kind) (assoc report *reports*)
    (declare (ignore key title))
    (let* ((names (remove-duplicates (split-names (report-names report)) :test #'string= :from-end t))
	   (own (member kind '(:empty-list-false)))
	   (variables (remove-if-not (lambda (n) (member n '("t" "nil") :test #'string=)) names))
	   (truth (and own (intersection names *truth-names* :test #'string=)))
	   (extras (intersection (set-difference names truth :test #'string=)
				 (ecase report
				   ((:r4rs :ieee) '("transcript-on" "transcript-off"))
				   (:r3rs '("transcript-on" "transcript-off" "number->string"
					    "string->number" "rationalize" "last-pair"))
				   (:r2rs *extras-names*))
				 :test #'string=))
	   (r5rs (set-difference (set-difference (set-difference names truth :test #'string=)
						 extras :test #'string=)
				 variables :test #'string=))
	   (known (library-export-strings '(scheme r5rs))))
      ;; what the report calls what R5RS doesn't have must come from the extras
      (let ((missing (remove-if (lambda (n) (member n known :test #'string=)) r5rs)))
	(when missing (error "reports: ~A has names nobody defines: ~{~A~^ ~}" name missing)))
      (read-scheme
       (format nil "(library (pseudoscheme reports ~A)
  (export ~{~A~^ ~})
  (import (only (scheme r5rs) ~{~A~^ ~})
          (only (pseudoscheme reports truth) ~{~A~^ ~})
          (only (pseudoscheme reports extras) ~{~A~^ ~})
          (prefix (only (rnrs) define quote) r6:))
  ~{~A~^ ~})"
	       name names r5rs truth extras
	       (loop for v in variables
		     collect (format nil "(r6:define ~A ~A)" v
				     (cond ((string= v "t") "#t")
					   ((eq report :r2rs) "(r6:quote ())")
					   (t "#f")))))))))

;;; ------------------------------------------------------------------
;;; Installing them

(defun report-library-name (report)
  (list "pseudoscheme" "reports" (second (assoc report *reports*))))

(defun report-interaction-library (report)
  (append (report-library-name report) (list "interaction")))

(defun eval-report-library (form)
  (handler-bind ((serious-condition
		   (lambda (e)
		     (format *error-output* "~&reports: installing ~A: ~A~%" (cadr form) e))))
    (psx:eval-library form)))

(defun install-report-libraries ()
  "Install the report libraries and their top levels.  Called while the
standard libraries are installed."
  (dolist (file '("src/reports/reports.sls" "src/reports/rrs.sls"))
    (dolist (form (read-forms (asdf:system-relative-pathname :pseudoscheme file)))
      (eval-report-library form)))
  (dolist (entry *reports*)
    (let ((report (first entry)))
      (unless (eq report :rrs)
	(eval-report-library (report-library-form report)))
      (install-interaction-library (report-interaction-library report)))))

(defun at-report-top-level-p ()
  "The report whose top level is being evaluated at, or NIL."
  (and psx:*host*
       (let ((name (mapcar #'sname* (funcall (psx:host-ref "psyntax:interaction-library-name")))))
	 (and (= (length name) 4)
	      (equal (subseq name 0 2) '("pseudoscheme" "reports"))
	      (equal (fourth name) "interaction")
	      (car (find (third name) *reports* :key #'second :test #'string=))))))

(defun report-keyword-p (x)
  (and (assoc x *reports*) t))

;;; #!null, in code, is a quoted () -- psyntax doesn't take a bare () --
;;; and in data is ().  The reader leaves a marker, which becomes one or
;;; the other when a form is evaluated.

(defvar *null-marker* (make-symbol "#!NULL"))

(defun replace-null-markers (form &optional quoted)
  (cond ((eq form *null-marker*) (if quoted '() (list (ssym "quote") '())))
	((consp form)
	 (let ((quoted (or quoted (head-is form "quote"))))
	   (cons (replace-null-markers (car form) quoted)
		 (replace-null-markers-tail (cdr form) quoted))))
	((simple-vector-p form) (map 'vector (lambda (x) (replace-null-markers x quoted)) form))
	(t form)))

(defun replace-null-markers-tail (form quoted)
  (cond ((consp form)
	 (cons (replace-null-markers (car form) quoted)
	       (replace-null-markers-tail (cdr form) quoted)))
	(t (replace-null-markers form quoted))))

(defun report-literal-reader (value)
  "A reader directive for #!true and its kind: reads as VALUE, and then
the port goes on being read as before."
  (lambda ()
    (lambda (port)
      (remhash port ps::*port-readers*)
      value)))

(setf ps:*reader-directives*
      (append (list (cons "true" (report-literal-reader t))
		    (cons "false" (report-literal-reader 'ps::false))
		    (cons "null" (report-literal-reader *null-marker*)))
	      ps:*reader-directives*))

(defun eval-at-report-top-level (report form)
  "Evaluate FORM at REPORT's top level: the bindings of its library, in an
interaction library of its own."
  (boot)
  (with-psyntax-parameter ("psyntax:interaction-library-name"
			   (mapcar #'ssym (report-interaction-library report)))
    (with-psyntax-parameter ("psyntax:interaction-source-name"
			     (mapcar #'ssym (report-library-name report)))
      (psx:eval-top-level (replace-null-markers form)))))

(defun load-file-at-report-top-level (report path)
  "Load PATH's forms one by one at REPORT's top level; case is folded, as
the reports say identifiers are."
  (let ((*include-directory* (make-pathname :name nil :type nil :defaults (pathname path)))
	(value ps:unspecific))
    (dolist (form (read-forms path :fold-case t) value)
      (setq value (eval-at-report-top-level report form)))))
