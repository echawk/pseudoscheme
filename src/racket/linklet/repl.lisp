; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-RACKET -*-

;;;; Strategy B's REPL (docs/racket.md, "The REPL"): `racket' with no
;;;; arguments, as Racket CS's main.sps runs it.  It requires the init
;;;; library, racket/init, into the startup namespace, then
;;;; racket/interactive (which loads the user's init file, and xrepl if
;;;; it is installed), then runs Racket's own read-eval-print-loop
;;;; (collects/racket/repl.rkt) under the default prompt, as a module
;;;; body's forms run.  What the loop reads, evaluates and prints is
;;;; Racket's: current-prompt-read, current-read-interaction,
;;;; current-eval and current-print, the last racket/init's.
;;;;
;;;; The prompt is this file's only part of the loop: the default
;;;; current-prompt-read writes "> " and reads a datum from the
;;;; interaction input port, which is the host's stdin; ours flushes
;;;; the prompt first (so a terminal shows it), and, when the front end
;;;; has a line editor (src/repl/racket.lisp), takes the text from it
;;;; instead, a datum at a time as Racket's reader finds them, so that
;;;; "1 2" evaluates both.

(in-package "QUASISCHEME-RACKET")

(defun make-input-string-port (text &optional (name "stdin"))
  (funcall (native-function (gethash "open-input-string" *natives*)) text (rsym name)))

(defun read-interaction (in)
  "A datum (a syntax object, as the REPL's reader makes them) from the
port IN, by current-read-interaction, or the eof object."
  (funcall (funcall *current-read-interaction*) (object-name-of in) in))

(defun flushing-prompt-read ()
  "current-prompt-read's default, which flushes the prompt."
  (port-write-string "> " (current-port :output))
  (finish-output *standard-output*)
  (let ((in (funcall (funcall *current-get-interaction-input-port*))))
    (read-interaction in)))

(defun editor-prompt-read (next-text)
  "A current-prompt-read that takes its text from NEXT-TEXT, a function
of no arguments returning a string (the front end shows the prompt) or
:EOF."
  (let ((pending nil))			; a port of the text read so far
    (lambda ()
      (loop
	(when pending
	  ;; (an error in the reader leaves no port to read again)
	  (let* ((in pending)
		 (datum (progn (setq pending nil) (read-interaction in))))
	    (unless (eq datum qs:eof-object)
	      (setq pending in)
	      (return datum))))
	(let ((text (funcall next-text)))
	  (if (eq text :eof)
	      (return qs:eof-object)
	      (setq pending (make-input-string-port text))))))))

(defun linklet-repl (&key next-text (banner t))
  "Racket's REPL, as `racket' runs it, until end of file (or exit).
NEXT-TEXT: a line editor, as EDITOR-PROMPT-READ takes.  BANNER: write
the welcome line.  Returns when the loop ends; the exit status is 1 if
init-file loading or the loop's start failed, else 0 (as racket's)."
  (setq *uncaught* nil)
  (unless *expander-instance*
    (unless (instantiate-expander :stream nil)
      (racket-error "racket" "the expander didn't instantiate")))
  (let ((status 0))
    ;; the base of continuations and the default prompt, as LINKLET-LOAD
    ;; runs a module: errors Lisp raises become Racket's, outside the base
    (with-racket-errors
      (qsx::call-with-continuation-base
       (lambda ()
	 (call-with-continuation-prompt
	  (lambda ()
	    (with-racket-errors
	      ;; stdin, stdout and stderr as Racket's io layer makes them: rktio's
	      (when (rktio-available-p) (install-console-ports))
	      (when banner
		(expander-eval
		 (read-racket (make-string-input-stream
			       "(begin (display \"Welcome to Racket v\") (display (version)) (display \" [\") (display (system-type 'gc)) (display \"].\") (newline))"))))
	      (let ((default-print (expander-eval (read-racket (make-string-input-stream "(current-print)")))))
		(expander-eval (read-racket (make-string-input-stream "(namespace-require '(lib \"racket/init\"))")))
		;; racket/init prints with pretty-print; where our ports can't
		;; run it yet (its printing port is a prop:output-port struct),
		;; the default, which agrees for what fits a line
		(unless (truthy (expander-eval (read-racket (make-string-input-stream
							     "(with-handlers ([void (lambda (e) #f)]) (pretty-print 1 (open-output-string)) #t)"))))
		  (funcall (primitive "current-print") default-print)))
	      ;; racket/interactive: xrepl, if installed, and the init file
	      (call-with-continuation-prompt
	       (lambda ()
		 (expander-eval (read-racket (make-string-input-stream "(dynamic-require 'racket/interactive 0)"))))
	       *default-prompt-tag*
	       (lambda (&rest args) (declare (ignore args)) (setq status 1)))
	      (funcall *current-prompt-read*
		       (if next-text (editor-prompt-read next-text) #'flushing-prompt-read))
	      (expander-eval (read-racket (make-string-input-stream "(read-eval-print-loop)")))))
	  *default-prompt-tag* nil))))
    (port-write-string (string #\Newline) (current-port :output))
    (finish-output)
    status))

(defun namespace-names ()
  "The names bound in the current namespace, strings, for completion; NIL
if the REPL isn't running."
  (ignore-errors
   (with-racket-errors
     (and *expander-booted*
	  (loop for s in (expander-eval (read-racket (make-string-input-stream "(namespace-mapped-symbols)")))
		collect (rname s))))))
