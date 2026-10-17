;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-REPL -*-

;;;; Racket's language for the REPL: Racket's own REPL, as `racket' with
;;;; no arguments runs it (src/racket/linklet/repl.lisp), on the
;;;; expander.  At a terminal it reads through the line editor: its
;;;; current-prompt-read asks READ-INPUT for the text, and Racket's reader
;;;; finds the data in it.  Whether there is a terminal is rktio's answer
;;;; (src/racket/native/rktio.lisp) when librktio is built.  It has no
;;;; meta-commands: a comma is Racket's unquote, as at `racket's own prompt;
;;;; (exit) leaves, as does end of file.

(in-package "PSEUDOSCHEME-REPL")

(defparameter *racket-prompt* "> ")

(defun racket-interactive-p ()
  "Is there a terminal for the line editor?  rktio's answer when librktio
is built, else the shared REPL's."
  (if (psr::rktio-available-p)
      (and (psr::rktio-std-terminal-p 0)
	   (psr::rktio-std-terminal-p 1)
	   (not (equal (uiop:getenv "TERM") "dumb")))
      (interactive-p)))

(defun racket-input ()
  "The next input for Racket's REPL, from the line editor: a string, or
:EOF."
  (loop
    (let ((text (read-input (colored *racket-prompt* :bold :green)
			    :history (history-for (find-language :racket))
			    :completer (lambda (prefix)
					 (sort (remove-if-not
						(lambda (n) (and (>= (length n) (length prefix))
								 (string= prefix n :end2 (length prefix))))
						(psr::namespace-names))
					       #'string<))
			    :complete-p #'input-complete-p)))
      (unless (eq text :interrupt) (return text)))))

(defun run-racket-repl ()
  "Racket's REPL, until it ends, for ,language: NIL, the REPL then quits."
  (let ((*interactive* (and *interactive* (racket-interactive-p))))
    (psr::boot)
    (unless (string= (psr::strategy-name psr:*strategy*) "linklet")
      (say ";; The Racket REPL needs --racket=linklet.")
      (return-from run-racket-repl nil))
    (psr::linklet-repl :next-text (and *interactive* #'racket-input))
    nil))

(defun racket-repl-main ()
  "`pseudoscheme --racket' with no program: Racket's REPL as `racket'
runs it, without the other languages' banner and tip.  The exit status."
  (let ((*interactive* t))
    (psr::boot)
    (unless (string= (psr::strategy-name psr:*strategy*) "linklet")
      (format *error-output* "pseudoscheme: the Racket REPL needs --racket=linklet~%")
      (return-from racket-repl-main 1))
    (let ((*interactive* (racket-interactive-p)))
      (psr::linklet-repl :next-text (and *interactive* #'racket-input)))))

(define-language :racket
  :title "Racket" :prompt "> "
  :run #'run-racket-repl
  :read-forms (lambda (text) (list text))
  :eval (lambda (text) (psr:eval-string text))
  :names #'psr::namespace-names)
