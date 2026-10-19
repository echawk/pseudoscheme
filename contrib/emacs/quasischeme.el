;;; quasischeme.el --- Scheme on Quasischeme, through SLY or SLIME  -*- lexical-binding: t -*-

;; Author: Quasischeme's authors
;; Keywords: languages, lisp, scheme
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Quasischeme is Scheme (R7RS, R6RS, R5RS, Chez Scheme, Guile, the
;; older reports) on Common Lisp.  `bin/quasischeme --lisp' is a Lisp
;; with all of it loaded, which SLY or SLIME can start and connect to as
;; usual.  This package then talks Scheme to it through whichever is
;; connected, calling the functions of its QUASISCHEME-IDE package
;; (src/repl/ide.lisp):
;;
;; - `quasischeme-mode', a minor mode for Scheme buffers: evaluate the
;;   form before point, the definition, the region or the file in the
;;   buffer's language; completion; the arglist of the procedure called
;;   at point in the echo area (eldoc); ,describe, ,expand, apropos.
;; - `quasischeme-repl', a REPL buffer per language, with the REPL's
;;   meta-commands (,describe ,expand ,trace ,bt ,language ...).
;;
;; sly-quasischeme.el and slime-quasischeme.el start Quasischeme
;; under SLY or SLIME.  See docs/emacs.md.

;;; Code:

(require 'cl-lib)
(require 'scheme)
(require 'eldoc)
(require 'ring)

(defgroup quasischeme nil
  "Scheme on Quasischeme, through SLY or SLIME."
  :group 'scheme
  :prefix "quasischeme-")

(defcustom quasischeme-program "quasischeme"
  "The quasischeme command (bin/quasischeme in a checkout)."
  :type 'string)

(defcustom quasischeme-default-language "r7rs"
  "The language of a Scheme buffer `quasischeme-guess-language' can't place."
  :type '(choice (const "r7rs") (const "r6rs") (const "r5rs") (const "chez")
                 (const "guile") (const "r4rs") (const "ieee") (const "r3rs")
                 (const "r2rs") (const "rrs") string))

(defcustom quasischeme-language-alist
  '(("\\.sls\\'" . "r6rs") ("\\.sps\\'" . "r6rs") ("\\.ss\\'" . "chez")
    ("\\.sld\\'" . "r7rs"))
  "File name patterns and the language of the files they match.
A .scm file is Guile's if it has a define-module or use-modules form,
else `quasischeme-default-language'."
  :type '(alist :key-type regexp :value-type string))

;;; ------------------------------------------------------------------
;;; Talking to the Lisp: SLY or SLIME, whichever is connected

(declare-function sly-connected-p "sly")
(declare-function sly-eval "sly")
(declare-function sly-eval-async "sly")
(declare-function slime-connected-p "slime")
(declare-function slime-eval "slime")
(declare-function slime-eval-async "slime")

(defun quasischeme--backend ()
  "`sly' or `slime', whichever is connected; nil if neither."
  (cond ((and (fboundp 'sly-connected-p) (sly-connected-p)) 'sly)
        ((and (fboundp 'slime-connected-p) (slime-connected-p)) 'slime)))

(defun quasischeme--check-connected ()
  (or (quasischeme--backend)
      (user-error "Quasischeme isn't running: M-x sly-quasischeme or M-x slime-quasischeme")))

(defun quasischeme--eval (form)
  "Evaluate the Lisp FORM in the connected Lisp; its value."
  (pcase (quasischeme--check-connected)
    ('sly (sly-eval form))
    ('slime (slime-eval form))))

(defun quasischeme--eval-async (form continuation)
  "Evaluate the Lisp FORM in the connected Lisp, then call CONTINUATION
with its value."
  (pcase (quasischeme--check-connected)
    ('sly (sly-eval-async form continuation))
    ('slime (slime-eval-async form continuation))))

;;; ------------------------------------------------------------------
;;; The language of a buffer

(defvar-local quasischeme-language nil
  "The language this buffer's Scheme is evaluated in: \"r7rs\", \"chez\", ...
Set by `quasischeme-mode' from the file's name and contents.")

(defun quasischeme-guess-language ()
  "The language of the current buffer's file, by its name and contents."
  (let ((name (or buffer-file-name "")))
    (or (cdr (cl-find-if (lambda (entry) (string-match-p (car entry) name))
                         quasischeme-language-alist))
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "^(\\(define-module\\|use-modules\\)\\_>" 20000 t)
            "guile"))
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "^#!r6rs\\|^(library\\_>" 20000 t)
            "r6rs"))
        quasischeme-default-language)))

(defun quasischeme-language ()
  (or quasischeme-language
      (setq quasischeme-language (quasischeme-guess-language))))

(defun quasischeme-set-language (language)
  "Evaluate this buffer's Scheme in LANGUAGE from now on."
  (interactive
   (list (completing-read "Language: "
                          (mapcar #'car (quasischeme--eval '(quasischeme-ide:languages)))
                          nil t nil nil (quasischeme-language))))
  (setq quasischeme-language language)
  (message "Quasischeme: %s" language))

;;; ------------------------------------------------------------------
;;; Showing results

(defface quasischeme-error-face '((t :inherit error))
  "How errors are shown.")

(defface quasischeme-value-face '((t :inherit font-lock-constant-face))
  "How values are shown.")

(defun quasischeme--result-text (result)
  "RESULT, a plist from QUASISCHEME-IDE:EVAL-STRING, as one line."
  (let ((values (plist-get result :values))
        (error (plist-get result :error)))
    (cond (error (propertize (concat "Error: " error) 'face 'quasischeme-error-face))
          ((null values) "; no values")
          (t (propertize (mapconcat #'identity values " ; ") 'face 'quasischeme-value-face)))))

(defun quasischeme--show-result (result)
  (let ((output (plist-get result :output)))
    (when (and output (> (length output) 0))
      (with-current-buffer (get-buffer-create "*quasischeme output*")
        (goto-char (point-max))
        (insert output)))
    (message "%s" (quasischeme--result-text result))))

(defun quasischeme--show-text (title text)
  (with-current-buffer (get-buffer-create (format "*quasischeme %s*" title))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert text)
      (goto-char (point-min))
      (special-mode))
    (display-buffer (current-buffer))))

;;; ------------------------------------------------------------------
;;; Evaluating from a buffer

(defun quasischeme-eval-string (string)
  "Evaluate STRING in the buffer's language; show its values."
  (let ((language (quasischeme-language)))
    (quasischeme--eval-async `(quasischeme-ide:eval-string ,language ,string)
                              #'quasischeme--show-result)))

(defun quasischeme-eval-region (start end)
  "Evaluate the region's forms."
  (interactive "r")
  (quasischeme-eval-string (buffer-substring-no-properties start end)))

(defun quasischeme-eval-last-sexp ()
  "Evaluate the form before point."
  (interactive)
  (quasischeme-eval-region (save-excursion (backward-sexp) (point)) (point)))

(defun quasischeme-eval-defun ()
  "Evaluate the top-level form around or after point."
  (interactive)
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (quasischeme-eval-region (point) end))))

(defun quasischeme-load-file (file)
  "Load FILE (by default, the buffer's) in the buffer's language."
  (interactive (list (or buffer-file-name (read-file-name "Load file: "))))
  (when (and buffer-file-name (string= file buffer-file-name) (buffer-modified-p))
    (when (y-or-n-p "Save the buffer first? ") (save-buffer)))
  (quasischeme--eval-async `(quasischeme-ide:load-file ,(quasischeme-language)
                                                         ,(expand-file-name file))
                            (lambda (result)
                              (if (plist-get result :error)
                                  (quasischeme--show-result result)
                                (message "Loaded %s" (file-name-nondirectory file))))))

;;; ------------------------------------------------------------------
;;; Looking things up

(defun quasischeme--symbol-at-point ()
  (let ((s (thing-at-point 'symbol t)))
    (and s (substring-no-properties s))))

(defun quasischeme--read-name (prompt)
  (let ((default (quasischeme--symbol-at-point)))
    (read-string (if default (format "%s (default %s): " prompt default) (concat prompt ": "))
                 nil nil default)))

(defun quasischeme-describe (name)
  "What NAME is bound to: a procedure and its arguments, syntax, a variable."
  (interactive (list (quasischeme--read-name "Describe")))
  (quasischeme--eval-async `(quasischeme-ide:describe-name ,(quasischeme-language) ,name)
                            (lambda (text) (message "%s" (string-trim-right text)))))

(defun quasischeme-apropos (string)
  "The names bound in the buffer's language that contain STRING."
  (interactive (list (read-string "Apropos: " (quasischeme--symbol-at-point))))
  (quasischeme--eval-async `(quasischeme-ide:apropos-names ,(quasischeme-language) ,string)
                            (lambda (names)
                              (quasischeme--show-text "apropos"
                                                       (mapconcat #'identity names "\n")))))

(defun quasischeme-expand ()
  "Expand the macros of the form after point (or before it, at its end)."
  (interactive)
  (let* ((start (if (looking-back ")" (1- (point))) (save-excursion (backward-sexp) (point)) (point)))
         (end (save-excursion (goto-char start) (forward-sexp) (point))))
    (quasischeme--eval-async
     `(quasischeme-ide:expand-string ,(quasischeme-language)
                                      ,(buffer-substring-no-properties start end))
     (lambda (text)
       (quasischeme--show-text "expansion" text)
       (with-current-buffer "*quasischeme expansion*"
         (let ((inhibit-read-only t))
           (scheme-mode)
           (read-only-mode 1)))))))

(defun quasischeme-backtrace ()
  "The backtrace of the last error."
  (interactive)
  (quasischeme--eval-async '(quasischeme-ide:backtrace)
                            (lambda (text) (quasischeme--show-text "backtrace" text))))

(defun quasischeme-command (text)
  "Run a REPL meta-command (,describe car ,trace f ,time (fib 20) ...)."
  (interactive "sCommand: ,")
  (quasischeme--eval-async `(quasischeme-ide:command ,(quasischeme-language) ,text)
                            (lambda (output) (quasischeme--show-text "output" output))))

;;; Completion and eldoc

(defun quasischeme-completion-at-point ()
  "Complete the name before point from the buffer's language's bindings."
  (when (quasischeme--backend)
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (when bounds
        (let ((prefix (buffer-substring-no-properties (car bounds) (cdr bounds)))
              (language (quasischeme-language)))
          (list (car bounds) (cdr bounds)
                (completion-table-dynamic
                 (lambda (_)
                   (quasischeme--eval `(quasischeme-ide:completions ,language ,prefix))))
                :exclusive 'no))))))

(defun quasischeme--operator ()
  "The name in operator position of the form point is in."
  (save-excursion
    (ignore-errors
      (backward-up-list)
      (forward-char)
      (let ((s (thing-at-point 'symbol t)))
        (and s (substring-no-properties s))))))

(defun quasischeme-eldoc (callback &rest _)
  "Show the arglist of the procedure called at point."
  (let ((name (quasischeme--operator)))
    (when (and name (quasischeme--backend))
      (quasischeme--eval-async `(quasischeme-ide:arglist ,(quasischeme-language) ,name)
                                (lambda (arglist) (when arglist (funcall callback arglist))))
      t)))

;;; The minor mode

(defvar quasischeme-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-e") #'quasischeme-eval-last-sexp)
    (define-key map (kbd "C-M-x") #'quasischeme-eval-defun)
    (define-key map (kbd "C-c C-c") #'quasischeme-eval-defun)
    (define-key map (kbd "C-c C-r") #'quasischeme-eval-region)
    (define-key map (kbd "C-c C-l") #'quasischeme-load-file)
    (define-key map (kbd "C-c C-k") #'quasischeme-load-file)
    (define-key map (kbd "C-c C-z") #'quasischeme-repl)
    (define-key map (kbd "C-c C-d d") #'quasischeme-describe)
    (define-key map (kbd "C-c C-d a") #'quasischeme-apropos)
    (define-key map (kbd "C-c RET") #'quasischeme-expand)
    (define-key map (kbd "C-c C-b") #'quasischeme-backtrace)
    (define-key map (kbd "C-c ,") #'quasischeme-command)
    (define-key map (kbd "C-c C-v") #'quasischeme-set-language)
    map)
  "Keys of `quasischeme-mode'.")

;;;###autoload
(define-minor-mode quasischeme-mode
  "Evaluate this buffer's Scheme in Quasischeme, through SLY or SLIME.

\\{quasischeme-mode-map}"
  :lighter (:eval (format " PS[%s]" (or quasischeme-language "?")))
  :keymap quasischeme-mode-map
  (if quasischeme-mode
      (progn
        (quasischeme-language)
        (add-hook 'completion-at-point-functions #'quasischeme-completion-at-point nil t)
        (add-hook 'eldoc-documentation-functions #'quasischeme-eldoc nil t)
        (eldoc-mode 1))
    (remove-hook 'completion-at-point-functions #'quasischeme-completion-at-point t)
    (remove-hook 'eldoc-documentation-functions #'quasischeme-eldoc t)))

;;; ------------------------------------------------------------------
;;; The REPL

(defvar-local quasischeme-repl--input-start nil
  "Where the input being typed begins.")

(defvar quasischeme-repl-history (make-ring 200)
  "Inputs sent from REPL buffers, newest first.")

(defvar-local quasischeme-repl--history-index nil)

(defface quasischeme-prompt-face '((t :inherit minibuffer-prompt))
  "The REPL's prompt.")

(defun quasischeme-repl--prompt ()
  (format "%s> " (quasischeme-language)))

(defun quasischeme-repl--insert-prompt ()
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (let ((start (point)))
      (insert (quasischeme-repl--prompt))
      (add-text-properties start (point) '(face quasischeme-prompt-face read-only t
                                           rear-nonsticky t front-sticky (read-only)
                                           field output))))
  (goto-char (point-max))
  (setq quasischeme-repl--input-start (point-marker)))

(defun quasischeme-repl--insert-output (text &optional face)
  "Insert TEXT before the prompt to come, read-only."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-max))
      (let ((start (point)))
        (insert text)
        (unless (bolp) (insert "\n"))
        (add-text-properties start (point)
                             `(read-only t rear-nonsticky t field output
                               ,@(and face (list 'face face))))))))

(defun quasischeme-repl--input ()
  (buffer-substring-no-properties quasischeme-repl--input-start (point-max)))

(defun quasischeme-repl--complete-p (text)
  "Do TEXT's parentheses close, outside strings and comments?"
  (with-temp-buffer
    (scheme-mode)
    (insert text)
    (let ((state (parse-partial-sexp (point-min) (point-max))))
      (and (<= (car state) 0) (not (nth 3 state)) (not (nth 4 state))))))

(defun quasischeme-repl-return ()
  "Send the input, if it's complete; else start a new line of it."
  (interactive)
  (let ((input (quasischeme-repl--input)))
    (if (or (string-match-p "\\`[ \t\n]*,[a-zA-Z]" input)
            (and (string-match-p "[^ \t\n]" input) (quasischeme-repl--complete-p input)))
        (quasischeme-repl--send input)
      (newline-and-indent))))

(defun quasischeme-repl--send (input)
  (ring-insert quasischeme-repl-history input)
  (setq quasischeme-repl--history-index nil)
  (goto-char (point-max))
  (let ((inhibit-read-only t))
    (insert "\n")
    (add-text-properties quasischeme-repl--input-start (point-max)
                         '(read-only t rear-nonsticky t)))
  (let ((buffer (current-buffer))
        (language (quasischeme-language)))
    (cond
     ;; ,language NAME: this buffer's language changes
     ((string-match "\\`[ \t\n]*,\\(language\\|L\\|mode\\)[ \t]+\\([^ \t\n]+\\)" input)
      (setq quasischeme-language (match-string 2 input))
      (rename-buffer (format "*quasischeme-repl[%s]*" quasischeme-language) t)
      (quasischeme-repl--insert-prompt))
     ((string-match-p "\\`[ \t\n]*," input)
      (quasischeme--eval-async
       `(quasischeme-ide:command ,language ,(string-trim input))
       (lambda (output)
         (with-current-buffer buffer
           (quasischeme-repl--insert-output output)
           (quasischeme-repl--insert-prompt)))))
     (t
      (quasischeme--eval-async
       `(quasischeme-ide:eval-string ,language ,input)
       (lambda (result)
         (with-current-buffer buffer
           (let ((output (plist-get result :output)))
             (when (and output (> (length output) 0))
               (quasischeme-repl--insert-output output)))
           (if (plist-get result :error)
               (quasischeme-repl--insert-output
                (concat ";; Error: " (plist-get result :error) "  (C-c C-b: backtrace)")
                'quasischeme-error-face)
             (dolist (v (plist-get result :values))
               (quasischeme-repl--insert-output v 'quasischeme-value-face)))
           (quasischeme-repl--insert-prompt))))))))

(defun quasischeme-repl--history-move (delta)
  (when (> (ring-length quasischeme-repl-history) 0)
    (setq quasischeme-repl--history-index
          (mod (+ (or quasischeme-repl--history-index (if (> delta 0) -1 0)) delta)
               (ring-length quasischeme-repl-history)))
    (delete-region quasischeme-repl--input-start (point-max))
    (goto-char (point-max))
    (insert (ring-ref quasischeme-repl-history quasischeme-repl--history-index))))

(defun quasischeme-repl-previous-input ()
  "The previous input."
  (interactive)
  (quasischeme-repl--history-move 1))

(defun quasischeme-repl-next-input ()
  "The next input."
  (interactive)
  (quasischeme-repl--history-move -1))

(defvar quasischeme-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map scheme-mode-map)
    (define-key map (kbd "RET") #'quasischeme-repl-return)
    (define-key map (kbd "C-j") #'newline-and-indent)
    (define-key map (kbd "M-p") #'quasischeme-repl-previous-input)
    (define-key map (kbd "M-n") #'quasischeme-repl-next-input)
    (define-key map (kbd "C-c C-b") #'quasischeme-backtrace)
    (define-key map (kbd "C-c C-d d") #'quasischeme-describe)
    (define-key map (kbd "C-c C-d a") #'quasischeme-apropos)
    (define-key map (kbd "TAB") #'completion-at-point)
    map)
  "Keys of the REPL buffer.")

(define-derived-mode quasischeme-repl-mode scheme-mode "QS-REPL"
  "A Quasischeme REPL: RET sends the input once its parentheses close,
M-p and M-n move through the history; ,help lists the meta-commands.

\\{quasischeme-repl-mode-map}"
  (add-hook 'completion-at-point-functions #'quasischeme-completion-at-point nil t)
  (add-hook 'eldoc-documentation-functions #'quasischeme-eldoc nil t)
  (eldoc-mode 1))

;;;###autoload
(defun quasischeme-repl (&optional language)
  "Switch to the REPL buffer for LANGUAGE (by default, the current
buffer's language, or `quasischeme-default-language')."
  (interactive)
  (quasischeme--check-connected)
  (let* ((language (or language
                       (and (or quasischeme-mode (derived-mode-p 'quasischeme-repl-mode))
                            (quasischeme-language))
                       quasischeme-default-language))
         (name (format "*quasischeme-repl[%s]*" language))
         (buffer (get-buffer name)))
    (unless buffer
      (setq buffer (get-buffer-create name))
      (with-current-buffer buffer
        (quasischeme-repl-mode)
        (setq quasischeme-language language)
        (quasischeme-repl--insert-output
         (format ";; Quasischeme, %s.  ,help lists the commands; ,language changes language." language))
        (quasischeme-repl--insert-prompt)))
    (pop-to-buffer buffer)
    (goto-char (point-max))
    buffer))

(provide 'quasischeme)
;;; quasischeme.el ends here
