;;; pseudoscheme.el --- Scheme on Pseudoscheme, through SLY or SLIME  -*- lexical-binding: t -*-

;; Author: Pseudoscheme's authors
;; Keywords: languages, lisp, scheme
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Pseudoscheme is Scheme (R7RS, R6RS, R5RS, Chez Scheme, Guile, the
;; older reports) on Common Lisp.  `bin/pseudoscheme --lisp' is a Lisp
;; with all of it loaded, which SLY or SLIME can start and connect to as
;; usual.  This package then talks Scheme to it through whichever is
;; connected, calling the functions of its PSEUDOSCHEME-IDE package
;; (src/repl/ide.lisp):
;;
;; - `pseudoscheme-mode', a minor mode for Scheme buffers: evaluate the
;;   form before point, the definition, the region or the file in the
;;   buffer's language; completion; the arglist of the procedure called
;;   at point in the echo area (eldoc); ,describe, ,expand, apropos.
;; - `pseudoscheme-repl', a REPL buffer per language, with the REPL's
;;   meta-commands (,describe ,expand ,trace ,bt ,language ...).
;;
;; sly-pseudoscheme.el and slime-pseudoscheme.el start Pseudoscheme
;; under SLY or SLIME.  See docs/emacs.md.

;;; Code:

(require 'cl-lib)
(require 'scheme)
(require 'eldoc)
(require 'ring)

(defgroup pseudoscheme nil
  "Scheme on Pseudoscheme, through SLY or SLIME."
  :group 'scheme
  :prefix "pseudoscheme-")

(defcustom pseudoscheme-program "pseudoscheme"
  "The pseudoscheme command (bin/pseudoscheme in a checkout)."
  :type 'string)

(defcustom pseudoscheme-default-language "r7rs"
  "The language of a Scheme buffer `pseudoscheme-guess-language' can't place."
  :type '(choice (const "r7rs") (const "r6rs") (const "r5rs") (const "chez")
                 (const "guile") (const "r4rs") (const "ieee") (const "r3rs")
                 (const "r2rs") (const "rrs") string))

(defcustom pseudoscheme-language-alist
  '(("\\.sls\\'" . "r6rs") ("\\.sps\\'" . "r6rs") ("\\.ss\\'" . "chez")
    ("\\.sld\\'" . "r7rs"))
  "File name patterns and the language of the files they match.
A .scm file is Guile's if it has a define-module or use-modules form,
else `pseudoscheme-default-language'."
  :type '(alist :key-type regexp :value-type string))

;;; ------------------------------------------------------------------
;;; Talking to the Lisp: SLY or SLIME, whichever is connected

(declare-function sly-connected-p "sly")
(declare-function sly-eval "sly")
(declare-function sly-eval-async "sly")
(declare-function slime-connected-p "slime")
(declare-function slime-eval "slime")
(declare-function slime-eval-async "slime")

(defun pseudoscheme--backend ()
  "`sly' or `slime', whichever is connected; nil if neither."
  (cond ((and (fboundp 'sly-connected-p) (sly-connected-p)) 'sly)
        ((and (fboundp 'slime-connected-p) (slime-connected-p)) 'slime)))

(defun pseudoscheme--check-connected ()
  (or (pseudoscheme--backend)
      (user-error "Pseudoscheme isn't running: M-x sly-pseudoscheme or M-x slime-pseudoscheme")))

(defun pseudoscheme--eval (form)
  "Evaluate the Lisp FORM in the connected Lisp; its value."
  (pcase (pseudoscheme--check-connected)
    ('sly (sly-eval form))
    ('slime (slime-eval form))))

(defun pseudoscheme--eval-async (form continuation)
  "Evaluate the Lisp FORM in the connected Lisp, then call CONTINUATION
with its value."
  (pcase (pseudoscheme--check-connected)
    ('sly (sly-eval-async form continuation))
    ('slime (slime-eval-async form continuation))))

;;; ------------------------------------------------------------------
;;; The language of a buffer

(defvar-local pseudoscheme-language nil
  "The language this buffer's Scheme is evaluated in: \"r7rs\", \"chez\", ...
Set by `pseudoscheme-mode' from the file's name and contents.")

(defun pseudoscheme-guess-language ()
  "The language of the current buffer's file, by its name and contents."
  (let ((name (or buffer-file-name "")))
    (or (cdr (cl-find-if (lambda (entry) (string-match-p (car entry) name))
                         pseudoscheme-language-alist))
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "^(\\(define-module\\|use-modules\\)\\_>" 20000 t)
            "guile"))
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "^#!r6rs\\|^(library\\_>" 20000 t)
            "r6rs"))
        pseudoscheme-default-language)))

(defun pseudoscheme-language ()
  (or pseudoscheme-language
      (setq pseudoscheme-language (pseudoscheme-guess-language))))

(defun pseudoscheme-set-language (language)
  "Evaluate this buffer's Scheme in LANGUAGE from now on."
  (interactive
   (list (completing-read "Language: "
                          (mapcar #'car (pseudoscheme--eval '(pseudoscheme-ide:languages)))
                          nil t nil nil (pseudoscheme-language))))
  (setq pseudoscheme-language language)
  (message "Pseudoscheme: %s" language))

;;; ------------------------------------------------------------------
;;; Showing results

(defface pseudoscheme-error-face '((t :inherit error))
  "How errors are shown.")

(defface pseudoscheme-value-face '((t :inherit font-lock-constant-face))
  "How values are shown.")

(defun pseudoscheme--result-text (result)
  "RESULT, a plist from PSEUDOSCHEME-IDE:EVAL-STRING, as one line."
  (let ((values (plist-get result :values))
        (error (plist-get result :error)))
    (cond (error (propertize (concat "Error: " error) 'face 'pseudoscheme-error-face))
          ((null values) "; no values")
          (t (propertize (mapconcat #'identity values " ; ") 'face 'pseudoscheme-value-face)))))

(defun pseudoscheme--show-result (result)
  (let ((output (plist-get result :output)))
    (when (and output (> (length output) 0))
      (with-current-buffer (get-buffer-create "*pseudoscheme output*")
        (goto-char (point-max))
        (insert output)))
    (message "%s" (pseudoscheme--result-text result))))

(defun pseudoscheme--show-text (title text)
  (with-current-buffer (get-buffer-create (format "*pseudoscheme %s*" title))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert text)
      (goto-char (point-min))
      (special-mode))
    (display-buffer (current-buffer))))

;;; ------------------------------------------------------------------
;;; Evaluating from a buffer

(defun pseudoscheme-eval-string (string)
  "Evaluate STRING in the buffer's language; show its values."
  (let ((language (pseudoscheme-language)))
    (pseudoscheme--eval-async `(pseudoscheme-ide:eval-string ,language ,string)
                              #'pseudoscheme--show-result)))

(defun pseudoscheme-eval-region (start end)
  "Evaluate the region's forms."
  (interactive "r")
  (pseudoscheme-eval-string (buffer-substring-no-properties start end)))

(defun pseudoscheme-eval-last-sexp ()
  "Evaluate the form before point."
  (interactive)
  (pseudoscheme-eval-region (save-excursion (backward-sexp) (point)) (point)))

(defun pseudoscheme-eval-defun ()
  "Evaluate the top-level form around or after point."
  (interactive)
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (pseudoscheme-eval-region (point) end))))

(defun pseudoscheme-load-file (file)
  "Load FILE (by default, the buffer's) in the buffer's language."
  (interactive (list (or buffer-file-name (read-file-name "Load file: "))))
  (when (and buffer-file-name (string= file buffer-file-name) (buffer-modified-p))
    (when (y-or-n-p "Save the buffer first? ") (save-buffer)))
  (pseudoscheme--eval-async `(pseudoscheme-ide:load-file ,(pseudoscheme-language)
                                                         ,(expand-file-name file))
                            (lambda (result)
                              (if (plist-get result :error)
                                  (pseudoscheme--show-result result)
                                (message "Loaded %s" (file-name-nondirectory file))))))

;;; ------------------------------------------------------------------
;;; Looking things up

(defun pseudoscheme--symbol-at-point ()
  (let ((s (thing-at-point 'symbol t)))
    (and s (substring-no-properties s))))

(defun pseudoscheme--read-name (prompt)
  (let ((default (pseudoscheme--symbol-at-point)))
    (read-string (if default (format "%s (default %s): " prompt default) (concat prompt ": "))
                 nil nil default)))

(defun pseudoscheme-describe (name)
  "What NAME is bound to: a procedure and its arguments, syntax, a variable."
  (interactive (list (pseudoscheme--read-name "Describe")))
  (pseudoscheme--eval-async `(pseudoscheme-ide:describe-name ,(pseudoscheme-language) ,name)
                            (lambda (text) (message "%s" (string-trim-right text)))))

(defun pseudoscheme-apropos (string)
  "The names bound in the buffer's language that contain STRING."
  (interactive (list (read-string "Apropos: " (pseudoscheme--symbol-at-point))))
  (pseudoscheme--eval-async `(pseudoscheme-ide:apropos-names ,(pseudoscheme-language) ,string)
                            (lambda (names)
                              (pseudoscheme--show-text "apropos"
                                                       (mapconcat #'identity names "\n")))))

(defun pseudoscheme-expand ()
  "Expand the macros of the form after point (or before it, at its end)."
  (interactive)
  (let* ((start (if (looking-back ")" (1- (point))) (save-excursion (backward-sexp) (point)) (point)))
         (end (save-excursion (goto-char start) (forward-sexp) (point))))
    (pseudoscheme--eval-async
     `(pseudoscheme-ide:expand-string ,(pseudoscheme-language)
                                      ,(buffer-substring-no-properties start end))
     (lambda (text)
       (pseudoscheme--show-text "expansion" text)
       (with-current-buffer "*pseudoscheme expansion*"
         (let ((inhibit-read-only t))
           (scheme-mode)
           (read-only-mode 1)))))))

(defun pseudoscheme-backtrace ()
  "The backtrace of the last error."
  (interactive)
  (pseudoscheme--eval-async '(pseudoscheme-ide:backtrace)
                            (lambda (text) (pseudoscheme--show-text "backtrace" text))))

(defun pseudoscheme-command (text)
  "Run a REPL meta-command (,describe car ,trace f ,time (fib 20) ...)."
  (interactive "sCommand: ,")
  (pseudoscheme--eval-async `(pseudoscheme-ide:command ,(pseudoscheme-language) ,text)
                            (lambda (output) (pseudoscheme--show-text "output" output))))

;;; Completion and eldoc

(defun pseudoscheme-completion-at-point ()
  "Complete the name before point from the buffer's language's bindings."
  (when (pseudoscheme--backend)
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (when bounds
        (let ((prefix (buffer-substring-no-properties (car bounds) (cdr bounds)))
              (language (pseudoscheme-language)))
          (list (car bounds) (cdr bounds)
                (completion-table-dynamic
                 (lambda (_)
                   (pseudoscheme--eval `(pseudoscheme-ide:completions ,language ,prefix))))
                :exclusive 'no))))))

(defun pseudoscheme--operator ()
  "The name in operator position of the form point is in."
  (save-excursion
    (ignore-errors
      (backward-up-list)
      (forward-char)
      (let ((s (thing-at-point 'symbol t)))
        (and s (substring-no-properties s))))))

(defun pseudoscheme-eldoc (callback &rest _)
  "Show the arglist of the procedure called at point."
  (let ((name (pseudoscheme--operator)))
    (when (and name (pseudoscheme--backend))
      (pseudoscheme--eval-async `(pseudoscheme-ide:arglist ,(pseudoscheme-language) ,name)
                                (lambda (arglist) (when arglist (funcall callback arglist))))
      t)))

;;; The minor mode

(defvar pseudoscheme-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-e") #'pseudoscheme-eval-last-sexp)
    (define-key map (kbd "C-M-x") #'pseudoscheme-eval-defun)
    (define-key map (kbd "C-c C-c") #'pseudoscheme-eval-defun)
    (define-key map (kbd "C-c C-r") #'pseudoscheme-eval-region)
    (define-key map (kbd "C-c C-l") #'pseudoscheme-load-file)
    (define-key map (kbd "C-c C-k") #'pseudoscheme-load-file)
    (define-key map (kbd "C-c C-z") #'pseudoscheme-repl)
    (define-key map (kbd "C-c C-d d") #'pseudoscheme-describe)
    (define-key map (kbd "C-c C-d a") #'pseudoscheme-apropos)
    (define-key map (kbd "C-c RET") #'pseudoscheme-expand)
    (define-key map (kbd "C-c C-b") #'pseudoscheme-backtrace)
    (define-key map (kbd "C-c ,") #'pseudoscheme-command)
    (define-key map (kbd "C-c C-v") #'pseudoscheme-set-language)
    map)
  "Keys of `pseudoscheme-mode'.")

;;;###autoload
(define-minor-mode pseudoscheme-mode
  "Evaluate this buffer's Scheme in Pseudoscheme, through SLY or SLIME.

\\{pseudoscheme-mode-map}"
  :lighter (:eval (format " PS[%s]" (or pseudoscheme-language "?")))
  :keymap pseudoscheme-mode-map
  (if pseudoscheme-mode
      (progn
        (pseudoscheme-language)
        (add-hook 'completion-at-point-functions #'pseudoscheme-completion-at-point nil t)
        (add-hook 'eldoc-documentation-functions #'pseudoscheme-eldoc nil t)
        (eldoc-mode 1))
    (remove-hook 'completion-at-point-functions #'pseudoscheme-completion-at-point t)
    (remove-hook 'eldoc-documentation-functions #'pseudoscheme-eldoc t)))

;;; ------------------------------------------------------------------
;;; The REPL

(defvar-local pseudoscheme-repl--input-start nil
  "Where the input being typed begins.")

(defvar pseudoscheme-repl-history (make-ring 200)
  "Inputs sent from REPL buffers, newest first.")

(defvar-local pseudoscheme-repl--history-index nil)

(defface pseudoscheme-prompt-face '((t :inherit minibuffer-prompt))
  "The REPL's prompt.")

(defun pseudoscheme-repl--prompt ()
  (format "%s> " (pseudoscheme-language)))

(defun pseudoscheme-repl--insert-prompt ()
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (let ((start (point)))
      (insert (pseudoscheme-repl--prompt))
      (add-text-properties start (point) '(face pseudoscheme-prompt-face read-only t
                                           rear-nonsticky t front-sticky (read-only)
                                           field output))))
  (goto-char (point-max))
  (setq pseudoscheme-repl--input-start (point-marker)))

(defun pseudoscheme-repl--insert-output (text &optional face)
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

(defun pseudoscheme-repl--input ()
  (buffer-substring-no-properties pseudoscheme-repl--input-start (point-max)))

(defun pseudoscheme-repl--complete-p (text)
  "Do TEXT's parentheses close, outside strings and comments?"
  (with-temp-buffer
    (scheme-mode)
    (insert text)
    (let ((state (parse-partial-sexp (point-min) (point-max))))
      (and (<= (car state) 0) (not (nth 3 state)) (not (nth 4 state))))))

(defun pseudoscheme-repl-return ()
  "Send the input, if it's complete; else start a new line of it."
  (interactive)
  (let ((input (pseudoscheme-repl--input)))
    (if (or (string-match-p "\\`[ \t\n]*,[a-zA-Z]" input)
            (and (string-match-p "[^ \t\n]" input) (pseudoscheme-repl--complete-p input)))
        (pseudoscheme-repl--send input)
      (newline-and-indent))))

(defun pseudoscheme-repl--send (input)
  (ring-insert pseudoscheme-repl-history input)
  (setq pseudoscheme-repl--history-index nil)
  (goto-char (point-max))
  (let ((inhibit-read-only t))
    (insert "\n")
    (add-text-properties pseudoscheme-repl--input-start (point-max)
                         '(read-only t rear-nonsticky t)))
  (let ((buffer (current-buffer))
        (language (pseudoscheme-language)))
    (cond
     ;; ,language NAME: this buffer's language changes
     ((string-match "\\`[ \t\n]*,\\(language\\|L\\|mode\\)[ \t]+\\([^ \t\n]+\\)" input)
      (setq pseudoscheme-language (match-string 2 input))
      (rename-buffer (format "*pseudoscheme-repl[%s]*" pseudoscheme-language) t)
      (pseudoscheme-repl--insert-prompt))
     ((string-match-p "\\`[ \t\n]*," input)
      (pseudoscheme--eval-async
       `(pseudoscheme-ide:command ,language ,(string-trim input))
       (lambda (output)
         (with-current-buffer buffer
           (pseudoscheme-repl--insert-output output)
           (pseudoscheme-repl--insert-prompt)))))
     (t
      (pseudoscheme--eval-async
       `(pseudoscheme-ide:eval-string ,language ,input)
       (lambda (result)
         (with-current-buffer buffer
           (let ((output (plist-get result :output)))
             (when (and output (> (length output) 0))
               (pseudoscheme-repl--insert-output output)))
           (if (plist-get result :error)
               (pseudoscheme-repl--insert-output
                (concat ";; Error: " (plist-get result :error) "  (C-c C-b: backtrace)")
                'pseudoscheme-error-face)
             (dolist (v (plist-get result :values))
               (pseudoscheme-repl--insert-output v 'pseudoscheme-value-face)))
           (pseudoscheme-repl--insert-prompt))))))))

(defun pseudoscheme-repl--history-move (delta)
  (when (> (ring-length pseudoscheme-repl-history) 0)
    (setq pseudoscheme-repl--history-index
          (mod (+ (or pseudoscheme-repl--history-index (if (> delta 0) -1 0)) delta)
               (ring-length pseudoscheme-repl-history)))
    (delete-region pseudoscheme-repl--input-start (point-max))
    (goto-char (point-max))
    (insert (ring-ref pseudoscheme-repl-history pseudoscheme-repl--history-index))))

(defun pseudoscheme-repl-previous-input ()
  "The previous input."
  (interactive)
  (pseudoscheme-repl--history-move 1))

(defun pseudoscheme-repl-next-input ()
  "The next input."
  (interactive)
  (pseudoscheme-repl--history-move -1))

(defvar pseudoscheme-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map scheme-mode-map)
    (define-key map (kbd "RET") #'pseudoscheme-repl-return)
    (define-key map (kbd "C-j") #'newline-and-indent)
    (define-key map (kbd "M-p") #'pseudoscheme-repl-previous-input)
    (define-key map (kbd "M-n") #'pseudoscheme-repl-next-input)
    (define-key map (kbd "C-c C-b") #'pseudoscheme-backtrace)
    (define-key map (kbd "C-c C-d d") #'pseudoscheme-describe)
    (define-key map (kbd "C-c C-d a") #'pseudoscheme-apropos)
    (define-key map (kbd "TAB") #'completion-at-point)
    map)
  "Keys of the REPL buffer.")

(define-derived-mode pseudoscheme-repl-mode scheme-mode "PS-REPL"
  "A Pseudoscheme REPL: RET sends the input once its parentheses close,
M-p and M-n move through the history; ,help lists the meta-commands.

\\{pseudoscheme-repl-mode-map}"
  (add-hook 'completion-at-point-functions #'pseudoscheme-completion-at-point nil t)
  (add-hook 'eldoc-documentation-functions #'pseudoscheme-eldoc nil t)
  (eldoc-mode 1))

;;;###autoload
(defun pseudoscheme-repl (&optional language)
  "Switch to the REPL buffer for LANGUAGE (by default, the current
buffer's language, or `pseudoscheme-default-language')."
  (interactive)
  (pseudoscheme--check-connected)
  (let* ((language (or language
                       (and (or pseudoscheme-mode (derived-mode-p 'pseudoscheme-repl-mode))
                            (pseudoscheme-language))
                       pseudoscheme-default-language))
         (name (format "*pseudoscheme-repl[%s]*" language))
         (buffer (get-buffer name)))
    (unless buffer
      (setq buffer (get-buffer-create name))
      (with-current-buffer buffer
        (pseudoscheme-repl-mode)
        (setq pseudoscheme-language language)
        (pseudoscheme-repl--insert-output
         (format ";; Pseudoscheme, %s.  ,help lists the commands; ,language changes language." language))
        (pseudoscheme-repl--insert-prompt)))
    (pop-to-buffer buffer)
    (goto-char (point-max))
    buffer))

(provide 'pseudoscheme)
;;; pseudoscheme.el ends here
