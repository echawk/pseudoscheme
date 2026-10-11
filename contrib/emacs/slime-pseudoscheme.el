;;; slime-pseudoscheme.el --- Pseudoscheme under SLIME  -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "27.1") (slime "2.26"))

;;; Commentary:

;; M-x slime-pseudoscheme starts `pseudoscheme --lisp' under SLIME, which
;; loads Swank into it and connects, and turns on `pseudoscheme-mode' in
;; Scheme buffers.  See docs/emacs.md.

;;; Code:

(require 'slime)
(require 'pseudoscheme)

(defun slime-pseudoscheme-implementation ()
  "Pseudoscheme's entry for `slime-lisp-implementations'."
  `(pseudoscheme (,pseudoscheme-program "--lisp")))

;;;###autoload
(defun slime-pseudoscheme ()
  "Start Pseudoscheme (`pseudoscheme-program' --lisp) under SLIME."
  (interactive)
  (add-hook 'scheme-mode-hook #'pseudoscheme-mode)
  (slime-start :program pseudoscheme-program :program-args '("--lisp")
               :name 'pseudoscheme))

(provide 'slime-pseudoscheme)
;;; slime-pseudoscheme.el ends here
