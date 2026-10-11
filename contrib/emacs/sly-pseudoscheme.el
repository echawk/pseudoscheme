;;; sly-pseudoscheme.el --- Pseudoscheme under SLY  -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "27.1") (sly "1.0"))

;;; Commentary:

;; M-x sly-pseudoscheme starts `pseudoscheme --lisp' under SLY, which
;; loads Slynk into it and connects, and turns on `pseudoscheme-mode' in
;; Scheme buffers.  With several Lisps in `sly-lisp-implementations', add
;; `sly-pseudoscheme-implementation' to it to have M-- M-x sly offer it.
;; See docs/emacs.md.

;;; Code:

(require 'sly)
(require 'pseudoscheme)

(defun sly-pseudoscheme-implementation ()
  "Pseudoscheme's entry for `sly-lisp-implementations'."
  `(pseudoscheme (,pseudoscheme-program "--lisp")))

;;;###autoload
(defun sly-pseudoscheme ()
  "Start Pseudoscheme (`pseudoscheme-program' --lisp) under SLY."
  (interactive)
  (add-hook 'scheme-mode-hook #'pseudoscheme-mode)
  (sly-start :program pseudoscheme-program :program-args '("--lisp")
             :name 'pseudoscheme))

(provide 'sly-pseudoscheme)
;;; sly-pseudoscheme.el ends here
