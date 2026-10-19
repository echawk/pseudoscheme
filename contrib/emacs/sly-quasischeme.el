;;; sly-quasischeme.el --- Quasischeme under SLY  -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "27.1") (sly "1.0"))

;;; Commentary:

;; M-x sly-quasischeme starts `quasischeme --lisp' under SLY, which
;; loads Slynk into it and connects, and turns on `quasischeme-mode' in
;; Scheme buffers.  With several Lisps in `sly-lisp-implementations', add
;; `sly-quasischeme-implementation' to it to have M-- M-x sly offer it.
;; See docs/emacs.md.

;;; Code:

(require 'sly)
(require 'quasischeme)

(defun sly-quasischeme-implementation ()
  "Quasischeme's entry for `sly-lisp-implementations'."
  `(quasischeme (,quasischeme-program "--lisp")))

;;;###autoload
(defun sly-quasischeme ()
  "Start Quasischeme (`quasischeme-program' --lisp) under SLY."
  (interactive)
  (add-hook 'scheme-mode-hook #'quasischeme-mode)
  (sly-start :program quasischeme-program :program-args '("--lisp")
             :name 'quasischeme))

(provide 'sly-quasischeme)
;;; sly-quasischeme.el ends here
