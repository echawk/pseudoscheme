;;; slime-quasischeme.el --- Quasischeme under SLIME  -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "27.1") (slime "2.26"))

;;; Commentary:

;; M-x slime-quasischeme starts `quasischeme --lisp' under SLIME, which
;; loads Swank into it and connects, and turns on `quasischeme-mode' in
;; Scheme buffers.  See docs/emacs.md.

;;; Code:

(require 'slime)
(require 'quasischeme)

(defun slime-quasischeme-implementation ()
  "Quasischeme's entry for `slime-lisp-implementations'."
  `(quasischeme (,quasischeme-program "--lisp")))

;;;###autoload
(defun slime-quasischeme ()
  "Start Quasischeme (`quasischeme-program' --lisp) under SLIME."
  (interactive)
  (add-hook 'scheme-mode-hook #'quasischeme-mode)
  (slime-start :program quasischeme-program :program-args '("--lisp")
               :name 'quasischeme))

(provide 'slime-quasischeme)
;;; slime-quasischeme.el ends here
