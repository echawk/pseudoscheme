; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

;;;; Control, natively: continuation marks, prompts with tags,
;;;; composable continuations, dynamic-wind, barriers, and #%paramz.
;;;; The plan: marks on the frames the continuation transformation
;;;; already pushes (docs/continuations.md, the Chez mode's marks), one
;;;; layer, where A has rumble's continuation attachments emulated on
;;;; top of them (docs/racket.md, "Three strategies").

(in-package "PSEUDOSCHEME-RACKET")

(defnative "call-with-continuation-prompt" (proc &optional tag handler &rest args)
    (:rumble "call-with-continuation-prompt")
  (declare (ignore proc tag handler args))
  (not-yet "call-with-continuation-prompt"))

(defnative "abort-current-continuation" (tag &rest vals)
    (:rumble "abort-current-continuation")
  (declare (ignore tag vals))
  (not-yet "abort-current-continuation"))

(defnative "continuation-mark-set-first" (marks key &optional none-v prompt-tag)
    (:rumble "continuation-mark-set-first")
  (declare (ignore marks key none-v prompt-tag))
  (not-yet "continuation-mark-set-first"))

(defnative "current-continuation-marks" (&optional prompt-tag)
    (:rumble "current-continuation-marks")
  (declare (ignore prompt-tag))
  (not-yet "current-continuation-marks"))

;;; The extension parameterization (#%paramz)
(defnative "extend-parameterization" (p &rest key-values)
    (:instance "#%paramz" :rumble "extend-parameterization")
  (declare (ignore p key-values))
  (not-yet "extend-parameterization"))
