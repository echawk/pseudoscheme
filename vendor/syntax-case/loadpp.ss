;;; loadpp.ss
;;; Robert Hieb & Kent Dybvig
;;; 92/06/18

;;; It's convenient but not necessary to enable graph printing in Chez
;;; Scheme for viewing expander output
; (print-graph #t)

;;; Optimize level 2 is safe but ties down primitive names to their
;;; predefined values, which allows the compiler to generate faster
;;; code.  Optimize level 3 additionally turns off some run-time type
;;; checking
; (optimize-level 2)
; (optimize-level 3)

;;; The first batch of loads brings up the macro system on top of
;;; the existing macro system.

(load "compat.ss")
(load "hooks.ss")
(load "output.ss")
(load "init.ss")
(load "expand.pp")
(expand-install-hook expand-syntax)
(load "macro-defs.ss")

;;; This second batch of loads uses the macro system to load itself.
;;; It's not necessary, but provides a reasonable test of your sanity.

;(load "expand.ss")
;(expand-install-hook expand-syntax)
;(load "macro-defs.ss")
