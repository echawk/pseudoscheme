;;; Copyright (c) 2006, 2007 Abdulaziz Ghuloum and Kent Dybvig
;;; 
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE. 

;;; PSEUDOSCHEME: replaces the original main, which ran a script named
;;; on the command line and exited.  Instead, hand the expander's entry
;;; points to the host as global variables (PSYNTAX:...) for
;;; src/psyntax.lisp to call.

(library (psyntax main)
  (export)
  (import 
    (rnrs base)
    (rnrs control)
    (psyntax compat)
    (psyntax library-manager)
    (psyntax expander))

  ;; An R6RS top-level program: a list of forms beginning with (import ...).
  (set-symbol-value! 'psyntax:eval-r6rs-top-level eval-r6rs-top-level)
  ;; One REPL form, in the extensible (pseudoscheme interaction) library.
  (set-symbol-value! 'psyntax:eval-top-level eval-top-level)
  ;; Expand and install one (library ...) form.
  (set-symbol-value! 'psyntax:library-expander (current-library-expander))
  (set-symbol-value! 'psyntax:eval eval)
  (set-symbol-value! 'psyntax:eval-hook eval-hook)
  (set-symbol-value! 'psyntax:expand expand)
  (set-symbol-value! 'psyntax:environment environment)
  (set-symbol-value! 'psyntax:environment? environment?)
  (set-symbol-value! 'psyntax:environment-symbols environment-symbols)
  (set-symbol-value! 'psyntax:null-environment null-environment)
  (set-symbol-value! 'psyntax:syntax->datum syntax->datum)
  (set-symbol-value! 'psyntax:installed-libraries installed-libraries)
  (set-symbol-value! 'psyntax:library-exists? library-exists?)
  (set-symbol-value! 'psyntax:library-path library-path)
  (set-symbol-value! 'psyntax:file-locator file-locator)
  (set-symbol-value! 'psyntax:library-locator library-locator)
  (set-symbol-value! 'psyntax:install-library install-library)
  ;; Compiled libraries (src/library-cache.lisp): the parameters, and a
  ;; library's (id name version), found or loaded by name.
  (set-symbol-value! 'psyntax:library-loader library-loader)
  (set-symbol-value! 'psyntax:library-expanded-hook library-expanded-hook)
  (set-symbol-value! 'psyntax:library-spec-by-name
    (lambda (name) (library-spec (find-library-by-name name))))
  (set-symbol-value! 'psyntax:interaction-library-name interaction-library-name)
  (set-symbol-value! 'psyntax:interaction-source-name interaction-source-name)
  (set-symbol-value! 'psyntax:identifier-binding identifier-binding)
  (set-symbol-value! 'psyntax:syntax->datum syntax->datum)
  (set-symbol-value! 'psyntax:free-identifier=? free-identifier=?)
  ;; A library's exports, for the Lisp side (src/interop.lisp): a list of
  ;; (name type value) -- TYPE is the binding type (global, core-prim,
  ;; global-macro, ...), VALUE the location or primitive name.  The
  ;; library is found (and loaded) if need be, and invoked.
  (set-symbol-value! 'psyntax:library-export-bindings
    (lambda (name)
      (let ((lib (find-library-by-name name)))
        (invoke-library lib)
        (map (lambda (x)
               (let ((b (imported-label->binding (cdr x))))
                 (if (pair? b)
                     (list (car x) (car b) (cdr b))
                     (list (car x) 'unknown #f))))
             (library-subst lib))))))
