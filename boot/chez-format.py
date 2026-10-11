#!/usr/bin/env python3
"""Make src/chez/format.scm from Chez Scheme's own format (s/format.ss),
$flonum->digits (s/print.ss) and three macros (s/cmacros.ss), at the
commit the mats come from (vendor/chez-mats/README.md).

Usage: python3 boot/chez-format.py [OUTPUT]   (default src/chez/format.scm)
It fetches the three files from GitHub."""
import sys, urllib.request
COMMIT = "e3b13636"
def fetch(name):
    url = "https://raw.githubusercontent.com/cisco/ChezScheme/%s/s/%s" % (COMMIT, name)
    return urllib.request.urlopen(url).read().decode("utf-8")
out = sys.argv[1] if len(sys.argv) > 1 else "src/chez/format.scm"
fmt = fetch("format.ss").split('\n')
cm = fetch("cmacros.ss")
pr = fetch("print.ss")

def extract_form(text, start_pat):
    i = text.index(start_pat)
    depth = 0; j = i; in_str = False; esc = False; comment = False
    while True:
        c = text[j]
        if comment:
            if c == '\n': comment = False
        elif in_str:
            if esc: esc = False
            elif c == '\\': esc = True
            elif c == '"': in_str = False
        elif c == ';': comment = True
        elif c == '"': in_str = True
        elif c == '#' and text[j+1] == '\\':
            j += 3; continue
        elif c == '(' or c == '[': depth += 1
        elif c == ')' or c == ']':
            depth -= 1
            if depth == 0: return text[i:j+1]
        j += 1

monad = extract_form(cm, '(define-syntax define-syntactic-monad')
state_case = extract_form(cm, '(define-syntax state-case')
datatype = extract_form(cm, '(define-syntax define-datatype').replace('literal-identifier=?', 'free-identifier=?')
# andmap, Chez's, is a run-time definition in (chezscheme)'s body, out of
# the transformers' reach: R6RS's for-all is the same
monad = monad.replace('(andmap ', '(for-all ')
state_case = state_case.replace('(andmap ', '(for-all ')
datatype = datatype.replace('(andmap ', '(for-all ')
def sexp_end(text, i):
    """The index after the datum starting at I."""
    while text[i].isspace(): i += 1
    if text[i] in '([':
        depth = 0
        j = i
        while True:
            c = text[j]
            if c == '"':
                j += 1
                while text[j] != '"':
                    j += 2 if text[j] == '\\' else 1
            elif c in '([': depth += 1
            elif c in ')]':
                depth -= 1
                if depth == 0: return j + 1
            j += 1
    if text[i] == '"':
        j = i + 1
        while text[j] != '"':
            j += 2 if text[j] == '\\' else 1
        return j + 1
    j = i
    while not text[j].isspace() and text[j] not in '()[]': j += 1
    return j

def syntax_violation(text):
    """(syntax-error form message) as R6RS's (syntax-violation #f message
    form): syntax-error isn't bound in (chezscheme)'s body."""
    while '(syntax-error ' in text:
        i = text.index('(syntax-error ')
        a = i + len('(syntax-error ')
        a_end = sexp_end(text, a)
        m_end = sexp_end(text, a_end)
        close = sexp_end(text, i)
        form = text[a:a_end].strip()
        msg = text[a_end:m_end].strip()
        text = text[:i] + '(syntax-violation #f ' + msg + ' ' + form + ')' + text[close:]
    return text

# format is (chezscheme)'s, out of the transformers' reach too
monad = monad.replace('''(format "undeclared ~s monad binding" 'name)''', '"undeclared monad binding"')
i = datatype.index('(format "unhandled')
datatype = datatype[:i] + '"unhandled variant"' + datatype[sexp_end(datatype, i):]
monad = syntax_violation(monad)
datatype = syntax_violation(datatype)
state_case = syntax_violation(state_case)

# format.ss's datatype names are all #{name uid}s: split-name needn't ask
# gensym? (another run-time definition) or make one
sn_start = datatype.index('(define split-name')
sn = datatype[sn_start:sn_start + len(datatype[sn_start:].split('\n')[0])]
split = '''(define split-name
           (lambda (x)
             (let ([sym (syntax->datum x)])
               (cons (datum->syntax x (string->symbol (symbol->string sym))) x))))'''
i = datatype.index('(define split-name')
depth = 0; j = i
while True:
    c = datatype[j]
    if c == '(' or c == '[': depth += 1
    elif c == ')' or c == ']':
        depth -= 1
        if depth == 0: break
    j += 1
datatype = datatype[:i] + split + datatype[j+1:]
digits = extract_form(pr, '(let ()\n(define terminator-1')
digits = digits.replace("(define terminator-1 '#1=(-1 . #1#))",
                        "(define terminator-1 (let ([l (list -1)]) (set-cdr! l l) l))")
digits = digits.replace("(define terminator-2 '#2=(-2 . #2#))",
                        "(define terminator-2 (let ([l (list -2)]) (set-cdr! l l) l))")
digits = digits.replace("[q-r ($quotient-remainder rob s)]", "[q-r (cons (quotient rob s) (remainder rob s))]")
digits = digits.replace("(ratnum? y)", "(and (exact? y) (not (integer? y)))")
digits = digits.replace("(set! $flonum->digits", "(define flonum->digits")
digits = digits.replace("(decode-float x)", "(%chez:decode-float x)")
# the let () body ends with the set!; make it return the procedure
assert digits.startswith('(let ()')
digits = '(define $flonum->digits\n' + digits[:-1] + '\nflonum->digits))'

# format.ss body: from the (let () after the comments to the end
body = '\n'.join(fmt)
start = body.index('(let ()\n  ;;; configuration')
body = body[start:]
# drop make-fmt->expr (compile-time format, cp1in)
mk = extract_form(body, '(define (make-fmt->expr build-quote build-seq build-primcall)')
body = body.replace(mk, ';; (make-fmt->expr, the compiler\'s inlining of format, left out)')
body = body.replace('(define flonum->digits #%$flonum->digits)', '(define flonum->digits $flonum->digits)')
body = body.replace('(define flonum-sign #%$flonum-sign)', '(define flonum-sign (lambda (x) (if (or (fl< x 0.0) (eqv? x -0.0)) 1 0)))')
# format ports: Lisp's (host.lisp)
fp = extract_form(body, '(define make-format-port')
body = body.replace(fp, '(define make-format-port %chez:make-format-port)')
oc = extract_form(body, '(define (output-column p)')
body = body.replace(oc, '(define (output-column p) (%chez:format-port-column p))')
body = body.replace('(define format-port-name "format port")', '')
# exports: definitions, not set!s of Chez's globals
for name in ['$dofmt', '$make-fmt->expr', '$parse-format-string']:
    f = extract_form(body, '(set! ' + name)
    body = body.replace(f, '')
body = body.replace('(set! format\n', '(define %format\n', 1)
body = body.replace('(set! printf\n', '(define %printf\n', 1)
body = body.replace('(set! fprintf\n', '(define %fprintf\n', 1)
# the (let () ...) around it all a module, whose definitions become the
# library's own: so that each is compiled by itself (SBCL can't compile
# the whole as one form, and would leave it to its interpreter)
assert body.startswith('(let ()') and body.rstrip().endswith(')')
body = '(module (%format %printf %fprintf)' + body[len('(let ()'):]

header = ''';;; -*- Mode: Scheme -*-
;;;; (chezscheme): format, printf and fprintf -- Chez Scheme's own,
;;;; s/format.ss, with $flonum->digits from s/print.ss and the macros
;;;; define-syntactic-monad, state-case and define-datatype from
;;;; s/cmacros.ss, all at cisco/ChezScheme e3b13636.
;;;;
;;;; Copyright 1984-2017 Cisco Systems, Inc.  Licensed under the Apache
;;;; License, Version 2.0 (vendor/chez-mats/LICENSE).
;;;;
;;;; Made by boot/chez-format.py from those files; the changes:
;;;; Chez's format ports are Lisp's (%chez:make-format-port, host.lisp),
;;;; $oops is assertion-violationf, the macros use for-all for andmap
;;;; (and define-datatype takes its names to be #{name uid}s), and
;;;; syntax-violation for syntax-error,
;;;; the compiler's inlining of format (make-fmt->expr)
;;;; is left out, cyclic constants are made at run time, and format,
;;;; printf and fprintf are defined rather than assigned.

'''
helpers = '''
(define ($oops who msg . args)
  (assertion-violation who (apply format #f msg args)))

(define (exceptional-flonum? x) (or (nan? x) (infinite? x)))

(define ($list-length ls who)
  (if (list? ls) (length ls) ($oops who "~s is not a proper list" ls)))

'''
result = header + monad + '\n\n' + state_case + '\n\n' + datatype + '\n\n' + digits + '\n' + helpers + \
  body + '\n\n(define format %format)\n(define printf %printf)\n(define fprintf %fprintf)\n'
open(out, 'w').write(result)
