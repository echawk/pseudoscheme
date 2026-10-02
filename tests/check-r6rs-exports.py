#!/usr/bin/env python3
"""Cross-check the (rnrs base) export list in src/r6rs/exports.lisp
against the entries of chapter 11 of r6rs.pdf.

Usage: python3 tests/check-r6rs-exports.py [r6rs.pdf]

Every transcribed (rnrs base) name must appear in the PDF text either
as a call/syntax pattern "(name ..." or as an entry heading; names that
don't are printed and make the script fail.
"""
import re, subprocess, sys, os
here = os.path.dirname(os.path.abspath(__file__))
pdf = sys.argv[1] if len(sys.argv) > 1 else os.path.join(here, '..', 'r6rs.pdf')
text = subprocess.run(['pdftotext', '-f', '36', '-l', '62', pdf, '-'],
                      capture_output=True, text=True, check=True).stdout
src = open(os.path.join(here, '..', 'src', 'r6rs', 'exports.lisp')).read()
m = re.search(r'\(\(rnrs base\)\s+"([^"]*)"', src)
names = m.group(1).split()
missing = []
for n in names:
    # The report defines the 28 c[ad]{2,4}r procedures together, as
    # "(caar pair) (cadr pair) ... (cddddr pair)"; and `_' is only ever
    # mentioned as the "underscore" pattern wildcard.
    if re.fullmatch(r'c[ad]{2,4}r', n) and 'cddddr' in text:
        continue
    if n == '_' and 'underscore' in text:
        continue
    esc = re.escape(n)
    if not (re.search(r'\(' + esc + r'[\s)]', text) or re.search(r'^' + esc + r'$', text, re.M)
            or re.search(r'\b' + esc + r'\b', text)):
        missing.append(n)
print(f"{len(names)} (rnrs base) names transcribed; not found in chapter 11: {missing}")
sys.exit(1 if missing else 0)
