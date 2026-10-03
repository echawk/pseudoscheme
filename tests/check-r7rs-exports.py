#!/usr/bin/env python3
"""Cross-check src/r7rs/exports.lisp against Appendix A of r7rs.pdf.

Usage: python3 tests/check-r7rs-exports.py [r7rs.pdf]

Extracts the identifier-looking tokens from pages 73-76 of the PDF (via
pdftotext -layout) and compares the set with the union of all exports in
exports.lisp.  Prose words show up as 'only in PDF' and are expected to
be a short list of English words; anything else is a transcription bug.
"""
import re, subprocess, sys, os
here = os.path.dirname(os.path.abspath(__file__))
pdf = sys.argv[1] if len(sys.argv) > 1 else os.path.join(here, '..', 'rnrs-pdfs', 'r7rs.pdf')
text = subprocess.run(['pdftotext', '-layout', '-f', '73', '-l', '76', pdf, '-'],
                      capture_output=True, text=True, check=True).stdout
src = open(os.path.join(here, '..', 'src', 'r7rs', 'exports.lisp')).read()
mine = set()
for m in re.finditer(r'\(\(scheme [a-z0-9-]+\)\s+"([^"]*)"', src):
    mine |= set(m.group(1).split())
# tokens in the PDF's identifier blocks: lines made only of 1-4 short tokens
pdf_tokens = set()
prose = re.compile(r'^(the|a|of|to|in|and|is|are|by|as|on|for|or|an|it|be|that|this|not|at|but)$')
for line in text.split('\n'):
    toks = line.split()
    if not toks or len(line) - len(line.lstrip()) > 80:
        continue
    for t in toks:
        if re.fullmatch(r"[^\s()\"';,.]+|\.\.\.|[+*/<=>-]+|[a-z0-9!$%&*/:<=>?^_~+-]+", t):
            pdf_tokens.add(t)
only_pdf = sorted(pdf_tokens - mine)
only_mine = sorted(mine - pdf_tokens)
print(f"{len(mine)} distinct exports transcribed")
print("in the transcription but NOT found in the PDF text:", only_mine)
words = [t for t in only_pdf if re.fullmatch(r"[a-zA-Z]+", t)]
print(f"{len(only_pdf)} PDF tokens not in the transcription (mostly prose words);")
print("lowercase alphabetic ones (check none is a real identifier):",
      sorted(t for t in words if t.islower()))
print("non-alphabetic ones, which would be suspicious:", [t for t in only_pdf if t not in words])
sys.exit(1 if only_mine else 0)
