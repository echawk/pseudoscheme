#!/usr/bin/env python3
"""Extract Racket's regexp tests for rx-tests.ss: from rx.rktl (argv[1]),
its test-regexp cases into rx-cases.ss and its PCRE table into
pcre-table.ss (in the current directory), with Racket's byte-string
literals #"..." rewritten as #vu8(...), which Chez reads."""
import re, sys

def unescape(s):
    res = bytearray(); i = 0
    named = {'n': 10, 't': 9, 'r': 13, 'a': 7, 'b': 8, 'v': 11, 'f': 12, 'e': 27,
             '\\': 92, '"': 34, "'": 39}
    while i < len(s):
        c = s[i]
        if c == '\\':
            i += 1; d = s[i]
            if d in named:
                res.append(named[d]); i += 1
            elif d in '01234567':
                j = i
                while j < len(s) and j < i + 3 and s[j] in '01234567': j += 1
                res.append(int(s[i:j], 8) & 255); i = j
            elif d == 'x':
                j = i + 1
                while j < len(s) and j < i + 3 and s[j] in '0123456789abcdefABCDEF': j += 1
                res.append(int(s[i + 1:j], 16)); i = j
            else:
                raise ValueError(d)
        else:
            res += c.encode('utf-8'); i += 1
    return res

def convert(text):
    out = []; pos = 0
    for m in re.finditer(r'#"((?:[^"\\]|\\.)*)"', text, re.S):
        out.append(text[pos:m.start()])
        out.append('#vu8(%s)' % ' '.join(str(b) for b in unescape(m.group(1))))
        pos = m.end()
    out.append(text[pos:])
    return ''.join(out)

src = open(sys.argv[1], encoding='utf-8').read()

# the (test-regexp ...) and (test-regexp-x ...) forms
cases = []; lines = src.split('\n'); i = 0
while i < len(lines):
    form = lines[i]
    if form.startswith('(test-regexp'):
        depth = form.count('(') - form.count(')')
        while depth > 0 and i + 1 < len(lines):
            i += 1; form += '\n' + lines[i]
            depth += lines[i].count('(') - lines[i].count(')')
        try:
            c = convert(form)
            if '#rx' not in c and '#px' not in c:
                cases.append(c)
        except ValueError:
            pass
    i += 1
open('rx-cases.ss', 'w').write('\n'.join(cases) + '\n')

# the PCRE table: the quoted list the (map (lambda (t) ...) ...) runs over
start = src.index("(map (lambda (t)\n       (if (pair? t)")
q = src.index("'(", start)
i = q + 1; depth = 0; instr = False
while True:
    c = src[i]
    if instr:
        if c == '\\': i += 2; continue
        if c == '"': instr = False
    elif c == '"': instr = True
    elif c == ';': i = src.index('\n', i); continue
    elif c == '(': depth += 1
    elif c == ')':
        depth -= 1
        if depth == 0: break
    i += 1
table = re.sub(r';[^\n]*', '', convert(src[q + 1:i + 1]))
open('pcre-table.ss', 'w').write("(define pcre-table '" + table + ")\n")
print("%d test-regexp cases; the PCRE table" % len(cases))
