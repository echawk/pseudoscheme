# Using Scheme libraries: Akku and snow

Scheme has a large body of portable libraries: SRFIs, JSON and XML
parsers, cryptography, data structures, test frameworks, pattern
matchers. They're distributed through two package managers:

- **[Akku](https://akkuscm.org)**: R6RS and R7RS. It installs packages
  per project, into the project's `.akku/lib`, from an index that also
  mirrors snow-fort.
- **[snow-fort](https://snow-fort.org)**: R7RS. Packages are installed
  with `snow-chibi`, the client that comes with chibi-scheme.

Pseudoscheme can load what either one installs. This guide shows how,
from the command line and from Lisp. It's written for Common Lisp
programmers who haven't used either tool.

## 1. Scheme libraries, briefly

A Scheme *library* is roughly what a CL package and its system are
together. It has a name, a list of exports, a list of imports, and a
body. Names are lists: `(srfi 1)`, `(hashing sha-2)`, `(macduffie json)`.
There are two notations:

```scheme
;; R7RS, usually in a .sld file
(define-library (demo greet)
  (export greet)
  (import (scheme base) (scheme write))
  (begin
    (define (greet who) (display "hello, ") (display who) (newline))))

;; R6RS, usually in a .sls file
(library (demo greet)
  (export greet)
  (import (rnrs))
  (define (greet who) (display "hello, ") (display who) (newline)))
```

A program imports the libraries it uses:
`(import (scheme base) (demo greet))`. Pseudoscheme expands both kinds
with the same expander, so R6RS and R7RS libraries can import each
other.

Scheme has no `defsystem`. Instead a library is found by its name: the
implementation looks for `(demo greet)` in `demo/greet.sld`,
`demo/greet.sls` and so on, in each directory of a search path. A
package manager's job is to put files in such a directory.

| Common Lisp | Scheme |
|---|---|
| package and system | library |
| `(:use ...)`, `(:import-from ...)` | `(import (only (srfi 1) fold) ...)` |
| Quicklisp `quickload` | `snow-chibi install` |
| qlot or ocicl (per project, with a lock file) | Akku (`Akku.manifest`, `Akku.lock`) |
| `asdf:*central-registry*` | the library path (`-L`, `r7rs:add-library-directory`) |

## 2. How Pseudoscheme finds a library

A library `(foo bar)` is looked for in every directory of the search
path, in order:

1. the directories you add: `-L DIR` on the command line (repeatable),
   `--akku`, `PSEUDOSCHEME_LIBRARY_PATH` (directories separated by
   colons), or `r7rs:add-library-directory` from Lisp. All of these put
   directories on `psx:*library-path*`, which starts as `("./")`;
2. then the libraries that ship with Pseudoscheme: the SRFIs in
   `src/srfi/`. `(scheme ...)`, `(rnrs ...)`, `(chezscheme)` and
   `(ikarus)` are built in.

Within a directory, `(foo bar)` is `foo/bar.sls`, `foo/bar.ss`,
`foo/bar.sld` or `foo/bar.scm`. Implementation-specific variants are
also tried, as Akku lays them out. The order is:
- `foo/bar.pseudoscheme.sls`;
- the generic files above;
- `foo/bar.chezscheme.sls`, then `foo/bar.ikarus.sls`.

So when a package has no portable version of a file, Chez Scheme's is
used, on top of Pseudoscheme's `(chezscheme)` compatibility library.

File names are escaped the way Akku writes them:
- `(srfi :1)` may be `srfi/%3a1.sls`, `srfi/:1.sls` or `srfi/1.sld`;
- `let-optionals*` is `let-optionals%2a.sls`.

R7RS names with numbers, `(srfi 1)`, and R6RS ones, `(srfi :1)`, name
the same library.

## 3. Before you start

Build the command-line program:

```sh
make -C contrib/cli           # writes bin/pseudoscheme
```

The examples below put `bin/` on `PATH`. From Lisp you need Quicklisp,
which fetches Pseudoscheme's own dependencies (float-features,
cl-unicode, trivial-gray-streams):

```lisp
(push #p"/path/to/pseudoscheme/" asdf:*central-registry*)
(ql:quickload :r7rs)
```

## 4. snow-fort, with snow-chibi

### Installing snow-chibi

`snow-chibi` comes with chibi-scheme:

```sh
brew install chibi-scheme            # macOS
sudo apt install chibi-scheme        # Debian and Ubuntu
```

Or build it from <https://github.com/ashinn/chibi-scheme> with `make
&& make install`.

### Finding and installing packages

```sh
snow-chibi search json
#  (macduffie json)   0.9.5
#  (srfi 180)         0.1.1
#  ...

mkdir -p myproj && cd myproj
snow-chibi --always-yes --impls=generic --install-prefix=$PWD/snow \
    install '(macduffie json)' '(lassik string-inflection)'
```

The flags matter:
- **`--install-prefix=DIR`** installs into `DIR`. Without it,
  snow-chibi installs system-wide, into chibi's own directories, and
  asks for `sudo`.
- **`--impls=generic`** writes the libraries straight into `DIR`:
  `DIR/macduffie/json.sld`, which is the layout Pseudoscheme searches.
  Without it, snow-chibi installs for each Scheme it finds on your
  machine.
- **`--always-yes`** answers yes to its prompts, including installing
  dependencies.

snow-chibi prints "couldn't load config: ~/.snow/config.scm" on every
run; that's harmless. Packages are named by their library names, quoted
for the shell: `'(macduffie json)'`.

### Using them

```scheme
;; json-demo.scm
(import (scheme base) (scheme write)
        (macduffie json) (lassik string-inflection))
(write (json-read-string "[1, {\"b\": null}]")) (newline)
(write (string-inflection-underscore "FooBar")) (newline)
```

```sh
pseudoscheme -L snow json-demo.scm
# (1 #<Record <srfi-hash-table>>)
# "foo_bar"
```

To avoid passing `-L` every time, export
`PSEUDOSCHEME_LIBRARY_PATH=$PWD/snow`.

## 5. Akku

### Installing Akku

Akku has no Homebrew formula. Two ways to install it:

- **From source**, with GNU Guile 3:

  ```sh
  brew install guile autoconf automake pkg-config     # macOS; apt has the same packages
  git clone https://gitlab.com/akkuscm/akku.git && cd akku
  ./bootstrap && ./configure --prefix=$HOME/.local && make && make install
  ```

- **From a release** at <https://gitlab.com/akkuscm/akku/-/releases>:
  a prebuilt binary for Linux on amd64, or a source tarball built with
  Chez Scheme.

Then fetch the package index once, and again whenever you want newer
packages:

```sh
akku update
```

### A project

Akku works per project, like qlot. The commands below create an
`Akku.manifest` (the dependencies you asked for), an `Akku.lock` (the
versions chosen) and `.akku/`, which holds the installed files:

```sh
mkdir hashdemo && cd hashdemo
akku install hashing wak-foof-loop pfds
```

**Akku names packages, not libraries.** The package `wak-foof-loop`
provides the library `(wak foof-loop)`; `akku install '(wak foof-loop)'`
says "Package not found". To find package names:
- `akku list` lists the whole index;
- `akku show hashing` shows one package's libraries and versions.

When a project is checked out somewhere else, `akku install` with no
arguments installs what `Akku.lock` lists. Commit `Akku.manifest` and
`Akku.lock`, and leave `.akku/` out of version control. Akku writes a
`.gitignore` there.

### Using them

Everything Akku installs is in `.akku/lib`. `--akku` adds that to the
search path. It looks in the current directory and then its parents, so
it works from anywhere inside the project.

```scheme
;; demo.sps
(import (rnrs) (hashing sha-2) (wak foof-loop))
(display (sha-256->string (sha-256 (string->utf8 "abc")))) (newline)
(display (loop ((for x (in-list (map string->number (cdr (command-line)))))
                (with s 0 (+ s x)))
           => s))
(newline)
```

```sh
pseudoscheme --r6rs --akku demo.sps 1 2 3
# ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad
# 6
```

`-L .akku/lib` does the same as `--akku`. The program would run without
`--r6rs` too, since `(import (rnrs) ...)` is an R7RS import as well;
`--r6rs` makes it an R6RS top-level program, and the REPL R6RS.

Akku also mirrors snow-fort, so R7RS packages can be installed with
`akku install` too. Akku's `.akku/env` and `.akku/bin/activate` scripts
set environment variables for other Schemes; Pseudoscheme doesn't need
them.

## 6. From Lisp

Add the directory, then import. `r7rs:import` makes a library's
procedures Lisp functions and its macros Lisp macros, in the current
package:

```lisp
(r7rs:add-library-directory "hashdemo/.akku/lib/")
(r7rs:add-library-directory "myproj/snow/")

(r7rs:import (prefix (hashing sha-2) sha-) (lassik string-inflection))

(sha-sha-256->string (sha-sha-256 (map '(vector (unsigned-byte 8)) #'char-code "abc")))
;; => "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
(string-inflection-underscore "FooBar")      ; => "foo_bar"
```

Import sets work as they do in Scheme: `only`, `except`, `rename` and
`prefix`. A prefix ending in a colon would confuse the Lisp reader, so
use a hyphen, as above. Names that would clash with `COMMON-LISP`
(`find`, `remove` and so on) are an error, so use `only`, `except` or
`prefix` for those.

To give a library a package of its own:

```lisp
(r7rs:use-library '(pfds queues) :package :q)
(q:dequeue (q:enqueue (q:make-queue) 1))     ; => 1, #<Record queue>
```

Or run Scheme directly:

```lisp
(r6rs:eval "(import (rnrs) (pfds queues))
            (dequeue (enqueue (make-queue) 'first))")   ; => FIRST
```

docs/interop.md explains how values convert between the languages, how
Scheme macros behave when called from Lisp, and how Scheme libraries can
in turn use Lisp libraries.

### In an ASDF system

A Lisp system can include Scheme libraries of its own as components.
For libraries from Akku or snow, add their directory in a Lisp file
that loads first:

```lisp
(defsystem "my-app"
  :defsystem-depends-on ("pseudoscheme/asdf")
  :components ((:file "paths")                         ; adds .akku/lib, below
               (:r7rs-library "scheme/my-app/util"     ; scheme/my-app/util.sld
                :depends-on ("paths"))
               (:file "main" :depends-on ("scheme/my-app/util"))))
```

```lisp
;; paths.lisp
(r7rs:add-library-directory (asdf:system-relative-pathname "my-app" ".akku/lib/"))
```

See `examples/mixed-system/`.

## 7. Troubleshooting

**"cannot find library (foo bar)".** Check that the file is where §2
says it should be, under one of the directories on the path. From Lisp,
`psx:*library-path*` shows the list.

**Your SRFIs replaced Pseudoscheme's.** Installing a snow package often
installs SRFIs it depends on, such as `snow/srfi/1.sld`. Akku projects
often install `chez-srfi`. Directories you add are searched before
Pseudoscheme's own SRFIs, so those copies win. Usually that's harmless,
but if a SRFI misbehaves, check which file is loaded. Removing the copy
makes the built-in one be used.

**"two imports with different bindings".** Two imported libraries export
the same name with different meanings. The usual case is `(rnrs)` with
SRFI 1, which both define `member`, `assoc` and others. Choose one, for
example:

```scheme
(import (except (rnrs) member assoc) (srfi :1 lists))
```

**Implementation-specific libraries.** A package that has only
`foo.guile.sls` or `foo.racket.sls` won't load. Pseudoscheme uses
generic, Chez and Ikarus variants only. `(chezscheme)` and `(ikarus)`
cover what portable libraries commonly need from those systems: file
system access, `format`, `printf`, boxes, time and dates. They don't
cover FFIs or engines.

**Continuations.** `call/cc` is escape-only, so a library that re-enters
continuations, for coroutines or generators, fails with an error that
says so.

**Finding out how much of a tree loads.** This command imports every
portable library under `DIR` and reports what fails and why:

```sh
sbcl --dynamic-space-size 4GB --control-stack-size 500MB \
     --script tests/run-library-corpus.lisp DIR [-L OTHER-LIB-DIR ...]
```
