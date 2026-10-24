# Our libguile: Guile's C API on Quasischeme

`--guile` runs Guile's Scheme (docs/guile.md). Much of the Guile
world is also C written against **libguile**, Guile's C library:
- extensions loaded with `load-extension`: guile-gnutls, guile-ssh and
  guile-avahi among Guix's dependencies;
- programs that embed Guile, LilyPond the largest of them.

`src/libguile/` is a libguile of our own. It is
`libguile-3.0.so.1`, **ABI-compatible with GNU Guile 3.0.11**: code
compiled against Guile's own headers runs on it unchanged, with
Quasischeme's Guile mode in place of Guile's VM. It is a thin C layer
over the Guile mode, which is Lisp.

Status (October 2026):
- **Extensions under `quasischeme --guile`: working.**
  tests/libguile/ext-basic.c is built against Guile 3.0.11's headers. It
  exercises the inline macros, in-place mutation from C, errors,
  callbacks, smobs, foreign objects and dynwind. Its output is the
  same as on real Guile (tests/libguile/run.sh).
- **Coverage:** all 1,793 functions Guile's libguile exports are
  there, and all 93 of its data symbols:
  - about 1,150 are implemented: 953 Scheme-level procedures, derived from the
    manual, and the rest written by hand;
  - 634 are stubs that raise "not implemented", so that anything linked
    against Guile still loads.
- **Port types written in C** (`scm_make_port_type`, `scm_c_make_port`,
  `SCM_STREAM`): Guile mode's own ports, bivalent as Guile's are, with
  the C type's read, write and close functions as their backend.
  tests/libguile/ext-ports.c and ports.scm give the same results as real
  Guile: reading lines and bytes, writing, closing, and reading from C.
  The one difference is in the output: three warnings our Guile mode
  prints when `(rnrs io ports)` is imported. Its `port?`,
  `input-port?` and `output-port?` are other bindings than the root
  module's, where in Guile they are the same.
- **Programs that embed Guile** (a C `main` calling `scm_with_guile` or
  `scm_boot_guile`): **working.** tests/libguile/embed-*.c, built once
  against real Guile and once against ours, print the same. **LilyPond
  2.24.4** (a binary built against Guile 3.0.11) runs on our library
  and typesets a score to a PDF whose page is pixel-identical to the one
  it makes on real Guile (tests/lilypond/run.sh; see "LilyPond").
- Linux only so far. It is built and tested in the container
  (utils/container/), which has a real Guile 3.0.11 to compare with.

## Using it

### Running a C extension built against GNU Guile

Nothing changes in how it's called:

```scheme
(load-extension "/path/to/guile-foo.so" "scm_init_foo")
```

Under `quasischeme --guile`, `load-extension` first looks for one of
the Guile mode's built-in extensions. If there is none and the library
is a file it can find, it:
1. loads our libguile into the process;
2. loads the extension, whose `libguile-3.0.so.1` dependency resolves to
   ours, since a library with that soname is already loaded;
3. calls the init function.

The extension is found:
- as given, with or without `.so`;
- or in the directories of `GUILE_EXTENSIONS_PATH`.

Our libguile itself is `build/libguile/libguile-3.0.so.1`.
`QUASISCHEME_LIBGUILE` names another copy.

Build it with:

```sh
make -C src/libguile          # needs a C compiler, libgc (the Boehm collector) and SBCL
```

### Building against it

There is nothing to build against differently. Compile with GNU Guile
3.0's headers, exactly as for Guile:

```sh
cc -shared -fPIC $(pkg-config --cflags guile-3.0) foo.c -o guile-foo.so
```

To link against our library instead of an installed Guile, point the
linker at it. Both have the soname `libguile-3.0.so.1`:

```sh
cc ... -L/path/to/quasischeme/build/libguile -lguile-3.0
```

The headers are Guile's (LGPL), from a Guile installation or its
source tarball. This project doesn't ship them. A header of our own,
written from the manual, would let a program build without Guile
installed; that is future work.

## How it works

```
  C code compiled against Guile's headers
      |   inline macros read and write objects' memory directly:
      |   SCM_CAR, scm_is_pair, SCM_SIMPLE_VECTOR_SET, SCM_BYTEVECTOR_CONTENTS, ...
      |   functions call libguile:  scm_c_define_gsubr, scm_call_1, scm_from_utf8_string, ...
      v
  libguile-3.0.so.1  (C, src/libguile/)
      core.c   the heap C sees (Boehm GC), finalization, threads, variadic functions
      data.c   the exported data (scm_smobs, scm_sym_*, ...), with Guile's sizes
      api.c    one stub per function, generated from the table: passes its
               arguments to psg_entry
      |
      v  psg_entry (an SBCL alien callback): table index + arguments
  Lisp (src/libguile/)
      table.lisp    every function: name, C types, how it's implemented
      scheme.lisp   the 953 that are Scheme procedures (generated from Guile's manual)
      bridge.lisp   Lisp objects <-> C's: mirrors, synchronization, lifetimes
      impl.lisp     the entry point, conversions, gsubrs, load-extension
      api.lisp      the hand-written functions
      |
      v
  Guile mode (src/guile/): the procedures themselves
```

### The ABI

A program compiled against Guile's headers depends on more than
function names:
- **Inline code.** Macros and inline functions read objects' memory
  directly:
  - a pair is two words, car and cdr (`SCM_CAR`);
  - a vector's first word holds its length and its elements follow;
  - a bytevector's third word points to its contents;
  - type tests read the first word's tag;
  - fixnums, characters, booleans and `'()` are immediates with Guile's
    encodings.
- **Allocation.** Inline `scm_cons` and `scm_new_smob` allocate through
  the exported `scm_gc_malloc`, which is ours.
- **Data.** `scm_smobs`, whose `mark` and `free` fields inline
  `scm_new_smob` reads; the `scm_*_key` and `scm_sym_*` symbols;
  `scm_tc16_*`. Executables may have copied these into themselves, so
  their sizes matter.
- **Symbol versions.** Every symbol is `GUILE_2.0`
  (src/libguile/libguile.map), which is what programs ask for.

These facts are measured, not copied. tests/libguile/probe.c is
compiled against a real Guile and builds objects; it prints their words
and the values of the constants. src/libguile/abi.lisp records the
result. The list of exports (exports-3.0.11.txt) comes from `nm` on
Guile's library. Nothing here is derived from libguile's source (LGPL).
The project is BSD-style, as for the Guile VM (docs/guile-vm.md).

### Objects on both sides

A Lisp object that C is given gets a **mirror**: memory from our heap,
laid out as Guile lays out that kind of object.
- Two hidden words before each mirror hold a handle, the index of its
  entry in a Lisp table, and some flags.
- An object C makes, such as a pair from inline `scm_cons`, gets a Lisp
  counterpart when Lisp is given it. A pair becomes a cons, and from
  then on the memory is that cons's mirror.
- A smob, which only C understands, becomes a `c-object` that holds its
  address. It prints through the smob's own print function.

Pairs, vectors, variables, bytevectors and structs exist twice, so they
are **synchronized** when they cross between C and Lisp:
- **Lisp to C** (arguments to a C procedure, results C asks for):
  export writes the Lisp object's contents to the mirror.
- **C to Lisp** (arguments C passes, results it returns, and the
  arguments of a C procedure after it returns): import copies back the
  words C changed.
- Each entry keeps a **snapshot** of the words last written. A word that
  differs from it is one C wrote, so only those are copied back, and
  what Lisp changed meanwhile isn't undone.
- Synchronization goes all through a structure (a list's spine and
  elements), once per crossing, but it is limited in two ways
  ("Dirty tracking" below): what Lisp must write to the mirrors is
  skipped when no Lisp code has run since the last write, and a
  function that reads only the top of a structure (`caar`, `hashq-set!`)
  reads back only that much. A C loop over a list doesn't cost the
  list's length at each step.

The one departure from Guile: suppose C keeps an object in a global
variable, changes it, and never passes it back. Lisp doesn't see the
change until the object next crosses. The reverse holds too: C keeps
the object and Lisp changes it. Guile has one heap, so it has neither
problem.

**Lifetimes.**
- Our heap is the Boehm collector's, as Guile's is. It finds the
  pointers C holds by scanning stacks and static data, as it does for
  Guile.
- An entry holds its Lisp object while the mirror is reachable from C.
  Mirrors are registered for finalization, and the collector reports
  the ones C can no longer reach. Lisp then drops their entries, unless
  the object was handed to C again in the meantime.
- A `c-object` holds its C memory with a root cell until SBCL collects
  the `c-object`.
- Cycles that run through both heaps are never collected.

**Errors** unwind through C frames, as Guile's `longjmp` does. An SBCL
non-local exit from an alien callback crosses the C frames between it
and the Lisp that called C (tested on arm64 macOS and Linux). C's
`scm_dynwind_*` frames are run on the way out.

**Threads.** A thread is registered with the Boehm collector the first
time it allocates from our heap. It is unregistered when it exits.

### Dirty tracking

Walking a whole structure at every call made a C loop over a list of n
elements cost n squared: LilyPond's loop over a font's 638 glyphs
(`scm_caar`, `scm_cdar`, `scm_hashq_set_x` per glyph) spent 3.9 s of a
7.5 s run in it. Now:
- **Lisp to C.** `*lisp-gen*` counts the times Lisp code that may have
  changed a mirrored object has run: a function C calls that isn't
  known to only read (`*reader-functions*`, `*reader-procedures*`:
  predicates, `c[ad]+r`, `hashq-ref`/`-set!`, `assq`, the conversions)
  advances it before and after, as does a Scheme call of a C procedure
  and every call from Lisp into C (`c-funcall`). Each entry records the
  generation of its last write, and a mirror written in the current
  generation isn't written again. The variables, which every crossing
  used to scan, are scanned for Lisp's changes only if it advanced, and
  for C's only before and after functions that aren't readers.
- **C to Lisp.** A reader of a bounded depth (`cdar` looks two levels
  down, `hashq-set!` none) reads back only that many levels
  (`*import-depth*`, `absorb-bounded`); the others go all through the
  structure as before.
- A reader doesn't write its arguments back after it runs (it can't
  have changed them), but still passes them through `export-scm`, which
  is free when nothing has changed.

What this assumes: Lisp changes a mirrored object only while Lisp code
runs, and that code is entered through the places above. Another Scheme
thread changing a list while C holds it isn't seen either way (as
before). A C loop that calls only readers is the fast path; one that
calls anything else (`scm_call_n`, `scm_primitive_eval`) pays the walk
the first time it passes each structure after that.

`QUASISCHEME_LIBGUILE_PROFILE=1` prints, at exit and every 20,000
calls, each function's own time (what it calls into isn't counted), its
calls, and the entries it wrote to and read from mirrors;
`=sprof` runs SBCL's statistical profiler (`SBCL_HOME` must name its
contrib). `PROFILE=1` in tests/lilypond/time.sh runs one score so.

## Embedding

For a C `main` that calls `scm_with_guile` or `scm_boot_guile`, the
library starts Lisp itself, at its first call:
- SBCL is built as a shared library, `libsbcl.so`; the container has one.
- `make -C src/libguile core` saves `build/libguile/quasischeme-guile.core`
  (build-core.lisp): the command line's system, which has the Guile mode
  booted, with `start-embedded` (impl.lisp) on `*init-hooks*` and
  `psg-entry` as a callable export. SBCL's start-up then returns to C,
  and writes the export's address into core.c's `psg_entry`.
- core.c's `boot_entry` is the entry point until then. It `dlopen`s
  `libsbcl.so` (`QUASISCHEME_LIBSBCL`) and calls `initialize_lisp` on the
  core, which is beside the library or named by `QUASISCHEME_GUILE_CORE`
  (`QUASISCHEME_HEAP` is the heap limit, 8GB).
- A call from the C main thread then arrives as a callback on a foreign
  thread; errors become Guile exceptions (`with-guile-errors`) from the
  first call on, and `scm_boot_guile` and `scm_shell` are in api.lisp.

The same `libguile-3.0.so.1` serves both cases. sbcl-librarian
(github.com/quil-lang/sbcl-librarian) shows that this works. We don't use
its generated API, since ours has to be Guile's exact one.

What embedding needed beyond the extension case:
- **Variables are shared in place.** C reads and writes a variable's
  value cell directly (`SCM_VARIABLE_REF`), so variables are the one
  kind of object synchronized at every crossing (bridge.lisp,
  `sync-variables-to-c` and `-to-lisp`), not only when passed.
- **Mark functions run.** A smob type with a mark function
  (`scm_set_smob_mark`) gets cells of a collector kind of its own
  (core.c): its mark procedure calls the type's mark function, and
  `scm_gc_mark` pushes what that marks. C++ objects whose members are
  SCMs, in memory the collector doesn't scan, stay alive that way.
- `scm_gc_protect_object` roots the object's mirror, not just the Lisp
  object. The mirrors of a call's arguments stay while it runs.
- A smob with an apply function (`scm_set_smob_apply`) is a funcallable
  instance, so Scheme can call it.
- The module system: `scm_c_define_module` makes the module as
  `define-module` does, with a public interface; `scm_module_tag` is set.
- Scheme procedures that change their arguments (`reverse!`, `append!`)
  are written back to the mirrors C holds.

Debugging: `QUASISCHEME_LIBGUILE_PROFILE` (see "Dirty tracking"),
`QUASISCHEME_LIBGUILE_TRACE=1` prints each libguile function
C calls; `=2` also their arguments, from the call number in
`QUASISCHEME_LIBGUILE_TRACE_AFTER` on.

## LilyPond

The container builds LilyPond 2.24.4 (the Containerfile's last layer,
pinned by sha256) against the real Guile 3.0.11 in /opt/lilypond.
`sh utils/container/run.sh lilypond 'sh tests/lilypond/run.sh'` runs
`{ c' d' e' }` on it, then the same binary with `LD_LIBRARY_PATH` on
build/libguile, and compares ghostscript's rendering of the two PDFs.
Status: the same page. Over 100,000 calls into libguile in the run,
and all of LilyPond's Scheme (scm/*.scm, ly/*.ly) runs in Guile mode.
Speed (arm64 container, warm module cache, a small score): about 3.7 s
here against 1.0 s on real Guile; 7.5 s before dirty tracking. Opening
the seven Emmentaler sizes went from 3.9 s to 0.5 s. What is left is
Guile mode's own: compiling LilyPond's top-level Scheme forms
(`scm_primitive_eval`, 1,100 calls, about 1.4 s, most of it SBCL's
compiler), loading scm/*.scm (0.4 s) and cached module code (0.2 s);
the bridge's share is now small. The first run, which fills the module
cache, takes 40 to 60 s; the compiling of Guile's modules and
LilyPond's files into the cache dominates it.
Not tried yet: bigger scores, fonts and `\markup` text, MIDI, other
backends.

## What's next

1. What guile-gnutls, guile-ssh and guile-avahi import that is still a
   stub (the port functions they need are done, above):
   - hooks (`scm_c_hook_add`);
   - array handles (`scm_array_get_handle`);
   - weak tables (`scm_c_make_weak_vector`).

   Then Guix's TLS (`guix download`, substitutes) runs on them.
2. More of LilyPond: bigger inputs (the ly/ regression files), speed.
3. Our own `libguile.h`.
4. Finer dirty tracking: Guile mode's mutators noting which mirrored
   objects they change, so that even after Lisp code has run only those
   are written again (now one change anywhere rewrites every structure
   passed next); and a way for C's writes to be found without scanning.
