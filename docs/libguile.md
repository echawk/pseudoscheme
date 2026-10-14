# Our libguile: Guile's C API on Pseudoscheme

`--guile` runs Guile's Scheme (docs/guile.md). Much of the Guile
world is also C written against **libguile**, Guile's C library:
- extensions loaded with `load-extension`: guile-gnutls, guile-ssh and
  guile-avahi among Guix's dependencies;
- programs that embed Guile, LilyPond the largest of them.

`src/libguile/` is a libguile of our own. It is
`libguile-3.0.so.1`, **ABI-compatible with GNU Guile 3.0.11**: code
compiled against Guile's own headers runs on it unchanged, with
Pseudoscheme's Guile mode in place of Guile's VM. It is a thin C layer
over the Guile mode, which is Lisp.

Status (October 2026):
- **Extensions under `pseudoscheme --guile`: working.**
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
- **Programs that embed Guile** (a C `main` calling `scm_with_guile`):
  **not yet.** The design is under "Embedding" below.
- Linux only so far. It is built and tested in the container
  (utils/container/), which has a real Guile 3.0.11 to compare with.

## Using it

### Running a C extension built against GNU Guile

Nothing changes in how it's called:

```scheme
(load-extension "/path/to/guile-foo.so" "scm_init_foo")
```

Under `pseudoscheme --guile`, `load-extension` first looks for one of
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
`PSEUDOSCHEME_LIBGUILE` names another copy.

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
cc ... -L/path/to/pseudoscheme/build/libguile -lguile-3.0
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
  elements), once per crossing. Functions that only read their
  arguments (`scm_car`, the conversions) go one level deep, so a C loop
  over a list doesn't cost the list's length at each step.

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

## Embedding (planned)

For a C `main` that calls `scm_with_guile` or `scm_boot_guile`, the
library has to start Lisp itself:
- SBCL can be built as a shared library, `libsbcl.so`; the container
  builds one.
- With a saved core holding the Guile mode and this bridge,
  `scm_with_guile` would call `initialize_lisp` the first time.
- The core's start-up sets the entry point and returns to C.

The same `libguile-3.0.so.1` then serves both cases. sbcl-librarian
(github.com/quil-lang/sbcl-librarian) shows that this works. We don't use
its generated API, since ours has to be Guile's exact one.

## What's next

1. The 137 functions guile-gnutls, guile-ssh and guile-avahi import:
   - ports (`scm_make_port_type`, `scm_c_make_port`, the
     `scm_set_port_*` setters), the largest group: drafted, to be tested;
   - hooks (`scm_c_hook_add`);
   - array handles (`scm_array_get_handle`);
   - weak tables.

   Then Guix's TLS (`guix download`, substitutes) runs on them.
2. Embedding, as above. Then LilyPond.
3. Our own `libguile.h`.
4. Cheaper synchronization: Lisp-side dirty tracking (Guile mode's
   mutators note which mirrored objects they change), so that an
   unchanged structure costs nothing to pass again.
