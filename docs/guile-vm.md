# Guile's VM in Lisp (design)

Guile mode already runs Guile's own compiler: Tree-IL, CPS and the
assembler produce Guile 3.0 bytecode inside Pseudoscheme. What it can't
do is run that bytecode. Nor can it load the `.go` files an installed
Guile ships, which are the same bytecode in ELF images. This document
plans a VM for that bytecode, written in Lisp, on top of the existing
Guile runtime (src/guile/).

The guide used for the plan is chatgpt-guile-vm-process.md (repository
root).

## Ground rules

- **Clean room.** The instruction semantics come from Guile's manual
  (chapter 9.3, "A Virtual Machine for Guile", and 9.2.5, "The SCM Type
  in Guile"). The opcode and operand tables, the intrinsics and the type
  tags are asked of Guile itself, at build or run time. Results are
  checked by differential testing against an installed `guile`.
  libguile's C sources (vm-engine.c and the rest, LGPL) are not
  translated or copied; this project's license is BSD-style.
- **Our own ELF reader.** ELF64 is small: a header, program and section
  tables, symbols, strings. lisp-binary is GPL-3, so it isn't used.
- **The runtime owns the semantics.** Numbers, modules, fluids,
  exceptions, prompts and the intrinsics are the existing Guile mode's.
  The VM decodes instructions, keeps Guile's frame layout, does unboxed
  arithmetic and branches, and calls into the runtime where libguile's
  VM would call C.

## Layers

1. **Metadata** (src/guile/vm/ops.lisp). The opcode table: name, opcode,
   kind and word formats, from `(instruction-list)` (as
   `bytecode-table` already fetches it), and the intrinsics, from
   `(language bytecode)`'s intrinsic list. Each word format (`X8_S24`,
   `X8_F12_F12`, `C32`, `L32`, `N32`, `B1_X7_L24`, ...) gives a decoder.
   A disassembler built on them is checked against Guile's own
   disassembler, run on the same bytecode.
2. **Machine state** (vm/machine.lisp). Per thread: a stack vector, `ip`
   (an image and a word index), `sp`, `fp`, and the comparison result
   (`LESS_THAN`, `EQUAL`, `NONE`, `INVALID`). The frame layout is the
   manual's: the dynamic link, vRA and mRA sit above `fp`; locals
   0..N-1 sit below, with local 0 the procedure. Operands are
   `sp`-relative (`sN`) or `fp`-relative (`fN`). Slots hold SCM values
   or unboxed u64/s64/f64 words. At first these are tagged, so that a
   wrong representation is caught where it happens.
3. **Values.** An SCM operand is a Lisp object of the runtime. The raw
   bits of immediates (fixnums `n<<2|2`, characters, `#f`, `'()`, `#t`,
   unspecified, undefined, eof, `#nil`) convert both ways, since
   `make-immediate`, `immediate-tag=?`, `eq-immediate?` and
   `tag-fixnum` work on bits. Heap objects stay Lisp objects.
4. **The virtual heap ABI.** Compiled code reads and writes heap objects
   by word: `scm-ref`, `word-ref`, `scm-ref/tag`, `heap-tag=?`,
   `allocate-words` with `word-set!` of a header. For each type the
   compiler inlines (pair, vector, struct, string, symbol, variable,
   program, box, bytevector, ...), a layout maps word N to the Lisp
   object's field. The tags (`%tc7-vector` and the rest) are asked of
   `(system base types internal)`.
   - A freshly allocated object is a word buffer until word 0 is
     written. `scm-set!` of word 0 makes it a pair; `scm-set!/tag` makes
     it a struct; `word-set!` of a header makes it the type the header
     names (a vector of that length, a program, ...). From then on it
     is the real Lisp object.
5. **Programs.** A compiled closure is a Lisp function (so the runtime
   applies it like any procedure). It carries its code address (an
   image and an entry word) and its free variables, and its word layout
   is the program's (header `tc7-program | nfree<<16`, the code, free
   variables). Calls between compiled procedures stay inside the
   dispatch loop, with frames on the VM stack. A call to any other
   procedure goes out to Lisp and comes back with the values in the
   frame. A tail call reuses the frame, so Lisp's stack doesn't grow
   with Scheme's.
6. **Images.** Bytecode from the assembler, or a `.go` file read by
   our ELF reader: `.rtl-text` (code), `.data` and `.rodata` (static
   objects), and `.dynamic` (the init and entry thunks). Static
   non-immediates (`make-non-immediate`) are decoded from the image's
   words into Lisp objects once, per address, after `static-patch!`
   and `static-set!` have run. `static-ref` cells are image words.
   `load-thunk-from-memory` and then `load-thunk-from-file` return the
   entry thunk.
7. **Control.** Prompts, aborts, `dynamic-wind` and fluids are the
   runtime's (`wind`, `push-fluid` and the rest are intrinsics).
   `capture-continuation` and `abort` with a reified continuation copy
   the VM stack's slice: since the VM stack is a Lisp vector,
   re-entering one means copying it back, alongside the runtime's own
   continuation frames (src/continuations.lisp).

## Order of work

1. The metadata, decoder and disassembler, checked against Guile's.
2. The machine state and frame ABI; moves, constants, branches,
   comparisons, unboxed arithmetic; `return-values`. First target:
   `(compile '(+ 1 2) #:to 'bytecode)` loaded and run.
3. Calls, tail calls, receive, the prologue instructions (`assert-nargs-*`,
   `bind-optionals`, `bind-rest`, `bind-kwargs`), and calls out to and
   back from Lisp.
4. Intrinsics, by index, to the runtime's procedures.
5. The heap ABI: closures (the manual's `make-adder` example), pairs,
   vectors, structs, boxes; the static data and its relocations.
6. Prompts, aborts, continuations, `handle-interrupts` (asyncs).
7. The ELF reader and `.go` loading: `load-thunk-from-file` on the
   installed Guile's own compiled modules.
8. Guile's VM tests (`rtl`, `rtl-compilation`, `vm`, `compiler`,
   `coverage`, `eval` stacks), then frames and stacks for backtraces.
9. Translating bytecode to Lisp for SBCL to compile, with the
   interpreter kept as the reference (below).

## Where it stands

- **Done:**
  - The ELF reader and the decoder, which agree with Guile's
    disassembler on the installed `.go` files.
  - The machine: calls, tail calls, returns, the prologues (optional,
    rest and keyword arguments), unboxed arithmetic, comparisons and
    branches, the heap by words, static data and its relocations,
    intrinsics, prompts, `dynamic-wind`, fluids and dynamic states.
  - Loading images: `load-thunk-from-memory` and `load-thunk-from-file`.
  - Introspection through Guile's own `(system vm debug)`: names,
    arities, docstrings, properties, the writer.
- **Checked:**
  - 102 of the installed `ice-9` and `srfi` modules load from their
    `.go` files.
  - With a module so loaded, its test file gives the same results as
    from source: srfi-1 (1,902 tests), srfi-9, 11, 19, 26, 31, 34, 35,
    37, 41, 43, 45, 60, 64, 67, 69, 171; ice-9 format, getopt-long,
    match, optargs, q, regex, vlist. `GUILE_VM_PRELOAD` in
    tests/run-guile-test.lisp does this.
  - Bytecode from Guile's compiler running here runs on the VM:
    rtl.test and rtl-compilation.test pass.
- **`compile`:** to `value` goes through bytecode, run by the VM, as
  Guile's does (PSEUDOSCHEME_GUILE_COMPILE_VALUE=lisp compiles it with the
  host instead). `eval` and `load` compile to Lisp. Data read from a
  loaded file have source properties (line and column), as Guile's
  reader gives them, so `program-sources` finds them.
- **How the heap ABI is met:**
  - A type's word layout is learned from the compiled code that uses it,
    and from observing a real Guile (a bytevector's header, a symbol's
    hash), not from libguile's sources.
  - Symbol hashes are the installed Guile's, asked of it once per name.
    Compiled `case` dispatches on symbols by hash.
  - Every instruction the installed Guile's 344 `.go` files use has a
    handler; so do the atomic-box instructions, which only `(ice-9
    atomic)` code compiled here uses.
  - Continuations through VM code: `call/cc` and prompts capture the VM
    stack's slices with the runtime's frames (src/continuations.lisp);
    re-entry, generators and composable continuations give what Guile
    gives.
- **Left:**
  - The instructions only libguile's own generated code uses:
    `capture-continuation`, `continuation-call`, `compose-continuation`,
    `subr-call`, `foreign-call`, `halt`, `return-from-interrupt`
    (here `call/cc` and the rest are runtime procedures, which VM code
    calls), and `pointer-set!/immediate`.
  - Re-entering a composable continuation whose prompt's body is a
    nested run of the machine (control.test's "nested prompts" at -O2):
    the body's end comes back as an extent exit, not as the values `k`
    returns.

## Frames and stacks

src/guile/stacks.lisp: `make-stack` and the primitives Guile's own
`(system vm frame)` is built on (`frame-instruction-pointer`,
`frame-local-ref`, `frame-num-locals`, `frame-previous`, `stack-ref`
and the rest), so that its Scheme code runs unchanged. A stack comes
from two places:

- **Bytecode:** the VM's stack, where a run of the machine is on Lisp's
  stack. Frames have their real locals and instruction pointers, so
  names and arguments come from the image's debug information, as in
  Guile.
- **Scheme compiled to Lisp, and primitives:** SBCL's stack, through its
  debugger interface.
  - A procedure's machine (src/continuations.lisp) is named with the
    procedure's name, which the compiler passes in its body as a
    constant that compiles to nothing.
  - Primitives are named by their code.
  - Arguments are the ones SBCL kept, `_` where it didn't.
  - Such a frame's instruction pointer is negative, in no image;
    `primitive-code-name` answers its name.

Two things make the frames Guile's:
- A tail call of `call-with-prompt`, `with-fluid*` or
  `with-dynamic-state` keeps its caller's frame. In Guile they are
  instructions in the caller, and the frame stays while their body runs.
- Their body thunks are left out, being inline in Guile.

Prompts are marked where they are on the stack, for `make-stack`'s
prompt-tag cuts. eval.test's stack tests and continuations.test pass.

## Translation to Lisp

src/guile/vm-jit.lisp. The interpreter counts the jumps to each
instruction; at 20, the region from there to the end of its code (until
an instruction that doesn't fall through, with no jump pending past it)
becomes one Lisp function, compiled by SBCL:

- A `tagbody` with a tag per instruction. Each instruction is its
  handler's own source (`defop` keeps it) with the operands as
  constants, so SBCL folds the decoding away. Falling through, or a
  jump to a constant target in the region, is a `go`.
- Any other target, such as a call's return into the region, comes back
  in through a `case` on the IP. So the function can be entered at any
  instruction it holds, and a call or return within the image stays in
  it.
- Anything else (another image, a return to Lisp, the end of a dynamic
  extent) is returned to the interpreter, which goes on from there.

Since the handlers are the same, so are the semantics, continuations
included. The translation is portable Common Lisp, except in the `vop`
backend (below).

- `PSEUDOSCHEME_GUILE_JIT=0` turns it off (the `interpret` backend).
- `PSEUDOSCHEME_GUILE_JIT_THRESHOLD=0` translates all code run. rtl,
  rtl-compilation, the continuation cases and srfi-1, match and format
  over their `.go` modules all pass so.

### Backends

`PSEUDOSCHEME_GUILE_VM_BACKEND` chooses how bytecode runs:

- **`interpret`:** the interpreter alone, the reference.
- **`jit`:** the default. Regions that run often are translated as
  above.
- **`aot`:** every region of an image is translated as the image loads.
  The translation is written as a Lisp file and compiled with
  `compile-file` into a fasl kept under the cache directory, keyed by the
  image's contents. An installed module's code is translated once (srfi-1:
  58 s the first time, then loaded).
- **`vop`:** as `jit`, but SBCL-specific (vm-vops.lisp; SBCL on arm64,
  the feature `:pseudoscheme-vops`; elsewhere `vop` is `jit`):
  - regions are compiled at `(safety 0)`, so the VM stack's slots are
    read and written unchecked;
  - the stack and stack pointer are kept in variables of the region's
    function. Instructions that only read and write slots use them; after
    any other instruction, or a call out that might run the VM again,
    they are loaded again;
  - a comparison and the conditional jump after it are one branch, and
    jumps, `call-label`, `tail-call-label` and returns into the region
    go straight to their targets (a call's return address is a constant);
    frame sizing and the call frame are inline;
  - most instructions (135 of Guile's 169) are translated inline, by
    VOPs:
    - VOPs written for arm64 here, after the manner of pvk.ca's "SBCL:
      the ultimate assembly code breadboard". They work on the tagged
      words themselves and give NIL for the general case (the intrinsic,
      or the handler's code):
      - Scheme fixnums: `add`, `sub` (also with an immediate, whose
        tagged word is the instruction's operand), `mul` (`mul`/`smulh`),
        `quo`, `rem` and `mod` (`sdiv`, `msub`), `logand`, `logior`,
        `logxor`, `logsub`, and `lsh`/`rsh` with or without an immediate;
      - unboxed u64s that are fixnums (the sign and tag bits clear, one
        `tst`): `uadd`, `usub`, `umul`, `ulogand`, `ulogior`,
        `ulogxor`, `ulogsub`, `ulsh`, `ursh`, `srsh` and their immediate
        forms;
      - predicates, as branches: both fixnums, a u64 that is a fixnum,
        Guile's 62-bit fixnum (`fixnum?`), and an index into a simple
        vector (its lowtag, widetag and length tested together), which
        guard the comparisons, the conversions (`untag-fixnum`,
        `scm->u64`, ...) and `scm-ref`/`scm-set!` on vectors.
    - SBCL's own VOPs, by typed Lisp: pairs' `car`/`cdr`, flonum
      arithmetic, characters, the type tests (`char?`, `pair?`,
      `vector?`, the constants), and raw memory (`u8-ref` through
      `f64-set!`, a `sap-ref` into a simple octet vector, in bounds).
    - Constants (`load-u64`, `load-f64`, `load-label`, and
      `make-non-immediate` once decoded) are decoded when translating;
      `jtable` is a `case`; `push`, `pop`, `drop`, `receive` and
      `bind-optionals` size the frame inline.
    The rest (calls, returns to Lisp, prompts, throws, keyword arguments)
    run their handlers.

  `tests/guile-vm/vops.lisp` tests each VOP against Lisp's arithmetic,
  open-coded and called.

All the backends translate the same handler sources, and all four pass
rtl, rtl-compilation, compiler, numbers, srfi-1 over its `.go`, and the
continuation cases, with every region translated.

`sh tests/guile-vm/bench.sh` compares them (best of 3, milliseconds,
on arm64):

| | interpret | jit | aot | vop | Guile 3.0.11 VM | Guile JIT |
|---|---|---|---|---|---|---|
| fib 30 | 555 | 68 | 69 | 22 | 60 | 60 |
| loop 10^6 | 98 | 10.5 | 10.0 | 2.1 | 11.9 | 11.9 |
| tak 24 16 8 | 451 | 52 | 52 | 21 | 49 | 49 |
| vector sum 10^6 | 290 | 29.4 | 27.5 | 9.3 | 22.1 | 24.8 |

What made translated code fast:
- compiling it at speed 2, so that the slot accessors are inlined;
- decoding constant immediates when translating;
- making one code pointer per address (a call's return address was
  allocated on every call);
- frame sizing on fixnums, inlined.

In `vop`, keeping the stack in variables, branching straight from
comparisons and calls within the region, and the VOPs are the gain
(fib 30 was 54 ms with only the `add` and `sub` VOPs).
