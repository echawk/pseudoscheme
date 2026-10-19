# A Linux container for Quasischeme

GNU Guix doesn't run on macOS, and our libguile (docs/libguile.md) needs
a real Guile 3.0.11 to compare with. This directory sets up a
Linux/aarch64 container with podman:

```sh
sh utils/container/setup.sh                       # once: the machine (macOS), the image, the volumes
sh utils/container/run.sh test 'make test'        # run something in it
sh utils/container/teardown.sh                    # remove it all
```

- **setup.sh**:
  - on macOS, makes a podman machine of its own, `quasischeme`: rootful,
    6 CPUs, 12 GB of memory and 60 GB of disk (`MACHINE_CPUS`,
    `MACHINE_MEMORY`, `MACHINE_DISK`). Your default machine isn't
    touched. Only one machine runs at a time, so stop another first;
  - builds the image from the Containerfile;
  - makes the volumes.

  The first build takes a while: SBCL, then Guile 3.0.11, are compiled
  from source.
- **run.sh NAME COMMAND**:
  - copies the repository (the files Git knows of) to `/work` in the
    container, and runs COMMAND there under `sh -c`;
  - the output goes to the terminal and to
    `build/container/logs/NAME.log`;
  - what COMMAND writes to `/logs` lands in `build/container/logs/NAME/`;
  - `LIMIT=SECONDS` kills it after that long;
  - one run at a time, since runs share `/work` and the compiled files.
- **teardown.sh**:
  - removes the volumes, the image and, on macOS, the machine;
    `KEEP_MACHINE=1` keeps the machine;
  - the logs stay.

What's in the image (Containerfile), every download pinned by its
sha256:

| | where | for |
|---|---|---|
| SBCL 2.6.9, from source, with `libsbcl.so` | /usr/local | building and running Quasischeme; embedding (docs/libguile.md) |
| Quicklisp and the build's Lisp libraries | /root/quicklisp | builds without the network |
| GNU Guile 3.0.11's sources | /opt/src/guile-3.0.11 | `--guile` loads its Scheme (`GUILE_SOURCE_DIR`) |
| GNU Guile 3.0.11, built | /opt/guile | the reference for our libguile's tests |
| GNU Guix 1.5.0, from its binary tarball | /gnu/store, /var/guix | `guix-daemon`, and Guix's modules and their Guile libraries |
| Guix 1.5.0's source tarball | /opt/src/guix-1.5.0 | its test suite |

The base is Debian, not Alpine:
- Guix's libraries in the store are linked against glibc, and `--guile`
  loads them into SBCL's process.
- Debian no longer packages Guix, which is why it comes from Guix's own
  tarball.

The volumes:
- `/work` and `/root/.cache` (the compiled files) stay between runs;
- so do Guix's `/gnu` and `/var/guix`.

## What runs in it

- `make test` and `make test-cli`: the same results as on macOS.
- tests/libguile/run.sh: our libguile against real Guile.
- tests/guix/run.sh: Guix on `--guile`. Each step logs to
  `build/container/logs/guix/`:
  - `reference`: real Guix's results;
  - `modules`: each Guix module loaded;
  - `derivation`: `guix build -d hello`, compared with real Guix's;
  - `hello`: a build;
  - `tests`: Guix's own test suite, on both.
