# Chez Scheme's mats

These files are Chez Scheme's own test suite, the `mats/` directory of
[cisco/ChezScheme](https://github.com/cisco/ChezScheme) at commit
`e3b13636820b13e0feb82b2fb944b60fab4b6e80` (8 October 2026), unmodified:
the `.ms` files, `mat.ss`, and the data and helper files they load
(`ht.ss`, `oop.ss`, `thread-check.ss`, `freq.in`, `freq.out`,
`m4test.in`, `m4test.out`). They are Cisco Systems' and the Chez Scheme
authors', under the Apache License, version 2.0 (`LICENSE`, `NOTICE`).

They measure the Chez mode (src/chez/, docs/chez.md): `make test-chez`
runs each `.ms` file in its own process (tests/run-chez-mats.sh), with
`tests/chez-mat.ss` in place of `mat.ss`, and counts each `mat` clause:
pass, fail, error, unbound (a name not bound here), unsup (needs Chez's
compiler, its inspector or a `#%$` system primitive), crash (killed the
process or ran past the time limit).
