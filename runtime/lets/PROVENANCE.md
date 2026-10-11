# Vendored LetS (stand-in for the missing `KBE: LMLIB; LETS` module)

`volume_1/sysdef.lisp` loads `LMLIB; LETS` before any KBE source. Those
listings were not printed in the KBE volumes. The KBE code uses Richard C.
Waters's LetS expressional loop notation (MIT AIM-680, 1982): `letS`, `letS*`,
`Elist`, `Rlist`, `Reqset`, `Rand-fast`, `enumerateS`, `generateS`, and
related forms.

Source: https://github.com/g000001/LetS at commit
`5675bace00c7a2d035eb3ff9f3e6ae8376634c08`. It is a Common Lisp port, with
minimal changes, of `src/libdoc/lets.rcw1` from the ITS sources
(https://github.com/PDP-10/its). `lets.rcw1` is the original Maclisp/LispM
text, kept for reference. `lets.lisp`, `package.lisp`, and `utils.lisp` are
what gets loaded. `tests.lisp` is the port's RT test suite: all 55 tests pass
under SBCL 2.2.6.

Local change (marked `KBE:` in `lets.lisp`): `(done x)` must exit the loop
returning x, as in `lets.rcw1`. The port's `&rest` rewrite of `done` kept a
`cdr` from the original, so the value was dropped; KBE's
`6fast-proto-name` (primop.lisp) relies on `(done let)` to pick a fresh
argument name, and without it plans got ports with no name.

Caveat: `lets.rcw1` is the ITS libdoc version, which may predate the LispM
copy used by KBE in 1984.
