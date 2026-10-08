# Session summary: 2026-10-08, starting an SBCL runtime for KBE

## Goal

Run the untested LLM transcription of the January 1985 KBE / KBEmacs listings
(`code/volume_1..4/`, Zetalisp for the Symbolics 3600) under SBCL 2.2.6.

The target is to reproduce the 14 December 1984 self-test in
`code/volume_1/358-test.lisp`, starting with the SQRT sequence (`MYSQRT`,
`MYOTHERSQRT`). After that, the other five sequences, including the three that
*failed* in 1984 (BUCKET, DELETE, MEMQ), as a fidelity check.

## Agreed approach

- **Flavors:** vendor an existing Common Lisp implementation (Spice Lisp
  Flavors) and extend it, rather than writing a CLOS shim.
- **Sources:** load the transcriptions as-is through a Zetalisp
  compatibility layer, staying as close to the original as possible.
  - Typo-level fixes to the transcriptions are allowed, checked against the
    PDF scans.
  - Lisp Machine–specific workarounds go in the runtime, not the sources.
- **Scope:** the headless core only; no ZWEI, TV, windows, actors or Ada. The
  self-test driver (`volume_4/test.lisp`) runs each command with
  `(eval-command (parse-command str))`, then `equal`-compares
  `(code-for-defun 'FN)` against the expected code.

## What exists now (`runtime/`)

| File | Purpose |
| --- | --- |
| `flavors/` | Vendored [g000001/spice-flavors](https://github.com/g000001/spice-flavors) @ `42dd072`; see `PROVENANCE.md` |
| `lets/` | Vendored [g000001/LetS](https://github.com/g000001/LetS) @ `5675bac`, a CL port of Waters's `lets.rcw1` from ITS; see `PROVENANCE.md` |
| `packages.lisp` | Packages: `ZL` (alias `GLOBAL`), `KBE`, and stand-ins `SYS SI TV FS TIME PP ZWEI DBG COMPILER CHAOS` |
| `zl-compat.lisp` | The `DEFUN` variants (`(defun (sym prop) ...)`, `fexpr`, `macro`); destructuring `LET`; old-style `DO`; multi-value `RETURN`; Zetalisp list, symbol, string and arithmetic functions; locatives (`LOCF`); `%POINTER`; stream-variable aliases (`standard-output`, etc.) |
| `flavors-ext.lisp` | `:case` and `:otherwise` method combination, `SEND` to functions and streams, flavor-aware `TYPEP`, Lisp Machine `DEFFLAVOR` options |
| `zl-conditions.lisp` | The Symbolics flavor-based condition system: `signal`/`error` on flavor names, `condition-bind` proceeding, `condition-case`, `ferror` |
| `zl-reader.lisp` | Zetalisp readtable: `/` escapes, `#/c`, `#\` Lisp Machine names, `#+3600`, and `set-syntax-macro-char` and friends |
| `lispm-stubs.lisp` | Stubs for `PP:`, `TIME:` and others |
| `load.lisp` | Loads everything. `(load-kbe)` reads the core files in sysdef order, form by form, and records every failing form in `*kbe-load-errors*`; compiler diagnostics go to `(kbe-compiler-log)` |

To try it:

```
sbcl --no-userinit --load runtime/load.lisp --eval '(load-kbe)'
```

## Findings worth remembering

- **The core is nearly Flavors-free.** The DB, analyzer and coder use none.
  Verb dispatch does: `send-delegate :interpret` goes to a vocabulary flavor
  with `:case` combination (`pseudo.lisp`, `refer.lisp`), so `:case` support
  was essential.
- **The missing `LMLIB; LETS` is Waters's LetS** (`letS`, `Elist`, `Reqset`,
  `Rand-fast`, ...). About 80 uses, mostly in `analyze` and `coder`.
  `g000001/zrseries` has a LetS-on-SERIES layer, but it was rejected: it is
  incomplete (no `Rand-fast` or `Ror`) and its `Reqset` returns elements in
  hash-table order, which could change code-generation order.
- **Spice Flavors bugs fixed:** instance-variable defaults from *component*
  flavors were never applied. There were two separate causes:
  - `:progn` combination without daemons called only the first method;
  - `INTERNAL-INIT` used an unmapped slot index.
- **`%pointer` matters.** `sets.lisp` keeps "fsets" sorted by machine
  address, which on the 3600 with the GC off follows allocation order. It is
  emulated with an eq-table of serial numbers, assigned at creation for
  symbols made by `make-symbol`, `gensym` or `intern`.
- **Characters:** `#/a` is read as a CL character, so ZL `=`, `<`, `>` and the
  other comparisons accept characters; `pseudo.lisp`'s tokenizer does
  `(>= c #/a)`.
- **CL symbols KBE redefines** (`find`, `string-capitalize`, `warn`,
  `define-condition`) are shadowed in `KBE`.

## Status at the end of the session

The runtime loads cleanly. `(load-kbe)` reports **171 load errors**, mostly
cascades from a few causes:

1. **`...` as a symbol.** Zetalisp reads `...` as a symbol; the SBCL reader
   says "too many dots". This stops `comment-macro.lisp` before it installs
   the `{...}` reader, so `syslib`, `kbelib` and `demo-lib` then fail. KBE
   also reads `(...)` at run time, so this needs a reader-level fix: probably
   a custom `(` reader plus a `.` macro character, or a hook into SBCL's
   reader.
2. **Toplevel Maclisp `(declare (special ...))`** (about 30 forms). The
   loader should turn these into `declaim`.
3. **Missing Lisp Machine facilities:** `define-user-option-alist`,
   `define-user-option` (used by `kmac` options), `working-storage-area`,
   `default-cons-area`, `fs:define-canonical-type`, `compiler-let`. These need
   stubs.
4. **Probable transcription errors** to verify against the PDF scans and fix:
   - `'',form` should be `` `',form `` (a backquote read as a quote) in
     `surface.lisp:231`, `primop.lisp:187` and `pseudo.lisp:254`;
   - `*: ignore` should be `':ignore` in `verbs.lisp:619`;
   - a stray `:` in `refer.lisp:231`.

## Next steps

1. Fix the `...` reading, toplevel `declare`, and the stubs; reload; iterate.
2. Make the transcription typo fixes, citing PDF pages.
3. Build `runtime/demo/sqrt.kbe` and `sqrt.lisp` from `358-test.lisp`, plus a
   headless driver that reuses `run-KBE-test`.
4. Get SQRT passing, then the other sequences.
