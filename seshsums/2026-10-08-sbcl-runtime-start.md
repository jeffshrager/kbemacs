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

---

## Second session: load errors from 171 to 30

### Runtime changes

- **Reader:**
  - `...` reads as a symbol. ZL now has its own `(` list reader, which
    handles the consing dot itself, and `.` is a non-terminating macro
    character.
  - The `#{` setter now has its correct name, `|SET-SYNTAX-#-MACRO-CHAR|`.
- **Loader:**
  - Toplevel forms are processed the way a compiler does: KBE and ZL macros
    are expanded and `progn` subforms are handled one at a time. Toplevel
    `(declare (special ...))` proclaims; other toplevel declarations are
    dropped.
  - SBCL compile-time errors are recorded as load errors.
- **Zetalisp semantics found, each confirmed by how the KBE code depends
  on it:**
  - `(listp nil)` is false: `listp` means `consp`. Without this,
    `fix-up-semi-kbe-/#-comment-macros` recursed forever on `NIL`.
  - `map` is Maclisp's `mapl` (it maps over tails); `&*` relies on this.
  - Arithmetic on characters (`#.(1- #/a)`) works on their codes.
  - A Lisp Machine `defstruct` constructor is a macro taking slot/value
    pairs.
  - `setf` accepts `#'name` places.
  - A macro's function cell holds `(macro . fn)`, for `fsymeval`/`fset`.
  - `rest1` through `rest4`.
- **Packages:**
  - `intern` into a locked CL package (from `(symbol-package 'keyword)`)
    goes to KBE instead.
  - KBE shadows `TYPE`, which it declares special.
- **Stubs:** `define-user-option(-alist)`, storage areas, `compiler-let`,
  `fs:define-canonical-type`, `define-editor-definition-type`.

### Transcription fixes

Ten typo-level fixes, all checked against the scans and logged in
`code/TRANSCRIPTION-FIXES.md`:

- backquotes misread as quotes (`'',` should be `` `', ``);
- `{ ... ]` should be `[ ... ]` in `refer.lisp`;
- `*: ignore` should be `':ignore`;
- a stray `:`;
- a doc string dropped at a page break in `kmac.lisp`.

To render scan pages: `gs -sDEVICE=pnggray -r110 -dFirstPage=N
-dLastPage=N`; the PDFs are too large for direct PDF reading.

### Status

Everything through `volume_3/implement` and `deflib` loads without
errors. **30 load errors** remain, all in the cliché libraries (`syslib`,
`kbelib`, `demo-lib`): `define-cliche` now runs deep into the analyzer and
fails on two Lisp Machine introspection functions, `ZL:ARGLIST` (18) and
`SI:DECLARED-DEFINITION` (11).

### Next

1. Implement `arglist` and `si:declared-definition` (check how the
   analyzer uses them), then reload.
2. Once the libraries load, build the SQRT test inputs from
   `358-test.lisp` and the headless driver.

---

## Third session (2026-10-10): ARGLIST and SI:DECLARED-DEFINITION

New file `runtime/lispm-functions.lisp`:

- **`arglist`** returns the lambda list of a lambda expression
  (`lambda`, `named-lambda`, `subst`) or of a function.
- **`si:declared-definition`** returns the function cell, which for a macro
  is `(macro . expander)`. The analyzer (`expand.lisp`
  `macroexpand-1-no-substs`) uses it to expand macros in the code it
  analyzes.
  - Fidelity point: `expand-special-forms` checks KBE's own `k*expander`s
    first (`cond`, `and`, `let`, `do`, `setf`, `loop`, ...). But **`if`** has
    none, and neither do `when`, `unless`, `push`, `pop`, `incf`, `decf`,
    `dolist`, `dotimes`, `caseq` and `selectq`. On the Lisp Machine these
    were macros expanding to `cond`/`setq`/`do`; SBCL's expansions use
    internals KBE doesn't know.
  - So `si:declared-definition` supplies **Lisp Machine–style expansions**
    for them, e.g. `(if a b c)` becomes `(cond (a b) (t c))`.
  - It warns once for any other CL macro that reaches it, so a gap can't
    fail silently.
- **`si:open-code-p`** returns NIL; there are no substs here.

### Status

Those 29 errors are gone. 30 load errors remain, all still in the cliché
libraries, and now with a deeper cause:

- **`DTP-LIST`, `%MAKE-POINTER-OFFSET`, `NCONS-IN-AREA`**: the DB's "hunk"
  representation (`dmac.lisp` around line 400) indexes into a block of conses
  by machine-pointer arithmetic, and allocates in storage areas. This needs a
  faithful emulation of what a dbhunk is and how it's accessed.
- One `compiler-let` form in `kbelib` (LINEAR-SEARCH), still to be
  investigated.

### Next

Emulate dbhunks (read `make-dbhunk` and every access first), then retry
the libraries.

---

## Fourth session (2026-10-10): toward a clean load of the libraries

### Runtime changes

- **DB hunks:** `(car (%make-pointer-offset dtp-list hunk (1+ n)))` reads
  element n of a cdr-coded list, so `%make-pointer-offset` is `nthcdr`
  (still SETF-able). Areas are integers (all area 0); `%area-number` is 0
  for a cons and -1 otherwise; `ncons-in-area` and `xcons-in-area` added.
- **SETF** consults the Lisp Machine `:setf` property, which KBE's
  `define-setf-handler` (macro.lisp) uses for destructuring assignment,
  e.g. `(setf [a b] x)`.
- **Exports:** ZL now exports only CL and library externals, defined
  symbols, the stream and printer aliases, and a list of shared data
  symbols (`macro`, `fexpr`, `&quote`, ...). Exporting incidental symbols
  such as the `loop` keyword `FROM` had made `factdef` create `ZL::~FROM`.
- **Zetalisp `defstruct`:** the default `:conc-name` is empty (accessors
  are `(end hunk)`).
- **Added:** `desetq`.
- **Loader:** reader warnings, such as "unmatched close parenthesis", are
  now recorded as load errors.
- **`&quote` arguments and `fexpr`s (written; NOT YET VERIFIED):**
  `(defun plan-def (&quote &rest body) ...)` takes its arguments
  unevaluated. The old `fexpr` translation was also wrong: it ran the body
  at macroexpansion time. Both now compile to an ordinary function plus a
  macro that quotes the `&quote` arguments.
  - **The first load after this change spun at 100% CPU and was killed.**
    The next step is to find out why: likely an expansion loop in the new
    macro, or `plan-def`/`expand-facts` now actually running. Run it with a
    timeout and check `load-raw.txt`-style output for the last file
    loading.

### Transcription fixes

All checked against the scans:

- `primop.lisp`: `-> sexpr.` should be `-> sexpr`. The scan shows a dot,
  but it is the only trailing dot in the core, and it would break K*PROG,
  which runs for every program; SQRT passed in 1984. The user agrees it is
  spurious.
- `analyze.lisp` `3fix-slot-assertions`: **two compensating paren
  errors** (one extra `)` at `ssrs`, one missing after `(t 'trn)`). The
  file still balanced, so only the scan showed them.
  - Lesson: compensating paren errors are silent. A worthwhile audit would
    re-indent each form by its structure and compare with the
    transcription's indentation, which follows the scan.

### Status before the `&quote` change

19 load errors, all in the cliché libraries: `NON-NULL` unbound (10; the
`&quote` problem above) and `K*INFINITE-LOOP-PSEUDO-EXIT` unbound (8;
probably the same `plan-def` cause), plus one `compiler-let` form in
kbelib.

### Next

1. Debug the hang after the `&quote` change.
2. Get the libraries loading with zero errors.
3. Build the headless SQRT harness and run the first command.
