# Session summary: 2026-10-10, getting the cliche libraries to load

Continues [2026-10-08-sbcl-runtime-start.md](2026-10-08-sbcl-runtime-start.md), which covers the goal, the agreed approach and the runtime's layout.

## Part 1: ARGLIST and SI:DECLARED-DEFINITION

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

## Part 2: toward a clean load of the libraries

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

---

## Part 3: the syslib clichés build plans

### Fixes

- **The hang was `boole`, not `&quote`.** Maclisp/Zetalisp `(boole k x y)`
  takes a **PDP-10 Boolean-instruction code** (SETZ, AND, ANDCA, SETM,
  ANDCM, ...): `(boole 4 x y)` is ANDCM, *x and not y*. That is set
  difference in `dflow.lisp` `6set-` (and in `coder.lisp`). In CL, 4 is
  `boole-c1` (*not x*), so the live-variable fixed point in `6trans-live`
  never converged. ZL now shadows `BOOLE` with the PDP-10 table.
- **The `&quote` / `fexpr` rewrite works:** `plan-def` gets its arguments
  unevaluated, which fixed the `NON-NULL` errors.
- **The loader prints with `*print-circle*`,** since DB facts contain
  circular context markers. An error message printing one had overflowed
  the stack.
- **Transcription:** `analyze.lisp:1600` repeated the line above it;
  per the scan (vol 2 p121) it is `(+& newbodycase segtype 'simple)`. The
  duplicate had produced a duplicate SEGTYPE fact.
- **Progress log (the user's idea):** `load-kbe` appends "time file form N"
  to `runtime/build/progress.log` as each form starts, flushed. Use
  `tail -f` on it to tell looping from working.

### Status

- All five `syslib` clichés (zeroary/unary/binary-function, simple-subseg,
  MAP) now load: `define-cliche` builds their plans.
- `kbelib` form 1 (USE) loads.
- **`kbelib` form 2, SUCCESSIVE-APPROXIMATION (the SQRT cliché), runs
  without finishing:** either another non-terminating loop or a very slow
  one. That is next.
- Note: `*debug*` is T during load, because the system initializations
  (`initialize-system`) haven't run, so `cbarf` signals continuable
  errors. The harness should call `(initialize-system)`.

### Next

1. Find why SUCCESSIVE-APPROXIMATION doesn't finish: use the interrupt
   backtrace technique (timer plus `interrupt-thread`; see scratch
   `dbg-hang.lisp` in the session, or rebuild it) and check for another
   Zetalisp semantic mismatch or transcription error.
2. Finish loading `kbelib` and `demo-lib`.
3. Build the SQRT harness.

---

## Part 4: SUCCESSIVE-APPROXIMATION loads; 8 load errors left

### What was wrong

SUCCESSIVE-APPROXIMATION looped forever in `analyze.lisp`'s data-flow
simplifier. `3dflow-simp1`/`3dflow-simp2` kept proposing the same move of
INITIAL-APPROXIMATION into TC (an SSR), about 400,000 times in 5 seconds,
and `3do-dflow-move` never performed it.

Cause: **another compensating pair of paren errors** in `3do-dflow-move`
(vol 2 p106):

- `(& newseg)))))))` had 7 closing parens where the scan has 8;
- `seg path)))))))))` had 9 where the scan has 8.

The file still balanced, but the `(t ;(not nterm) ...)` clause had become
part of the `nterm` branch. So moving a segment into a non-terminal (an SSR)
silently did nothing. The scan settled it; it was also visible with a
small Python paren-nesting check that lists the open parens enclosing a
given line.

(A detour: I first suspected LetS `generateS`, which really is unbounded.
But the function ends its loop with LetS `(done)` in the last-step
branch, exactly as LetS intends. No LetS problem.)

### Status: `(load-kbe)` takes seconds and reports 8 load errors

All of these are clichés SQRT does not use:

- **"No facts found when 1 expected"** for pattern `(TCBODYn . ~*) DFLOW ~*
  NIL`: TRAILING-POINTER-LIST-ENUMERATION,
  TRAILING-POINTER-SUBLIST-ENUMERATION, ENUMERATION, LIST-ENUMERATION,
  SUBLIST-ENUMERATION, VECTOR-ENUMERATION (kbelib) and SIMPLE-REPORT
  (demo-lib). One shared cause is likely: a data-flow arc into a temporal
  composition body. It could be another paren slip in analyze.lisp's TC
  code, or a runtime semantic difference.
- **LINEAR-SEARCH**, inside `(compiler-let ((*separate-gens-and-terms*
  t)) ...)` in kbelib: compile error.

The SQRT clichés SUCCESSIVE-APPROXIMATION, AVERAGE, SQUARE and
EQUALITY-WITHIN-EPSILON load, as do USE and the syslib clichés.

### Questions for a Zetalisp / KBEmacs expert

These are the places where the runtime *assumes* a Lisp Machine behavior
(Symbolics Release 4.5, KBE 358, December 1984). Each was inferred from
how the KBE code uses it; confirmation or correction would be valuable.

1. **`(listp nil)`.** We made Zetalisp `listp` mean `consp`, so
   `(listp nil)` is NIL. `fix-up-semi-kbe-/#-comment-macros`
   (comment-macro.lisp) recursed forever otherwise. Was `listp` really
   `consp` in Release 4.5, or did the code rely on something else?
2. **Characters.** We read `#/a` as a character object, and made `=`, `<`,
   `+`, `-` and so on accept characters as their codes; `pseudo.lisp`'s
   tokenizer does `(>= c #/a)` and gram uses `#.(1- #/a)`. In Release 4.5,
   were characters fixnums? If so, is there any place where KBE would treat
   a character and its code differently (e.g. `eq` against a number)?
3. **`%pointer` and fset order.** `sets.lisp` keeps "fsets" sorted by
   object address. We emulate addresses with creation-order serial numbers
   (symbols numbered when created by `make-symbol`/`gensym`/`intern`).
   Does the order of plan elements, and so of generated code, depend on
   address order in a way this might not reproduce?
4. **`boole`.** We use the Maclisp/PDP-10 instruction-code table
   (`(boole 4 x y)` is ANDCM, x and not y). Right for the LispM?
5. **`map`** applies the function to successive tails (like CL `mapl`).
   Right?
6. **Macros in analyzed code.** The analyzer expands macros it has no
   `k*expander` for, via `si:declared-definition`. We supply Lisp Machine
   expansions for `if`, `when`, `unless`, `push`, `pop`, `incf`, `decf`,
   `dolist`, `dotimes`, `caseq` and `selectq` (e.g. `if` becomes `cond`).
   Is that what Release 4.5 produced, especially for `if`, which the SQRT
   clichés use?
7. **The missing LMLIB libraries.** KBE's sysdef loads `LMLIB; LETS`,
   `LMLIB; PP`, `LMLIB; GPRINT` and `LISPM2; LET`. We used the ITS LetS
   (`lets.rcw1`, Waters) and stubbed PP and GPRINT. Do copies of the
   1984 LMLIB LETS and PP survive anywhere?
8. **`primop.lisp` `-> sexpr.`** The scan shows a dot after `sexpr` in
   K*PROG's `over`; we treat it as a printout mark. (Agreed with the user;
   just noting it.)

### Next

1. Optionally fix the 8 remaining load errors (they don't block SQRT).
2. Build the SQRT harness:
   - extract the commands and expected code from `358-test.lisp` into
     `runtime/demo/`;
   - call `(initialize-system)`;
   - set up a headless `*delegate*`;
   - drive `volume_4/test.lisp`'s `run-KBE-test`, logging progress to a
     file.

---

## Part 5: the SQRT harness runs; command 1 passes, command 2 reaches the coder

New files:

- `runtime/run-test.lisp`: the headless driver. Run it with
  `KBE_BACKTRACE=1 sbcl --script runtime/run-test.lisp sqrt`.
- `runtime/demo/sqrt.kbe` and `sqrt.lisp`: the inputs and expected code,
  from 358-test.
- `runtime/patches.lisp`: a headless `*delegate*` flavor.

Runtime fixes:

- Parameters named `standard-input` (and the other aliases) now bind the
  CL variable; `read-command` relies on this.
- `minusp`, `plusp`, `zerop`, `max`, `min` and `fixp` accept characters.
- `tyipeek` honors its peek-type and returns -1 at end of file.
- `current-process`, `process-lock` and `process-unlock` work.

**Evidence that characters were fixnums in Release 4.5** (expert question
2): the tokenizer tests `(minusp (tyi s -1))`, and `in-chars?` recognizes
characters with `fixp`.

Transcription fixes (in `TRANSCRIPTION-FIXES.md`):

- `pseudo.lisp` `'(,(car bvl))`: a quote misread as a backquote.
- `pseudo.lisp` `define-verb-action`: the line had been copied from
  `define-verb-unparse`, so every verb's arguments were shifted by one.
- `verbs.lisp` `seg1ype` should be `segtype`.

Status:

- "Reinitialize database." **works**.
- "Define a program MYSQRT with a parameter NUM." parses, dispatches, and
  builds the program. It fails in the **coder** (`5init-order`) on the
  undefined function `*ARRAY`, a Maclisp array constructor that needs a
  runtime definition.

Note: deferred compile errors (like `seg1ype`) are *not* caught by the
loader's "caught ERROR" check. Grepping for odd symbols, or having the
loader record SBCL compiler errors properly, would help.

Next: define `*array`, then continue through the SQRT commands.

---

## Part 6: THE SQRT SEQUENCE PASSES

`sbcl --script runtime/run-test.lisp sqrt` runs all 14 commands from the
14 December 1984 self-test. The `MYSQRT` and `MYOTHERSQRT` that KBE
generates are `equal` to the code in `358-test.lisp`:

```lisp
(DEFUN MYSQRT (NUM)
  (LET* ((APPROXIMATION 1))
    (LOOP DO
      (IF (< (ABS (- (* APPROXIMATION APPROXIMATION) NUM)) 0.0001) (RETURN APPROXIMATION))
      (SETQ APPROXIMATION (// (+ APPROXIMATION (// NUM APPROXIMATION)) 2.0)))))
```

The generated code also runs: `(mysqrt 2.0)` gives 1.4142157 and
`(mysqrt 9)` gives 3.0. As a side effect, `(load-kbe)` now loads the whole
core with **0 load errors**.

### Fixes in this part

**Runtime:**

- `*array` and `adjust-array-size` (Maclisp arrays, adjustable in place).
- `get` on a list scans pairs leniently, as on the Lisp Machine.
- Named `prog`: `(prog outer () ...)`.
- `string-parser` and `parse-error`: the Lisp path of volume_4/parse.lisp,
  in patches.lisp. Per the user, only Lisp generation matters.
- `*input-language*` (from parse.lisp).
- `volume_4/focus.lisp` is loaded (code<->path functions).
- KBE shadows `PARSE-ERROR`.
- The driver binds the ZL readtable while commands run, so typed code like
  `(// NUM APPROXIMATION)` reads `//` as the symbol `/`, as on the Lisp
  Machine. That was the last mismatch.
- The driver catches stack overflow. `KBE_TRACE=fn,...` traces functions;
  `KBE_NO_RUN=1` loads without running.

**LetS port bug** (vendored g000001/LetS): `(done x)` dropped its value; a
`cdr` was carried over from the original. KBE's `6fast-proto-name` picks
fresh argument names with `(done let)`, so plans had ports with **no
name**, `kill-port` then recursed forever, and the 8 remaining cliché load
errors came from the same cause.

**Transcription** (in `TRANSCRIPTION-FIXES.md`):

- **coder.lisp: two compensating pairs of paren errors.** Lines 543 and
  786 were each one short; line 1213 had two extra. As a result, about 30
  defuns, including `5annotate`, were nested inside earlier defuns and
  never defined.
  - A Python check now confirms every `(def` form in the core files is at
    top level (`depth.py` in the session scratchpad; worth adding to the
    repo as a tool).
- `implement.lisp`: `&to` and `&from` should be `&~to` and `&~from`;
  `(&  (&* seg) port)` should be `(&* (&* seg) port)`, a dropped `*`.
- `coder.lisp`: `tnen` should be `then` (by symmetry; the scan is not yet
  checked).

### Still open

- `implement.lisp:1366` has `(&  (car to-ancesters))`, another double
  space where a `*` may have been dropped. It is not on the SQRT path;
  check the scan.
- Run the other five 358-test sequences (BUCKET, DELETE, HASH, MEMQ,
  SUMLIST). Fidelity target: HASH and SUMLIST should pass, and BUCKET,
  DELETE and MEMQ should fail *with the same wrong code as in 1984*.
