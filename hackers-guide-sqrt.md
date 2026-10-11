# A Hacker's Guide to KBEmacs: A Day in the Life of SQRT

Copyright © 2026 Jeff Shrager (<jshrager@gmail.com>). All rights
reserved. (See copyright details at the end of this document.)

*Following one program through MIT's Knowledge-Based Editor, from typed English to generated Lisp, as it runs again under SBCL in 2026*

---

*Warning*: Large parts of this document were created using AI
assistance. *Doveryai, No Proveryai!*

---

## Before You Arrive

On 14 December 1984, KBE 358.0x tested itself on a Symbolics Lisp Machine. One of
its test sequences, SQRT, asked the system to build a square-root routine out of
algorithmic clichés. That run is recorded in
[`code/volume_1/358-test.lisp`](code/volume_1/358-test.lisp). The listings of
the system that ran it survive as scans; their transcriptions are in `code/`. With
the runtime in [`runtime/`](runtime/), the same fourteen commands can be
replayed today and produce the same code, character for character.

This guide follows that run command by command. At each step it shows what the
programmer typed, what KBE parsed, which code ran, and what KBE then knew about
the program. Everything quoted below comes from tracing the real system, not
from a reconstruction.

This is the long form of the **A Day in the Life** section of the companion source tour, [A Hacker's Guide to KBEmacs](hackers-guide.md). Read that one for the architecture: plans, clichés, the analyzer and the coder, as Waters and Rich describe them. This guide is the worked example.

---

## Practical Notes: Running It Yourself

```sh
sbcl --script runtime/run-test.lisp sqrt
```

That loads the runtime and the KBE sources, replays
[`runtime/demo/sqrt.kbe`](runtime/demo/sqrt.kbe), and compares the result
with [`runtime/demo/sqrt.lisp`](runtime/demo/sqrt.lisp). The last lines
should be:

```
;;; MYSQRT: matches
;;; MYOTHERSQRT: matches
;;; sqrt sequence was successful.
```

Useful switches:

| Variable | Effect |
| --- | --- |
| `KBE_BACKTRACE=1` | print a backtrace on the first error in a command |
| `KBE_TRACE=fn1,fn2` | `trace` those KBE functions (output can be huge) |
| `KBE_NO_RUN=1` | load everything but don't run, for your own scripts |

While anything runs, `runtime/build/progress.log` gets a flushed line per form
loaded and per command, so `tail -f` tells you whether a slow run is working or
stuck.

### What gets loaded

KBE was written in Zetalisp for the 3600. The runtime makes SBCL look enough
like that machine to run the sources as transcribed:

- `runtime/flavors/`: Spice Lisp Flavors (vendored; two bugs fixed), plus
  `flavors-ext.lisp` for the Lisp Machine's `:case` method combination,
  which verb dispatch depends on.
- `runtime/lets/`: Dick Waters's LetS loop notation (ITS `lets.rcw1`, via a
  CL port; one bug fixed), which KBE's `sysdef` loads as `LMLIB; LETS`.
- `zl-reader.lisp`, `zl-compat.lisp`, `zl-conditions.lisp`: the Zetalisp
  reader (`/` escapes, `#/c`, `...` as a symbol), Zetalisp functions and
  macros (destructuring `let`, old-style `do`, `listp` meaning `consp`,
  PDP-10 `boole` codes, characters that behave as numbers, ...), and the
  flavor-based condition system.
- `lispm-stubs.lisp`, `lispm-functions.lisp`: system facilities (areas,
  options, `arglist`, `si:declared-definition`).
- `load.lisp`: reads each KBE file form by form, in
  [`sysdef.lisp`](code/volume_1/sysdef.lisp) order, and reports any form
  that fails.
- `patches.lisp`: headless stand-ins for pieces that live in the editor
  layer: the delegate, `string-parser` for Lisp, `*input-language*`.

Loading reports `;; 0 load errors.` Along the way, the 31 `define-cliche` forms
in `syslib`, `kbelib` and `demo-lib` have each been analyzed into plans.

---

## The Map: The Cast and the Clichés

The parts of KBE that SQRT exercises:

| Part | What it does | Where |
| --- | --- | --- |
| Command reader | English-like commands into parse trees | `volume_3/pseudo.lisp`, `dict.lisp`, `gram.lisp` |
| Verbs | What each command does | `volume_3/refer.lisp` (references, `define-db-action`), `verbs.lisp` |
| Implementer | Instantiates clichés, fills roles, connects data flow | `volume_3/implement.lisp` |
| Plan database | Segments, ports, flow, roles, as facts | `volume_1/dmac.lisp`, `db.lisp` |
| Analyzer | Code into plans | `volume_2/surface.lisp`, `expand.lisp`, `primop.lisp`, `dflow.lisp`, `analyze.lisp` |
| Coder | Plans back into code | `volume_2/coder.lisp` |
| Cliché library | SUCCESSIVE-APPROXIMATION, AVERAGE, ... | `volume_3/kbelib.lisp`, `syslib.lisp`, `deflib.lisp` |

---

### The clichés SQRT will use

These are in [`kbelib.lisp`](code/volume_3/kbelib.lisp), "coded in the exact
style that the coder produces in order to facilitate comparison":

```lisp
(DEFINE-CLICHE SUCCESSIVE-APPROXIMATION
   (PRIMARY-ROLES (INITIAL-APPROXIMATION)
    DESCRIBED-ROLES (INITIAL-APPROXIMATION CRITERION IMPROVEMENT-STEP)
    COMMENT "iteratively improves an approximation until {the criterion} is
             satisfied" ...)
  (LET* ((APPROXIMATION {the input initial-approximation}) )
    (LOOP DO
      (IF ({the criterion} APPROXIMATION) (RETURN APPROXIMATION) )
      (SETQ APPROXIMATION ({the improvement-step} APPROXIMATION) ))))

(DEFINE-CLICHE AVERAGE (PRIMARY-ROLES (FIRST-NUMBER SECOND-NUMBER) ...)
  (// (+ {the input first-number} {the input second-number}) 2.0))

(DEFINE-CLICHE EQUALITY-WITHIN-EPSILON
    (PRIMARY-ROLES (FIRST-NUMBER SECOND-NUMBER)
     DESCRIBED-ROLES (FIRST-NUMBER SECOND-NUMBER EPSILON) ...)
  (< (ABS (- {the input first-number} {the input second-number}))
     {the epsilon}))
```

The `{...}` braces are KBE read syntax (`comment-macro.lisp`). `{the criterion}`
names a *role*, a hole in the cliché. `({the criterion} APPROXIMATION)` says the
role is a computation that receives APPROXIMATION; `{the input
initial-approximation}` says the role is a value flowing in.

Note that a cliché is not kept as text. When `kbelib.lisp` loads,
`define-cliche` ([`deflib.lisp:41`](code/volume_3/deflib.lisp)) calls
`expand-define-cliche` (`deflib.lisp:80`). That runs the code through the
**analyzer** and stores the resulting *plan* on the cliché's `cliche-plan`
property, as a `plan-def` form ([`db.lisp:1057`](code/volume_1/db.lisp)) that
rebuilds the plan's facts when evaluated. The cliché library is a library of
plans.

---

## A Day in the Life

### 8:00 "Reinitialize database."

```
>Reinitialize database.
PARSED: (REINITIALIZE)
RESULT: (:TEXT :TEXT "Database has been reinitialized.")
```

The path every command takes:

1. `parse-command` ([`pseudo.lisp:421`](code/volume_3/pseudo.lisp)) opens the
   string and calls `read-command`, which rebinds `standard-input` to it.
   `read-token-internal` peeks with `tyipeek`, scans words with
   `really-scan-word`, and returns `'REINITIALIZE DATABASE |.|`.
2. `parse-sentence` (`pseudo.lisp:476`) finds the verb's parse function.
   `define-verb` (`pseudo.lisp:1039`) generated one per verb from its pattern,
   here `(reinitialize database)`, and it checks for the word *database*.
3. `eval-command` ([`refer.lisp:156`](code/volume_3/refer.lisp)) looks up the
   verb's `verb-action` property and applies it to the parsed arguments.
4. The action calls `(send-delegate ':interpret 'reinitialize)`
   ([`behavior.lisp:24`](code/volume_1/behavior.lisp)). `*delegate*` is a
   flavor instance whose vocabulary uses the **`:case` method combination**:
   `:interpret` dispatches on its first argument to the method
   `(programmer-vocabulary :case :interpret reinitialize)`. That method was
   written by `define-db-action` (`refer.lisp:30`), which wraps the body in
   `with-db-locked` and `with-consistent-db`.
5. The body calls `db-init`. The database is empty: `*db-segs*` is NIL.

On the Lisp Machine, the delegate was an actor in the editor's society of actors.
Headless, it's `headless-delegate` (`runtime/patches.lisp`), a flavor with just
the two vocabularies.

---

### 8:05 "Define a program MYSQRT with a parameter NUM."

```
PARSED: (DEFINE NIL
          (NAME PROGRAM-DEFINITION MYSQRT "program")
          (PARAMETER (NAME VARIABLE NUM)))
CODE:   (DEFUN MYSQRT (NUM))
SEGS:   1
```

The parse is three arguments: no cliché name, a program definition named
MYSQRT, and one parameter. `make-definition-internal`
([`verbs.lisp:607`](code/volume_3/verbs.lisp)) checks the name and calls
`make-program-definition` (`verbs.lisp:669`). That builds the plan for an empty
`defun`, through `create-plan-for-code` (`verbs.lisp:53`), the single entry
into the analyzer.

The DB now holds one **segment**, `MYSQRT`: a box with an input port `NUM` and an
output port `RET`, and an `@seg` fact recording that it is a `lambda-exp` named
MYSQRT. Then `code-and-display-it` (`verbs.lisp:522`) runs the **coder**, and
`code-for-defun` ([`db.lisp:1485`](code/volume_1/db.lisp)) can now answer
`(DEFUN MYSQRT (NUM))`.

> **★ DON'T MISS: every command ends by recoding.**
>
> KBE keeps program text and plan consistent after each edit, and the text is always the coder's rendering of the plan. Even `(DEFUN MYSQRT (NUM))` was *generated*.

---

### 8:10 "Implement it as a successive-approximation."

The big step.

```
PARSED: (IMPLEMENT (IT) AS (CLICHE-INSTANCE (NAME CLICHE SUCCESSIVE-APPROXIMATION)))
SEGS:   11
CODE:   (DEFUN MYSQRT (NUM)
          (LET* ((APPROXIMATION {the input initial-approximation}))
            (LOOP DO
              (IF ({the criterion} APPROXIMATION) (RETURN APPROXIMATION))
              (SETQ APPROXIMATION ({the improvement-step} APPROXIMATION)))))
```

(The coder writes `{...}` placeholders as `(KBE-/#-COMMENT-MACRO code name)`
forms. They're shown here in `{}` notation.)

**"it"** refers to the focus, which the DEFINE command set to MYSQRT
(`make-be-it`). `*implement` ([`implement.lisp:537`](code/volume_3/implement.lisp))
evaluates both references:

- `(IT)`: the MYSQRT segment.
- `(CLICHE-INSTANCE ...)`: `cliche-instance-action` (`implement.lisp:131`) gets
  a fresh **instance** of the cliché with `a*seg`, then `2get-cliche`
  (`implement.lisp:167`). That *evaluates the stored `plan-def`*, creating new
  segments with fresh names (`genseg`, `genobj`; `db.lisp:474`, `501`). It then
  applies the cliché's constraints.

`implement-internal` (`:644`) and `implement-seg` (`:699`) then replace
MYSQRT's empty body with the instance. `2imp-top` (`:785`) splices the
instance's ports onto MYSQRT's (`2replace-port`) and keeps MYSQRT's name and
interface.

#### What KBE now knows

This is the plan, straight from the DB:

```
MYSQRT                 plantype TEMPCOMP  @seg (LAMBDA-EXP MYSQRT)
  role BSSR -> BSSR,  role DFJOIN -> DJ,  role REC -> REC
  BSSR                 plantype SSR
    role INIT -> INITIAL-APPROXIMATION,  role BODY -> TCBODY,  ...
    INITIAL-APPROXIMATION   plantype NEW-VIEW  (an empty funcall)
    TCBODY             plantype SSR-BODY
      PRED             segtype SPLIT  plantype PRED
        CRITERION      (an empty funcall)
        NON-NULL       segtype SPLIT  (the test of CRITERION's result)
      IMPROVEMENT-STEP (an empty funcall)
      REC              @seg (RECURSION)
      DJ, JOIN6        segtype JOIN
```

with data flow among the ports:

```
INITIAL-APPROXIMATION.APPROXIMATION -> TCBODY.APPROXIMATION
TCBODY.APPROXIMATION   -> PRED.APPROXIMATION -> CRITERION.APPROXIMATION
CRITERION.Z            -> NON-NULL.OBJ
TCBODY.APPROXIMATION   -> IMPROVEMENT-STEP.A
IMPROVEMENT-STEP.APPROXIMATION -> REC.APPROXIMATION
REC.APPROXIMATION1     -> DJ.APPROXIMATION2      ; around the loop
BSSR.APPROXIMATION1    -> MYSQRT.RET
```

control flow such as `(PRED . NON-NULL) -> (JOIN6 . PRED)` (the exit) and
`(PRED . NULL) -> (IMPROVEMENT-STEP)` (go round again), and three **slots**,
the unfilled roles:

```
MYSQRT: slot INITIAL-APPROXIMATION  type INPUT    -> INITIAL-APPROXIMATION
MYSQRT: slot CRITERION              type SEGMENT  -> CRITERION
MYSQRT: slot IMPROVEMENT-STEP       type SEGMENT  -> IMPROVEMENT-STEP
```

> **★ DON'T MISS: what isn't in the plan.**
>
> There's no `LOOP` and no variable called APPROXIMATION anywhere in the plan. Both are reinvented by the coder every time it writes the program.

The things to notice:

- **The `LOOP` isn't in the plan.** The analyzer recognized the loop as a
  *temporal composition* (TEMPCOMP) around a *basic SSR* (BSSR, "sequence of
  states"; see `3ssr-analyze` at
  [`analyze.lisp:1181`](code/volume_2/analyze.lisp)): an init, a body, a
  recursion point and a data-flow join. The coder will turn it back into
  `LET*`/`LOOP`/`SETQ`.
- **The variable APPROXIMATION isn't in the plan either.** What's there is
  data flow between ports. The coder chooses variable names when it writes
  code: `5annotate` ([`coder.lisp:819`](code/volume_2/coder.lisp)) and its
  "name classes" decide that every port in this region is called
  APPROXIMATION.
- **Roles are segments.** `{the criterion}` is a real segment, an empty
  `funcall` with an input and an output, so the rest of the plan can already
  connect to it.

---

### 8:15 "Implement the initial-approximation as [1.] ."

```
PARSED: (IMPLEMENT (RELATIVE-REFERENCE NIL '((NAME ROLE INITIAL-APPROXIMATION)) NIL 'IN (TOP))
         AS (CODE '"1."))
CODE:   ... (LET* ((APPROXIMATION 1)) ...
```

- **Reference.** "the initial-approximation" is a `relative-reference`
  (`refer.lisp`): find the *unimplemented* segment playing role
  INITIAL-APPROXIMATION within the top segment.
- **Code.** `[1.]` is literal code, kept as a string. The `code` action
  (`verbs.lisp:46`) parses it with `string-parser` (for Lisp, simply the Lisp
  reader plus `{}` syntax; see `volume_4/parse.lisp:370`) and analyzes `1.` into
  a tiny plan: a constant.
- **Splice.** `implement-seg` replaces the empty INITIAL-APPROXIMATION funcall
  with it, and the data flow that ran out of the placeholder now runs out of
  the constant.

> **★ DON'T MISS: `1.` is an integer.**
>
> In Zetalisp the trailing dot means *decimal*, not floating point, which is why the expected code has `1`, not `1.0`. The other number, `2.0` in AVERAGE, comes from the cliché.

---

### 8:20 "Implement the improvement-step as an average of [APPROXIMATION] and [(// NUM APPROXIMATION)] ."

```
PARSED: (IMPLEMENT (RELATIVE-REFERENCE NIL '((NAME ROLE IMPROVEMENT-STEP)) NIL 'IN (TOP))
         AS (CLICHE-INSTANCE (NAME CLICHE AVERAGE)
                             (CODE '"APPROXIMATION") (CODE '"(// NUM APPROXIMATION)")))
SEGS:   21
CODE:   ... (SETQ APPROXIMATION (// (+ APPROXIMATION (// NUM APPROXIMATION)) 2.0))
```

A cliché instance *with role fillers*. `cliche-instance-action` pairs the fillers
with AVERAGE's primary roles (FIRST-NUMBER, SECOND-NUMBER) and calls
`implement-internal0` on each role's slot. Two things happen to the fillers:

- **`APPROXIMATION` is a variable reference.** No segment computes it, so it
  becomes a *pending data flow*: "connect this input to wherever a value
  called APPROXIMATION comes from". `implement-dflow` and
  `2find-var-source` ([`implement.lisp:1244`](code/volume_3/implement.lisp),
  `:1118`) search the surrounding plan, using `2brothers-before` to find
  segments that run earlier, and connect it to the loop's current value.
- **`(// NUM APPROXIMATION)` is analyzed into a segment.** NUM resolves to
  MYSQRT's parameter and APPROXIMATION to the same loop value. (`//` is
  Zetalisp's division function, the symbol `/` read through the `/` escape.)

Then the AVERAGE instance replaces the IMPROVEMENT-STEP placeholder, and its
output takes over the placeholder's data flow into REC: round the loop.

---

### 8:25 "Implement the criterion as an equality-within-epsilon of [(* APPROXIMATION APPROXIMATION)] and [NUM] ."

```
CODE: ... (IF (< (ABS (- (* APPROXIMATION APPROXIMATION) NUM)) {the epsilon})
              (RETURN APPROXIMATION)) ...
```

The same move for the predicate. Two observations:

- The criterion role was a *segment* role filled with a *predicate*. The
  instance's output goes to the PRED/NON-NULL split, so the coder writes it as
  the `IF` test.
- EQUALITY-WITHIN-EPSILON has a third role, EPSILON, which wasn't a primary
  role and got no filler.

> **★ DON'T MISS: filling a hole can make a new one.**
>
> The unfilled EPSILON shows up in the program as `{the epsilon}`. The slots of the instance became slots of the program, so the programmer is now asked about a role they never mentioned.

### 8:30 "Implement the epsilon as [0.0001] ."

```
CODE: (DEFUN MYSQRT (NUM)
        (LET* ((APPROXIMATION 1))
          (LOOP DO
            (IF (< (ABS (- (* APPROXIMATION APPROXIMATION) NUM)) 0.0001) (RETURN APPROXIMATION))
            (SETQ APPROXIMATION (// (+ APPROXIMATION (// NUM APPROXIMATION)) 2.0)))))
SEGS: 32
```

No holes left. Seven commands; 32 segments of plan; one finished program.

---

### Afternoon: MYOTHERSQRT, and a change of interface

The next seven commands build the same program again as MYOTHERSQRT, with one
difference:

```
>Append a parameter CRITERION.
>Implement the epsilon as [CRITERION] .
```

"Append a parameter" changes the *interface* of a program that already exists:
MYOTHERSQRT gets a second input port. "[CRITERION]" is then a variable
reference resolved to that new parameter. The result:

```lisp
(DEFUN MYOTHERSQRT (NUM CRITERION)
  (LET* ((APPROXIMATION 1))
    (LOOP DO
      (IF (< (ABS (- (* APPROXIMATION APPROXIMATION) NUM)) CRITERION) (RETURN APPROXIMATION))
      (SETQ APPROXIMATION (// (+ APPROXIMATION (// NUM APPROXIMATION)) 2.0)))))
```

### Evening: the checkout

The self-test compares `(code-for-defun 'MYSQRT)` and `(code-for-defun
'MYOTHERSQRT)` with `equal` against the code recorded in 1984. Both match.

The code also runs: `(mysqrt 2.0)` gives `1.4142157`, `(mysqrt 9)` gives `3.0`,
and `(myothersqrt 2.0d0 1.0d-10)` gives `1.4142135623746899d0`.

---

## Districts Along the Way

### The plan database

Everything in the plan is a fact in the DB. `factdef`
([`dmac.lisp:188`](code/volume_1/dmac.lisp)) declares each kind; the
declarations are the schema:

```lisp
(factdef (segcase S 8.) (keyword K nil INPUT)
  (kind L nil NIL ID) (obj NP nil) (id NP nil))
(factdef (segcase S 6.) (keyword K nil DFLOW) (from P 12.) (to P 13.))
(factdef (segcase S 6.) (keyword K nil CFLOW) (from S 14.) (to S 15.))
```

The numbers are slots in each segment's index **hunk**, a cdr-coded list that
`dbx` reads by pointer arithmetic (`(car (%make-pointer-offset dtp-list hunk
(1+ n)))`, which is just `nthcdr` here). `factdef` also writes the accessors
`~from`, `&~from` and so on. The query macros are `??&` (find all), `?&`
(find one), `>&` (assert, avoiding duplicates), `+&` (add), and `--&`
(erase). `&*` builds patterns, with `*` as a wildcard.

A fact is in or out *per context*: the circular `*CMARKER*` structures at the
head of every fact record its membership in database layers. That's what makes
undo possible. (It's also why printing a fact needs `*print-circle*`.)

### The analyzer

`create-plan-for-code` → `surface-analyze`
([`surface.lisp:74`](code/volume_2/surface.lisp)) → `6flow-analyze`:

1. `expand-special-forms` ([`expand.lisp:110`](code/volume_2/expand.lisp))
   rewrites code into a handful of primitive forms: `k*prog`, `k*psetq`,
   `k*if`, `k*go`, `k*tag`, `k*slot`, ... `{...}` holes become `k*slot`s. Macros
   it has no `k*expander` for are expanded through `si:declared-definition`.
   For Zetalisp macros such as `if` and `when`, the runtime supplies the Lisp
   Machine expansions (`if` → `cond`) in `runtime/lispm-functions.lisp`.
2. `6eval` ([`primop.lisp:184`](code/volume_2/primop.lisp)) "evaluates" the
   primitive forms abstractly, building segments, ports and control flow.
3. `6data-flow` ([`dflow.lisp:44`](code/volume_2/dflow.lisp)) computes data
   flow with live-variable analysis over bit sets. (Set difference is
   `(boole 4 x y)`, PDP-10 ANDCM.)
4. `analyze` (`analyze.lisp:148`) groups (`3group-up`), simplifies data flow
   (`3dflow-simp`, which moves segments across segment boundaries), and
   recognizes loops (`3ssr-analyze`), producing the TEMPCOMP/BSSR structure
   shown above.

### The coder

`code` ([`coder.lisp:186`](code/volume_2/coder.lisp)) → `code1` →
`5codetop` (`:1739`):

- builds a partial order of everything that must happen before what
  (`5init-order`, bit vectors in Maclisp `*array`s);
- `5annotate` (`:819`) groups ports into *data-flow regions* and assigns each
  region a variable name, preferring names already in the code
  (APPROXIMATION, NUM);
- then generates code segment by segment, recomposing TEMPCOMP/SSR structures
  into `LET*` + `LOOP` and predicates into `IF`.

---

## Provenance: What the Transcription Taught Us

Getting SQRT through took 21 logged typo-level corrections to the transcribed
listings (some covering several lines), each checked against the scans and logged in
[`code/TRANSCRIPTION-FIXES.md`](code/TRANSCRIPTION-FIXES.md). The patterns
worth knowing if you hack here:

- **Paren errors that cancel out.** One missing `)` and one extra a few lines
  or a few hundred lines later leaves the file balanced but the structure
  wrong. In `coder.lisp`, two such pairs left about 30 defuns, including
  `5annotate`, nested inside other defuns and never defined. Check paren depth
  at every toplevel `(def`.
- **Quote vs. backquote.** In this listing font the two are nearly identical,
  in both directions: `'',x` that must be `` `',x ``, and `` `(,x) `` inside a
  backquote that must be `'(,x)`.
- **Dropped characters.** `&to` for `&~to`; `(&  (&* seg) port)` with a lost
  `*` (the double space is the tell).
- **OCR substitutions:** `seg1ype`, `tnen`.
- **Copied lines:** a line repeated from the line above, or from the previous
  definition (`define-verb-action` had `define-verb-unparse`'s `let`).

And the Zetalisp semantics the code really depends on (all in the runtime):
`(listp nil)` is false; characters are numbers (`(minusp (tyi s -1))`,
`(fixp c)`); `map` maps over tails; `boole` takes PDP-10 codes; `prog` can be
named; `&quote` arguments arrive unevaluated (`plan-def`); `get` on a list is
lenient; and a lambda-list parameter named `standard-input` rebinds the stream.

## What Makes SQRT Worth the Trip

Seven sentences turn into a working program, and every intermediate state is a real program with named holes. The run shows the representation shift doing exactly what Waters said it would. Clichés combine because they're plans, not text: AVERAGE drops into the IMPROVEMENT-STEP role and its data flow simply connects. Variable names are an output of the coder, not an input. And forty years on, the same fourteen sentences yield the same code.

## Known gaps

- KBE's explanation generator (`explanation`, document-fn) returns NIL here, so
  the "The program MYSQRT is a successive-approximation..." text in
  `358-test.lisp` isn't reproduced yet. `format-explanation-for-comment` needs
  the pretty-printer (`print-code`, from the missing `LMLIB; PP`).
- Only Lisp is supported (no Ada or PL/I), and only headless: no ZWEI,
  windows, or actors.
- The other five sequences of `358-test.lisp` haven't been run yet. The
  fidelity test there is that HASH and SUMLIST pass and BUCKET, DELETE and
  MEMQ fail *with the same wrong code as in 1984*.

---

## Sources

- The run: `code/volume_1/358-test.lisp` (KBE 358.0x self-test, 14 December 1984), replayed by `runtime/run-test.lisp`. Every parsed form, code state, and plan fact quoted above was captured from that replay.
- The source: the January 1985 four-volume KBE listing, transcribed in `code/`. Excerpts: Copyright © 1983/1984 Massachusetts Institute of Technology, as indicated by each source listing; copied by permission of MIT under the notice in those listings (quoted in full in [A Hacker's Guide to KBEmacs](hackers-guide.md)).
- Background: Richard C. Waters, "KBEmacs: Where's the AI?", *AI Magazine* 7(1), 1986; Charles Rich and Richard C. Waters, *The Programmer's Apprentice Project: A Research Overview*, MIT AI Memo 1004, 1987.
- The tour format: [Hacker's Guides](https://github.com/jeffshrager/HackersGuides).

---

# Copyright

**Hacker's Guides** — guide texts, commentary, and supporting material.

Copyright © 2026 Jeff Shrager (<jshrager@gmail.com>). All rights reserved.

Permission requests, corrections, and questions: <jshrager@gmail.com>.

## Scope

This copyright covers the original text of the guides and the README — the prose, structure, commentary, and annotations written for this repository: https://github.com/jeffshrager/HackersGuides

It does **not** cover:

- **The historic source code being read.** Quoted program source (and any complete source files included in this repository) remains the property of its respective authors and rights holders, or is in the public domain, as the case may be. Excerpts appear here for purposes of commentary, criticism, and scholarship. Provenance for each program's source is stated in the corresponding guide.
- **Quoted third-party material.** Brief quotations from cited historical accounts and documentation remain the property of their respective rights holders and are used for the same purposes.

## Attribution

If you quote or reference a guide, please credit "Hacker's Guides, Jeff Shrager" and link to this repository: https://github.com/jeffshrager/HackersGuides
