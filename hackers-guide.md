# A Hacker's Guide to KBEmacs

Copyright © 2026 Jeff Shrager (<jshrager@gmail.com>). All rights
reserved. (See copyright details at the end of this document.)

*A source tour of MIT's Implementation Apprentice: programming with algorithmic clichés, and the shift between program text and plans*

---

*Warning*: Large parts of this document were created using AI
assistance. *Doveryai, No Proveryai!*

---

## Before You Arrive: What Is KBEmacs?

KBEmacs, the **Knowledge-Based Editor in Emacs**, is the *Implementation Apprentice* of MIT's Programmer's Apprentice project. It was built by Richard C. Waters, with Kent Pitman, Charles Rich and others, at the MIT AI Lab in the early 1980s. It helps an expert programmer with **program construction**: the programmer chooses which standard algorithms to combine, and the apprentice takes care of many of the details of turning that choice into code.

You talk to it in a stylized English command language, phrased in terms of standard algorithms (*clichés*) and their parts (*roles*):

```
>Define a program MYSQRT with a parameter NUM.
>Implement it as a successive-approximation.
>Implement the improvement-step as an average of [APPROXIMATION] and [(// NUM APPROXIMATION)] .
```

and it writes, and keeps rewriting, the program:

```lisp
(DEFUN MYSQRT (NUM)
  (LET* ((APPROXIMATION 1))
    (LOOP DO
      (IF (< (ABS (- (* APPROXIMATION APPROXIMATION) NUM)) 0.0001) (RETURN APPROXIMATION))
      (SETQ APPROXIMATION (// (+ APPROXIMATION (// NUM APPROXIMATION)) 2.0)))))
```

Two contemporary accounts give the vocabulary used here:

- Richard C. Waters, "KBEmacs: Where's the AI?", *AI Magazine* 7(1), 1986. It explains the architecture and the AI ideas in it.
- Charles Rich and Richard C. Waters, *The Programmer's Apprentice Project: A Research Overview*, MIT AI Memo 1004, 1987. It identifies KBEmacs as the completed prototype Implementation Apprentice and places it in the wider research program.

**The witness.** This tour reads a January 1985 printout: four bound volumes of Lisp listings from MIT. It covers the KBE system as of **KBE 358.0x** (December 1984), with each file carrying its own date and version. The scans have been transcribed into the 87 files under [`code/volume_1`](code/volume_1/) through [`code/volume_4`](code/volume_4/). The transcription was made with AI assistance and then *executed*: under the SBCL compatibility runtime in [`runtime/`](runtime/), the system loads and reproduces one of its own December 1984 self-tests exactly (see **A Day in the Life**). Getting there required a few dozen typo-level corrections, each checked against the scans and logged in [`code/TRANSCRIPTION-FIXES.md`](code/TRANSCRIPTION-FIXES.md). So read the excerpts as *transcriptions of the 1985 printout, partly verified by execution*.

KBEmacs was written in **Zetalisp** for the Symbolics 3600 Lisp Machine. It leans on Flavors, the ZWEI/Zmacs editor, Lisp Machine processes, and the OZ file host. None of that is assumed below; **Practical Notes** has a primer.

### The assistant approach

The idea is a **division of labor grounded in shared knowledge**. The programmer decides what to do and which algorithms to use. The assistant knows enough about those algorithms to do the routine implementation work without having every detail explained. Waters calls KBEmacs a *semiexpert* assistant: its expertise is limited, and it's meant to help an expert.

It is also **nonintrusive**. You can issue a knowledge-based command or just edit the text, so the interface has to keep program text and the internal plan consistent while letting either one change.

### Algorithmic clichés

An **algorithmic cliché** is a standard computational method: a report generator, a linear search, successive approximation. Its **roles** are the parts that vary between uses; the fixed computation around them is the *matrix* (Waters's term). You **instantiate** a cliché by filling its roles. In AIM-1004's terms, the cliché has *roles and constraints*: constraints describe fixed structure, relate role fillers, and help decide how unfilled roles get filled. The **cliché library** is the shared vocabulary: `successive-approximation` carries knowledge about a computation and how it's organized.

### Plans and the representation shift

Internally, programs are **plans**: roughly, hierarchical flowcharts with explicit **data-flow** and **control-flow** arcs. The unit is the **segment**, a computation with input and output **ports**. A segment can contain sub-segments; a whole program or cliché is one outermost segment. Extra facts record roles and constraints.

The key operation is the **representation shift** between text and plan. Text is convenient for people; plans make certain reasoning and editing straightforward. A data-flow arc records that a value goes from here to there, and leaves it to the coder to decide whether that becomes a variable, a nested expression, or an argument. Waters gives three reasons this helps:

- **Locality**: a flow relationship is visible in a small part of the plan.
- **Additivity**: independently built pieces can be put side by side and then connected, without variable-name clashes.
- **Intermediate segmentation**: a segment's ports channel communication, so its insides can be changed separately.

AIM-1004 calls the project's fuller formalism the *Plan Calculus*. KBEmacs uses a restricted version. The overview's richer overlays, logical specifications, and the CAKE reasoning system belong to the later Design and Requirements Apprentices, and shouldn't be read into this editor. Waters's paper is explicit that KBEmacs's reasoning is procedural and special-purpose, not general deduction.

---

## The Map: Text, Plans, and the Cliché Library

Waters's architecture diagram names the **text editor**, **interface**, **analyzer**, **coder**, **knowledge-based editor**, and **cliché library**, with program text and plans as the two representations of a program:

```
Programmer
    → [Interface] ──→ [Text editor] ──→ program text
         │                                 │
         └──→ [Knowledge-based editor] ←── [Cliché library]
                        │
                        ↓
   program text → [Analyzer] → PLAN → [Coder] → program text
                                 │
                                 └→ [Program documentation]
```

(This is a simplified relationship, following the 1986 account, not a call graph.)

[`sysdef.lisp`](code/volume_1/sysdef.lisp) supplies the real module names and load order. Where each part lives:

| Part | Responsibility | Start reading at |
| --- | --- | --- |
| Plan representation | segments, ports, data/control flow, roles, slots, as database facts | [`dmac.lisp`](code/volume_1/dmac.lisp), [`db.lisp`](code/volume_1/db.lisp) |
| Analyzer | text into plans | [`surface.lisp`](code/volume_2/surface.lisp), [`analyze.lisp`](code/volume_2/analyze.lisp) |
| Cliché library | reusable plans with roles | [`deflib.lisp`](code/volume_3/deflib.lisp), [`kbelib.lisp`](code/volume_3/kbelib.lisp), [`demo-lib.lisp`](code/volume_3/demo-lib.lisp) |
| Knowledge-based editor | command language; combining clichés | [`pseudo.lisp`](code/volume_3/pseudo.lisp), [`verbs.lisp`](code/volume_3/verbs.lisp), [`implement.lisp`](code/volume_3/implement.lisp) |
| Coder | plans back into text | [`coder.lisp`](code/volume_2/coder.lisp) |
| Text editor and interface | editing, requests, consistency between representations | [`edit-commands.lisp`](code/volume_4/edit-commands.lisp), [`edit-actor.lisp`](code/volume_4/edit-actor.lisp), [`consistency.lisp`](code/volume_4/consistency.lisp) |
| Program documentation | describing a program from its plan | [`explain.lisp`](code/volume_4/explain.lisp), [`document.lisp`](code/volume_4/document.lisp) |

---

## District 1: The Land Registry: Plans as Facts

**Visit:** [`dmac.lisp`](code/volume_1/dmac.lisp), [`db.lisp`](code/volume_1/db.lisp), [`dvars.lisp`](code/volume_1/dvars.lisp)

A plan is stored as **facts** in a small database. A **segcase** pairs a segment with a case (`~seg` and `~case` are literally `car` and `cdr`), so `(PRED . NULL)` is the "null" exit of segment PRED. The `factdef` declarations are a compact schema:

```lisp
(factdef (segcase S 8.) (keyword K nil INPUT)
  (kind L nil NIL ID) (obj NP nil) (id NP nil))
(factdef (segcase S 0.) (keyword K nil SUBSEG) (subseg S 11.))
(factdef (segcase S 6.) (keyword K nil DFLOW) (from P 12.) (to P 13.))
(factdef (segcase S 6.) (keyword K nil CFLOW) (from S 14.) (to S 15.))
```

`subseg` gives containment, `dflow` connects ports, and `cflow` connects segment cases. Other kinds record structural types, roles, open slots, cliché identity, and the code each segment corresponds to (`@seg`, `@io`). There are two classifications not to confuse: `segtype` (`SPLIT`, `JOIN`, `SIMPLE`) is basic structure, while `plantype` (`EXPR`, `PRED`, `SSR`, `TEMPCOMP`, ...) is the higher-level organization the analyzer recognized or the editor built.

The query vocabulary is a family of macros:

```lisp
(defmacro +& (&rest body) `(add-fact (& . ,body)))
(defmacro >& (&rest body) `(assert-fact (& . ,body)))
(defmacro ??& (&rest body) `(find-facts (&* . ,body)))
(defmacro --& (&rest body) `(erase-facts (&* . ,body)))
```

`&` builds a fact; `&*` builds a *pattern*, with `*` as a wildcard and omitted fields filled in. `+&` adds and `>&` asserts (avoiding a duplicate), `??&` finds all matches, `?&` finds exactly one, and `--&` erases. A typical helper, read inside-out:

```lisp
(defun ??subseg (seg) (~nset ~seg ~subseg (??& (& seg) subseg)))
```

The database also has **contexts**: layers that facts are in or out of, recorded in the circular `*CMARKER*` structure at the head of every fact. That is what makes undo and the coder's scratch work possible.

> **★ DON'T MISS: the query language writes its own selectors.**
>
> `factdef` constructs the field readers `~from`, `~to`, `~obj`, ... and the field changers `&~from`, `&~to`, ... at load time. Searching for a hand-written `(defun ~from` will disappoint you.

> **★ DON'T MISS: an index made of pointer arithmetic.**
>
> Each segment's facts are indexed in a **hunk**, a cdr-coded list made by `make-dbhunk`, and read like this:
>
> ```lisp
> (defmacro dbx (num hunk)
>   `(car (%make-pointer-offset dtp-list ,hunk (1+ ,num))))
> ```
>
> It addresses element *n* by machine-pointer arithmetic on the Lisp Machine's cdr-coded storage, so the `factdef` slot numbers above are hunk offsets. The listing explains: "In Maclisp, these were hunks (hence the name)... Because we use %MAKE-POINTER-OFFSET below in the definition of the DBHUNK accessors, the cdr-coding of these lists must be maintained or total randomness will start to occur."

> **★ DON'T MISS: sets sorted by address.**
>
> [`sets.lisp`](code/volume_1/sets.lisp) keeps "fsets" sorted by object address so that union and intersection are merges. The comment admits it: "In principle, fsets shouldn't work on the LispM because the gc might move stuff... Since the gc is never really used, we ignore that problem and let things be fast." (The 1984 self-test header duly records "Garbage collector enabled: No.")

---

## District 2: The Customs House: The Analyzer

**Visit:** [`surface.lisp`](code/volume_2/surface.lisp), [`expand.lisp`](code/volume_2/expand.lisp), [`primop.lisp`](code/volume_2/primop.lisp), [`dflow.lisp`](code/volume_2/dflow.lisp), [`analyze.lisp`](code/volume_2/analyze.lisp), [`segtrn.lisp`](code/volume_2/segtrn.lisp)

The analyzer turns code into a plan. Its single sanctioned entry is `create-plan-for-code` in [`verbs.lisp`](code/volume_3/verbs.lisp): "This is the only entry into the analyzer, etc. which should be used." It calls `surface-analyze`, then `analyze`:

1. **`expand-special-forms`** (`expand.lisp`) rewrites the code into a handful of primitive forms (`k*prog`, `k*psetq`, `k*if`, `k*go`, `k*tag`, `k*slot`). It uses a `k*expander` property per construct, and macroexpands anything it doesn't know. The `{...}` role holes become `k*slot`s.
2. **`6eval`** (`primop.lisp`) "evaluates" those forms *abstractly*, building segments and control flow. Here "evaluate" means following a property-dispatched handler (`6fexpr`), not running the user's program.
3. **`6data-flow`** (`dflow.lisp`) computes data flow with a live-variable analysis over bit sets.
4. **`analyze`** groups (`3group-up`), simplifies data flow (`3dflow-simp`), recognizes loops as *temporal* structures (`3ssr-analyze`, giving the `SSR` and `TEMPCOMP` plantypes), and simplifies expressions.

Deriving a plan isn't the same as **recognizing** which clichés a program uses. AIM-1004 (section 4.1) is frank that KBEmacs's recognition is weak, and discusses Linda Wills's separate graph-parsing recognizer.

[`segtrn.lisp`](code/volume_2/segtrn.lisp) is the structural workbench: grouping, ungrouping, and moving segments. Drawing a boundary around some segments changes which arcs cross it, so these operations must repair interfaces, not just re-parent nodes.

> **★ DON'T MISS: the number prefixes are namespaces.**
>
> `surface.lisp` uses the prefix `6`, `analyze.lisp` uses `3`, `coder.lisp` uses `5`, and `implement.lisp` uses `2`: `6eval`, `3ssr-analyze`, `5annotate`, `2imp-top`. These are legal Lisp symbols and a local convention for avoiding name clashes. A leading digit is not OCR noise.

> **★ DON'T MISS: a loop is not a loop.**
>
> In a plan, `(LOOP DO ... (RETURN x) ... (SETQ x ...))` disappears. What's left is a *temporal composition* (TEMPCOMP) around a *basic SSR* (sequence of states), with an `INIT` role, a `BODY`, a recursion point `REC`, and a data-flow join `DFJOIN`. The coder regenerates the `LET*`/`LOOP`/`SETQ` from that structure. The SQRT walkthrough shows the actual facts.

---

## District 3: The Pattern Book: The Cliché Library

**Visit:** [`deflib.lisp`](code/volume_3/deflib.lisp), [`syslib.lisp`](code/volume_3/syslib.lisp), [`kbelib.lisp`](code/volume_3/kbelib.lisp), [`demo-lib.lisp`](code/volume_3/demo-lib.lisp), [`implement.lisp`](code/volume_3/implement.lisp)

A cliché is written as code with holes:

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
```

The braces are KBE read syntax ([`comment-macro.lisp`](code/volume_1/comment-macro.lisp)), not annotations added by this guide. `{the criterion}` names a role; `({the criterion} APPROXIMATION)` says the role is a computation receiving APPROXIMATION. A note above it in `kbelib.lisp` says the clichés are "CODED IN THE EXACT STYLE THAT THE CODER PRODUCES IN ORDER TO FACILITATE COMPARISON."

But a cliché isn't stored as text. `define-cliche` calls `expand-define-cliche`, which runs the code *through the analyzer* at load time. It stores the resulting plan on the cliché's `cliche-plan` property, as a `plan-def` form that rebuilds the facts when evaluated. The library is a library of plans.

To use one, `cliche-instance-action` ([`implement.lisp`](code/volume_3/implement.lisp)) gets a fresh instance (`2get-cliche` evaluates the stored `plan-def`), pairs supplied fillers with role names, fills each slot with `implement-internal0`, and checks constraints. Connections that can't be made yet go on `*pending-dflows*` and are retried by `2check-pending-dflows`. This is AIM-1004's "reasoning by means of constraints", done procedurally on the plan.

The papers' showpiece is Ada's `UNIT-REPAIR-REPORT`, built from a `simple-report`. The local Lisp library's `SIMPLE-REPORT` in `demo-lib.lisp` shows the same division between roles and matrix:

```lisp
     CONSTRAINTS
      ((DEFAULT {the file-name} "report.txt")
       (DERIVED {the line-limit}
                (- 65
                   (SIZE-IN-LINES {the print-item})
                   (SIZE-IN-LINES {the epilog})))))
```

> **★ DON'T MISS: a "default" is executable domain knowledge.**
>
> The page's line limit is *derived* from the sizes of other components. Read the constraint beside the body that consumes `{the line-limit}`: knowledge and algorithm meet at a named slot.

---

## District 4: The Parliament: The Knowledge-Based Editor

**Visit:** [`pseudo.lisp`](code/volume_3/pseudo.lisp), [`dict.lisp`](code/volume_3/dict.lisp), [`gram.lisp`](code/volume_3/gram.lisp), [`refer.lisp`](code/volume_3/refer.lisp), [`verbs.lisp`](code/volume_3/verbs.lisp), [`bridge.lisp`](code/volume_4/bridge.lisp)

`parse-command` reads a command from a string. `read-command` sets up a token buffer and insists that nothing follows the sentence's end. Verbs are declared with `define-verb`, `define-action`, and `define-db-action`, which together generate a parse function, an unparser, and an action. The action is dispatched to a *delegate* object with the Lisp Machine's `:case` method combination: `(send-delegate ':interpret 'implement ...)` runs the method `(programmer-vocabulary :case :interpret implement)`.

`gram.lisp` is mostly a commented grammar description: a map of the intended command forms, not proof that each phrasing works. `verbs.lisp` has the actions (analyze, define, implement, share, remove, describe, document, undo), and `implement.lisp` the heavy lifting.

**Reference** is central: "it", "the criterion", "the input structure of the enumerator" must each denote something in the plan. `refer.lisp` resolves them. `make-be-it` maintains the focus that "it" means, and `bridge.lisp` connects parsed references, facts, and code; its `parsed->facts` turns a resolution failure into an error message.

> **★ DON'T MISS: undo has a price.**
>
> At the top of `verbs.lisp`, `*automatic-reap*` defaults differently under `#+3600` and `#-3600`: "3600 is fast enough that this doesn't matter much." `finish-edit` saves an undo context only when automatic reaping is off. Performance policy reaches all the way to whether you can take an edit back.

---

## District 5: The Print Shop: The Coder

**Visit:** [`coder.lisp`](code/volume_2/coder.lisp): its opening commentary, `code`, `code1`, `5annotate`, `5codetop`

The coder writes program text for a plan. It decides where to nest an expression, where to introduce a variable, how to preserve ordering, and how to carry side effects across interfaces. Waters describes it as using simple planning and constraint propagation. The opening commentary distinguishes three annotations: `@SEG` (a segment's coding kind, name, and code), `@IO` (whether a port is a lambda argument, actual argument, free variable, return value, or side effect), and `@CREATEVAR` (a variable created to carry a value). The core of `code1`:

```lisp
    (setq ioc (5temporal-compose-all TOP-SEG))
    ;Must must make sure slot info is not lost.
    (let ((*force-slots-up* t))
      (3expr-simp TOP-SEG)) ;kill unneeded exprs created by editing.
    (5annotate TOP-SEG)
    (5codetop TOP-SEG mode)
```

Temporal pieces are composed, the plan is simplified, annotations are built, and source is constructed. `5annotate` sorts ports into **data-flow regions** and gives each region a variable name, preferring names the programmer already used. This is why SQRT's loop variable comes out as APPROXIMATION, even though no variable exists in the plan. `5init-order` keeps a partial order of everything that must happen before what, as bit patterns in Maclisp `*array`s.

> **★ DON'T MISS: pretty code depends on semantics.**
>
> The opening comments discuss when to use `PSETQ` (assignments that interfere) and `PROG2` (an assignment that must follow code whose value still matters). These are small decisions about evaluation order, and they make the difference between plausible-looking output and correct output.

---

## District 6: The Switchboard: Editor and Interface

**Visit:** [`actor.lisp`](code/volume_1/actor.lisp), [`mbox.lisp`](code/volume_1/mbox.lisp), [`standard-actor.lisp`](code/volume_4/standard-actor.lisp), [`edit-actor.lisp`](code/volume_4/edit-actor.lisp), [`consistency.lisp`](code/volume_4/consistency.lisp), [`edit-commands.lisp`](code/volume_4/edit-commands.lisp), [`zwei-magic.lisp`](code/volume_4/zwei-magic.lisp)

The interface unifies ordinary text editing and knowledge-based editing. Flavors supply objects; **actors** and **mailboxes** organize communication. The editing actor tracks pending requests and beliefs about definitions: its `coder-confirmation` behavior stores the new `:code` belief and queues a `:definition` request when the code changed. Producing source and inserting it into the editor buffer are separate events.

The December 1984 documentation (volume 1, PDF pages 3–5) gives the queues a user-visible meaning:

| Key | Documented role |
| --- | --- |
| `Super-X` | Enter a request and execute it immediately |
| `Hyper-X` | Submit a request to run in the background |
| `Hyper-S` | Check the status of requests |
| `Hyper-Resume` | Collect results of background requests |

In the other direction, `code-is-consistent?` compares the buffer's definition with the remembered and generated code. When `*auto-submit*` is on, it tries to turn textual edits back into plan operations (`make-code-consistent?`, `compare-code`). That's an *attempt*. AIM-1004's recognition caveat applies: accepting hand-edited code isn't unrestricted recovery of its cliché-level design. Even `code-equal` is special: it treats the `{...}` slot markers differently from ordinary list structure.

> **★ DON'T MISS: the comments describe the boundary they wish they had.**
>
> Beside the consistency repair, a comment proposes moving the work into actor negotiation and wrapping the generated command sequence in a context so it could be undone if a request failed. It's a proposal, not evidence that the snapshot does this.

---

## District 7: The Guidebook Office: Program Documentation

**Visit:** [`explain.lisp`](code/volume_4/explain.lisp), [`document.lisp`](code/volume_4/document.lisp), [`pbm-handlers.lisp`](code/volume_4/pbm-handlers.lisp)

KBEmacs can describe a program from its plan. Helpers ask about cliché names, plan types, slots, and segments, and `pbm-handlers.lisp` defines summary and detail handlers per kind of structure. Reading strategy: find the structural category of the thing explained, then its handler. The cliché definitions carry the raw material: `main` and `detail` phrases such as `("average of" first-number "and" second-number)`.

The header of `explain.lisp` attributes the original code to Scott Cyphers (spring 1982), citing his working paper *Automated Program Description* (MIT AI WP-237, 1982; the source header calls it "Automatic Program Description"), and records a later merge by KMP (Kent Pitman). AIM-1004 lists documentation among the demonstrated capabilities but calls its coverage preliminary.

> **★ DON'T MISS: the self-test checks the explanations too.**
>
> `358-test.lisp` prints, beside each generated program, KBE's own description: "The program MYSQRT is a successive-approximation. The initial-approximation is 1. The criterion is an equality-within-epsilon...". The test driver collects it with `(explanation fn)`.

### A side trip: Ada

Volume 3 includes a substantial Ada front end: [`ada-parse.lisp`](code/volume_3/ada-parse.lisp), [`ada-eval.lisp`](code/volume_3/ada-eval.lisp), [`ada-anal.lisp`](code/volume_3/ada-anal.lisp), [`ada-unparse.lisp`](code/volume_3/ada-unparse.lisp), and the example library [`demo-ada-lib.lisp`](code/volume_3/demo-ada-lib.lisp). The parser describes its scope as the Ada subset supported by the 1984 demonstration. The papers' `UNIT-REPAIR-REPORT` example is Ada. (The SBCL runtime supports only Lisp.)

---

## A Day in the Life: SQRT, End to End

On **14 December 1984**, KBE 358.0x tested itself on the Lisp Machine *Merlin*. The results are in [`code/volume_1/358-test.lisp`](code/volume_1/358-test.lisp). Its **SQRT** sequence builds a square-root routine from three clichés in fourteen commands. Unlike the rest of this guide, what follows is an **executed** trace: the 2026 SBCL runtime replays the commands, and the code KBE generates matches the 1984 record exactly. The companion guide, [**A Day in the Life of SQRT**](hackers-guide-sqrt.md), follows it command by command with the plan facts dumped from the database. Here is the outline.

**`>Reinitialize database.`** parses to `(REINITIALIZE)`. Every command takes this path: `parse-command` → the verb's generated parse function → `eval-command` → the verb action → `(send-delegate ':interpret ...)` → a `:case` method on `programmer-vocabulary`, written by `define-db-action`.

**`>Define a program MYSQRT with a parameter NUM.`** parses to `(DEFINE NIL (NAME PROGRAM-DEFINITION MYSQRT "program") (PARAMETER (NAME VARIABLE NUM)))`. The analyzer makes one segment. The coder, which runs after every command, writes `(DEFUN MYSQRT (NUM))`.

**`>Implement it as a successive-approximation.`** gets a fresh instance of the cliché's stored plan and splices it into MYSQRT. The database grows from 1 segment to 11. The code KBE now holds is the matrix, with the roles as holes:

```lisp
(DEFUN MYSQRT (NUM)
  (LET* ((APPROXIMATION {the input initial-approximation}))
    (LOOP DO
      (IF ({the criterion} APPROXIMATION) (RETURN APPROXIMATION))
      (SETQ APPROXIMATION ({the improvement-step} APPROXIMATION)))))
```

**`>Implement the initial-approximation as [1.] .`** fills an input role with literal code. **`>Implement the improvement-step as an average of [APPROXIMATION] and [(// NUM APPROXIMATION)] .`** fills a role with *another cliché* whose own roles are filled with code. `APPROXIMATION` becomes a pending data-flow connection, resolved to the loop's current value. **`...criterion as an equality-within-epsilon of [(* APPROXIMATION APPROXIMATION)] and [NUM] .`** does the same for the test, and EQUALITY-WITHIN-EPSILON's unfilled EPSILON appears as a new hole, `{the epsilon}`. **`>Implement the epsilon as [0.0001] .`** closes it. After seven commands there are 32 segments and a finished `MYSQRT`.

The second seven build `MYOTHERSQRT` the same way, except that **`>Append a parameter CRITERION.`** changes the program's interface and the epsilon becomes `[CRITERION]`. Both programs come out `equal` to the 1984 code, and the generated `MYSQRT` really does compute square roots.

---

## Practical Notes for the Independent Traveler

### Enough Zetalisp to read the excerpts

| Notation | How to read it here |
| --- | --- |
| `'form`, backquote, `,`, `,@` | as in any Lisp; but in the listing font quote and backquote look almost identical |
| `/x` | escapes `x`, in symbols *and strings*: `//` is the symbol `/` (division); `"Retry /"~A/""` contains quotes |
| `1.` | decimal 1 (the trailing dot means decimal) |
| `#/a`, `#\space` | characters, which in Release 4.5 are fixnums: `(minusp c)` tests for end of file |
| `#+3600`, `#-3600` | reader conditionals for the Symbolics 3600 |
| `[a b ! c]`, `!!` | KBE list construction: `!` splices with NCONC, `!!` with APPEND ([`readmacro.lisp`](code/volume_1/readmacro.lisp)); also used as destructuring patterns |
| `{the role}`, `{code, the role}` | KBE role holes ([`comment-macro.lisp`](code/volume_1/comment-macro.lisp)) |
| `(let (((a b) x)) ...)` | destructuring LET (sysdef loads `LISPM2; LET` into KBE) |
| `(do i 0 (1+ i) (= i n) ...)` | old Maclisp-style DO is accepted |
| `(listp nil)` | false: Zetalisp `listp` means `consp` |
| `(defun (sym prop) ...)` | defines a function stored on a property |
| `&quote` | following arguments are passed unevaluated |
| `(over x <- l ... -> y ...)` | Waters's OVER loop macro ([`outil.lisp`](code/volume_1/outil.lisp)) |
| `(letS ((x (Elist l))) ...)` | Waters's LetS series notation (`LMLIB; LETS`, described in MIT AIM-680) |
| `send`, `defflavor`, `defmethod (f :case :m sub)` | Flavors, including the `:case` method combination |
| `~name`, `&~name`, `??&`, `>&` | DB selectors and queries (District 1) |

Some important functions are generated by macros (`factdef`'s selectors, `define-verb`'s parsers), and some handlers are property definitions such as `(defun (k*prog 6fexpr) ...)`. When a symbol is used but never seems to be defined, read the defining macros.

### Running it

The historical startup is in [`lispm-init.lisp`](code/volume_1/lispm-init.lisp) (`(LOGIN 'KBE 'OZ)`). To run it today:

```sh
sbcl --script runtime/run-test.lisp sqrt
```

[`runtime/`](runtime/) is a Zetalisp compatibility layer for SBCL:
- Spice Lisp Flavors, extended with the `:case` combination;
- Waters's LetS;
- a Zetalisp reader, functions, and flavor-based conditions;
- stand-ins for the editor-layer pieces the core needs.

It loads the *headless* core: everything except ZWEI, windows, actors, and Ada. That core loads without errors, and the SQRT self-test passes. The other five 1984 sequences (BUCKET, DELETE, HASH, MEMQ, SUMLIST) haven't been run yet; three of them *failed* in 1984, which makes them a sharp fidelity test. See the `seshsums/` notes for how the runtime was built and the transcription debugged.

### A good first afternoon

`sysdef` → `dmac` → `create-plan-for-code` in `verbs` → `surface` → `analyze` → the opening commentary and `code1` in `coder` → `kbelib`'s SUCCESSIVE-APPROXIMATION → the [SQRT guide](hackers-guide-sqrt.md). Save loop analysis (`3ssr-analyze`) and the Ada machinery for a return visit.

---

## What Makes KBEmacs Worth Visiting

KBEmacs is an experiment in **editing a program in terms of its algorithmic structure**. The cliché library gives programmer and machine a shared vocabulary; plans make structure explicit; the editor combines and modifies clichés; the coder takes care of the details. And the whole thing coexists with ordinary text editing, because the assistant shifts between text and plan in both directions. AIM-1004 (section 3.1) lists the demonstrated capabilities as algorithmic editing, a user-extendable cliché library, escape to the surrounding environment, and language independence. Constraint reasoning, detail handling, modification, and documentation are listed as less developed.

The 1986 paper reports serious limits in performance, robustness, language coverage, and library size, and says KBEmacs had been mothballed as a research prototype. What survives in the listings is the architectural idea, and its engineering. You can watch a `LOOP` dissolve into a temporal composition and come back as a `LOOP`. You can see a variable name chosen by a planner, and a fact database indexed by pointer arithmetic. Run SQRT and it all still works.

---

## Provenance and Sources

### The source being toured

The witness is a four-volume printout, *Knowledge-Based Editor* (January 1985), scanned to PDF. The scans aren't in this repository. The transcriptions are, each file noting its PDF pages:

| Volume | Contents | Listings |
| --- | --- | --- |
| 1 | the 358 self-test, system definition, utilities, actors, database | [30 listings](code/volume_1/README.md) |
| 2 | analyzer and coder | [8 listings](code/volume_2/README.md) |
| 3 | command language, implementer, Ada, cliché libraries | [17 listings](code/volume_3/README.md) |
| 4 | editor interface, explanation, parser, debugging | [32 listings](code/volume_4/README.md) |

Individual files carry their own dates and versions (e.g. `implement.lisp.123`, 19 December 1984), so the collection is not a single release. The transcriptions were made with AI assistance. The SBCL execution corrected them where they broke; every such correction was checked against the scans and is listed in [`code/TRANSCRIPTION-FIXES.md`](code/TRANSCRIPTION-FIXES.md). Code that SQRT doesn't exercise is unverified beyond loading.

The listings retain MIT copyright notices, generally 1983, with 1984 on `demo-lib.lisp`. Excerpts here are copied by permission of the Massachusetts Institute of Technology under the notice in those listings:

> Permission to copy all or part of this material is granted,
> provided that the copies are not made or distributed for resale,
> the MIT copyright notice and reference to the source file and
> the software distribution version appear, and that notice is
> given that copying is by permission of Massachusetts Institute
> of Technology.

Excerpts: Copyright © 1983/1984 Massachusetts Institute of Technology, as indicated by each source listing; all rights reserved. The source files are those linked in each district, from the January 1985 four-volume KBE listing (KBE 358.0x era).

### The principal papers

- Richard C. Waters, "KBEmacs: Where's the AI?", *AI Magazine* 7(1):47–56, 1986.
  - pp. 47–49: the semiexpert assistant and the report demonstration.
  - pp. 50–52: the assistant approach; clichés, roles and the matrix; plans, segments and ports; locality, additivity, intermediate segmentation.
  - pp. 53–55: the representation shift, the architecture, procedural reasoning, limits.
  - p. 56: why the interface emphasizes text over showing plans.
- Charles Rich and Richard C. Waters, *The Programmer's Apprentice Project: A Research Overview*, MIT AI Memo 1004, 1987.
  - §§1.1–1.2: the additive, incremental approach; the three apprentices.
  - §§2.1–2.3: the Plan Calculus and CAKE.
  - §3.1: demonstrated capabilities and limits.
  - §4.1: the limits of recognition.

The papers supply the conceptual vocabulary and historical claims. The listings supply entry points, database operations, and behavior. Where this guide describes what code *does*, the claim comes from the listing, or, for SQRT, from executing it.

### Further reading

- Richard C. Waters, *KBEmacs: A Step Toward the Programmer's Apprentice*, MIT AI-TR-753, 1985.
- Kent M. Pitman, *Interfacing to the Programmer's Apprentice*, MIT AI WP-244, 1983.
- Scott Cyphers, *Automated Program Description*, MIT AI WP-237, 1982.
- Richard C. Waters, *LetS: An Expressional Loop Notation*, MIT AIM-680, 1982.
- Charles Rich and Richard C. Waters, *Approaches to Automatic Programming*, MERL TR92-04, 1992 (§5.2 discusses KBEmacs).
- The SQRT companion: [A Day in the Life of SQRT](hackers-guide-sqrt.md).
- The tour format: [Hacker's Guides](https://github.com/jeffshrager/HackersGuides) (see especially the SHRDLU and ELIZA guides).

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
