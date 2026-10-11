# Transcription fixes

Typo-level corrections to the transcribed listings, each checked against
the scanned PDF page. PDF page numbers count the cover as page 1.

In this listing font, backquote and quote are nearly identical ticks, so
`'` for `` ` `` is the most likely misreading. It only matters (and is only
detected) when the form contains a comma.

| File | Line | Was | Now | Scan |
| --- | --- | --- | --- | --- |
| `volume_1/kmac.lisp` | 92 | (doc string missing) | `"Network address for KBE bug reports"` | vol 1 p146, first line (lost at the page break) |
| `volume_2/surface.lisp` | 231 | `'',return-segcase` | `` `',return-segcase `` | vol 2 p7 |
| `volume_2/analyze.lisp` | 224 | `(ssrs ... (Elist prs)))))))` (7 parens) | 6 parens | vol 2 p90 |
| `volume_2/analyze.lisp` | 244 | `(t 'trn))))` (4 parens) | 5 parens | vol 2 p90; compensated the extra paren above, so the file still balanced |
| `volume_2/analyze.lisp` | 1600 | `(+& newssrcase segtype 'simple)` (line above repeated) | `(+& newbodycase segtype 'simple)` | vol 2 p121; caused a duplicate SEGTYPE fact |
| `volume_2/analyze.lisp` | 1027, 1044 | `(& newseg)))))))` (7) and `seg path)))))))))` (9) | 8 and 8 | vol 2 p106; compensating pair: the `(t ;(not nterm)` clause had become part of the `nterm` branch, so moving a seg into an SSR did nothing and 3DFLOW-SIMP retried it forever |
| `volume_2/coder.lisp` | 543, 786, 1213 | `'action))))))))` (8), `'split))))))))))` (10), `90)))))))` (7) | 9, 11, 5 | vol 2 p143, p148, p159; two missing parens compensated by two extras, so ~30 defuns from `5tc-move-frag-body-ports` to `5determine-name-class` (including `5annotate`) were nested inside earlier defuns and never defined |
| `volume_2/coder.lisp` | 2244 | `(car tnen)` | `(car then)` | OCR; by symmetry with `emptyelse` on the line above (scan not yet checked) |
| `volume_2/primop.lisp` | 535 | `-> sexpr.` | `-> sexpr` | vol 2 p40 shows a dot, judged a printout mark: with it the OVER variable would be `SEXPR.` and the body's `sexpr` unbound, but K*PROG runs for every program analyzed and SQRT passed in the 358 test the next day |
| `volume_2/primop.lisp` | 187 | `'',form` | `` `',form `` | same pattern as surface |
| `volume_3/pseudo.lisp` | 254 | `'',string` | `` `',string `` | same pattern as surface |
| `volume_3/pseudo.lisp` | 256 | `'',(let ...` | `` `',(let ... `` | same pattern as surface |
| `volume_3/implement.lisp` | 1212 | `(&  (&* seg) port)` | `(&* (&* seg) port)` | vol 3 p111: `*` dropped (note the double space) |
| `volume_3/implement.lisp` | 1225-1226 | `(&to _ newport)`, `(&from _ newport)` | `&~to`, `&~from` | vol 3 p112: tildes dropped |
| `volume_3/pseudo.lisp` | 925 | `` `(,(car bvl)) `` inside a backquote | `'(,(car bvl))` | vol 3 p25: same tick glyph as the outer backquote, but the inner one must be a quote (otherwise `(car bvl)` is evaluated at run time); the reverse of the usual misreading |
| `volume_3/pseudo.lisp` | 983 | `` `(((nil ,@bvl) ,var)) `` in DEFINE-VERB-ACTION | `` `((,bvl ,var)) `` | vol 3 p26: copied from DEFINE-VERB-UNPARSE just above, which does skip the verb; the action's &rest args have no verb, so every verb's arguments were shifted by one |
| `volume_3/refer.lisp` | 231 | `(program-definition   :` | `(program-definition` | vol 3 p55 |
| `volume_3/refer.lisp` | 386 | `{(corr-code _) ! _]` | `[(corr-code _) ! _]` | vol 3 p58 |
| `volume_3/refer.lisp` | 391 | `{(nreverse 2result) \|! ...` | `[(nreverse 2result) ! ...` | vol 3 p58 |
| `volume_3/verbs.lisp` | 635, 678 | `seg1ype` | `segtype` | OCR (1 for T) |
| `volume_3/verbs.lisp` | 619 | `*: ignore` | `':ignore` | vol 3 p77 |
