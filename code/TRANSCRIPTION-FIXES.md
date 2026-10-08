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
| `volume_2/primop.lisp` | 187 | `'',form` | `` `',form `` | same pattern as surface |
| `volume_3/pseudo.lisp` | 254 | `'',string` | `` `',string `` | same pattern as surface |
| `volume_3/pseudo.lisp` | 256 | `'',(let ...` | `` `',(let ... `` | same pattern as surface |
| `volume_3/refer.lisp` | 231 | `(program-definition   :` | `(program-definition` | vol 3 p55 |
| `volume_3/refer.lisp` | 386 | `{(corr-code _) ! _]` | `[(corr-code _) ! _]` | vol 3 p58 |
| `volume_3/refer.lisp` | 391 | `{(nreverse 2result) \|! ...` | `[(nreverse 2result) ! ...` | vol 3 p58 |
| `volume_3/verbs.lisp` | 619 | `*: ignore` | `':ignore` | vol 3 p77 |
