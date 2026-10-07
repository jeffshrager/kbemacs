# Volume 3 Lisp listings

Transcriptions from `PDFs/Knowledge-Based_Editor_Jan-1985_volume_3.pdf`.
One Lisp file per printed listing. PDF page numbers below count the cover as
page 1; the printed source page numbers restart for each listing.

| File | PDF pages | Printed source pages |
| --- | --- | --- |
| [pseudo.lisp](pseudo.lisp) | 3–29 | 1–27 |
| [dict.lisp](dict.lisp) | 30–35 | 1–6 |
| [gram.lisp](gram.lisp) | 36–47 | 1–12 |
| [refer.lisp](refer.lisp) | 48–61 | 1–14 |
| [verbs.lisp](verbs.lisp) | 62–84 | 1–23 |
| [implement.lisp](implement.lisp) | 85–123 | 1–39 |
| [ada-eval.lisp](ada-eval.lisp) | 124–154 | 1–31 |
| [ada-anal.lisp](ada-anal.lisp) | 155–178 | 1–24 |
| [ada-parse.lisp](ada-parse.lisp) | 179–202 | 1–24 |
| [ada-unparse.lisp](ada-unparse.lisp) | 203–211 | 1–9 |
| [view.lisp](view.lisp) | 212–214 | 1–3 |
| [pli.lisp](pli.lisp) | 215–226 | 1–12 |
| [deflib.lisp](deflib.lisp) | 227–232 | 1–6 |
| [syslib.lisp](syslib.lisp) | 233–234 | 1–2 |
| [kbelib.lisp](kbelib.lisp) | 235–242 | 1–8 |
| [demo-lib.lisp](demo-lib.lisp) | 243–248 | 1–6 |
| [demo-ada-lib.lisp](demo-ada-lib.lisp) | 249–263 | 1–15 |

The cover and contents occupy PDF pages 1–2. PDF page 97 (printed
`implement.lisp` page 13) is blank; it contributes no code. The listings cover
all remaining PDF pages through page 263.

Indentation is reconstructed for readability. Source comments, copyright
notices, historical spellings, and Lisp Machine reader conventions are retained,
including slash escapes, `#/` character literals, backquote templates, and the
`#$...$` Ada text literals. These files are transcriptions, not a port to Common
Lisp. Existing partial transcriptions were checked against the PDF and their
missing portions restored.

Validation included delimiter and string checks using the historical reader
conventions, comparison of scanned declaration names with the extracted files,
and visual review of ambiguous OCR, macro quoting, and format strings. No
execution in the original Lisp Machine environment was performed.
