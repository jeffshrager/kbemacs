# Vendored Spice Lisp Flavors

Source: https://github.com/g000001/spice-flavors at commit
`42dd0727fbbe6bc661c2e3bd441643e701c0714f` (CMU Spice Lisp Flavors by
Steven Handerson, public domain / Unlicense; SBCL port by CHIBA Masaomi).

Files copied unchanged except as noted below; `test.lisp` and the `.asd`
were not vendored (the KBE loader loads these files directly).

Local changes (each marked `KBE:` in the source):

- `package.lisp`: dropped the `fiveam` dependency.
- Bug fixes so that instance-variable defaults declared in *component*
  flavors are applied (e.g. `(defflavor a ((p 'x)) ())` mixed into `b`;
  upstream left `p` unbound in instances of `b`):
  - `vanilla.lisp`: the `:progn` combination without daemons called only
    the first primary method (`(car (call-methods primary))`); now it calls
    all of them.
  - `flavors.lisp` (`%flavor-forms`): the generated `INTERNAL-INIT` tested
    `(slot-unbound-p self i)` with the defining flavor's raw slot index;
    now it uses `(iv var)`, which maps through the instance's entry map.
- `vanilla.lisp`: printing sends `:print-self` with a third argument,
  `prin1?` (= `*print-escape*`), matching the Lisp Machine protocol.

Lisp Machine features not in Spice Flavors (`:case` method combination,
`send` to functions and streams, flavor-aware `typep`) are added in
`../flavors-ext.lisp`, not here.
