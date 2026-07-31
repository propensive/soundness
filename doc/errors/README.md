# Soundness error codes

Every compile error raised by a Soundness macro carries a stable
identifier of the form `SN-d` or `SN-d.e`, where:

- `d` is a globally-unique error number, zero-padded to three digits.
- `e`, when present, identifies a specific variant of the error
  (typically the case of a `Reason` enum).

The rendered prefix appears in compile output as `[↯SN-d.e]`,
coloured if the compiler's `-color` setting is on.

Each error is documented in a markdown file at `<d>.md`. When an
error has variants, every `SN-d.e` is documented as a sub-section
of the same `<d>.md` file, or — where there are more than a handful —
as a row of a table.

A code is never reused. When an error is removed, its page stays, marked
`(retired)`, saying what became of it: a reference to the code from an
older release still resolves, and the number cannot be allocated again
by accident.

Every page names the source file its error is raised from. It does not
give a line number, since a line number is invalidated by any edit above
it and nothing checks that it is still right.
