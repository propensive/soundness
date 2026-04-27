# Soundness error codes

Every compile error raised by a Soundness macro carries a stable
identifier of the form `SN-xx/d` or `SN-xx/d.e`, where:

- `xx` is the two-letter module code (see `codes.tsv`).
- `d` is a per-module error number, starting at 1.
- `e`, when present, identifies a specific variant of the error
  (typically the case of a `Reason` enum).

The rendered prefix appears in compile output as `[↯SN-xx/d.e]`,
coloured if the compiler's `-color` setting is on.

Each error is documented in a markdown file at
`<xx>/<d>.md` (or `<xx>/<d>.<e>.md` for variant errors).
