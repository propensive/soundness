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
of the same `<d>.md` file.
