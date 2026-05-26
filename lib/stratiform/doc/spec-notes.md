# TEL Specification Notes

Implementation-discovered ambiguities, inconsistencies, and inadequacies in the [TEL specification](https://github.com/propensive/tel/tree/main/spec) noticed while building `stratiform`. Each entry references the relevant spec section, summarises the issue, and records the decision taken for stratiform pending upstream resolution.

## Open issues

### §18.1 / §19.1 vs §19.5 — schema-aware indentation recovery
**Issue:** §18.1 and §19.1 say "the schema informs error recovery decisions (particularly for indentation errors, whose recovery algorithm is defined in §19.5)" — implying the parser consults the schema during recovery. But §19.5 defines the v1.0 recovery rule as the schema-independent **shallower-wins rule**, with the schema-aware variant "deliberately deferred" to a future revision.

**Stratiform decision:** The parser is structured single-pass and schema-aware (per §18.1) but uses only the shallower-wins rule in phase 1. When schema integration lands in phase 3, the parser will consult the schema during recovery as the normative §18.1 wording allows, while still falling back to shallower-wins where the schema provides no signal.

**Spec resolution:** open.

### `tel-schema` self-validation circularity
**Issue:** §20.5 specifies `tel-schema` as itself a TEL document conforming to `tel-schema`. A parser bootstrapping its first run has no `tel-schema` to validate the `tel-schema.tel` document against.

**Stratiform decision:** A hand-encoded `TelSchema` Scala value will be the axiom in phase 3. A self-consistency test parses `tel-schema.tel` with the axiom and asserts structural equality — a check that the axiom matches the canonical document.

**Spec resolution:** acceptable as a bootstrap mechanism; flagged here only as a concern for implementers.

### §15 literal-atom CR/LF normalisation
**Issue:** §15 states that literal-atom payloads preserve every byte between the opening LF and
the closing-delimiter LF, "including any CR, bare LF, or CR LF sequence". The Rust reference
implementation, however, normalises CRLF to LF inside the payload — the
`pos/literal-atom-cr-in-payload` fixture has a `hello\r\nworld` payload that produces text
`"hello\nworld"` (CR stripped), not `"hello\r\nworld"`.

**Stratiform decision:** Match the reference (strip CR adjacent to LF in literal-atom payloads).
The spec text should be amended to reflect the normalisation.

**Spec resolution:** open.

### §14 source-atom trailing LF
**Issue:** §14 reads "The captured lines are joined with a single LF between each pair into a
single text string. **The trailing LF of the last captured line is NOT included in text.** The
array of captured lines therefore yields a text field of the form `line_0 LF line_1 LF … LF
line_{n-1}`." The upstream reference implementation, however, emits a trailing LF for every
captured line — including the last one. Across all the `pos/source-atom-*` fixtures, an
n-captured-line atom has exactly n LFs in the payload (e.g. `code\n    source line\n` produces
`"source line\n\n"` — two LFs, not one).

**Stratiform decision:** Follow the reference implementation, not the literal text — emit one
LF per captured line. The text of §14 should be updated to read "yields a text field of the
form `line_0 LF line_1 LF … LF line_{n-1} LF`".

**Spec resolution:** open.

## Resolved
*(none yet)*
