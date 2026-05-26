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

## Phase status

Phases 1 and 2 fully landed; phase 3 (schema) substantially progressed.

**Phase 1 — presentation parser + printer:** all 117 positive corpus
cases parse and round-trip; 10 of 23 negative E1xx cases detect the
expected E-code.

**Phase 2 — typed access surface:**
- `Tel` value class wrapping `Subtree` (Document | Compound)
- `Encodable in Tel` / `Decodable in Tel` typeclasses with Wisteria
  product / sum derivation; primitive instances for Text, String, Int,
  Long, Double, Boolean, Tel, Optional[T]
- Dynamic field access with camelCase ↔ kebab-case keyword mapping
- `tel"…"` interpolator macro (compile-time parse + typed
  `Encodable in Tel` holes)
- `tel"…"` extractor macro (compile-time pattern match + atom-text
  capture)

**Phase 3 — schema component (in progress):**
- `stratiform.schema` sub-component declared
- `TelSchema` data model per §20
- `TelElement` semantic model per §18.2
- E201–E218 and E301–E311 in `TelError.Reason`
- Hand-encoded `tel-schema` axiom per §20.5
- Type assignment algorithm per §20.2 (Field with Struct / Scalar /
  Flag; Reference resolution; required-member and non-repeatable
  checks)
- Layer composition per §20.3 (MergeStruct / MergeRecord /
  MergeScalar / MergeSelect / MergePolarity)
- Validator infrastructure per §21 (Registry, Diagnostic, the four
  built-in scalar validators)

Phase 3 follow-ups (in priority order):

- SelectRef handling in type assignment (sum types — E303, E304)
- Wiring `TelValidator.Registry` into `TelTypeAssignment` so
  validators run during type assignment and raise E310 on Invalid
- Full `Exclude(K)` data-model encoding for layer SelectDefinition
  bodies (currently the `variant` form rejects additions but
  `exclude` operations need their own Member-kind encoding)
- Schema-aware `tel.as[T]` decoder that consults a `TelSchema` for
  keyword indices and types
- The §20.5 self-consistency test: parse `tel-schema.tel` against
  the axiom, walk the resulting `TelElement` tree to reconstruct a
  `TelSchema` value, and assert structural equality with
  `TelSchemaAxiom.telSchema` — the phase-3 merge blocker

Phase 4+ deferred items:

- Panopticon `Lens` given (requires §22 `modify` mutation primitive)
- `tel"…"` extractor multi-marker patterns within a single atom text
- Mutation operations (§22)
- BinTEL binary format (§7 of bintel.md)
- BASE-256 codec

## Resolved

### `tel"…"` resource-path package shadowing
**Issue:** The test corpus was originally stored at
`lib/stratiform/res/test/stratiform/tel/`. Mill adds test resources to
the compile classpath; the Scala compiler then interpreted the directory
at `stratiform/tel/` as a package and shadowed the `tel"…"` extension
method on `StringContext`, producing `value tel is not a member of
StringContext` at every call site.

**Stratiform decision:** Moved the corpus to
`lib/stratiform/res/test/stratiform/corpus/`. Sync script and
`CorpusLoader` updated. The general rule for resource layout: avoid
sub-directories under `stratiform/` whose names collide with public API
identifiers.

**Spec resolution:** local fix; no spec change needed.
