# TEL Specification Notes

Implementation-discovered ambiguities, inconsistencies, and inadequacies in the [TEL specification](https://github.com/propensive/tel/tree/main/spec) noticed while building `stratiform`. Each entry references the relevant spec section, summarises the issue, and records the decision taken for stratiform pending upstream resolution.

## Open issues

### §18.1 / §19.1 vs §19.5 — schema-aware indentation recovery
**Issue:** §18.1 and §19.1 say "the schema informs error recovery decisions (particularly for indentation errors, whose recovery algorithm is defined in §19.5)" — implying the parser consults the schema during recovery. But §19.5 originally defined the v1.0 recovery rule as the schema-independent **shallower-wins rule**.

**Spec resolution:** **resolved.** §19.5 now defines two E107 recovery rules — a schema-independent shallower-wins rule (used when no schema is in scope) and a schema-aware rule (used when a schema is in scope) that picks between shallower and deeper based on keyword admissibility, falling back to shallower on a tie. §18.1 / §19.1 wording is now consistent.

**Stratiform implementation:** the schema-aware rule is implemented when the consumer calls `Tel.parse(bytes, schema)`; `bytes.read[Tel]` / `text.load[Tel]` (no schema in scope) continue to use shallower-wins (raising E107 in the absence of recovery). The parser maintains an `ancestors` stack of resolved struct types alongside the open-compound chain to enable the lookup.

_(No remaining open issues — see Resolved below.)_

## Phase status

Phases 1, 2 and 3 fully landed; phase 4 partially landed.

**Phase 1 — presentation parser + printer:** all 117 positive corpus
cases parse and round-trip; 22 of 23 negative E1xx cases detect the
expected E-code (E102, E103, E105, E106, E107, E108, E109, E110, E111,
E112, E113, E114, E115, E116, E121 bare-CR, E122, E123). The mixed-
line-endings half of E121 is deferred — it needs a pass that can tell
structural line breaks apart from literal-atom payload bytes. The
column-rule errors (E117–E120) need a tabulation-row alignment check
that's still TODO.

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

**Phase 3 — schema component:**
- `stratiform.schema` sub-component
- `Tels` data model per §20
- `TelElement` semantic model per §18.2
- E201–E218 and E301–E311 in `TelError.Reason`
- Hand-encoded `tel-schema` axiom per §20.5, shape-aligned with the
  canonical `tel-schema.tel` document
- Type assignment algorithm per §20.2 — Field with Struct / Scalar /
  Flag types, SelectRef with variant-keyword matching, Reference
  resolution, atom-phase skip-non-matching-Flag rule, post-combined
  required-member / non-repeatable / E303 / E304 / E306 / E311 checks
- Layer composition per §20.3 (MergeStruct / MergeRecord /
  MergeScalar / MergeSelect / MergePolarity)
- Validator infrastructure per §21 (Registry, Diagnostic, the four
  built-in scalar validators) plus integration with type assignment
  (E310 on Invalid responses)
- `TelsDecoder.validate` and `asValidated[T]` extension methods
  routing a schema-validated decode through type assignment first
- Canonical `tel-schema.tel` saved to the corpus, parsed, **and**
  asserted to type-assign cleanly against the axiom (§20.5
  self-consistency)

**Phase 4 — mutations + edit DSL (partial):**
- Primitive `Mutation.Op` enum per §22.2 — UpdateAtom, Insert,
  InsertBefore, InsertAfter, Delete, Replace, AttachRemark,
  RemoveRemark, SetFlag, UnsetFlag — with a presentation-preserving
  interpreter
- `TelPointer` keyword/index path with `Empty`, `of(...)`, `/`
- `Edit` DSL with `Edit.at(pointer).<op>` cursor, `++` composition,
  `tel.edited(edit)` extension, and `Edit.compound(...)` helper

**Phase 4 outstanding:**
- Polyvinyl records integration (structurally-typed records over a
  schema document)

**Recently landed:**
- §20.5 full structural-equality self-consistency: `TelsReconstructor`
  walks a parsed canonical document and rebuilds a `Tels`; the
  reconstruction equals the hand-encoded axiom (modulo the built-in
  scalars `Identifier`, `TypeName`, `Sigil`, `String`, which the
  reconstructor injects to match the axiom's explicit declarations).
- Panopticon `Lens` given over field names via `Tel.modify`.

Phase 5+ deferred items:

- `tel"…"` extractor multi-marker patterns within a single atom text
- Reorder/ResizeTabulation/Construct mutation operations
- BinTEL binary format (§7 of bintel.md)
- BASE-256 codec

## Resolved

### BinTEL §3 / §5 keyword-index semantics
**Issue:** `Tel.Type.assign` originally populated `TelElement.keywordIndex` with the **member index** (each Field=1 slot, each SelectRef=1 slot regardless of how many variants it carries). The BinTEL §5 keyword-index, however, is a **flat** position: each Field contributes 1 entry, each SelectRef contributes 1 entry **per variant**. The conflation made BinTEL output non-conformant — distinct Member variants on the same SelectRef collided to the same keyword index, and the encoded tel-schema body diverged from the §3 normative test vector at the first SelectRef-with-multiple-variants member.

**Stratiform fix:** rewrote `keywordMap`, `assignAtoms`, and `applyConstraints` in `TelTypeAssignment` to track and emit flat keyword indices. Verified by the §3 test vector: encoding `tel-schema.tel` now matches `demo/tel-schema.bintel.hex` byte-for-byte and hashes to `9033cf054ed14fc460cfd04502a2b69e1ac840cd1035f213492b74af7df2a8dd`.

### §14 source-atom trailing LF
**Issue:** §14 originally read "The captured lines are joined with a single LF between each pair into a single text string. **The trailing LF of the last captured line is NOT included in text.**" The Rust reference implementation, however, emits a trailing LF for every captured line — including the last one.

**Stratiform behaviour:** follows the reference — emits one LF per captured line. Across the `pos/source-atom-*` fixtures an n-line atom has exactly n LFs in the payload.

**Spec resolution:** **upstream** — §14 text amended to read "yields a text field of the form `line_0 LF line_1 LF … LF line_{n-1} LF`". Stratiform's behaviour already matches; no implementation change needed.

### §15 literal-atom CR/LF normalisation
**Issue:** §15 originally stated that literal-atom payloads preserve every byte between the opening LF and the closing-delimiter LF, "including any CR, bare LF, or CR LF sequence". The Rust reference implementation, however, normalises CRLF to LF inside the payload — the `pos/literal-atom-cr-in-payload` fixture has a `hello\r\nworld` payload that produces text `"hello\nworld"`.

**Stratiform behaviour:** strips CR adjacent to LF in literal-atom payloads, matching the reference.

**Spec resolution:** **upstream** — §15 text amended to describe the CRLF → LF normalisation. Stratiform's behaviour already matches; no implementation change needed.

### `tel-schema` self-validation circularity
**Issue:** §20.5 specifies `tel-schema` as itself a TEL document conforming to `tel-schema`. A parser bootstrapping its first run has no `tel-schema` to validate the canonical `tel-schema.tel` document against.

**Stratiform behaviour:** a hand-encoded `Tels` Scala value (`TelsAxiom.tels`) is the bootstrap axiom. The §20.5 self-consistency test parses `tel-schema.tel` with the axiom, type-assigns successfully, AND reconstructs an equivalent `Tels` via `TelsReconstructor.fromTel` to verify the axiom matches the canonical document.

**Spec resolution:** **disregard** — flagged only as an implementer concern; stratiform's hand-encoded-axiom + self-consistency-test approach is adequate verification.

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
