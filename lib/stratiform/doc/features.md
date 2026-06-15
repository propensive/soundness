- parse and serialize TEL with full round-trip presentation preservation
- generic derivation of `Encodable in Tel` and `Decodable in Tel` via Wisteria
- statically-checked `tel"…"` interpolator and pattern extractor
- dynamic field access with camelCase ↔ kebab-case keyword translation
- structured error taxonomy aligned with the TEL specification's E-codes
- hand-encoded `tel-schema` axiom and §20.5 self-consistency check
- type-assignment of TEL documents against a schema (§20.2)
- layer composition (§20.3) — MergeStruct, MergeRecord, MergeScalar,
  MergeSelect, MergePolarity
- pluggable validator registry with the four built-in scalar validators
- presentation-preserving mutation primitives (§22.2) — UpdateAtom,
  Insert, Delete, Replace, AttachRemark, RemoveRemark, SetFlag,
  UnsetFlag, InsertBefore, InsertAfter
- composable `Revision` DSL with `++` sequencing
