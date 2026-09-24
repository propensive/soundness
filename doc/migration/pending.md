# Changes since 0.68.0

This file is read by an LLM agent to upgrade code that consumes Soundness libraries from
0.68.0 to the next release. Each entry states precisely what changed; see `AGENTS.md` for the
format. Entries are grouped by module, most-recently-added last within a module.

## corpuscular

- `corpuscular.Crc64.table: Array[Long]^{}` changed shape: it now holds eight slicing tables
  flattened end to end (2048 entries, indexed `k*256 + n` for slice `k` in `0 until 8`),
  previously the single 256-entry bytewise table. The first 256 entries are the same bytewise
  table as before, so code that indexes it by a byte value is unaffected; code that relied on
  `table.length == 256` or iterated the whole table must use `table.readable.take(256)`.
  `Crc64.Accumulator`'s results are unchanged.

## stratiform

- `stratiform.Tel.Error.Reason.UnconstrainedScalar` (E224) removed: `Tels.Validation` no longer
  rejects a `scalar` declaring neither `validate` nor `pattern`, so such a schema now validates
  where it previously raised `Tel.Error(Reason.UnconstrainedScalar)`. Code matching on the case
  must drop that branch; the error number 224 stays reserved and is not reused. (#2048)
