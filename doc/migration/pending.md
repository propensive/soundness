# Changes since 0.66.0

This file is read by an LLM agent to upgrade code that consumes Soundness libraries from
0.66.0 to the next release. Each entry states precisely what changed; see `AGENTS.md` for the
format. Entries are grouped by module, most-recently-added last within a module.

## rudiments

- `rudiments.matchable` renamed to `rudiments.unsafeMatchable`. Signature unchanged:
  `extension [value](value: value) transparent inline def unsafeMatchable(using erased
  Unsafe): value & Matchable`. Also renamed in the `soundness` umbrella export. (#TBD)
- `rudiments.mutable` renamed to `rudiments.unsafeMutable`. Signature unchanged:
  `extension [element](value: Array[element]^{}) inline def unsafeMutable(using erased
  Unsafe): scala.Array[element]`. Also renamed in the `soundness` umbrella export. (#TBD)
- `rudiments.immutable` renamed to `rudiments.unsafeImmutable`. Signature unchanged:
  `extension [element](array: scala.Array[element]) inline def unsafeImmutable(using erased
  Unsafe): Array[element]^{}`. Also renamed in the `soundness` umbrella export. (#TBD)
- `rudiments.attested` renamed to `rudiments.unsafeAttested`, in all three overloads: the
  interval form `extension (interval: Interval) inline def unsafeAttested[within](within:
  within)(using erased Unsafe): Interval in within.type`, the index form `def
  unsafeAttested(index: applicable.Operand)(using erased Unsafe)`, and the lambda form
  `inline def unsafeAttested[result](index: applicable.Operand)(inline lambda:
  (applicable.Operand in value.type) => result)(using erased Unsafe)`. Signatures unchanged.
  (#TBD)

## zephyrine

- `zephyrine.Stream#storage(using Unsafe)` renamed to `unsafeStorage`. Signature unchanged.
  (#TBD)
- `zephyrine.Intake#buffer(using Unsafe)` renamed to `unsafeBuffer`. Signature unchanged.
  (#TBD)
- `zephyrine.Region#raw(using Unsafe)` renamed to `unsafeRaw`. Signature unchanged. (#TBD)
- `zephyrine.Slate#raw(using Unsafe)` renamed to `unsafeRaw`. Signature unchanged. (#TBD)
- `zephyrine.Cursor#datum(using erased Unsafe)` renamed to `unsafeDatum`. Signature
  unchanged. (#TBD)
- The two typed cursor-buffer extensions, previously both named `buffer` and distinguished by
  their receiver's element type, are renamed: `extension [cap^](cursor: Cursor[Data, cap])
  inline def buffer(using erased Unsafe): scala.Array[Byte]` becomes `unsafeDataBuffer`
  (`@targetName("dataBuffer")` unchanged), and `extension [cap^](cursor: Cursor[Text, cap])
  inline def buffer(using erased Unsafe): scala.Array[Char]` becomes `unsafeTextBuffer`
  (`@targetName("textBuffer")` unchanged). A call site must choose by the cursor's element
  type: `Cursor[Data, ?]` takes `unsafeDataBuffer`, `Cursor[Text, ?]` takes
  `unsafeTextBuffer`. Signatures otherwise unchanged. (#TBD)

## serpentine

- `serpentine.Path#child(value: Text)(using erased Unsafe)` renamed to `unsafeChild`.
  Signature unchanged. (#TBD)

## kaleidoscope

- `kaleidoscope.Regex.apply(parts: List[String])(using erased Unsafe): Regex` renamed to
  `kaleidoscope.Regex.unsafeFrom(parts: List[String])(using erased Unsafe): Regex`. The other
  overload, `Regex.apply(text: Text): Regex in JavaBaseRegex raises Regex.Error`, is
  unchanged; a call of the form `Regex(List(…))(using Unsafe)` must become
  `Regex.unsafeFrom(List(…))(using Unsafe)`. (#TBD)
