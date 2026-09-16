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

## geodesy

- `geodesy.Location#longitude` now returns an angle in `[-π, π)` (−180° to 180°); it previously
  returned `[0, 2π]`, so an eastern longitude of 10° is unchanged but a location built with
  longitude 270° now reads back as −90°. `Location#encode` and the `geo:` URI encoding follow,
  writing western longitudes as negative numbers. Code that assumed a non-negative longitude
  should call `.principal` on the result. (#TBD)
- `geodesy.Location(latitude: Angle, longitude: Angle)` no longer loses western longitudes: any
  negative longitude previously saturated and read back as 0°. `Location(north: Int, east:
  Int)` (microdegrees) previously returned a latitude of roughly 0° for every input; it now
  returns the given latitude. Geohashes and `surfaceDistance` of such locations change
  accordingly. Stored geohashes computed from western or microdegree locations were wrong and
  should be recomputed. (#TBD)
- `geodesy.Location#bearing` now distinguishes east from west (it took the absolute longitude
  difference, so every westward bearing came out eastward), and returns a bearing normalised
  to `[0, 2π)` before it reaches the `Directional` instance. `Compass[n](angle)` accepts any
  angle, including negative ones, which previously threw `ArrayIndexOutOfBoundsException`. (#TBD)
- `Geolocation`'s `Decodable in Text` instance now parses `;crs=…`, `;u=…` and further
  parameters, which previously always raised `Geolocation.Error` with reason `MissingEquals`,
  and accepts parameters directly after the longitude (`geo:1,2;u=3`), which previously raised
  `UnexpectedSuffix`. `Geolocation`'s `Encodable in Text` instance now writes `crs` and
  `parameters`, in the order `;crs=…`, `;u=…`, then each parameter, which it previously
  dropped. (#TBD)

## gossamer

- `gossamer.Decimalizer#decimalize` no longer drops a rounding carry out of the leading digit.
  When every retained digit is 9 and the next rounds up, the output lost its leading `1`
  (`Decimalizer(decimalPlaces = 1).decimalize(9.96)` gave `"0.0"`); it is now the next power
  of ten at the same precision (`"10.0"`; `Decimalizer(2).decimalize(9.99)` gives `"10"`).
  If the carry reaches `exponentThreshold`, the value is written in exponent form
  (`Decimalizer(4).decimalize(999.96)` gives `"1.000×10³"`). Every `Showable` for `Double`
  that goes through a `Decimalizer` changes the same way. Code that matched the old truncated
  output must expect the corrected text. (#TBD)

## gastronomy

- `gastronomy.Feistel.apply(subkeys, round)(input)` no longer corrupts its result when a round
  function returns a negative `Int`. The XOR of the high word with the round output was widened
  with `.toLong`, which sign-extends, filling the high 32 bits of the result — the half just
  shifted in — with ones; it is now masked to 32 bits. Any value derived from a `Feistel`
  network whose round function could return a negative number changes, so persisted values must
  be recomputed. (#TBD)
