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

## hieroglyph

- `hieroglyph.Chars.superscript` (reached as `char.superscript`) returned the **subscript**
  characters for `(`, `)`, `+`, `-` and `=` — U+208D, U+208E, U+208A, U+208B, U+208C. It now
  returns the superscript forms U+207D, U+207E, U+207A, U+207B, U+207C. Digits were and remain
  correct. Code or test data pinning the old (wrong) characters for those five inputs must be
  updated. (#TBD)
- `hieroglyph.Chars.subscript` (reached as `char.subscript`) now maps `(`, `)`, `+`, `-` and
  `=` to U+208D, U+208E, U+208A, U+208B, U+208C; previously it mapped only digits and returned
  `Unset` for those five. Code relying on `Unset` for them must handle a present value. (#TBD)

## gesticulate

- `Media.Suffix`'s `Showable` instance now renders the suffix as it is written in a media type,
  using `Suffix#name`: `JsonSeq` shows as `json-seq` (was `jsonseq`), `CborSeq` as `cbor-seq`
  and `FastInfoset` as `fast-infoset`. The other twelve suffixes are unchanged. (#TBD)
- `MediaType.parse` (and so `Text#as[MediaType]` and `media"…"`) now accepts the hyphenated
  suffixes `+json-seq`, `+cbor-seq` and `+fast-infoset`, which previously always raised
  `MediaType.Error` with reason `InvalidSuffix`, because the whole suffix was capitalized
  (`Json-seq`) before the enum lookup rather than each hyphenated word. An `InvalidSuffix`
  reason now reports the suffix as written, in lower case, rather than capitalized. (#TBD)
- `MediaType.parse` now raises `MediaType.Error` with reason `MissingParam` for a parameter
  with no `=` (`text/plain; foo`); it previously threw `IndexOutOfBoundsException`, which no
  `Tactic` could catch. (#TBD)

## gastronomy

- `gastronomy.Feistel.apply(subkeys, round)(input)` no longer corrupts its result when a round
  function returns a negative `Int`. The XOR of the high word with the round output was widened
  with `.toLong`, which sign-extends, filling the high 32 bits of the result — the half just
  shifted in — with ones; it is now masked to 32 bits. Any value derived from a `Feistel`
  network whose round function could return a negative number changes, so persisted values must
  be recomputed. (#TBD)

## scintillate

- `scintillate.SocketServer`'s listening socket is now bound with a backlog of 1024
  (`jn.ServerSocket(port, 1024, address)` and, for TLS, `createServerSocket(port, 1024, address)`),
  and `scintillate.Reactor`'s with 1024 (was 128); previously `SocketServer` passed `0`, i.e. the
  JDK default of 50. The kernel caps the value (`kern.ipc.somaxconn`, `net.core.somaxconn`).
  Observable only as the number of simultaneous pending connections accepted before SYNs are
  dropped. (#TBD)

## sedentary

- `sedentary.LocalhostDevice#invoke` launches the measurement JVM with its standard error inherited
  from the harness (`ProcessBuilder.Redirect.INHERIT`) instead of piped and discarded, so the
  child's stderr now appears on the harness's stderr; a non-zero exit of the child raises
  `Bench.Error`. (#TBD)
- In `sedentary.Stress`, a body that throws any `Throwable` other than `OutOfMemoryError` no longer
  terminates its worker thread with an uncaught exception: the window is recorded as failed (`ok =
  false`, ending a sweep or capacity search as an SLO failure would) and one line `sedentary: a
  worker failed at N=<n>: <throwable>` is printed to stderr. Previously such a window completed with
  the dead worker's operations missing and the exception printed per worker by the default
  uncaught-exception handler. (#TBD)

## parasite

- `snooze(duration)`, and so `sleep(instant)`, `delay` and `hibernate`, which are built on it,
  now always wait out their duration unless the task is cancelled. Under the pooled supervisor
  (`threading.pooledThreading`), a task handed to a carrier that was still spinning could find
  a park permit left over from the hand-off, and its next `snooze` returned at once. The same
  applies to any JVM `Supervisor` whose `sleep` comes from `ThreadSupervisor`: `sleep` now parks
  repeatedly until its deadline, stopping early only if the thread is interrupted. Code that
  relied on a `snooze` returning early without cancellation must use `park` and an `unpark`
  instead. (#2006)

## escritoire

- `escritoire.Columnar`'s abstract member is now `def flex(metrics: tessellate.Metrics,
  maxWidth: Int): Flex`, which derives a column's claim from the aggregate intrinsic widths of
  its lines. The former abstract `def flex[text: Textual { type Result = Char }](lines:
  Array[text]^{}, maxWidth: Int)(using Text is Measurable): Flex` is now concrete and delegates
  to it through `Columnar.metrics(lines)`. An external `Columnar` implementation must implement
  `flex(metrics, maxWidth)` instead, computing from its `metrics` parameter where it previously
  folded `Flow.metrics` over the lines; it no longer needs to override the `lines` form. (#2007)
- `Columnar` gains `accommodates(aggregate: Metrics, cell: Metrics): Boolean`, true when a cell
  cannot change the column's claim; it defaults to `cell.min <= aggregate.min &&
  cell.natural <= aggregate.natural`. A strategy whose claim ignores its content should override
  it to `true`, as `columnar.Fixed` does. `Columnar.metrics(lines): Metrics` is new: the max-fold
  of each line's `Flow.metrics`. (#2007)

## tessellate

- `tessellate.Flow.wrap` no longer strips trailing spaces from a line which ends at a hard break
  (`\n` or `\r`) or at the end of the content; it drops them only as far as needed to keep the
  line within `width`. A line ending at a soft break still loses the spaces the break absorbed.
  Wrapped text, and tables rendered through `Flow.wrap` (such as escritoire's `Grid.render`),
  therefore keep meaningful trailing spaces: a styled cell such as `e"$Bg(green)( ✓ )"` now
  keeps its third, styled cell rather than being re-padded with an unstyled space. Code or test
  fixtures which expected the trimmed lines must be updated. (#2000)
## cartouche

- New library `cartouche`, a text-positioning engine: `def arrange[result](body:
  (Arranger.Pass^) ?=> result)(using Arranger): result` runs its body twice, recording each
  `position(...)` call in the first run and answering it from the solved arrangement in the
  second. Also `def position(caption: Caption)(using Arranger.Pass^): Caption.Position`, `def
  position(width: Double, height: Double, x: Double, y: Double, attachments:
  List[Caption.Attachment] = Caption.Attachment.compass, standoff: Double = 0.0, reach: Double
  = 0.0, padding: Double = 0.0, priority: Int = 0, fallback: Caption.Fallback =
  Caption.Fallback.Overlap)(using Arranger.Pass^): Caption.Position`, `def avoid(obstacles:
  Obstacle*)(using Arranger.Pass^): Unit`, `def canvas(box: Obstacle.Box)(using
  Arranger.Pass^): Unit`, the types `Caption` (with `Caption.Attachment`, `Caption.Fallback`,
  `Caption.Leader`, `Caption.Position`), `Obstacle` (with `Obstacle.Box`, `Obstacle.Line`,
  `Obstacle.Disc`) and `Arranger` (with `Arranger.Greedy`, `Arranger.Annealing` and
  `Arranger.Pass`), and the importable givens `arrangers.greedyArranger` and
  `arrangers.annealingArranger`. All exported from the `soundness` umbrella. Additive: no
  existing code changes. (#TBD)

