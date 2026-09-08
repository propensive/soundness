# Changes since 0.65.0 (pending release)

This file is read by an LLM agent to upgrade code that consumes Soundness libraries. Each
entry states precisely what changed; see `AGENTS.md` for the format. Entries are grouped
by module, most-recently-added last within a module.

## build

- Soundness is compiled with Proscala's `-Zliterate-literals` flag, and every module that can
  reach `anticipation.text` adds `anticipation.literacy` to its `-Yimports` prelude. Under
  that combination a string literal in a Soundness source is typed as `Text` rather than
  `String` wherever its expected type does not already accept a `String`; Soundness's own
  sources now write `"…"` where they wrote `t"…"` or `"…".tt`. Consumers are unaffected unless
  they opt in: pass `-Zliterate-literals` (Proscala 3.9.0-p16 or later) and bring
  `anticipation.literacy.literate` into scope (by import, or by the same `-Yimports` entry).
  Under the opt-in: a literal that must be a `String` is written `s"…"` (the `s` interpolator's
  parts are typed against `String`, so it always yields a `String`); `"…".s` also reads a
  `String` back. A literal keeps `String` where the expected type accepts it — notably as an
  operand of `==` (compare a `Text` with `"…"` freely; `Optional[Text] == "…"` also compiles,
  see vacuous below) — and in positions the compiler types before the expected type is known,
  where `t"…"` is still needed for a `Text`: any argument, at any depth, of an *overloaded*
  method (`proscenium.List(…)` and `Sequence(…)` are overloaded, so `List("a", "b")` is a
  `List[String]`; write `List(t"a", t"b")` or `List[Text]("a", "b")`), an argument of an
  `inline` method with an `inline` parameter (`optional.or("…")`, `.lay("…")`), and the
  right-hand side of a pattern definition (`val (a, b) = …`).

## anticipation

- `anticipation.TextLiterate[str <: String & Singleton]` added: a `scala.Literate[str]` whose
  `Result` is `Text`, and `anticipation.literacy.literate`, the given that supplies it. Both
  are the opt-in described under `build`. The literal's singleton is not carried as a
  refinement (`Text { type Topic = … }`): a literal is exactly a `Text`, so inferred types,
  type-parameter instantiation and `Self`-typed given lookups see what `t"…"` gave them.
- `Printable`'s `Text` instance is `given text: [text <: Text] => text is Printable` (was
  `Text is Printable`), so a subtype of `Text` prints without an upcast.

## vacuous

- `Optional`'s companion gains `CanEqual[Optional[Text], String]` and its mirror
  (`optionalTextEquality`, `equalityOptionalText`), so an `Optional[Text]` may be compared
  with a `String` (in particular with a literal that stayed `String`) using `==`/`!=`.

## stratiform

- `Tels.Layers.compose`, `Tels.Layers.select` and `Tels.validate` take their tactics as an
  explicit `(using Tactic[Tel.Error], Tactic[Resolution.Error])` clause instead of the
  chained `raises Tel.Error raises Resolution.Error` result type; `SchemaResolver.resolve`
  likewise takes `(using Tactic[Tel.Error], Tactic[Bintel.Error], Tactic[ResolutionError])`.
  Call sites with the tactics in scope are unchanged; only references that relied on the
  result being a context function must adapt.

## sibylline

- `Llm.Dialect.stream` returns `Iterator[Event]^{this, caps.any}` (was `^{this}`): the
  iterator retains the live response as well as the dialect, which capture-checked callers
  storing it must now account for.
- `OpenAI`'s dialects' `reply` and `blocks` take `(using Diagnostics, Tactic[Json.Error],
  Tactic[Llm.Error])` instead of a chained `raises` result type, as for stratiform above.

## gesticulate

- `Media.parseTrusted(string: Text): MediaType` added: the runtime entry the `media"…"`
  interpolation expands to (validation happens at compile time). `media"…"` sites are
  unchanged; code that pattern-matched the expansion's shape (an `unsafely(Media.parse(…))`
  call) no longer sees it.
