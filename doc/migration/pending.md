# Changes since 0.65.0 (pending release)

This file is read by an LLM agent to upgrade code that consumes Soundness libraries. Each
entry states precisely what changed; see `AGENTS.md` for the format. Entries are grouped
by module, most-recently-added last within a module.

## build

- Soundness is compiled with Proscala's `-Zliterate-literals` flag, and every module that can
  reach `anticipation.text` adds `anticipation.literacy` to its `-Yimports` prelude. Under
  that combination a string literal in a Soundness source is typed as `Text` (refined with
  `type Topic = <the literal's singleton type>`) rather than `String`, wherever its expected
  type does not already accept a `String`; a literal passed where `String` is expected is
  unchanged. Consumers are unaffected unless they opt in: pass `-Zliterate-literals` (Proscala
  3.9.0-p16 or later) and bring `anticipation.literacy.literate` into scope (by import, or by
  the same `-Yimports` entry). Under the opt-in, code that calls `String` members on a bare
  literal (`"…".charAt`, `.getBytes`, `.repeat`, `.stripMargin`, `.length`) must write
  `"…".s` or ascribe `("…": String)`; a literal that heads a `+` chain of `String`s or seeds a
  `foldLeft`, `getOrElse`/`.or` default, generic factory (`Set("a")`, `List("a")`) or map key
  must be ascribed likewise when a `String` result is intended, and written `t"…"` when `Text`
  is intended.

## anticipation

- `anticipation.TextLiterate[str <: String & Singleton]` added: a `scala.Literate[str]` whose
  `Result` is `Text { type Topic = str }`, and `anticipation.literacy.literate`, the given that
  supplies it. Both are the opt-in described under `build`.
- `Text` gains an identity `.tt` extension (target name `ttIdentity`): `text.tt` on a value
  that is already `Text` returns it unchanged, so a `"…".tt` that becomes `Text` under the
  opt-in keeps compiling. Transitional; it will be removed once `.tt` on literals is gone.

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
