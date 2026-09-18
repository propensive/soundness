# Changes since 0.67.0

This file is read by an LLM agent to upgrade code that consumes Soundness libraries from
0.67.0 to the next release. Each entry states precisely what changed; see `AGENTS.md` for the
format. Entries are grouped by module, most-recently-added last within a module.

## anticipation

- `anticipation.Chroma#red` changed semantics: it is now `inline def red: Int = (chroma >>
  16)&255`, previously `inline def red: Int = chroma >> 16`. `green` and `blue` were already
  masked and are unchanged. Code must be audited where a `Chroma` was constructed by
  `Chroma(value: Int)` from an `Int` with any bit set above the low twenty-four (including a
  negative `Int`), since `red` previously returned those bits and now discards them. `Chroma`
  values originating from `Chroma(red: Int, green: Int, blue: Int)`, the `rgb"…"` interpolator,
  `iridescence.Pixel#chroma`, `iridescence.Rgb12#chroma` or `iridescence.Rgb32#chroma` are
  unaffected, as all of those already masked. (#2036)

## ultimatum

- `ultimatum.GaugePalette.hue(chroma: anticipation.Chroma): iridescence.Color in
  iridescence.Srgb` removed. Replace `hue(c)` with `c.color`, the extension `extension (chroma:
  anticipation.Chroma) def color: Color in Srgb` declared in `iridescence` and exported by the
  `soundness` umbrella as `color`. Results are identical. (#2036)
