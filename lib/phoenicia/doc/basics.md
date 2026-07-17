All terms and types are defined in the `phoenicia` package:
```scala
import phoenicia.*
```

### Reading a TTF font

A TrueType font, which usually has the `.ttf` extension, can be loaded from any
value [Turbulence](https://github.com/propensive/turbulence/) source that can
be read as `Bytes`, and includes files on disk (using
[Galilei](https://github.com/propensive/galilei/)), classpath resources (using
[Hellenism](https://github.com/propensive/hellenism/)) or URLs (using
[Telekinesis](https://github.com/propensive/telekinesis/)).

A new `Ttf` object can be created using the `Ttf` factory, like so:
```scala
val font = Ttf(url"https://example.com/typography.ttf")
```

This will capture the binary data of the font, but will not interpret it; that
will be performed lazily on different parts of the, only when it is needed.

#### Reading an OTF font

Currently, _Phoenicia_ does not provide any features that are unique to
OpenType fonts; every feature is supported by both TrueType and OpenType fonts.
So it is sufficient to use the `Ttf` factory to load OTF files too. At a later
time, it may be necessary to provide an `Otf` factory as well.

### Operations on `Ttf`s

The `glyph` method takes a `Char` and returns the font's glyph for it, chosen
through the font's preferred character mapping (favouring full-Unicode `cmap`
subtables over Basic Multilingual Plane and legacy mappings). A character the
font does not map yields glyph `0`, the conventional missing-glyph.

The `advanceWidth` method takes a `Char` value and returns its width as a
number of font design units. This width is specifically its _advance width_,
the amount of horizontal distance traveled to accommodate the glyph before the
next glyph; typically a lower value for a narrow character like `l` than for a
wide character like `w`. `leftSideBearing` is its counterpart on the leading
side of the glyph.

A related method, `width`, takes a `Text` value, and returns the sum of the
advance widths of its characters, when rendered at normal spacing. The result
is specified in `ems`, with the type `Quantity[Ems[1]]`.

The names by which the font describes itself are available through `fontName`
(its PostScript name) and `familyName`, decoded from the naming table with a
preference for Windows-English records.

### Font tables

The tables of the font are interpreted on demand, and are accessible through
methods named after their tags: `head` (global metrics, including the glyph
bounding box and `unitsPerEm`), `hhea` and `hmtx` (horizontal metrics), `maxp`
(the glyph count), `post` (the italic angle, underline metrics and fixed-pitch
flag), `os2` (weight, typographic metrics, cap height and x-height where
present, and embedding rights) and `name` (localized naming records, keyed by
`Ttf.NameId`). Glyph outlines are located through `loca` and read from `glyf`,
including the components of composite glyphs. A table absent from the font
raises a `FontError`.

### Subsetting

The `subset` method produces a new `Ttf` containing only the outlines needed
to render a given set of characters — or a `Text` value — while every other
table is carried over unchanged:
```scala
val smaller = font.subset(t"Hello, World!")
```

Glyphs keep their original numbers, so character mappings, metrics and
references between glyphs remain valid; composite glyphs retain their
components (their closure computed with `glyphClosure`), and unused glyphs are
left with empty outlines. The result is a complete, valid font file —
checksums included — suitable for embedding in a PDF or elsewhere.
