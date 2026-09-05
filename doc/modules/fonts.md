## Fonts

### About

A [TrueType or OpenType](https://en.wikipedia.org/wiki/TrueType) font file is binary data with a
well-defined internal structure — tables of glyphs, mappings and metrics — and Soundness reads it
directly. An `Sfnt` loads from any source of bytes, resolves characters to glyphs, and answers the
question typography code most often asks: how wide is this text in this font?

### On font metrics

Text does not have a width; text *in a font* does. Laying out a heading, sizing a button, breaking
a line to fit a measure — each needs the advance widths that live inside the font file, in tables
indexed through a character-to-glyph mapping. Reaching them usually means a rendering toolkit
brought in for what is, at heart, table lookup in a documented binary format.

A font parsed directly from its tables, with every value typed, follows [safety by construction](../philosophy/safety-by-construction.md): an invalid font is rejected on loading, not at first use.

Soundness parses the format itself: the font is a value, its tables read lazily, and its metrics
are ordinary method calls. Everything comes from the `soundness` package:

```scala
import soundness.*
import strategies.throwUnsafely
```

### Loading a font

`Sfnt` reads a font from any streamable source of bytes — a file, a classpath resource, a URL.
The name is the format's own: an *sfnt* is the table container that both TrueType and OpenType
share, and the two specializations are told apart by their tables — a `CFF ` table means
PostScript outlines, so the font is an `Opentype`; otherwise it is a `Truetype`:

<!-- doccheck: skip -->
```scala
val font: Sfnt = Sfnt(cp"/fonts/text.ttf")

font match
  case truetype: Truetype => truetype.subset(t"Hello")   // glyph outlines are TrueType
  case opentype: Opentype => opentype                    // outlines are PostScript
```

Constructing a font from bytes is total; the tables parse lazily, so a file that is not a font,
or lacks a table an operation needs, raises a `Font.Error` naming the problem when that table is
first read.

### Measuring text

`width` measures a text in the font, returning a quantity in `em`s — the font-relative unit that
multiplies by the point size to give a physical width:

```scala
def measure(font: Sfnt): Quantity[Ems[1]] = font.width(t"Hello world")
```

Because the result is a typed [quantity](quantities.md), an em-width cannot be mistaken for a
pixel or point measurement; scaling it by a font size is explicit arithmetic.

### Glyphs

Character-level questions go through the glyph machinery: a character resolves to its glyph, and
each glyph carries its advance width and left side bearing, in the font's design units:

```scala
def metrics(font: Sfnt): (Int, Int) = (font.advanceWidth('H'), font.leftSideBearing('H'))
```

The font's `head` table exposes the scaling factor — units per em — that relates design units to
em measurements, along with the glyph bounding box, and `hhea` the ascender and descender heights
that vertical layout needs.

Character-to-glyph mapping covers the `cmap` subtable formats fonts actually use, and the best
subtable is chosen by Unicode preference rather than by taking the first one present. A character
the font does not map yields the missing glyph, rather than an error or a wrong glyph.

### Names and metadata

A font describes itself in its `name`, `post` and `OS/2` tables, and those descriptions are read
directly rather than guessed at; the names are `Optional`, since a font need not record them:

```scala
def describe(font: Sfnt): Text =
  t"${font.fontName.or(t"?")} (${font.familyName.or(t"?")}), ${font.post.italicAngle.show}°"
```

Records are decoded from both UTF-16BE and Macintosh encodings, preferring Windows-English where
several are present. Weight, typographic metrics, x-height and embedding rights come from `OS/2`,
which is exactly what building a [PDF](pdf.md) font descriptor needs.

### Subsetting

Embedding a whole font to render a page of text is wasteful, and often not permitted. `subset`
builds a new TrueType font containing only the glyphs a given set of characters needs:

```scala
def reduce(font: Truetype): Truetype = font.subset(t"Hello world")
```

Subsetting is not simply a matter of keeping the glyphs a text maps to. A composite glyph is
assembled from others, so the retained set is the transitive closure under composition — computed
from the glyph outlines themselves, so an accented character keeps the components it is drawn
from.

Nor are the glyphs renumbered. Discarded glyphs keep their numbers and are left with empty
outlines rather than being removed, so the character mapping, the metrics and every reference one
glyph makes to another remain valid without rewriting; every other table is carried across
unchanged. The result is a complete and valid font file, checksums included, ready to embed in a
[PDF](pdf.md).
