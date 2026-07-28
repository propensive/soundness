## Fonts

### About

A [TrueType or OpenType](https://en.wikipedia.org/wiki/TrueType) font file is binary data with a
well-defined internal structure — tables of glyphs, mappings and metrics — and Soundness reads it
directly. A `Ttf` loads from any source of bytes, resolves characters to glyphs, and answers the
question typography code most often asks: how wide is this text in this font?

### On font metrics

Text does not have a width; text *in a font* does. Laying out a heading, sizing a button, breaking
a line to fit a measure — each needs the advance widths that live inside the font file, in tables
indexed through a character-to-glyph mapping. Reaching them usually means a rendering toolkit
brought in for what is, at heart, table lookup in a documented binary format.

Soundness parses the format itself: the font is a value, its tables read lazily, and its metrics
are ordinary method calls. Everything comes from the `soundness` package:

```scala
import soundness.*
import strategies.throwUnsafely
```

### Loading a font

`Ttf` reads a font from any streamable source of bytes — a file, a classpath resource, a URL. The
same reader serves OpenType fonts, which share the table structure:

```scala
val font = Ttf(cp"/fonts/text.ttf")
```

A file that is not a font, or lacks a table an operation needs, raises a `FontError` naming the
problem.

### Measuring text

`width` measures a text in the font, returning a quantity in `em`s — the font-relative unit that
multiplies by the point size to give a physical width:

```scala
val measure = font.width(t"Hello world")   // a Quantity[Ems[1]]
```

Because the result is a typed [quantity](quantities.md), an em-width cannot be mistaken for a
pixel or point measurement; scaling it by a font size is explicit arithmetic.

### Glyphs

Character-level questions go through the glyph machinery: a character resolves to its glyph, and
each glyph carries its advance width and left side bearing, in the font's design units:

```scala
font.advanceWidth('H')     // the horizontal advance, in font units
font.leftSideBearing('H')
```

The font's header exposes the scaling factor — units per em — that relates design units to em
measurements, along with the glyph bounding box and the ascender and descender heights that
vertical layout needs.

Character-to-glyph mapping covers the `cmap` subtable formats fonts actually use, and the best
subtable is chosen by Unicode preference rather than by taking the first one present. A character
the font does not map yields the missing glyph, rather than an error or a wrong glyph.

### Names and metadata

A font describes itself in its `name`, `post` and `OS/2` tables, and those descriptions are read
directly rather than guessed at:

```scala
font.fontName      // the PostScript name
font.familyName    // the family name
font.post.italicAngle
font.os2.capHeight
```

Records are decoded from both UTF-16BE and Macintosh encodings, preferring Windows-English where
several are present. Weight, typographic metrics, x-height and embedding rights come from `OS/2`,
which is exactly what building a [PDF](pdf.md) font descriptor needs.

### Subsetting

Embedding a whole font to render a page of text is wasteful, and often not permitted. `subset`
builds a new font containing only the glyphs a given set of characters needs:

```scala
val reduced = font.subset(t"Hello world")
```

Subsetting is not simply a matter of keeping the glyphs a text maps to. A composite glyph is
assembled from others, so the retained set is the transitive closure under composition — computed
from the glyph outlines themselves, so an accented character keeps the components it is drawn
from.
