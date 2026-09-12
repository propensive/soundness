## Fonts

### About

A [TrueType or OpenType](https://en.wikipedia.org/wiki/TrueType) font file is binary data with a
well-defined internal structure — tables of glyphs, mappings and metrics — and Soundness reads it
directly. An `Sfnt` loads from any source of bytes, resolves characters to glyphs, and answers the
question typography code most often asks: how wide is this text in this font? Above the file, a
`Typeface` names a font family, a `Face` chooses a weight, slant and features from it, and a
`Typesettable` provision guarantees that a document naming the typeface carries what its medium
needs to show it.

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

### Typefaces and faces

A font file is one thing; the typeface a document *names* is another. `Typeface["Inter"]` is a
typeface by its family name, which is also its type, `Typeface of "Inter"`, so that one typeface
is distinguishable from another as the code compiles. A `Face` says how the typeface is to be set — its weight,
slant and stretch, positions on any further axes of a variable font, and the layout features to
turn on or off — and nothing about where the font comes from:

```scala
val inter = Typeface["Inter"]
val heading = inter.bold.italic
val figures = inter.face.enabling(Face.Feature.TabularNumerals).disabling(Face.Feature.Ligatures)
val display = inter.face.varying(Variation.Axis.OpticalSize, 24.0)
```

Weights are on the 1–1000 scale that CSS and OpenType share, with the usual names from
`Weight.Thin` to `Weight.Black`; a slant is `Upright`, `Italic` or `Oblique` at an angle; a
stretch is a percentage of the normal width. The registered weight, width and italic axes of a
variable font are set through these, so a static and a variable font are asked for alike; a
`Variation` is for optical size and a font's own axes. Features are OpenType layout features by
their tags — `Face.Feature.OldstyleNumerals` is `onum` — with stylistic sets and character
variants numbered.

### Providing a typeface

Naming a typeface does not make it available where the document is shown: a browser without the
font falls back to another, and a PDF viewer substitutes. A provision closes that gap. It is an
instance of `Typesettable` for the typeface *in a medium*, saying how that medium obtains the
font and which faces it can then set, and it is declared once, as a given:

<!-- doccheck: skip -->
```scala
object fonts:
  val interFile: Sfnt = Sfnt(cp"/fonts/Inter-Variable.ttf")

  given inter: (Typeface of "Inter") is Typesettable in Medium = Typesettable.embedded(interFile)
```

A font file can be carried in any document, so an embedded provision is `in Medium`, the medium
of every medium. Its coverage — the weights, slants and widths it offers, the axes of a variable
font and the features its `GSUB` and `GPOS` tables implement — is read from the file, and several
files (regular, bold, italic) make one provision whose coverage is their union. Other kinds of
provision belong to one medium and are declared through it: on the web, `Web.linked` names a font
file at a URL, `Web.imported` a stylesheet that declares the typeface, and `Web.local` a font the
reader has installed, each with the coverage it is declared to have, since there is no file to
read. The CSS generic families need no declaration at all.

### Fonts

A `Font` is a face paired with the provision in scope for its typeface, in a medium: a `Font in
Web` for a browser, a `Font in Print` for a PDF. It is the only form in which a renderer accepts a
font, and it can be made only where a provision is in scope — so a document cannot name a
typeface it does not carry — and only for a face the provision covers, so asking an upright-only
file for italics, or a font without `onum` for oldstyle numerals, is a `Font.Error` at the
pairing rather than a synthesized or silently ignored rendering:

<!-- doccheck: skip -->
```scala
import fonts.inter

val label: Font in Web = Web.font(inter.bold)
```

Where the expected type already names the medium, as a figure's `font` field does, `Font(face)`
suffices. The document then does the rest: an SVG collects the fonts of its lettering and writes
their `@font-face` rules into its definitions, a stylesheet gains them through `Css.fontFace`, and
a PDF embeds the file or names one of its standard fourteen fonts.

### Checking as the code compiles

A font file on the classpath can be checked before anything runs. A provision made from a
classpath resource keeps the resource's path as its `Locus`:

<!-- doccheck: skip -->
```scala
given inter: ((Typeface of "Inter") is Typesettable in Medium at "/fonts/Inter.ttf") =
  Typesettable.embedded(cp"/fonts/Inter.ttf")
```

A face, meanwhile, records what the code asked for in its type: `inter.bold.italic` has the
weight `700` and the slant `"italic"` as type members, and `.enabling(Face.Feature.OldstyleNumerals)`
adds `"onum"` to the features it needs. When a `Font` is made from such a face and such a
provision, the compiler reads the file, derives its coverage exactly as the runtime would, and
refuses the pairing with the same message if the file cannot set the face — a weight it does not
have, an italic it lacks, a feature its tables do not implement. The mistake is then a compile
error, with the file and the request named, rather than a fallback in the browser.

Only what is known statically is checked statically. A weight computed at runtime
(`inter.weighing(weight)`), a feature made from a tag (`Face.Feature(t"ss03")`), a face annotated
as plain `Face of "Inter"`, or a provision without a classpath file all leave the check to the
pairing at runtime, which always runs.

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
