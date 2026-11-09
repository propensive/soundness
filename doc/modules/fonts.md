## Fonts

### About

A [TrueType or OpenType](https://en.wikipedia.org/wiki/TrueType) font file is binary data with a
well-defined internal structure: tables of glyphs, mappings and metrics. It is read directly
here. An `Sfnt` loads from any source of bytes, resolves characters to glyphs, and answers the
question which typography code most often asks: how wide is this text in this font?

Above the file, a `Typeface` names a font family. A `Face` chooses a weight, slant and features
from it. A `Typesettable` provision guarantees that a document which names the typeface carries
what its medium needs to show it.

### On font metrics

Text does not have a width. Text *in a font* does. Laying out a heading, sizing a button and
breaking a line to fit a measure each need the advance widths which live inside the font file,
in tables indexed through a mapping from characters to glyphs. Reaching them usually means a
rendering toolkit, brought in for what is, at heart, table lookup in a documented binary format.

Here the format is parsed directly. The font is a value, its tables are read lazily, and its
metrics are ordinary method calls. A font parsed from its tables, with every value typed,
follows [safety by construction](../philosophy/safety-by-construction.md): an invalid font is
rejected when it is loaded, and not at first use.

Everything comes from the `soundness` package:

```scala
import soundness.*
import decimalConverters.javaDecimalConverter
import strategies.throwUnsafely
```

The samples on this page read font files which are not shipped with the documentation, so they
are compiled but not run.

### Loading a font

`Sfnt` reads a font from any streamable source of bytes: a file, a classpath resource, a URL.
The name is the format's own. An *sfnt* is the table container which TrueType and OpenType share.
The two specializations are told apart by their tables. A `CFF ` table means PostScript
outlines, so the font is an `Opentype`. Otherwise it is a `Truetype`:

```scala fuel
typecheck
##
import classloaders.threadContextClassloader

val font: Sfnt = Sfnt(t"/fonts/text.ttf".as[Path on Classpath])

val outlines: Text = font match
  case truetype: Truetype => t"TrueType outlines"
  case opentype: Opentype => t"PostScript outlines"
```

Constructing a font from bytes cannot fail. The tables parse lazily, so a file which is not a
font, or which lacks a table an operation needs, raises a `Font.Error` which names the problem
when that table is first read.

### Measuring text

`width` measures a text in the font. It returns a [quantity](quantities.md) in `em`s, the
font-relative unit which multiplies by the point size to give a physical width:

```scala
def measure(font: Sfnt): Quantity[Ems[1]] = font.width(t"Hello world")
```

Since the result is a typed quantity, an em-width cannot be mistaken for a measurement in pixels
or points. Scaling it by a font size is explicit arithmetic.

### Glyphs

Questions about characters go through the glyph machinery. A character resolves to its glyph,
and each glyph carries its advance width and left side bearing, in the design units of the font:

```scala
def metrics(font: Sfnt): (Int, Int) = (font.advanceWidth('H'), font.leftSideBearing('H'))
```

Each table which the font reads is a method of the same name, parsed on first use:

```tabular
column  Table
column  Method
column  Gives
row  `head`  `head`  units per em, relating design units to ems; the glyph bounding box
row  `hhea`  `hhea`  the ascender and descender heights that vertical layout needs
row  `hmtx`  `hmtx`  each glyph's advance width and left side bearing
row  `cmap`  `cmap`  the mapping from characters to glyphs
row  `maxp`  `maxp`  the number of glyphs
row  `name`  `name`  names and descriptions, as `fontName` and `familyName`
row  `post`  `post`  italic angle, underline position and thickness, monospacing
row  `OS/2`  `os2`  weight and width classes, typographic metrics, embedding rights
row  `fvar`  `fvar`  a variable font's axes, or `Unset`
```

The mapping from characters to glyphs covers the `cmap` subtable formats which fonts actually
use, and the best subtable is chosen by Unicode preference, not by taking the first one present.
A character which the font does not map gives the missing glyph, and not an error or a wrong
glyph.

### Names and metadata

A font describes itself in its `name`, `post` and `OS/2` tables, and those descriptions are read
directly rather than guessed at. The names are [optional] values, since a font need not record
them:

```scala
def describe(font: Sfnt): Text =
  t"${font.fontName.or(t"?")} (${font.familyName.or(t"?")}), ${font.post.italicAngle.show}°"
```

Records are decoded from both UTF-16BE and Macintosh encodings, with Windows-English preferred
where several are present. Weight, typographic metrics, x-height and embedding rights come from
`OS/2`, which is exactly what building a [PDF](pdf.md) font descriptor needs.

### Typefaces and faces

A font file is one thing. The typeface which a document *names* is another. `Typeface["Inter"]`
is a typeface by its family name, which is also its type, `Typeface of "Inter"`, so one typeface
is distinguishable from another as the code compiles. A `Face` says how the typeface is to be
set: its weight, slant and stretch, its positions on any further axes of a variable font, and
the layout features to turn on or off. It says nothing about where the font comes from:

```scala
val inter = Typeface["Inter"]
val heading = inter.bold.italic
val figures = inter.face.enabling(Face.Feature.TabularNumerals).disabling(Face.Feature.Ligatures)
val display = inter.face.varying(Variation.Axis.OpticalSize, 24.0)
```

A face has these attributes:

```tabular
column  Attribute
column  Values
column  Set with
row  weight  1–1000, the scale CSS and OpenType share, `Weight.Thin` to `Weight.Black`  `thin` … `black`, `weighing(weight)`
row  slant  `Upright`, `Italic`, or `Oblique` at an angle  `upright`, `italic`, `oblique(angle)`, `slanted(slant)`
row  stretch  a percentage of the normal width  `condensed`, `expanded`, `stretched(stretch)`
row  variation  a position on a variable font's axis  `varying(axis, value)`
row  features  OpenType layout features, on or off  `enabling`, `disabling`, `featuring`
```

The registered weight, width and italic axes of a variable font are set through the first three,
so a static and a variable font are asked for alike. A `Variation` is for optical size and a
font's own axes. Features are named by their OpenType tags. Stylistic sets,
`Face.Feature.stylisticSet(n)`, and character variants, `Face.Feature.characterVariant(n)`, are
numbered:

```tabular
column  Feature
column  Tag
row  `Ligatures`  `liga`
row  `DiscretionaryLigatures`  `dlig`
row  `HistoricalLigatures`  `hlig`
row  `ContextualAlternates`  `calt`
row  `Kerning`  `kern`
row  `OldstyleNumerals`  `onum`
row  `LiningNumerals`  `lnum`
row  `TabularNumerals`  `tnum`
row  `ProportionalNumerals`  `pnum`
row  `SlashedZero`  `zero`
row  `Fractions`  `frac`
row  `Ordinals`  `ordn`
row  `SmallCapitals`  `smcp`
row  `CapitalSmallCapitals`  `c2sc`
row  `Swashes`  `swsh`
row  `Superscript`  `sups`
row  `Subscript`  `subs`
```

### Providing a typeface

Naming a typeface does not make it available where the document is shown. A browser without the
font falls back to another, and a PDF viewer substitutes. A provision closes that gap. It is an
instance of `Typesettable` for the typeface *in a medium*. It says how that medium obtains the
font, and which faces it can then set. It is declared once, as a [given]:

```scala fuel
typecheck
##
object fonts:
  val interFile: Sfnt = Sfnt(t"/fonts/Inter-Variable.ttf".as[Path on Classpath])

  given interProvision: (Typeface of "Inter") is Typesettable in Medium =
    Typesettable.embedded(interFile)
```

A font file can be carried in any document, so an embedded provision is `in Medium`, the medium
of every medium. Its coverage, which is the weights, slants and widths it offers, the axes of a
variable font, and the features which its `GSUB` and `GPOS` tables implement, is read from the
file. Several files, such as regular, bold and italic, make one provision whose coverage is
their union. Other kinds of provision belong to one medium, and are declared through it, each
with the coverage it is declared to have, since there is no file to read:

```tabular
column  Provision
column  Medium
column  The font comes from
column  Coverage
row  `Typesettable.embedded(files)`  `Medium`  the files, carried in the document  read from the files
row  `Web.linked(url, coverage)`  `Web`  a font file at a URL  declared
row  `Web.imported(url, coverage)`  `Web`  a stylesheet that declares the typeface  declared
row  `Web.local(coverage)`  `Web`  a font the reader has installed  declared, or unknown
row  the CSS generic families  `Web`  the browser, with no declaration  every face
row  PDF's standard fourteen  `Print`  the viewer, with no declaration  the faces the standard defines
```

### Fonts

A `Font` is a face paired with the provision in scope for its typeface, in a medium: a
`Font in Web` for a browser, or a `Font in Print` for a PDF. It is the only form in which a
renderer accepts a font. It can be made only where a provision is in scope, so a document cannot
name a typeface which it does not carry. And it can be made only for a face which the provision
covers. Asking an upright-only file for italics, or a font without `onum` for oldstyle numerals,
is a `Font.Error` at the pairing, and not a synthesized or silently ignored rendering:

```scala fuel
typecheck
##
import fonts.interProvision

val label: Font in Web = Web.font(inter.bold)
```

Where the expected type already names the medium, as the `font` field of a figure does,
`Font(face)` suffices. The document then does the rest:

```tabular
column  Document
column  What it does with its fonts
row  SVG  writes `@font-face` rules for its lettering into its definitions
row  stylesheet  gains the rules through `Css.fontFace`
row  PDF  embeds the file, or names one of its standard fourteen fonts
```

### Checking as the code compiles

A font file on the classpath can be checked before anything runs. A provision made from a
classpath resource keeps the path of the resource as its `Locus`:

```scala fuel
parse
##
given interResource: ((Typeface of "Inter") is Typesettable in Medium at "/fonts/Inter.ttf") =
  Typesettable.embedded(cp"/fonts/Inter.ttf")
```

A face, meanwhile, records what the code asked for in its type. `inter.bold.italic` has the
weight `700` and the slant `"italic"` as type members, and
`.enabling(Face.Feature.OldstyleNumerals)` adds `"onum"` to the features it needs. When a `Font`
is made from such a face and such a provision, the compiler reads the file, derives its coverage
exactly as the runtime would, and refuses the pairing with the same message if the file cannot
set the face: a weight it does not have, an italic it lacks, a feature its tables do not
implement. The mistake is then a compile error, with the file and the request named, and not a
fallback in the browser.

Only what is known statically is checked statically. Anything else leaves the check to the
pairing at runtime, which always runs:

```tabular
column  Checked as the code compiles
column  Checked only at runtime
row  a named weight, `inter.bold`  a computed weight, `inter.weighing(weight)`
row  a named feature, `Face.Feature.OldstyleNumerals`  a feature from a tag, `Face.Feature(t"ss03")`
row  a face whose type records its request  a face annotated as plain `Face of "Inter"`
row  a provision from a classpath file  any other provision
```

### Subsetting

Embedding a whole font to render a page of text is wasteful, and often not permitted. `subset`
builds a new TrueType font which contains only the glyphs a given set of characters needs:

```scala
def reduce(font: Truetype): Truetype = font.subset(t"Hello world")
```

Subsetting is not simply a matter of keeping the glyphs which a text maps to. A composite glyph
is assembled from others, so the retained set is the transitive closure under composition. It is
computed from the glyph outlines themselves, so an accented character keeps the components it is
drawn from.

Nor are the glyphs renumbered. Discarded glyphs keep their numbers, and are left with empty
outlines rather than removed, so the character mapping, the metrics and every reference which one
glyph makes to another remain valid without rewriting. Every other table is carried across
unchanged. The result is a complete and valid font file, checksums included, ready to embed in a
[PDF](pdf.md).

### Related

- [PDF](pdf.md) embeds fonts and their subsets in documents.
- [SVG](svg.md) and [CSS](css.md) carry `@font-face` rules for a provision.
- [Quantities](quantities.md) describes the `em` in which text is measured.
- [Classpath](classpath.md) describes the resources which a font is loaded from.
- [Text](text.md) describes the text whose width is measured.
