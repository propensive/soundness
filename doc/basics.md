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

The only useful operation currently implemented on `Ttf` instances is the
`advanceWidth` method, which takes a `Char` value and returns its width as a
number of font design units. This width is specifically its _advance width_,
the amount of horizontal distance traveled to accommodate the glyph before the
next glyph; typically a lower value for a narrow character like `l` than for a
wide character like `w`.

A related method, width, takes a `Text` value, and returns the sum of the
advance widths of its characters, when rendered at normal spacing. The result
is specified in `ems`, with the type `Quantity[Ems[1]]`.

