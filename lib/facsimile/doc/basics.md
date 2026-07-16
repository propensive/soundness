### Opening a PDF

A `PdfFile` is a locator for a PDF, constructed from a path or from bytes in memory:

```scala
val pdfFile = PdfFile(path)   // any path type with an Abstractable instance
val pdfFile2 = PdfFile(data)  // a Data value
```

Nothing is read until `open` is called. Its block receives a contextual `Pdf` value,
available as `pdf`, and the file is held open exactly for the block's duration:

```scala
pdfFile.open():
  pdf.version          // the PDF version from the header
  pdf.trailer          // the trailer dictionary
```

An encrypted document is decrypted transparently; pass the user password to `open`, or
omit it for the common empty-password case:

```scala
pdfFile.open(t"the password"):
  pdf.encrypted        // true
  pdf.pages.head.text  // decrypted like any other
```

The standard security handler is supported at all revisions — RC4, AES-128 and AES-256 —
with a wrong password rejected immediately, when `open` is called.

### The COS object model

Every value in a PDF is a `Cos` object: `Cos.Integral`, `Cos.Real`, `Cos.Name`, `Cos.Chars`
(a string, as raw bytes), `Cos.Truth`, `Cos.Nil`, `Cos.Sequence` (an array),
`Cos.Dictionary`, `Cos.Body` (a stream) or `Cos.Ref` (an indirect reference). Objects are
resolved on demand through the contextual document:

```scala
pdfFile.open():
  val catalog = pdf.resolved(pdf.trailer.at(t"Root").or(Cos.Nil))
  catalog(t"Type") // Cos.Name(t"Catalog")
```

Dictionary access with `apply` returns `Optional` values, treating `null` entries as absent,
per the PDF specification.

### Streams

A `Cos.Body` is a stream *locator*: its dictionary plus the position of its payload in the
file. The decoded payload — passed through its `/Filter` chain — is obtained through the
document:

```scala
pdfFile.open():
  pdf(12, 0) match
    case body: Cos.Body => pdf.payload(body) // decoded Data
    case _              => ()
```

Decoded `Data` and parsed `Cos` values are pure and may escape the `open` block; the `Pdf`
itself, and anything which still resolves lazily through it, may not.

For a large payload — an image or an embedded file — `pdf.spring(body)` yields a
re-materializable `Spring[Data]` instead: each application mints a fresh streaming pull
endpoint which reads and decodes incrementally, never holding the whole payload. The
endpoint reads through the document, so it is likewise confined to the scope.

### Pages

The page tree is flattened into reading order, with the inheritable attributes — resources,
boxes and rotation — applied along each path. Page geometry is expressed in typesafe
lengths: a PDF point is exactly 1/72 inch, `quantitative`'s `Points` unit.

```scala
pdfFile.open():
  val page = pdf.pages(0)
  page.mediaBox.width   // Quantity[Points[1]]
  page.rotation         // Page.Rotation.Quarter, for /Rotate 90
  page.width            // rotation-aware display width
  page.annotations      // typed Link, Note and Widget annotations
```

A `Page` still resolves through the document, so — like the `Pdf` — it cannot leave the
`open` block; everything extracted from it can.

### Content, fonts and text

A page's content parses into a typed operator AST, and its fonts — including embedded
TrueType and OpenType programs, which surface as Phoenicia `Ttf`s — are materialized as pure
values:

```scala
pdfFile.open():
  val page = pdf.pages(0)
  page.operators       // List[PdfOperator]: Concat(PdfMatrix(...)), ShowText(...), ...
  page.fonts           // Map[Text, PdfFont], keyed by resource name
  page.text            // the page's plain text, in content order
  page.runs            // List[TextRun]: decoded text with font, size and position in points
```

Text extraction interprets the full text machinery — transformation and text matrices,
character and word spacing, horizontal scaling, leading and kerning — decoding bytes through
each font's `/ToUnicode` map, or its declared encoding and `/Differences`.

### Document structure

Metadata and navigation structures are fully materialized as pure values:

```scala
pdfFile.open():
  pdf.info.title        // Optional[Text]
  pdf.info.created      // Optional[PdfInfo.Timing]: a Timestamp and optional UTC offset
  pdf.bookmarks         // the outline tree, with Destinations
  pdf.destinations      // named destinations
  pdf.attachments       // embedded files (read `data` inside the scope)
  pdf.pageLabel(0.z)    // "i", "42", "A-7"...
  pdf.xmp               // the raw XMP packet, if any
```
