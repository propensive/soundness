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

### Damaged files

Real-world PDFs are frequently damaged — a missing or corrupt cross-reference table,
prepended junk shifting every recorded offset, a truncated tail. When the cross-reference
machinery cannot be trusted, Facsimile falls back to scanning the whole file for objects and
rebuilding the table, and it corrects individually shifted offsets as objects are resolved,
so a document that any conformant reader would open still opens here.

### Editing

An on-disk document opened with the `Write` grant may be edited; the changes are written as a
PDF *incremental update* — appended to the file, leaving the original bytes untouched — when
the scope closes. Write operations are available on the granted handle (named in the block,
since the grant refines its type), and every read view reflects a change at once:

```scala
import aperture.*

pdfFile.open(Read & Write): doc ?=>
  doc.set(Cos.Ref(2, 0), Cos.Dictionary(Map(t"Value" -> Cos.Chars(t"edited".in[Data]))))
  val added = doc.allocate(Cos.Dictionary(Map(t"Type" -> Cos.Name(t"Metadata"))))
  doc.free(Cos.Ref(9, 0))
// the changed, added and freed objects are appended, chained via /Prev, on exit
```

An on-disk file with a valid cross-reference table can be edited in place; opening in-memory
data writably, or a file whose table could only be recovered by scanning, raises
`PdfError.Reason.WriteUnsupported`. An encrypted document is edited transparently — pass its
password to `open`, and new and edited objects are encrypted with the document's key as the
incremental update is written.

Higher-level page and content edits sit on top of the same kernel: a page's content is set
from a list of typed `PdfOperator`s (serialised to a fresh content stream), and its boxes,
rotation and other entries are set directly. Pages can be appended and removed, and every
change is reflected in the read views at once — `page.text`, `page.rotation`, `pdf.pages`.

```scala
pdfFile.open(Read & Write): doc ?=>
  val page = doc.pages(0)
  doc.setContents(page, List(BeginText, SetFont(t"F1", 12), Offset(72, 720),
      ShowText(t"Hello".in[Data]), EndText))
  doc.setRotation(page, Page.Rotation.Quarter)
  doc.appendPage(mediaBox, operators, resources)
  doc.removePage(doc.pages(2))
```

Document-level structures can be edited too: `setInfo` replaces the information dictionary
(title, author, dates), `setBookmarks` rebuilds the outline tree, and `setAnnotations` /
`addLink` edit a page's annotations.

```scala
pdfFile.open(Read & Write): doc ?=>
  doc.setInfo(PdfInfo(t"New title", t"Author", Unset, Unset, Unset, Unset, Unset, Unset))
  doc.setBookmarks(List(Bookmark(t"Chapter 1", Destination.Fit(Prim), Nil)))
  doc.addLink(doc.pages(0), rect, uri = t"https://soundness.dev/")
```

A font can be embedded from a Phoenicia `Ttf` and used in content, as a simple WinAnsi
TrueType font. Its `FontDescriptor` is built from the font's own tables — bounding box,
italic angle, cap height, pitch and style — its name defaults to the font's PostScript name,
and `winAnsi` encodes text for the `ShowText` operand:

```scala
pdfFile.open(Read & Write): doc ?=>
  val font = doc.embedFont(ttf)
  doc.addResource(doc.pages(0), t"Font", t"F1", font)
  doc.setContents(doc.pages(0), List(BeginText, SetFont(t"F1", 12), Offset(72, 720),
      ShowText(winAnsi(t"Hello")), EndText))
```

Passing `subset` embeds only the glyphs the given text needs — composite components
included, with glyph numbering retained — under the conventional six-letter subset tag:

```scala
doc.embedFont(ttf, subset = t"Hello")  // /BaseFont /ABCDEF+FontName
```

### Creating a document

A new document is authored with `create[Pdf]`, through the same write surface as editing:

```scala
import aperture.*, galilei.CreateFlag

path.create[Pdf](): doc ?=>
  doc.appendPage(a4, operators, resources)
  doc.setInfo(PdfInfo(t"Title", Unset, Unset, Unset, Unset, Unset, Unset, Unset))
// written in full to the target on scope exit
```

The document is written whole to a temporary sibling and moved atomically onto the target,
so a failure leaves nothing behind; `CreateFlag.Replace` and `CreateFlag.Parents` govern a
pre-existing target and missing parent directories.

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
