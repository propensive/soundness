## PDF

### About

A [PDF](https://en.wikipedia.org/wiki/PDF) document reads as a typed value: its pages and their
geometry, the text laid out on them, the fonts they use, its metadata, bookmarks, links and
attachments. Documents are also edited — content replaced, pages appended or removed, fonts
embedded, metadata and links set — and an edit appends to the file rather than rewriting it, so
the original bytes survive untouched.

Everything is implemented directly against the format: the object syntax, the cross-reference
tables, the stream filters, the encryption. Nothing is delegated to an external PDF library.

### On PDF

PDF's outer layer is COS, a small object language of numbers, names, strings, arrays,
dictionaries and streams, addressed by indirect references and indexed by a cross-reference
table at the end of the file. Its inner layer is a content stream: a stack machine whose
operators place glyphs on a page. Neither layer is hard, and both are unforgiving — a
cross-reference offset that is wrong by one byte, a stream whose declared length disagrees with
its content, an object that is present in an update and absent in the original.

Reading such a format well means never assuming the file is well-formed and never holding more
of it than is needed. A document is *opened*, which maps the file and reads its cross-reference
table; pages, metadata and text are read through that scope; and a stream's payload is pulled
lazily, decoded through its filter chain, rather than materialized on arrival. Where a document
is damaged past the point its cross-reference table can be trusted, it can be recovered by
scanning the file for objects directly.

Reading a document lazily through a scope that owns the file is the shape described under [delimited scopes](../philosophy/delimited-scopes.md).

Everything comes from the `soundness` package, with a text encoding for the content that will
be written:

```scala
import soundness.*
import strategies.throwUnsafely
import charEncoders.utf8Encoder
```

### Creating a document

A document is created at a path, and written within the scope of the block: a fresh document has
a catalog and an empty page tree, and `appendPage` adds a page with the media box it should have.
Boxes are [quantities](quantities.md) in points, not bare numbers, so an A4 page is 595 by 842
points:

```scala
val a4 = Pdf.Rect
  ( Quantity[Points[1]](0.0), Quantity[Points[1]](0.0),
    Quantity[Points[1]](595.0), Quantity[Points[1]](842.0) )

val path = t"/tmp/tutorial.pdf"

path.create[Pdf](CreateFlag.Replace): doc ?=>
  doc.appendPage(a4)
```

The file is written whole to a temporary sibling and moved onto the target atomically, so a
failure part way through leaves nothing behind.

### Opening a document

A document is opened from a path or from bytes, and read within the scope of the block. The
`pdf` accessor reaches the contextual document:

```scala
PdfFile(path).open():
  pdf.version.major   // 1
```

Opening for writing takes the `Read & Write` mode, and yields the document as a handle whose
editing operations are reachable only with the `Write` grant:

```scala
PdfFile(path).open(Read & Write): doc ?=>
  doc.setRotation(doc.page(Prim), Page.Rotation.Quarter)
```

An encrypted document is opened by supplying its password, which is checked as the document is
opened rather than when a string is first decrypted:

<!-- doccheck: skip -->
```scala
PdfFile(path).open(Password(t"open sesame")):
  pdf.info.title
```

The standard security handler is supported through RC4 and AES-256; a public-key handler raises
a `Pdf.Error` naming the encryption it cannot use.

### Pages

The page tree flattens into a sequence, indexed by [ordinal](numbers.md) — `Prim` is the first
page — with each page's geometry resolved against the inheritance the format allows: a media box
declared on the tree root applies to every page beneath it, a crop box defaults to the media box,
and a trim box to the crop box:

```scala
PdfFile(path).open():
  pdf.pageCount                     // 1
  pdf.page(Prim).mediaBox.width     // 595 points, as a Quantity[Points[1]]
  pdf.page(Prim).rotation           // Page.Rotation.Quarter, after the edit above
  pdf.page(Prim).width              // 842 points: the axes exchange when quarter-turned
```

A page's `width` and `height` account for its rotation, and a `/UserUnit` scales the boxes, so
the dimensions read are the ones the page presents.

### Extracting text

A page's `text` runs its content stream and reconstructs the reading order from the positions of
the glyphs: a gap along a baseline becomes a space, a change of baseline becomes a newline, and
adjacent shows run together:

```scala
PdfFile(path).open():
  pdf.page(Prim).text
```

Positioned runs are available too, for a reader that needs coordinates rather than a paragraph.

### Metadata, navigation and attachments

The document information dictionary reads as a `Pdf.Info`, with its dates parsed from PDF's `D:`
format — including its offset, where one is given — and a malformed date reported as absent
rather than as an error. Named destinations resolve through either the modern name tree or the
old `/Dests` dictionary, and bookmarks form a tree of `Bookmark` values:

```scala
PdfFile(path).open():
  pdf.info.title                            // Optional[Text]
  pdf.destinations.at(t"intro")             // Optional[Destination]
  pdf.bookmarks                             // List[Bookmark]
  pdf.attachments.prim.let(_.filename)      // the first attachment's name, if any
  pdf.page(Prim).annotations                // List[Annotation]
```

An annotation is a typed case — `Annotation.Link` with its rectangle and URI, `Annotation.Note`
with its contents — rather than a dictionary to be inspected by key.

### Editing

Editing operations run inside a write scope and are committed as an incremental update: the
original bytes stay where they were, and the changed objects are appended with a fresh
cross-reference section. A reader that only understands the original file still sees a valid
document.

```scala
PdfFile(path).open(Read & Write): doc ?=>
  val operators = List
    ( Pdf.Operator.BeginText, Pdf.Operator.SetFont(t"F1", 12),
      Pdf.Operator.Offset(72, 720), Pdf.Operator.ShowText(winAnsi(t"Written")),
      Pdf.Operator.EndText )

  doc.setContents(doc.page(Prim), operators)
  doc.setInfo(Pdf.Info(t"A Title", t"An Author", Unset, Unset, Unset, Unset, Unset, Unset))
  doc.addLink(doc.page(Prim), a4, uri = t"https://soundness.dev/")
```

`appendPage` and `removePage` change the page tree, `setBox` and `setRotation` change a page's
geometry, and `setBookmarks` and `setAnnotations` replace those structures wholesale. Editing
operations that need a new object use `allocate` and `newStream`, which take the next free
object number.

A [TrueType font](fonts.md) — any `Sfnt`, loaded here from a classpath resource — is embedded
with `embedFont`, which writes the program as a `FontFile2` and builds the simple WinAnsi font
dictionary around it; `addResource` names it on a page so content can select it:

<!-- doccheck: skip -->
```scala
PdfFile(path).open(Read & Write): doc ?=>
  val font = doc.embedFont(Sfnt(cp"/fonts/text.ttf"), t"MyFont")
  doc.addResource(doc.page(Prim), t"Font", t"F1", font)
```

### Stream payloads

A PDF stream's payload is read lazily, through whatever chain of filters the stream declares —
`FlateDecode`, `LZWDecode`, `ASCII85Decode`, `ASCIIHexDecode`, `RunLengthDecode`, and the PNG
and TIFF predictors that often follow them. The payload arrives as a
[stream](streams.md) of bytes, so a large embedded file or image is never held whole:

```scala
PdfFile(path).open():
  pdf(2, 0) match
    case body: Cos.Body => pdf.spring(body)().read[Data].length
    case _              => 0
```

### Recovering a damaged document

Where a cross-reference table is missing or wrong — a truncated download, a file repaired by
another tool — the table is rebuilt rather than trusted: the whole file is scanned for `N G obj`
markers, the latest copy of each object winning, and the trailer recovered from a surviving
`trailer` dictionary or from the catalog itself. Individually shifted offsets are corrected as
objects resolve. A document that any conformant reader would open, opens here too, and is read
exactly like any other.
