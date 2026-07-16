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
