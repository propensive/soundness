# A document is read while it is open

A PDF is not loaded; it is opened, and its pages are read from the file as they are asked for.
The `pdf` accessor is the open document, and it means something only inside the block which
holds the file.

```scala fuel
compile
frame
  label A document which nothing has opened
  errors show
  code
      import strategies.throwUnsafely, systems.javaBaseSystem

      def pages(folio: Path on Local): Int = pdf.pageCount
frame
  label The same question, within the scope of an open file
  highlight open():  the file is held until the block ends
  code
      import strategies.throwUnsafely, systems.javaBaseSystem

      def pages(folio: Path on Local): Int =
        PdfFile(folio).open():
          pdf.pageCount
##
```

The example is compiled, and is not run, since it reads a file.
[Opening a document](../../soundness/modules/pdf.md#opening-a-document) describes the scope,
and [delimited scopes](../../philosophy/delimited-scopes.md) the pattern.
