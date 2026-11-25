# An edit needs a document opened for writing

Turning a page changes the file, so `setRotation` asks for the `Write` grant. A document which
was opened to be read does not have it, and the edit is refused before the file is touched.

```scala fuel
compile
frame
  label An edit to a document opened for reading
  errors show
  code
      import strategies.throwUnsafely, systems.javaBaseSystem

      def turn(folio: Path on Local): Unit =
        PdfFile(folio).open(Read): doc ?=>
          doc.setRotation(doc.page(Prim), Page.Rotation.Quarter)
frame
  label The document, opened for writing
  highlight Read..Write  the grant which every edit asks for
  code
      import strategies.throwUnsafely, systems.javaBaseSystem

      def turn(folio: Path on Local): Unit =
        PdfFile(folio).open(Read & Write): doc ?=>
          doc.setRotation(doc.page(Prim), Page.Rotation.Quarter)
##
```

The example is compiled, and is not run, since it edits a file.
[Editing](../../soundness/modules/pdf.md#editing) lists the operations, and
[honest signatures](../../philosophy/honest-signatures.md) explains why each names what it
needs.
