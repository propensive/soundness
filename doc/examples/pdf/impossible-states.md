# The first page is not page zero, or page one

Whether pages are counted from zero or from one is a question every PDF library answers in
its documentation. Here a page is found by an ordinal, and the first is `Prim`, so a bare
number, which could mean either, is not accepted.

```scala fuel
compile
frame
  label A page by number
  errors show
  code
      import strategies.throwUnsafely, systems.javaBaseSystem

      def opening(folio: Path on Local): Text =
        PdfFile(folio).open():
          pdf.page(1).text
frame
  label A page by ordinal
  highlight Prim  the first, however pages are numbered
  code
      import strategies.throwUnsafely, systems.javaBaseSystem

      def opening(folio: Path on Local): Text =
        PdfFile(folio).open():
          pdf.page(Prim).text
##
```

The example is compiled, and is not run, since it reads a file.
[Pages](../../soundness/modules/pdf.md#pages) describes a page and its boxes, and
[impossible states](../../philosophy/impossible-states.md) the principle.
