# A document which may have no title

The information dictionary of a PDF is often incomplete, and sometimes missing. Each of its
fields is an optional value, so a title cannot be shown without saying what is shown when
there is none.

```scala fuel
compile
frame
  label A title, used as though every document had one
  errors show
  code
      import strategies.throwUnsafely, systems.javaBaseSystem

      def title(folio: Path on Local): Text =
        PdfFile(folio).open():
          pdf.info.title
frame
  label The author in its place, and a word for neither
  highlight or(pdf.info.author).or(t"Anonymous")  each absence is answered in turn
  code
      import strategies.throwUnsafely, systems.javaBaseSystem

      def title(folio: Path on Local): Text =
        PdfFile(folio).open():
          pdf.info.title.or(pdf.info.author).or(t"Anonymous")
##
```

The example is compiled, and is not run, since it reads a file.
[Metadata, navigation and attachments](../../soundness/modules/pdf.md#metadata-navigation-and-attachments)
describes what is read, and [optionality](../../philosophy/optionality.md) describes `or`.
