# A font which need not name itself

The name of a font is a record in one of its tables, and a font file need not have it. The
name is therefore an optional value, and a caption for the font says what is shown when
there is none.

```scala fuel
compile
frame
  label A name, used as though every font had one
  errors show
  code
      import strategies.throwUnsafely

      def caption(font: Sfnt): Text = font.fontName
frame
  label A name, or a word for its absence
  highlight or(t"untitled")  what absence means here
  code
      import strategies.throwUnsafely

      def caption(font: Sfnt): Text = font.fontName.or(t"untitled")
##
```

The example is compiled, and is not run, since it needs a font file.
[Names and metadata](../../soundness/modules/fonts.md#names-and-metadata) describes the tables
which are read, and [optionality](../../philosophy/optionality.md) describes `or`.
