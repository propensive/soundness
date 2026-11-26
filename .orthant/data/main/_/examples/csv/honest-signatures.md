# Delimited by what?

Text does not say whether its cells are divided by commas, tabs or spaces. Reading a sheet
needs a format in scope, and without one it does not compile, so a delimiter is never guessed.

```scala fuel
compile
frame
  label A sheet in no stated format
  errors show
  code
      import strategies.throwUnsafely

      val sheet: Sheet = t"Bottom,261\nQuince,121".read[Sheet]
frame
  label Comma-separated values
  highlight csvFormat  commas, and quoting with "
  code
      import strategies.throwUnsafely
      import dsvFormats.csvFormat

      val sheet: Sheet = t"Bottom,261\nQuince,121".read[Sheet]
##
```

[Formats](../../soundness/modules/csv.md#formats) lists the formats, and
[honest signatures](../../philosophy/honest-signatures.md) explains the demand.
