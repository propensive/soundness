# The delimiter is an import

Whether cells are divided by commas or tabs, and whether the first row names the columns, is
decided by the format in scope. The code which reads a row is the same under each.

```scala fuel
compile
alternative Commas
  highlight csvFormat  commas
  code
      import strategies.throwUnsafely
      import dsvFormats.csvFormat

      case class Weaver(first: Text, last: Text)

      val bottom = t"Nick,Bottom".read[Weaver in Dsv]
alternative Tabs
  highlight tsvFormat  tabs
  code
      import strategies.throwUnsafely
      import dsvFormats.tsvFormat

      case class Weaver(first: Text, last: Text)

      val bottom = t"Nick\tBottom".read[Weaver in Dsv]
alternative A header row
  highlight csvWithHeaderFormat  columns matched by name
  code
      import strategies.throwUnsafely
      import dsvFormats.csvWithHeaderFormat

      case class Weaver(first: Text, last: Text)

      val bottom = t"last,first\nBottom,Nick".read[Weaver in Dsv]
##
```

With a header, each column goes to the field of its name, in whatever order the columns come.
[Formats](../../soundness/modules/csv.md#formats) lists the formats, and
[declarative context](../../philosophy/declarative-context.md) says why the format is not an
argument.
