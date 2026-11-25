# The first row of a sheet which may be empty

A sheet may have no rows, so its first row is an optional value. It cannot be used as
though it were there, and `let` decodes it if it is.

```scala fuel
compile
frame
  label A row which may be absent, used as a row
  errors show
  code
      import strategies.throwUnsafely
      import dsvFormats.csvFormat

      case class Weaver(first: Text, last: Text)

      def first(sheet: Sheet): Dsv = sheet.rows(Prim)
frame
  label Decoded if it is there
  highlight let(_.as[Weaver])  applies only to a row which is there
  highlight Optional[Weaver]  absent for an empty sheet
  code
      import strategies.throwUnsafely
      import dsvFormats.csvFormat

      case class Weaver(first: Text, last: Text)

      def first(sheet: Sheet): Optional[Weaver] = sheet.rows(Prim).let(_.as[Weaver])
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(first(t"Nick,Bottom\nFrancis,Flute".read[Sheet]).inspect)
Out.println(first(t"".read[Sheet]).inspect)
```

[Decoding to a case class](../../soundness/modules/csv.md#decoding-to-a-case-class) has the
rest, and [optionality](../../philosophy/optionality.md) the reasons.
