# An order for text, chosen once

Text has no natural order: whether `café` sorts before `caff` depends on the collation. A
comparison does not compile until an import has chosen one.

```scala fuel
compile
frame
  label No collation has been chosen
  errors show
  code
      val earlier = t"café" < t"caff"
frame
  label Code-point order
  highlight collations.codepointCollation  by code point
  code
      import soundness.collationComparable
      import collations.codepointCollation

      val earlier = t"café" < t"caff"
frame
  label Dictionary order
  highlight collations.unicodeCollation  as a dictionary would
  code
      import soundness.collationComparable
      import collations.unicodeCollation

      val earlier = t"café" < t"caff"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(earlier.inspect)
```

By code point, `é` comes after every unaccented letter, and the answer would be `false`. In
dictionary order it is `true`.
[Sorting and comparing text](../../soundness/modules/text.md#sorting-and-comparing-text)
describes the two collations, and
[declarative context](../../philosophy/declarative-context.md) why the choice is an import.
