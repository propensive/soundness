# A distance whose type follows its measure

The closeness of two pieces of text is measured by the `Proximity` in scope. An edit distance
is a whole number and a similarity is a fraction, so the type of the result is that of the
measure which was imported.

```scala fuel
compile
alternative An edit distance
  highlight Int  a count of edits
  code
      import proximities.levenshteinProximity
      import caseSensitivity.caseSensitive

      val closeness: Int = t"Rosalind".proximity(t"Rosaline")
alternative A similarity
  highlight Double  a fraction between zero and one
  code
      import proximities.jaroProximity
      import caseSensitivity.caseSensitive

      val closeness: Double = t"Rosalind".proximity(t"Rosaline")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(closeness.inspect)
```

[Fuzzy matching](../../soundness/modules/text.md#fuzzy-matching) builds a spelling suggestion
on these measures.
