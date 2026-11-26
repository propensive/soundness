# Patterns which are extended, not changed

A set of patterns is a value. Teaching it a word gives a new set, and the set it was made
from breaks every word as it did before, wherever else it is in use.

```scala fuel
compile
frame
  label The English patterns
  code
      import hyphenations.englishHyphenation

      val standard: Text = t"tomorrow".hyphenate(hyphen = '-')
frame
  label A new set, which knows one word more
  highlight extending  gives a new set of patterns
  code
      import hyphenations.englishHyphenation

      val standard: Text = t"tomorrow".hyphenate(hyphen = '-')

      val players: Hyphenation =
        englishHyphenation.extending(exceptions = List(t"to-morrow"))
frame
  label The same word, under each
  highlight (using..players)  the new set, passed by hand
  code
      import hyphenations.englishHyphenation

      val standard: Text = t"tomorrow".hyphenate(hyphen = '-')

      val players: Hyphenation =
        englishHyphenation.extending(exceptions = List(t"to-morrow"))

      val taught: Text = t"tomorrow".hyphenate(hyphen = '-')(using players)
      val after: Text = t"tomorrow".hyphenate(hyphen = '-')
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(standard)
Out.println(taught)
Out.println(after)
```

[Extending the patterns](../../soundness/modules/hyphenation.md#extending-the-patterns) adds
patterns as well as exceptions, and [immutability](../../philosophy/immutability.md) has the
principle.
