# A minimum which is left to the language

The fewest letters which may stand before the first break is a matter of typography, and each
language's patterns have their own answer. `leftMin` is an optional parameter: left unset it
means the language's minimum, and no magic number stands for "the default".

```scala fuel
compile
frame
  label The minimum of the patterns
  code
      import hyphenations.englishHyphenation

      val word: Text = t"presentation".hyphenate(hyphen = '-')
frame
  label A minimum of one's own
  highlight leftMin..4  four letters before the first break
  code
      import hyphenations.englishHyphenation

      val word: Text = t"presentation".hyphenate(hyphen = '-')
      val wider: Text = t"presentation".hyphenate(hyphen = '-', leftMin = 4)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(word)
Out.println(wider)
```

[Hyphenating](../../soundness/modules/hyphenation.md#hyphenating) has `rightMin` too, and
[optionality](../../philosophy/optionality.md) explains why absence is a value.
