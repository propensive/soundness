# Text which is not one word

Hyphenation is defined for any text, and not only for a word. Each run of letters is broken
on its own, and spaces, punctuation and digits pass through, so nothing need be split up
before it is hyphenated or put together again afterwards.

```scala fuel
compile
frame
  label A word
  code
      import hyphenations.englishHyphenation

      val word: Text = t"tomorrow".hyphenate(hyphen = '-')
frame
  label A line
  highlight line:..hyphenate  each run of letters, on its own
  code
      import hyphenations.englishHyphenation

      val word: Text = t"tomorrow".hyphenate(hyphen = '-')
      val line: Text = t"Tomorrow, and tomorrow, and tomorrow!".hyphenate(hyphen = '-')
frame
  label Its parts
  highlight syllables  the commas and spaces are parts too
  code
      import hyphenations.englishHyphenation

      val word: Text = t"tomorrow".hyphenate(hyphen = '-')
      val line: Text = t"Tomorrow, and tomorrow, and tomorrow!".hyphenate(hyphen = '-')
      val parts: List[Text] = t"Tomorrow, and".syllables.to[List]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(line)
Out.println(parts.inspect)
```

[Text that is not one word](../../soundness/modules/hyphenation.md#text-that-is-not-one-word)
has the rule, and [total transitions](../../philosophy/total-transitions.md) the principle.
