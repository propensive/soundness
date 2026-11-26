# The language of a hyphenation

Where a word may break depends on its language, and the patterns of a language are chosen by
an import. With none in scope nothing is broken, since no break is ever guessed, and a set of
patterns can be given a word of its own.

```scala fuel
compile
alternative No patterns
  code
      val word: Text = t"presentation".hyphenate(hyphen = '-')
alternative English
  highlight englishHyphenation  the patterns which TeX uses
  code
      import hyphenations.englishHyphenation

      val word: Text = t"presentation".hyphenate(hyphen = '-')
alternative English, with an exception
  highlight extending  the same patterns, and one word
  code
      given theatrical: Hyphenation =
        hyphenations.englishHyphenation.extending(exceptions = List(t"present-ation"))

      val word: Text = t"presentation".hyphenate(hyphen = '-')
##
```

The three give `presentation`, `pre-sen-ta-tion` and `present-ation`.
[Extending the patterns](../../soundness/modules/hyphenation.md#extending-the-patterns) has
more on exceptions, and [declarative context](../../philosophy/declarative-context.md) the
reasons for choosing by import.
