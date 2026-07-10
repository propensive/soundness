## Hyphenation

### About

Breaking a word at the end of a line is only acceptable at certain points, and finding those
points is the job of [hyphenation](https://en.wikipedia.org/wiki/Syllabification). Soundness
hyphenates words with the same algorithm and pattern files as [TeX](https://en.wikipedia.org/wiki/TeX),
whose typesetting the technique was invented for: a word gains its permissible break points, is
hyphenated with a chosen character, or is split into its syllables.

The language's patterns are a contextual value, with American English provided, and the pattern
set can be extended — or replaced from any TeX pattern file — where a word breaks in a way the
standard patterns do not know.

### On hyphenation

Where a word may break is not something an algorithm can read off its spelling: *hy-phen-ation*
breaks between syllables that no simple rule identifies. Liang's algorithm, built for TeX,
answers this with a dictionary of thousands of small scoring patterns derived from real
hyphenation dictionaries, and it remains the standard because it is accurate, fast, and its
pattern files exist for many languages.

Soundness implements that algorithm against the standard pattern files, and exposes it as three
operations on text, with the language supplied as a given. Everything comes from the `soundness`
package, with the English patterns in scope:

```scala
import soundness.*
import hyphenations.englishHyphenation
```

### Hyphenating

`hyphenate` inserts a hyphen character at each permissible break. The default is the Unicode
[soft hyphen](https://en.wikipedia.org/wiki/Soft_hyphen), which is invisible unless a line
actually breaks there; passing a visible character shows the break points:

```scala
t"hyphenation".hyphenate(hyphen = '-')    // t"hy-phen-ation"
t"presentation".hyphenate(hyphen = '-')   // t"pre-sen-ta-tion"
```

`leftMin` and `rightMin` keep a minimum number of characters attached before the first break and
after the last, so a break never strands one or two letters:

```scala
t"hyphenation".hyphenate(hyphen = '-', leftMin = 4)   // t"hyphen-ation"
```

### Syllables

`syllables` splits text into its parts, each syllable a segment; runs of non-letters pass
through as segments of their own:

```scala
t"the algorithm".syllables.to(Seq)
// Seq(t"the", t" ", t"al", t"go", t"rithm")
```

### Break points

`breakPoints` gives the raw positions where a word may break, for code — a line-wrapping
algorithm, say — that places the breaks itself rather than inserting characters:

```scala
t"hyphenation".breakPoints   // the positions of the permissible breaks
```

### Extending the patterns

A domain word that the standard patterns break wrongly, or not at all, is taught with an extra
pattern in TeX's notation, layered over the existing set:

```scala
given Hyphenation = englishHyphenation.extending(patterns = Seq(t"klm9nop"))

t"klmnop".hyphenate(hyphen = '-')   // t"klm-nop"
```

A whole language is supplied the same way: `Hyphenation.fromTex` reads any TeX hyphenation
pattern file, so the languages published for TeX are available by loading their patterns.
