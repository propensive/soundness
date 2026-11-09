## Hyphenation

### About

Breaking a word at the end of a line is acceptable only at certain points, and finding those
points is the job of [hyphenation](https://en.wikipedia.org/wiki/Syllabification). Words are
hyphenated with the same algorithm, and the same pattern files, as
[TeX](https://en.wikipedia.org/wiki/TeX), whose typesetting the technique was invented for.
A word gains its permissible break points, is hyphenated with a chosen character, or is
split into its syllables.

The patterns of a language are a [contextual value], and American English is provided. The
set of patterns can be extended, or replaced from any TeX pattern file, where a word breaks
in a way which the standard patterns do not know.

### On hyphenation

Where a word may break is not something which an algorithm can read from its spelling.
*hy-phen-ation* breaks between syllables which no simple rule identifies. Liang's algorithm,
which was built for TeX, answers this with a dictionary of thousands of small scoring
patterns, derived from real hyphenation dictionaries. It remains the standard, because it is
accurate and fast, and its pattern files exist for many languages.

That algorithm is implemented here, against the standard pattern files, and is offered as
three operations on [text](text.md), with the language supplied as a [given]:

```tabular
column  Operation
column  Returns
column  For
row  `hyphenate`  the `Text` with a hyphen character at each permissible break  text to be set in lines
row  `syllables`  a `Sequence[Text]` of syllables and runs of non-letters  analysis of a word's parts
row  `breakPoints`  an `Array[Int]` of break positions  code that places the breaks itself
```

Breaking text at the right points is a pure function of the language's patterns, and stays
one, which is [immutability](../philosophy/immutability.md) in a domain which tempts
stateful iteration.

Everything comes from the `soundness` package, with the English patterns in scope. The last
import lets the samples print their results:

```scala
import soundness.*
import hyphenations.englishHyphenation
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Hyphenating

`hyphenate` inserts a hyphen character at each permissible break. The default is the
Unicode [soft hyphen](https://en.wikipedia.org/wiki/Soft_hyphen), which is invisible unless
a line actually breaks there. Passing a visible character shows the break points:

```scala fuel
run
##
Out.println(t"hyphenation".hyphenate(hyphen = '-'))
Out.println(t"presentation".hyphenate(hyphen = '-'))
```

`leftMin` and `rightMin` keep a minimum number of characters attached before the first
break, and after the last, so that a break never strands one or two letters:

```scala fuel
run
##
Out.println(t"hyphenation".hyphenate(hyphen = '-', leftMin = 4))
Out.println(t"presentation".hyphenate(hyphen = '-', leftMin = 4))
```

### Syllables

`syllables` splits text into its parts. Each syllable is a segment, and each run of
characters which are not letters passes through as a segment of its own:

```scala fuel
run
##
Out.println(t"the algorithm".syllables.to[List].inspect)
```

### Break points

`breakPoints` gives the raw positions at which a word may break. It is for code which places
the breaks itself, such as an algorithm for wrapping lines, instead of inserting characters:

```scala fuel
run
##
Out.println(t"hyphenation".breakPoints.to[List].inspect)
```

### Text that is not one word

Hyphenation applies to letters, and text is rarely only letters. Each run of letters is
hyphenated independently. Everything else, such as spaces, punctuation and digits, passes
through untouched:

```scala fuel
run
##
Out.println(t"hello, world!".hyphenate(hyphen = '-'))
Out.println(t"the algorithm runs".hyphenate(hyphen = '-'))
```

Both words of the first text are too short to break. `syllables` follows the same rule, and
gives the runs which are not letters as segments of their own, so putting the segments back
together reproduces the original text exactly.

### Extending the patterns

A word from a specialist field, which the standard patterns break wrongly, or not at all,
is taught with an extra pattern, in TeX's notation, layered over the existing set:

```scala fuel
run
##
locally:
  given hyphenation: Hyphenation = englishHyphenation.extending(patterns = List(t"klm9nop"))
  Out.println(t"klmnop".hyphenate(hyphen = '-'))
```

Patterns are merged by taking the higher score at each position. An added pattern with an
odd score therefore restores a break which a standard pattern with an even score had
forbidden. That is how one word is corrected, without disturbing the rest of the language.

Some words no pattern gets right. An *exception* states a word's breaks outright, and takes
precedence over the algorithm:

```scala fuel
run
##
locally:
  given hyphenation: Hyphenation = englishHyphenation.extending(exceptions = List(t"hy-phenation"))
  Out.println(t"hyphenation".hyphenate(hyphen = '-'))
```

The standard English set already has the list of exceptions which TeX ships, so `associate`
breaks as `as-so-ciate`, and `table` as `ta-ble`, where the patterns alone would not.

A whole language is supplied in the same way. `Hyphenation.fromTex` reads any TeX pattern
file, so the languages which are published for TeX are available by loading their patterns.

### When no patterns are available

A language for which no set of patterns has been loaded hyphenates nothing, and does not
guess. `hyphenate` gives the text unchanged, and `breakPoints` is empty. An algorithm for
wrapping lines therefore falls back to breaking at spaces, which is the correct behavior,
and does not invent breaks in a language whose rules it does not know.

### Related

- [Tables](tables.md) wraps the text of a column with hyphenation.
- [Text](text.md) describes the other operations on text.
- [PDF](pdf.md) and [Pages](pages.md) set paragraphs which break across lines.
