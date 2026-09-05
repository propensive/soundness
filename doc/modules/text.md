## Text

### About

`Text` is the string type used throughout Soundness, written with the `t"…"` interpolator.
It carries a large, coherent surface of operations — splitting and joining, slicing, case
conversion, trimming, padding, replacing, searching, fuzzy matching — each typed so that a
mistake is caught as the code compiles rather than surfacing as a wrong answer or an
exception at runtime.

`Text` is an [opaque](https://docs.scala-lang.org/scala3/reference/other-new-features/opaques.html)
alias for the JVM's `String`, so it costs nothing at runtime, and every operation is written
against a `Textual` abstraction, so the same methods work on `Text` and on other textual
types alike.

### On text

Java's `String` gathered its methods over decades, and they show it. `indexOf` returns `-1`
for "not found", `substring` throws on a bad index, `split` takes a regular expression where
a literal was meant, and `null` lurks behind a returned string. None of this is caught by
the types; each mistake waits for runtime.

`Text` presents the same underlying strings through a deliberately designed surface. Absence
is an `Optional`, not a magic `-1`; a search returns a position that cannot be confused with
a length; and the operations read as plain verbs. Three rules hold throughout: nothing returns
`null`, nothing returns a mutable array, and nothing takes `Any` and quietly calls `toString` on
it. That last rule is why a value must say how it becomes text before it can be treated as text. Because `Text` is opaque over `String`, the
safety is free — the two are the same bytes at runtime. Everything comes from the `soundness`
package, with a text-metrics given in scope for the operations that measure width:

```scala
import soundness.*
import textMetrics.uniformMetric
```

Text operations that require a collation, an encoding or a metric to be named are [declarative context](../philosophy/declarative-context.md): nothing about text is assumed.

### Writing text

The `t"…"` interpolator builds a `Text`, substituting any value that can be shown. The
`txt"…"` interpolator additionally folds runs of whitespace — including newlines — into single
spaces, for writing a long message across several lines of source:

```scala
val name = t"Ada"
t"Hello, $name!"          // t"Hello, Ada!"

txt"""This message is written
      across several lines."""   // t"This message is written across several lines."
```

### Splitting and joining

`cut` divides text at a delimiter — a `Text`, a `Char`, or a regular expression — returning
the pieces; `words` and `lines` are the common cases:

```scala
t"one,two,three".cut(t",")   // List(t"one", t"two", t"three")
t"a short sentence".words    // List(t"a", t"short", t"sentence")
```

`join` combines a collection of text, optionally with a separator, and optionally with the
distinct final separator that reads naturally in English:

```scala
List(t"one", t"two", t"three").join(t", ")            // t"one, two, three"
List(t"one", t"two", t"three").join(t", ", t" and ")  // t"one, two and three"
```

`join` is the counterpart of the standard library's `mkString`, and differs in the way that
matters: `mkString` calls `toString` on every element without saying so, so a collection of
anything at all produces text of some kind. `join` requires the elements to be textual, so a
collection whose elements have no meaningful rendering does not compile rather than rendering
badly.

### Slicing

`keep` and `skip` take or drop a number of characters, from the left by default or from the
right with `Rtl`; `snip` splits at a position into a pair:

```scala
t"Hello".skip(1)          // t"ello"
t"Hello".keep(1, Rtl)     // t"o"
t"Hello".snip(2)          // (t"He", t"llo")
```

A range of positions is taken with `segment`, using [ordinals](https://en.wikipedia.org/wiki/Ordinal_number)
— `Prim`, `Sec`, `Ter` and so on — that count from one, so an off-by-one slip is hard to make:

```scala
t"Hello world".segment(Quin thru Sept)   // t"o w"
```

### Transforming

Case conversion, trimming, and replacement each read as a verb. `sub` replaces one piece of
text with another:

```scala
t"  Hello  ".trim           // t"Hello"
t"hello".capitalize         // t"Hello"
t"LOUD".lower               // t"loud"
t"naive".sub(t"i", t"ï")    // t"naïve"
```

Character-level filters keep or drop by a predicate:

```scala
t"HELLOworld".keep(_.isUpper)   // t"HELLO"
```

### Sorting and comparing text

Text has no natural order: whether `café` sorts before or after `caff` depends on the collation,
and a program that sorts text without saying which is making a choice it does not know it is
making. So sorting text, or comparing it with `<`, needs a *collation* in scope, and without one
the sort does not compile. `unicodeCollation` applies the Unicode Collation Algorithm — dictionary
order, ranking accents before case — and `codepointCollation` orders by code point, which is fast
and stable but places supplementary characters after the whole basic plane:

```scala
import collations.unicodeCollation

List(t"caff", t"café", t"cafe").sort   // List(t"cafe", t"café", t"caff")
t"apple" < t"banana"                    // true
```

### Case conventions

Text splits into words and rejoins in a naming convention. `uncamel` breaks a camel-case or
Pascal-case identifier into its words, and `kebab`, `snake`, `camel` and `pascal` join words
in the corresponding style:

```scala
t"oneTwoThree".uncamel.kebab   // t"one-two-three"
```

### Padding and fitting

`pad` extends text to a width, `center` pads on both sides, and `fit` forces an exact width,
truncating if need be. Each measures width through the `Text is Measurable` given in scope —
here `uniformMetric`, which counts every character as one column:

```scala
t"123".pad(5, Bidi.Rtl)         // t"  123"
t"123".fit(5, Bidi.Rtl, '.')    // t"..123"
```

### Searching

Membership and position are asked with `contains`, `starts`, `ends`, `count`, and `offsetOf`,
the last returning an `Optional` position rather than a sentinel:

```scala
t"hello world".contains(t"ello")   // true
t"hello world".offsetOf(t"o")      // an Optional position, present here
t"banana".count(_ == 'a')          // 3
```

### Bytes

Text encodes to bytes through the character encoding in scope, and bytes decode back:

```scala
import charEncoders.utf8Encoder

val bytes = t"Adélaïde".in[Data]   // the UTF-8 bytes
bytes.utf8                     // t"Adélaïde"
```

### Fuzzy matching

Two pieces of text can be compared for closeness rather than equality, through a `Proximity`
measure in scope. The [Levenshtein](https://en.wikipedia.org/wiki/Levenshtein_distance)
distance counts the single-character edits between them:

```scala
import proximities.levenshteinProximity

t"Hello world".proximity(t"Hello orld")   // 1
```

Comparing one word against a whole vocabulary that way is quadratic, and a spelling suggestion
needs it to be fast. A `Lexicon` is a
[BK-tree](https://en.wikipedia.org/wiki/BK-tree) over the words, which uses the triangle
inequality the distance metric obeys to prune most of the vocabulary without measuring it:

```scala
val words = List(t"book", t"boot", t"cook", t"look", t"bake")
val lexicon = Lexicon(words)

lexicon.search(t"book", 0)   // exact matches only
lexicon.search(t"booq", 1)   // everything within one edit
```

A search at distance zero is an exact lookup; widening the radius admits progressively more
distant candidates, which is how a "did you mean" suggestion is produced from a large word list.

### Prefix dictionaries

Where lookup is by exact key rather than by proximity, a `Dictionary` maps text to values through
a trie, so a lookup costs the length of the key rather than a hash of the whole of it, and a
prefix query costs the length of the prefix:

```scala
val dictionary = Dictionary(t"color" -> 0, t"colour" -> 1)

dictionary(t"color")     // 0
dictionary.size          // 2
```

### Graphemes and width

A character is not a unit of text a person would recognize. `é` may be one code point or two, and
a family emoji is several joined together, so counting `Char`s answers the wrong question.
`Writing` gives a text's *grapheme clusters* — what a reader would call characters — and the
boundaries between them:

```scala
Writing(t"abc").graphemeCount      // 3
Writing(t"").boundaries.length     // 1: the single boundary at position 0
```

Nor is a grapheme one column wide on a terminal. Under a metric in scope, `metrics` gives the
display width the terminal will actually use — one for ASCII, one for a letter with a combining
accent, two for CJK and for most emoji — which is what aligning columns of text requires:

```scala
import textMetrics.wideCharacterWidthMetric

Grapheme("a").metrics    // 1
Grapheme("é").metrics    // 1 — a base plus a combining mark
Grapheme("日").metrics   // 2
```

### Rendering numbers

Turning a number into text involves a choice — how many digits are worth showing — that
`toString` makes badly. A `Decimalizer` in scope states it as significant figures, and is what
every `show` of a floating-point number consults:

```scala
Decimalizer(3).decimalize(-3.14159)   // t"-3.14"
```

The same value therefore renders consistently everywhere in a program, and changing the precision
is one given rather than a search for format strings.

### Multi-line literals

Prose in source code wants to be wrapped for the reader, and unwrapped for the output. The
`txt"…"` interpolator collapses a wrapped paragraph into a single line, treats a blank line as a
paragraph break, and strips the indentation the source needed:

```scala
txt"""Hello
      world"""      // t"Hello world"

txt"""Hello

      world"""      // t"Hello\nworld"
```

### Any textual type

Every operation above is defined over the `Textual` typeclass, not over `Text` alone. A type
that describes itself as textual — an ANSI-styled string, for instance — gains the same `cut`,
`join`, `keep`, `pad` and the rest without redefining any of them, so styled and plain text
are manipulated with one vocabulary.
