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
a length; and the operations read as plain verbs. Because `Text` is opaque over `String`, the
safety is free — the two are the same bytes at runtime. Everything comes from the `soundness`
package, with a text-metrics given in scope for the operations that measure width:

```scala
import soundness.*
import textMetrics.uniformMetric
```

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
t"123".pad(5, Rtl)         // t"  123"
t"123".fit(5, Rtl, '.')    // t"..123"
```

### Searching

Membership and position are asked with `contains`, `starts`, `ends`, `count`, and `offsetOf`,
the last returning an `Optional` position rather than a sentinel:

```scala
t"hello world".contains(t"ello")   // true
t"hello world".offsetOf(t"o")      // an Optional position, present here
t"banana".count(t"a")              // 3
```

### Bytes

Text encodes to bytes through the character encoding in scope, and bytes decode back:

```scala
import charEncoders.utf8Encoder

val bytes = t"Adélaïde".data   // the UTF-8 bytes
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

### Any textual type

Every operation above is defined over the `Textual` typeclass, not over `Text` alone. A type
that describes itself as textual — an ANSI-styled string, for instance — gains the same `cut`,
`join`, `keep`, `pad` and the rest without redefining any of them, so styled and plain text
are manipulated with one vocabulary.
