## Text

### About

`Text` is the string type used throughout Soundness, written with the `t"…"` [interpolator]. It
has a large and consistent set of operations: splitting and joining, slicing, changing case,
trimming, padding, replacing, searching and fuzzy matching. Each is typed so that a mistake is
caught at [compiletime], instead of surfacing as a wrong answer or an exception at [runtime].

`Text` is an [opaque type] over the JVM's `String`, so it costs nothing at runtime. Every
operation is written against the `Textual` [typeclass], so the same methods work on `Text` and
on other textual types alike.

### On text

Java's `String` gathered its methods over decades, and it shows. None of its pitfalls is caught
by the types, so each mistake waits for runtime:

```tabular
column  In Java's `String`
column  The pitfall
column  With `Text`
row  `indexOf`  returns `-1` for "not found"  `offsetOf` returns an `Optional` position
row  `substring`  throws on a bad index  `keep`, `skip`, `snip` and `segment`, counting with ordinals
row  `split`  takes a regular expression where a literal was meant  `cut` takes a `Text`, a `Char` or a regular expression, as typed
row  any method returning a string  may return `null`  nothing returns `null`
```

`Text` presents the same underlying strings through a deliberately designed surface. Three rules
hold throughout:

- Nothing returns `null`. Absence is an [optional](optional-values.md) value.
- Nothing returns a mutable array.
- Nothing takes `Any` and quietly calls `toString` on it. A value must say how it becomes text
  before it can be treated as text.

Everything comes from the `soundness` package. The operations which measure width need a text
metric in scope, and the last import lets the samples print their results:

```scala
import soundness.*
import textMetrics.uniformMetric
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

Wherever an operation depends on a collation, an encoding or a metric, that choice is named by
an import. Nothing about text is assumed, which is
[declarative context](../philosophy/declarative-context.md) at work.

### Writing text

The `t"…"` interpolator builds a `Text`, substituting any value which can be shown:

```scala
val name = t"Hamlet"
```

```scala fuel
run
##
Out.println(t"Hello, $name!")
```

A value with no way of being shown cannot be substituted. Where Java would print something like
`Object@1b6d3586`, this does not compile:

```scala fuel
compile
errors show
##
t"Hello, ${new Object()}!"
```

Prose in source code wants to be wrapped for the programmer, and unwrapped for the output. The
`txt"…"` interpolator joins the lines of a wrapped paragraph with single spaces, and strips the
indentation the source needed. A blank line is a paragraph break, and becomes a single newline:

```scala fuel
run
##
Out.println(txt"""This message is written
                  across several lines.

                  This is its second paragraph.""")
```

### Splitting and joining

`cut` divides text at a delimiter, which may be a `Text`, a `Char` or a
[regular expression](patterns.md). `words` and `lines` are the common cases:

```scala fuel
run
##
Out.println(t"one,two,three".cut(t",").inspect)
Out.println(t"a short sentence".words.inspect)
```

`join` combines a collection of text. It takes an optional separator, and an optional final
separator, for a list which reads naturally in English:

```scala fuel
run
##
Out.println(List(t"one", t"two", t"three").join(t", "))
Out.println(List(t"one", t"two", t"three").join(t", ", t" and "))
```

`join` does the job of the standard library's `mkString`, and differs in the way that matters.
`mkString` calls `toString` on every element without saying so, so a collection of anything at
all produces text of some kind. `join` produces text only from textual elements. A collection
whose elements have no meaningful rendering does not compile. If the elements are themselves
collections, `join` concatenates them, and the separator is then one of *their* elements;
[Collections](collections.md#reshaping) describes that form.

### Slicing

`keep` takes a number of characters and `skip` drops them. Both count from the left by default,
and from the right with `Rtl`. `snip` splits the text at a position, into a pair:

```scala fuel
run
##
Out.println(t"Hello".skip(1))
Out.println(t"Hello".keep(1, Rtl))
Out.println(t"Hello".snip(2).inspect)
```

`segment` takes a range of positions. The positions are
[ordinals](https://en.wikipedia.org/wiki/Ordinal_number), `Prim`, `Sec`, `Ter` and so on, which
count from one. The fifth to the seventh characters are `Quin thru Sept`, so there is no
zero-based index to get wrong by one:

```scala fuel
run
##
Out.println(t"Hello world".segment(Quin thru Sept).inspect)
```

[Collections](collections.md#positions-and-access) uses the same ordinals for positions in a
list.

### Transforming

Trimming, changing case and replacing are each a verb. `sub` replaces one piece of text with
another:

```scala fuel
run
##
Out.println(t"  Hello  ".trim.inspect)
Out.println(t"hello".capitalize)
Out.println(t"LOUD".lower)
Out.println(t"naive".sub(t"i", t"ï"))
```

`keep` also takes a predicate, and keeps the characters which satisfy it:

```scala fuel
run
##
Out.println(t"HELLOworld".keep(_.isUpper))
```

[Characters](characters.md) describes what can be asked of a single character.

### Sorting and comparing text

Text has no natural order. Whether `café` sorts before or after `caff` depends on the
*collation*, and a program which sorts text without choosing one is making a choice it does not
know about. So sorting text, or comparing it with `<`, needs a collation in scope. Without one,
the comparison does not compile:

```scala fuel
compile
errors show
##
t"apple" < t"banana"
```

Two collations are provided:

```tabular
column  Import
column  Orders text
row  `collations.unicodeCollation`  by the Unicode Collation Algorithm: dictionary order, ranking accents before case
row  `collations.codepointCollation`  by code point, which is fast and stable, but places supplementary characters after the whole basic plane
```

```scala
import soundness.collationComparable
import collations.unicodeCollation
import sortingAlgorithms.timsort
```

```scala fuel
run
##
Out.println(List(t"caff", t"café", t"cafe").sort.inspect)
Out.println((t"apple" < t"banana").inspect)
```

[Collections](collections.md#sorting) explains `sort` and the choice of sorting algorithm.

### Case conventions

Text splits into words, and the words rejoin in a naming convention. `uncamel` breaks a
camel-case or Pascal-case identifier into its words, in lower case. `unkebab` and `unsnake` do
the same for the other styles. Four methods join words in each style:

```tabular
column  Method
column  Joins words
row  `kebab`  with hyphens
row  `snake`  with underscores
row  `camel`  capitalizing every word but the first
row  `pascal`  capitalizing every word
```

```scala fuel
run
##
Out.println(t"oneTwoThree".uncamel.kebab)
Out.println(t"oneTwoThree".uncamel.snake)
Out.println(t"one-two-three".unkebab.camel)
Out.println(t"one_two_three".unsnake.pascal)
```

### Padding and fitting

`pad` extends text to a width, `center` pads it on both sides, and `fit` forces an exact width,
truncating if need be. Each measures width through the `Text is Measurable` given in scope.
Here that is `uniformMetric`, which counts every character as one column:

```scala fuel
run
##
Out.println(t"123".pad(5, Bidi.Rtl).inspect)
Out.println(t"123".fit(5, Bidi.Rtl, '.').inspect)
Out.println(t"123456789".fit(5).inspect)
```

[Tables](tables.md) uses these to lay out columns of text.

### Searching

Five methods ask about membership and position:

```tabular
column  Method
column  Answers
row  `contains`  whether the text contains another
row  `starts`  whether the text starts with another
row  `ends`  whether the text ends with another
row  `count`  how many characters satisfy a predicate
row  `offsetOf`  where another text occurs, searching from the left by default
```

`offsetOf` gives an `Optional` position. Text which is not found gives `Unset`, never a number
which could be mistaken for a position:

```scala fuel
run
##
Out.println(t"hello world".contains(t"ello").inspect)
Out.println(t"banana".count(_ == 'a').show)
Out.println(t"hello world".offsetOf(t"o").inspect)
Out.println(t"hello world".offsetOf(t"z").inspect)
```

[Patterns](patterns.md) matches text against regular expressions and globs, and extracts parts
of it.

### Bytes

Text encodes to bytes through the character encoding in scope, and bytes decode back:

```scala
import codepages.utf8Codepage

val bytes = t"Adélaïde".in[Data]
```

```scala fuel
run
##
Out.println(bytes.length.show)
Out.println(bytes.utf8)
```

The eight characters are ten bytes in UTF-8, since each accented letter takes two.
[Characters](characters.md) covers encodings, and [Base encoding](base-encoding.md) writes bytes
as hexadecimal or Base64 text.

### Fuzzy matching

Two pieces of text can be compared for closeness, not just equality, through a `Proximity`
measure in scope. The [Levenshtein](https://en.wikipedia.org/wiki/Levenshtein_distance) distance
counts the single-character edits between them:

```scala
import proximities.levenshteinProximity
import caseSensitivity.caseSensitive
```

```scala fuel
run
##
Out.println(t"Hello world".proximity(t"Hello orld").show)
```

Comparing one word against a whole vocabulary in this way is slow, and a spelling suggestion
needs it to be fast. A `Lexicon` is a [BK-tree](https://en.wikipedia.org/wiki/BK-tree) over the
words. It uses the triangle inequality, which the distance obeys, to rule out most of the
vocabulary without measuring it:

```scala
val words = List(t"book", t"boot", t"cook", t"look", t"bake")
val lexicon = Lexicon(words)
```

A search at distance zero is an exact lookup. Widening the radius admits more distant
candidates, which is how a "did you mean" suggestion is produced from a large word list:

```scala fuel
run
##
Out.println(lexicon.search(t"book", 0).inspect)
Out.println(lexicon.search(t"booq", 1).inspect)
```

### Prefix dictionaries

Where lookup is by exact key, a `Dictionary` maps text to values through a
[trie](https://en.wikipedia.org/wiki/Trie). A lookup costs the length of the key, not a hash of
the whole of it, and a query by prefix costs the length of the prefix:

```scala
val dictionary = Dictionary(t"color" -> 0, t"colour" -> 1)
```

```scala fuel
run
##
Out.println(dictionary(t"colour").inspect)
Out.println(dictionary.size.show)
```

### Graphemes and width

A `Char` is not a unit of text a person would recognize. `é` may be one code point or two, and a
family emoji is several code points joined together, so counting `Char`s answers the wrong
question. `Writing` gives a text's *grapheme clusters*, which are what a reader would call
characters, and the boundaries between them:

```scala fuel
run
##
Out.println(Writing(t"abc").graphemeCount.show)
```

Nor is a grapheme always one column wide on a terminal. Under a metric in scope, `metrics` gives
the width the terminal will actually use. That is one column for ASCII, one for a letter with a
combining accent, and two for Chinese, Japanese and Korean characters and for most emoji:

```scala fuel
run
##
locally:
  import textMetrics.wideCharacterWidthMetric

  Out.println(Grapheme("a").metrics.show)
  Out.println(Grapheme("é").metrics.show)
  Out.println(Grapheme("日").metrics.show)
```

Aligning columns of text in a [terminal](terminal.md) depends on this width.

### Rendering numbers

Turning a number into text involves a choice, of how many digits are worth showing, which
`toString` makes badly. A `Decimalizer` states the choice as a number of significant figures,
and every `show` of a floating-point number consults the one in scope:

```scala fuel
run
##
Out.println(Decimalizer(3).decimalize(-3.14159))
```

The same value therefore renders consistently everywhere in a program, and changing the
precision is a matter of one given, not a search for format strings. [Numbers](numbers.md)
covers the numeric types themselves.

### Any textual type

Every operation above is defined over the `Textual` typeclass, not over `Text` alone. A type
which declares itself textual gains the same `cut`, `join`, `keep`, `pad` and the rest, without
redefining any of them. The styled text of a [terminal](terminal.md) is one such type, so styled
and plain text are manipulated with one vocabulary.
