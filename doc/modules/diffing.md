## Diffing

### About

A [diff](https://en.wikipedia.org/wiki/Diff) describes how one sequence becomes another. `diff`
takes two sequences and returns a `Diff`: an [immutable] value which lists the insertions, the
deletions and the unchanged elements that connect them.

A diff can be applied to the original as a patch, or inverted. It can be written in the familiar
unix format and read back. It can also be refined into an *aligned* diff, which pairs each
deletion with the insertion that replaced it.

### On differences

Version control, synchronization and test output all depend on diffs. They are usually met as
text: the output of a tool, read by eye or picked apart with a regular expression.

But a diff is data. It is a precise, minimal edit script, computed by
[Myers' algorithm](http://www.xmailserver.org/diff2.pdf), and it is useful wherever two versions
of any sequence must be reconciled. Here it is a value over elements of any type, not only lines
of text, and the caller may replace equality with a looser test of similarity.

Everything comes from the `soundness` package. The second import chooses a
[strategy](errors.md#choosing-a-strategy) for errors, and the third lets the samples print their
results:

```scala
import soundness.*
import strategies.throwUnsafely
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
import Diff.{Del, Ins, Par, Sub}
```

### Computing a diff

`diff` compares two sequences, and returns the edit script:

```scala
val letters = diff(Sequence('A', 'C'), Sequence('A', 'B', 'C'))
```

Each edit is an insertion (`Diff.Ins`), a deletion (`Diff.Del`) or an unchanged element
(`Diff.Par`), and holds
its position in the left sequence, the right sequence, or both. The edits of a `Diff` are an
ordinary sequence, which a pattern match takes apart:

```scala fuel
run
##
letters.edits.each:
  case Par(left, right, value) => Out.println(t"  ${value.inspect}  left $left, right $right")
  case Ins(right, value)       => Out.println(t"+ ${value.inspect}  right $right")
  case Del(left, value)        => Out.println(t"- ${value.inspect}  left $left")
```

`A` and `C` are in both sequences, and `B` was inserted at position 1 of the right one. The
value of a `Par` or a `Del` is an [optional] value, since a diff need not carry the elements it
keeps or removes.

The script is minimal: it holds the fewest insertions and deletions which turn the left sequence
into the right. The elements may be characters, lines of text, or records. A third argument to
`diff` replaces equality with a comparison of the caller's choosing.

### Applying a patch

A diff applied to the original sequence gives the target. A difference computed once can
therefore be sent elsewhere and replayed:

```scala
val original = List(t"Macbeth", t"Duncan", t"Banquo")
val revised = List(t"Macbeth", t"Malcolm", t"Macduff", t"Banquo")

val changes = diff
  ( Sequence(t"Macbeth", t"Duncan", t"Banquo"),
    Sequence(t"Macbeth", t"Malcolm", t"Macduff", t"Banquo") )
```

```scala fuel
run
##
changes.patch(original).each(Out.println(_))
```

`flip` inverts a diff. Each insertion becomes a deletion, and each deletion an insertion, so
the patch which went forward becomes the patch which goes back.

### Aligned differences

A plain diff reports a changed element as a deletion and an insertion. For display, and for
comparing records, it is more useful to *pair* the two, and to see the new element as a
modification of the old.

`rdiff` does this. It takes a predicate which says whether two elements are similar, and gives
`Sub` entries where a deletion and an insertion match. Here two words are similar when their
[edit distance](text.md) is small:

```scala
import proximities.levenshteinProximity
import caseSensitivity.caseSensitive

val italian = Sequence(t"zero", t"uno", t"due", t"tre")
val spanish = Sequence(t"cero", t"uno", t"dos", t"tres")
```

```scala fuel
run
##
diff(italian, spanish).rdiff(_.proximity(_) < 4).changes.each:
  case Sub(_, _, left, right) => Out.println(t"~ ${left.inspect} became ${right.inspect}")
  case Par(_, _, value)       => Out.println(t"  ${value.inspect}")
  case Ins(_, value)          => Out.println(t"+ ${value.inspect}")
  case Del(_, value)          => Out.println(t"- ${value.inspect}")
```

Similar words pair as substitutions. Only an element which is really new, or really gone, would
remain as an insertion or a deletion.

Between them, `diff` and `rdiff` produce four kinds of edit:

```tabular
column  Edit
column  Records
column  Positions
column  In
row  `Ins`  an element of the right sequence only  right  `Diff`, `RDiff`
row  `Del`  an element of the left sequence only  left  `Diff`, `RDiff`
row  `Par`  an element common to both  left and right  `Diff`, `RDiff`
row  `Sub`  a left element paired with its right replacement  left and right  `RDiff` only
```

Pairing is done within each changed *region*, and only where a region holds at least one
deletion and at least one insertion, since only there is a substitution possible. Within such a
region, the order in which deletions and insertions are applied does not change the result. That
leaves the algorithm free to pair some of them, and to arrange the rest around the pairs.

A second parameter, `subSize`, covers the case where similarity is the wrong test. A short
region with equal numbers of insertions and deletions becomes substitutions whatever the
predicate says. By default this applies to a region with one of each. At that size the
correspondence is obvious, even when the two values have nothing in common as text.

### The unix format

A diff of text is written in the conventional format with `serialize`:

```scala fuel
run
##
changes.serialize.each(Out.println(_))
```

The same format is read back into a `Diff`, so a patch file is readable data:

```scala fuel
run
##
val patch: Chain[Text] = changes.serialize
patch.read[Diff[Text]].patch(original).each(Out.println(_))
```

### Redrafts

The unix format is exact. A patch must state its context and its line numbers correctly, or it
does not apply. That suits a version-control system. It does not suit a person, or a
[language model](llm.md), proposing an edit in prose.

A *redraft* is the forgiving form. It states only the lines to remove and the lines to add.
Unchanged lines are left out, and the redraft finds where its lines belong:

```scala
val source = Sequence(t"line1", t"line2", t"line3")
```

```scala fuel
run
##
Redraft.parse(Chain(t"- line2", t"+ new line 2a")).patch(source).each(Out.println(_))
```

An addition with no context goes at the start:

```scala fuel
run
##
Redraft.parse(Chain(t"+ line0")).patch(source).each(Out.println(_))
```

A redraft may name a line which is not in the source at all. That is reported as an error, and
the edit is not applied by guesswork:

```scala fuel
run
errors show
##
Redraft.parse(Chain(t"- line9", t"+ line10")).patch(source)
```

### Evolution

A diff compares two versions. An *evolution* follows each element through many. A value which is
in the first version and the last is recognized as the same value, even if it disappeared and
came back in between:

```scala
val versions = List(List('d', 'o', 'g'), List('c', 'a', 't'), List('d', 'o', 'g'))
val evolution = evolve(versions)
```

Each version is addressed by an ordinal, and is reconstructed exactly as it was:

```scala fuel
run
##
Out.println(evolution(Ter).inspect)
```

This is the structure which a document history, an undo stack or a series of drafts needs.

### Related

- [Text](text.md) describes `proximity`, the edit distance used above to decide similarity.
- [Testing](testing.md) shows a diff when two compared values are not equal.
- [Git](git.md) works with the diffs of a repository.
- [Collections](collections.md) describes `Sequence`, `Chain` and ordinals such as `Ter`.
- [Immutability](../philosophy/immutability.md) explains why a diff is a value.
