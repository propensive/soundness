## Diffing

### About

Comparing two sequences and describing how one becomes the other is
[diffing](https://en.wikipedia.org/wiki/Diff), and Soundness implements it as a pure function:
`diff` takes two sequences and returns a `Diff`, an immutable value listing the insertions,
deletions and unchanged elements that connect them. A diff can be applied to the original as a
patch, inverted, serialized in the familiar unix format and parsed back, and refined into an
*aligned* diff that pairs up deletions with the insertions that replaced them.

### On differences

The diff is one of computing's quiet workhorses — version control, synchronization, test output —
and it is almost always consumed as text, the output of a tool, parsed by eye or by regex. Yet a
diff is data: a precise, minimal edit script computed by
[Myers' algorithm](http://www.xmailserver.org/diff2.pdf), useful far beyond source files whenever
two versions of any sequence must be reconciled.

A diff is an immutable value that can be inspected, rendered or applied, in keeping with [immutability](../philosophy/immutability.md) everywhere else.

Soundness computes it as a value over any elements, not just lines of text, with equality — or a
looser similarity — supplied by the caller. Everything comes from the `soundness` package:

```scala
import soundness.*
import strategies.throwUnsafely
```

### Computing a diff

`diff` compares two sequences and returns the edit script. Each edit is an insertion (`Ins`), a
deletion (`Del`), or an unchanged element (`Par`), with the positions in each sequence:

```scala
diff(Sequence('A', 'C'), Sequence('A', 'B', 'C'))
// Diff(Par(0, 0, 'A'), Ins(1, 'B'), Par(1, 2, 'C'))
```

The script is minimal — the fewest insertions and deletions that turn the left sequence into the
right. The elements may be anything: characters, lines of text, records. A third argument
replaces equality with a comparison of the caller's choosing.

### Applying a patch

A diff applied to the original sequence yields the target, so a difference computed once can be
transmitted and replayed:

```scala
val original = List(t"foo", t"bar", t"baz")
val revised = List(t"foo", t"quux", t"bop", t"baz")

val changes = diff(Sequence(t"foo", t"bar", t"baz"), Sequence(t"foo", t"quux", t"bop", t"baz"))
changes.patch(original)   // List(t"foo", t"quux", t"bop", t"baz")
```

`flip` inverts a diff, turning the patch that goes forward into the one that goes back.

### Aligned differences

A raw diff reports a changed element as a deletion plus an insertion, but for display — and for
comparing structured records — it is more useful to *pair* them, recognizing the new element as a
modification of the old. `rdiff` does this, taking a similarity predicate and producing `Sub`
entries where a deletion and insertion match:

```scala
import proximities.levenshteinProximity
import caseSensitivity.caseSensitive

val italian = Sequence(t"zero", t"uno", t"due", t"tre")
val spanish = Sequence(t"cero", t"uno", t"dos", t"tres")

diff(italian, spanish).rdiff(_.proximity(_) < 4)
// RDiff(Sub(0, 0, t"zero", t"cero"), Par(1, 1, t"uno"),
//       Sub(2, 2, t"due", t"dos"), Sub(3, 3, t"tre", t"tres"))
```

Similar elements — here, words within a small edit distance — pair as substitutions, and only the
genuinely new or removed remain as insertions and deletions.

The pairing is done per changed *region*, and only where a region contains at least one deletion
and at least one insertion, since only there is a substitution possible. Within such a region the
order in which deletions and insertions are applied does not affect the result, which is what
gives the algorithm the freedom to pair some of them and arrange the rest around the pairs.

A second parameter, `subSize`, catches the case where similarity is the wrong test. A short
region with equal numbers of insertions and deletions — one of each, by default — becomes
substitutions regardless of what the predicate says, because at that size the correspondence is
obvious even when the values have nothing textually in common.

### The unix format

A diff of text serializes to the conventional format tools expect, and that format parses back to
a `Diff`, so a patch file is readable data:

```scala
val patch: Chain[Text] = changes.serialize   // the lines of a unix diff

patch.read[Diff[Text]].patch(original)      // List(t"foo", t"quux", t"bop", t"baz")
```

### Redrafts

The unix format is precise and unforgiving: a patch must state its context exactly, with the right
line numbers, or it does not apply. That is right for a version-control system and wrong for a
person — or a language model — proposing an edit in prose.

A *redraft* is the forgiving form. It states only the lines to remove and the lines to add, with
unchanged lines omitted entirely, and finds where they belong:

```scala
val source = Sequence(t"line1", t"line2", t"line3")

Redraft.parse(Chain(t"- line2", t"+ new line 2a")).patch(source)
// List(t"line1", t"new line 2a", t"line3")

Redraft.parse(Chain(t"+ line0")).patch(source)
// List(t"line0", t"line1", t"line2", t"line3")
```

Where a redraft is ambiguous — the lines it names appear more than once — that is reported rather
than resolved by guessing, so an edit is never applied in the wrong place.

### Evolution

A diff compares two versions. An *evolution* tracks an element through many, so that a value
present in the first version and the last can be recognized as the same value even where it
disappeared and returned in between:

```scala
val versions = List(List('d', 'o', 'g'), List('c', 'a', 't'), List('d', 'o', 'g'))
val evolution = evolve(versions)

evolution(Ter)   // List('d', 'o', 'g'): the third version, reconstructed
```

Each version is addressed by ordinal, and reconstructing one gives back exactly what it was — the
structure a document history, an undo stack, or a series of drafts wants.
