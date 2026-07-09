## Diffing

### About

Comparing two sequences and describing how one becomes the other is
[diffing](https://en.wikipedia.org/wiki/Diff), and Soundness implements it as a pure function:
`diff` takes two sequences and returns a `Diff`, an immutable value listing the insertions,
deletions and unchanged elements that connect them. A diff can be applied to the original as a
patch, inverted, serialized in the familiar unix format and parsed back, and refined into an
*aligned* diff that pairs up deletions with the insertions that replaced them.

### On differences

The diff is one of computing's quiet workhorses — version control, synchronisation, test output —
and it is almost always consumed as text, the output of a tool, parsed by eye or by regex. Yet a
diff is data: a precise, minimal edit script computed by
[Myers' algorithm](http://www.xmailserver.org/diff2.pdf), useful far beyond source files whenever
two versions of any sequence must be reconciled.

Soundness computes it as a value over any elements, not just lines of text, with equality — or a
looser similarity — supplied by the caller. Everything comes from the `soundness` package:

```scala
import soundness.*
```

### Computing a diff

`diff` compares two sequences and returns the edit script. Each edit is an insertion (`Ins`), a
deletion (`Del`), or an unchanged element (`Par`), with the positions in each sequence:

```scala
diff(t"AC".chars, t"ABC".chars)
// Diff(Par(0, 0, 'A'), Ins(1, 'B'), Par(1, 2, 'C'))
```

The script is minimal — the fewest insertions and deletions that turn the left sequence into the
right.

### Applying a patch

A diff applied to the original sequence yields the target, so a difference computed once can be
transmitted and replayed:

```scala
val changes = diff(original, revised)
changes.patch(original)   // the revised sequence
```

`flip` inverts a diff, turning the patch that goes forward into the one that goes back.

### Aligned differences

A raw diff reports a changed element as a deletion plus an insertion, but for display — and for
comparing structured records — it is more useful to *pair* them, recognising the new element as a
modification of the old. `rdiff` does this, taking a similarity predicate and producing `Sub`
entries where a deletion and insertion match:

```scala
import proximities.levenshteinProximity

val italian = IArray(t"zero", t"uno", t"due", t"tre")
val spanish = IArray(t"cero", t"uno", t"dos", t"tres")

diff(italian, spanish).rdiff(_.proximity(_) < 4)
// RDiff(Sub(0, 0, t"zero", t"cero"), Par(1, 1, t"uno"),
//       Sub(2, 2, t"due", t"dos"), Sub(3, 3, t"tre", t"tres"))
```

Similar elements — here, words within a small edit distance — pair as substitutions, and only the
genuinely new or removed remain as insertions and deletions.

### The unix format

A diff of text serializes to the conventional format tools expect, and that format parses back to
a `Diff`, so a patch file is readable data:

```scala
import strategies.throwUnsafely

changes.serialize            // a Stream[Text] in unix diff format
diffStream.read[Diff[Text]]  // parsed back to a value
```
