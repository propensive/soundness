# Naming

Names in Soundness are chosen with care, because a well-named thing reads correctly and
a badly-named one obscures every line that uses it. Methods, types, and contextual
values are named so that the expressions they form read as natural language, and
consistent conventions mean a name carries the same meaning wherever it appears. Good
naming is inseparable from making code read like [elegant prose](elegant-prose.md): the
right name in the right place removes the need for a comment to explain it.

Names are also unique across the whole of Soundness. Every public type has one meaning
in the `soundness` namespace: there is one `Path`, one `Message`, one `Error`, and a
module needing a distinct concept coins a distinct name — `Teletype`, not another
`String`; `GitHash`, not another `Hash`. Uniqueness is what makes the umbrella import
possible: `import soundness.*` never forces a choice between two things called the same,
a bare name in documentation or an error message is unambiguous, and moving code between
modules never silently changes which `Path` it means. Where the same word genuinely
suits two things, one of them is renamed rather than qualified forever.

Descriptiveness extends to the smallest names. Type parameters are words, not letters:
a method is generic over `element`, `format`, `duration` or `plane` — never `A`, `B`,
`T` — so a signature reads as a sentence about the kinds of thing involved, and its
constraint clauses (`element: Encodable in Json`) read as statements about them. The
convention costs a few characters per declaration and repays them at every reading,
which is the trade naming should always make.

## Names that form phrases

A name is chosen for how it reads *in position*, not in isolation. The test is whether
the resulting expression can be read aloud:

```scala
5.25.pm on 2018-Aug-11
key.uncloak(message.decrypt.as[Text])
worktree.merge(GitBranch(t"feature"), ff = FastForward.Never)
path.open[Directory](Read & Exclusive)
```

`on` joins a time to a date because that is the English preposition; `uncloak` says what
happens to a secret; `Read & Exclusive` reads as the mode it names. None would be
improved by a more literal name, and several would be worse: `combineTimeAndDate` says
less than `on` and reads far worse.

The same applies to contextual values, which are named for what they *select* rather
than for their type, since the import is what a reader sees:

```scala
import strategies.throwUnsafely
import charEncoders.utf8Encoder
import dateFormats.iso8601DateFormat
import probates.cancelProbate
```

Someone encountering `import strategies.accrue` in unfamiliar code learns something about
that code from the import line alone.

## Type parameters as words

The convention is easiest to judge by comparison. A signature in the conventional style:

```scala
def read[T](using R: Readable[S, T]): T
```

and the same signature in the Soundness style:

```scala
def read[result](using readable: value is Readable to result): result
```

The second says what the pieces *are*: `result` is what is produced, `value` what it is
read from, and the constraint clause reads as a claim about them. No key is needed to
decode the letters, and there is no room for the mental slip where `T` in one signature
means something different from `T` in the next.

## Uniqueness is enforced, not aspired to

Name uniqueness is not merely a convention that reviewers watch for. Because every module
re-exports into one `soundness` namespace, a clash is a compile error in the umbrella
module — so a collision is discovered when it is introduced, not when a user meets it.

The consequences appear in the repository's history as deliberate renames: aviation's
`Posix` timeline became `Unix` to free `soundness.Posix` for the filesystem plane;
`Transmitter`, `Trust` and `enumerate` were renamed when they collided; MathML's element
types were nested inside `Mathml` rather than shadowing the general names. Each was
churn, accepted because the alternative — two meanings for one word, qualified forever —
is a permanent tax on every reader.

## What it costs

**Renaming is disruptive.** A collision found late means changing a published name and
breaking downstream code. The policy accepts that, on the grounds that the alternative
compounds.

**Good names are hard to find.** `Teletype` for styled terminal text took thought that
`AnsiString` would not have, and the search is not always successful on the first
attempt.

**A namespace of unique names has many names in it.** `import soundness.*` brings in a
great deal, and a name that looks free may not be — which is the direct price of the
umbrella import, and why a new module's names are checked against the whole before it is
added.

See [elegant prose](elegant-prose.md) for what the naming is in service of, and
[small APIs](small-apis.md) for why there are fewer names to choose than there might be.
