# Elegant Prose

Code in Soundness should read like elegant prose: someone who knows the domain ought to
be able to read a method aloud and understand what it does. This is the aim that the
other principles serve — [direct style](direct-style.md), careful [naming](naming.md),
[small APIs](small-apis.md), and [honest signatures](honest-signatures.md) all exist so
that the meaning of code rises to the surface and the machinery recedes. Clarity for
the reader is treated as a primary goal of design, not as a fortunate side effect of it.

Prose is learnable by analogy, and so is a well-designed API. A reader who has seen
`text.read[Json]` should correctly guess that `text.read[Markdown]` parses Markdown,
that `bytes.read[Audio in Wave]` reads a WAV file, and that `stream.read[Csv]` would
read CSV — one verb, meaning the same thing across every format. The same analogy runs
through the whole vocabulary: whatever can be decoded is `text.as[T]`, whatever can
be shown is `value.show`, whatever streams is `source.stream[Data]`, every checked
literal is an interpolator (`url"…"`, `p"…"`, `json"…"`, `v"…"`), and every serialized
form round-trips through the same pair of operations. Learning one corner of Soundness
is learning the grammar of all of it.

The analogy is a design constraint, not an accident: a new module *must* spell its
parsing `read`, its rendering `show`, its literals as an interpolator — because the
moment it invents `parseFrom` or `stringify`, the reader's correct guesses start
failing, and the prose stops scanning.

## The test

The claim is checkable: read the code aloud and see whether it makes sense to someone who
knows the domain but not the library.

```scala
supervise:
  val server = SocketServer(8080).handle:
    request.target match
      case t"/"       => Http.Response(Http.Ok)(homePage)
      case t"/status" => Http.Response(Http.Ok)(t"OK")
      case _          => Http.Response(Http.NotFound)(t"No such page")
```

"Supervise: the server is a socket server on port 8080 that handles — match the request's
target: for slash, an HTTP response, OK, the home page…". A reader who knows HTTP follows
that without knowing anything about Soundness.

The same test applied elsewhere:

```scala
worktree.merge(GitBranch(t"feature"), ff = FastForward.Never, message = t"Merge feature")

path.open[Directory](Read & Exclusive): dir ?=>
  (dir/"greeting.txt").overwrite(t"Hello directory")

5.25.pm on 2018-Aug-11

key.uncloak:
  t"Hello world".encrypt(InitializationVector.random).decrypt.as[Text]
```

Each reads as a statement about the domain rather than as an instruction to a machine.

## What the machinery costs to hide

Prose that reads well is not prose with nothing behind it. Each line above rests on
several of the other principles at once, and the reader is meant to notice none of them:

`path.open[Directory](Read & Exclusive)` involves a [scoped capability](delimited-scopes.md),
a type-level grant set, and [capture checking](capture-checking.md) proving the handle
does not escape. The reader sees a directory being opened for exclusive reading.

`t"2011-12-13".as[Date]` involves a typeclass resolved by target type, a contextual error
strategy, and a calendar. The reader sees text becoming a date.

`5.25.pm on 2018-Aug-11` involves a compiletime-checked literal, an
[infix](infix-types.md) constructor, and validation that rejects `13.00.am`. The reader
sees a time on a date.

That is the aim: machinery that recedes. When the machinery *has* to be visible — when a
capability must be named, or a type ascribed — it is because the reader genuinely needs
to know, not because the design leaked.

## Where the prose breaks down

Being honest about this is more useful than the principle stated alone.

**Type ascriptions intrude.** Where inference cannot reach, a type must be written, and a
long infix type in the middle of an expression stops the sentence dead.

**Error messages are not prose.** A missing given is reported as a failed implicit search,
in the compiler's vocabulary rather than the library's — which is why
`explainMissingContext` exists, and why it is a repair rather than a solution.

**Some domains have no good English.** A streaming pipeline reads well; the `Duct` and
`Intake` vocabulary underneath it does not, and could not, because the concepts have no
everyday names. The response is to keep such vocabulary below the surface where most
readers never meet it, not to pretend it reads as prose.

**Reading aloud is a test, not a proof.** Code can scan beautifully and be wrong, and the
principle is not a substitute for the checking the other principles provide. It makes
code that is correct *look* correct, and — more usefully — makes code that is wrong look
wrong to a reader who knows the domain.

See [naming](naming.md) and [small APIs](small-apis.md) for the two disciplines that most
directly produce this, and [direct style](direct-style.md) for the shape that lets a
sentence stay a sentence.
