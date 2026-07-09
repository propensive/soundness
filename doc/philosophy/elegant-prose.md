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
through the whole vocabulary: whatever can be decoded is `text.decode[T]`, whatever can
be shown is `value.show`, whatever streams is `source.stream[Data]`, every checked
literal is an interpolator (`url"…"`, `p"…"`, `json"…"`, `v"…"`), and every serialized
form round-trips through the same pair of operations. Learning one corner of Soundness
is learning the grammar of all of it.

The analogy is a design constraint, not an accident: a new module *must* spell its
parsing `read`, its rendering `show`, its literals as an interpolator — because the
moment it invents `parseFrom` or `stringify`, the reader's correct guesses start
failing, and the prose stops scanning.
