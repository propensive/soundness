# Small APIs

Soundness keeps its APIs small by giving one operation one name. A single polymorphic
method — `read`, `as`, `show` — serves every type that supports it, instead of a
family of near-duplicates that differ only in what they apply to. There is one obvious
way to do a thing, so there is less to learn, less to remember, and less chance of
reaching for the wrong variant. The breadth of what an API can do comes from the range
of types it ranges over, not from the number of methods it offers.

## One verb, many types

`read` parses a source into the type asked for. There is no `readJson`, no `parseXml`,
no `fromCsv` — the type argument says what is wanted, and an instance for that type does
the work:

```scala
source.read[Json]
source.read[Xml]
source.read[Markdown of Layout]
source.read[Raster in Png]
source.read[Person in Dsv]
```

Adding a format adds instances, not methods. The vocabulary a reader must learn does not
grow when the library does.

The same holds throughout. `as` decodes, `show` renders for a person, `inspect` renders
for a programmer, `in` encodes into a format, `stream` exposes a value as a stream,
`digest` hashes, `serialize` renders bytes in a base encoding:

```scala
t"2024-01-15".as[Date]
t"example.com".as[Hostname]

person.in[Json]
person.in[Cbor]

payload.digest[Sha2[256]].serialize[Hex]
```

## What makes it possible

Three things, and none of them is naming discipline alone.

**Typeclasses supply the meaning.** `read` is one method whose behaviour comes from a
`Readable` instance; `as` from a `Decodable`; `show` from a `Showable`. The method is a
name for a *relationship*, not for an implementation.

**The type argument carries the intent** that a longer name would otherwise carry.
`read[Json]` says as much as `readJson` in the same number of characters, and composes:
`read[Person in Json]` has no reasonable name at all in the other scheme.

**Uniqueness of names makes the umbrella import safe.** Because there is one `Path` and
one `Error` in the whole of `soundness`, a single `import soundness.*` never forces a
choice between two things called the same — which is what allows one verb to be
genuinely one verb rather than several shadowing each other. See
[naming](naming.md).

## The counter-pressure

A small API is not an API with few capabilities, and the principle is not "add no
methods". It is a constraint on *how* capability is added: by widening the range of
types an existing operation covers, before adding an operation.

Two forces push the other way, and both are legitimate in their place.

The first is when an operation genuinely differs. `read` and `load` are two verbs for
[XML](../modules/xml.md), because reading yields the content and loading yields the
document with its header — a real distinction, not a variant. Collapsing them would
force a flag argument, which is a worse way to say the same thing.

The second is when a domain has its own vocabulary that a general verb would obscure. A
`Tarfile` has `gzip`, not `in[Gzip]`, because "a gzipped tarball" is the thing the domain
names. Insisting on the general verb everywhere makes an API uniform and unreadable,
which trades one kind of learning cost for another.

## What it costs

Polymorphic methods put weight on inference and on error messages. When
`source.read[Person in Json]` fails because an instance is missing, the compiler's
default complaint is about an implicit search, not about a missing codec — which is
further from the problem than "no method `readPerson`" would have been.

That cost is paid down rather than denied: importing `explainMissingContext` turns the
bare implicit error into a diagnosis that names the import which would satisfy it. The
trade — a slightly worse failure mode for a much smaller vocabulary — is only worth
making if the failure mode is repaired, so it is.

See [elegant prose](elegant-prose.md) for why the vocabulary being learnable by analogy
is the real prize, and [composability](composability.md) for what the uniformity buys.
