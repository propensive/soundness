# Composability

Soundness is built to compose: small pieces combine into larger ones without special
cases, and the combination behaves as the parts lead one to expect. APIs are designed
so that the result of one operation is the input to the next, types combine to describe
richer types, and capabilities nest within capabilities. Composability is what lets a
small, consistent set of parts cover an enormous range of uses, and it is treated as a
deliberate design constraint rather than a property hoped for after the fact.

## Values compose

A [stream](../modules/streams.md) pipeline is the clearest case. Each stage takes a
stream and yields a stream, so stages chain without adapters and in any order that makes
sense:

```scala
file.stream.decompress[Gzip].via(decoder).records
```

Nothing in that line is special-cased for its neighbours. Decompression does not know it
is reading from a file; the decoder does not know its input was compressed. Because each
stage is a value, the chain can be assembled at runtime, held in a variable, or built by
a function that decides which stages a particular job needs.

## Types compose

An [infix type](infix-types.md) adds one clause without nesting a bracket, so a precise
type is built up the way an English description is:

```scala
Path on Linux
Raster by Rgba in Png
Text is Decodable in Json
Element of "ul" over "li" in Whatwg
```

Each preposition means the same thing wherever it appears, so a reader who has understood
one such type reads the next by analogy. The alternative — `Element["ul", "li", Whatwg]` —
carries the same information and tells the reader nothing about which parameter is which.

## Capabilities compose

Two capabilities in scope are two capabilities; there is no combined type to construct
and no ordering to choose. Fallible, asynchronous and logging code compose by being
written next to each other:

```scala
supervise:
  recover:
    case error: HttpError => fallback
  . protect:
      async(url"https://example.com/".fetch().receive[Text])
```

This is the property that [direct style](direct-style.md) exists to preserve. Under a
monadic encoding the same combination requires a transformer stack, and the order in
which the effects are stacked becomes a decision with consequences. Here it is not a
decision at all.

## Scopes compose

Because a capability is introduced by a block, and blocks nest, resource lifetimes nest:

```scala
archive.open[Zip](): zip ?=>
  target.open[File](Write): handle ?=>
    handle.write(zip.entries.head.stream)
```

The inner scope sees both capabilities; each is withdrawn at its own boundary, in the
right order, whether the block completes or fails. Adding a third resource adds a line
rather than restructuring the other two — and [capture checking](capture-checking.md)
proves that nothing escapes outward.

## The constraint this places on design

Composability is easy to describe and hard to keep, because the pressure against it is
always local. Every special case, every "convenience" overload that takes the pair rather
than the parts, every operation that works only when called first — each is a small
convenience that removes a combination someone else needed.

Three rules follow, and they are why some Soundness APIs look more austere than they
might:

**Return the type you accept.** An operation that takes a stream returns a stream, so it
can sit in the middle of a chain rather than only at the end.

**Do not fuse what the caller can combine.** There is no `readAndDecompress`, because
`stream.decompress[Gzip].read[Text]` already exists and the fused version would have to
be repeated for every pair.

**Make the general case the only case.** If an operation works for `List` but not for a
stream, the abstraction is wrong — which is why the operations are defined over
typeclasses rather than over concrete types.

## What it costs

A composable API is less immediately convenient than a fused one. `Tarfile.from(dir).gzip`
is two calls where `tarGzip(dir)` would be one, and a reader who wanted exactly the fused
operation pays a small tax for the generality they did not need.

That tax is accepted because it is bounded and the alternative is not: fused operations
multiply combinatorially, and each one added is a place where the general path and the
shortcut can drift apart.

See [small APIs](small-apis.md) for the vocabulary this keeps small, and
[decoupling](decoupling.md) for how independent modules manage to compose at all.
