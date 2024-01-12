### `Message`s

A `Message` is a string of text which may have other `Message`s embedded within
it. Unlike a string, though, `Message`s retain their structure, and references
to the embedded `Message`s are retained, unchanged.

The simplest way to construct a `Message` is with the `msg""` interpolator, for example:
```scala
val message = msg"this is a message"
```

Such a message can be embedded within another, as in,
```scala
val message2 = msg"We can see that $message."
```
which would represent the text, `We can see that this is a message.`.

This exact text can be produced by calling `Message`'s `text` method, but this
flattens the structure of the message. Other libraries, such as
[Punctuation](https://github.com/propensive/punctuation/),
[Honeycomb](https://github.com/propensive/honeycomb/) and
[Escapade](https://github.com/propensive/escapade/) can render `Message`s as
richer presentations of text, such as Markdown, HTML and console output, and in
doing so, can highlight those parts of the `Message` which were embedded.

The `msg""` interpolator also allows other types to be embedded, provided an
`MessageShow` typeclass instance exists for that type. By default, that includes
primitive types, `Text` strings and any type for which a `Show` typeclass
instance exists, all of which will be automatically converted to `Message`s
when they are substituted into the interpolator. While the `Message` that gets
provided by an `MessageShow` instance may be essentially the same as the `Text`
that is provided by a `Show` instance in most cases, `MessageShow` can provide
additional structure to the text content, that becomes apparent when rendered
as Markdown, HTML or console text.

### `Error`s

An `Error` is a subclass of `java.lang.Exception`, not be confused with
`java.lang.Error`, whose error message is expressed as a `Message`.

This will typically be subclassed with a case class whose parameters will be
substituted into the message, for example:
```scala
case class SizeError(expected: Size, actual: Size)
extends Error(msg"expected a size $expected, but the actual size was $actual")
```

This would require an appropriate `MessageShow[Size]` (or a `Show[Size]`)
instance in scope for the substitution to be acceptable.

### `fail`

When writing macros, with a `Quotes` instance in scope, the `fail` method takes
a `Message` and will produce a compile error showing that message.

If the compiler is running in a terminal with color capability, then color will
be used to highlight embeddings in the failure message, by including ANSI
escape codes in the output message.

### `Mistake`

Sometimes we need to raise exceptions which are not intended to be handled, and
not even intended to be thrown, for example when a code branch is run which
should be impossible to reach. The `Mistake` class provides a standard way of
handling such situation, and takes a `Message` parameter which should briefly
explain the reason why the situation was believed to be impossible.

Should a `Mistake` be thrown, it should represent a programming mistake: the
manifestation of a misconception of impossibility on the part of the
programmer.



