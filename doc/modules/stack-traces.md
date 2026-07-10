## Stack Traces

### About

A JVM stack trace describes compiled classes, not the Scala that produced them: lambdas appear as
`$anonfun`, extension methods gain `$extension`, operators are spelled `$plus`. Soundness captures
a stack trace as an immutable value — frames, positions, cause chain — and *demangles* it, so the
names a developer reads are closer to the names they wrote. Alongside it sit two related small
tools: `Codepoint`, the source position of a call site, captured automatically; and `Fqcn`, a
validated fully-qualified class name.

### On stack traces

A `Throwable`'s stack trace is mutable, printable state: the standard idiom writes it to a stream,
and inspecting it programmatically means poking at `StackTraceElement`s whose names are the
compiler's, not the programmer's. For error *values* — the kind [Soundness errors](errors.md)
are — the trace should be a value too: comparable, transformable, renderable where and how the
program chooses.

`StackTrace` is that value. Everything comes from the `soundness` package:

```scala
import soundness.*
```

### Capturing a trace

Any `Throwable` converts to a `StackTrace` — its class, its message, its frames, and its cause as
another `StackTrace`:

```scala
val trace = exception.stackTrace

trace.frames.head.method   // the topmost method, demangled
trace.cause                // an Optional[StackTrace]
```

`crop` and `drop` trim frames — the machinery below a test framework's entry point, say — so a
rendered trace shows the frames that matter.

### Demangling

Frame names are rewritten from the JVM's encoding toward Scala's: operator names become their
symbols, and the compiler's synthetic markers become compact glyphs — `λ` for a lambda, `ⲛ` for a
constructor, `Λ` for an adapted lambda — with a legend available for rendering alongside. The
result is a trace that reads as the program was written, not as it was compiled.

Whether Soundness's own errors capture traces at all is the `Diagnostics` choice described in
[error handling](errors.md) — traces for debugging, omitted where errors are expected and handled.

### Codepoints

A `Codepoint` is the source file and line of a call site, captured automatically wherever a method
asks for one as a given:

```scala
def note(message: Text)(using codepoint: Codepoint): Unit =
  record(t"${codepoint.text}: $message")   // e.g. app.scala:42
```

This is how Soundness's logs, tests and caches know where they were called from, without any caller
passing a location by hand.

### Class names

An `Fqcn` is a fully-qualified class name that satisfies the JVM's rules — validated segments, no
keywords — checked at compiletime by the `fqcn"…"` interpolator and at runtime by parsing:

```scala
fqcn"com.example.Main"     // a valid Fqcn
fqcn"com.example.class"    // does not compile: a Java keyword
```

Anywhere a class name travels — a manifest's `Main-Class`, a reflective lookup — an `Fqcn` carries
the guarantee that it could at least name a class.
