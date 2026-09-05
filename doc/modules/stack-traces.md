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

A trace as an immutable value that can be trimmed, resolved and rendered follows [immutability](../philosophy/immutability.md) into diagnostics.

`StackTrace` is that value. Everything comes from the `soundness` package:

```scala
import soundness.*
```

### Capturing a trace

Any `Throwable` converts to a `StackTrace` — its class, its message, its frames, and its cause as
another `StackTrace`:

```scala
val exception = Exception("boom")
val trace = exception.stackTrace

trace.frames.prim.let(_.method)   // the topmost method, demangled
trace.cause                       // an Optional[StackTrace]
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

### Resolving frames to their source

Demangling can only tidy a compiled name up. `$anonfun$3` becomes `λ₃`, which says a lambda ran
but not *which* lambda — and the names that most need explaining are exactly the ones the compiler
mints after its output is pickled: anonymous functions, initializers, bridges, specializations. No
amount of rewriting recovers them, because the information is not in the name.

Position does what name cannot. The compiler records the extent of every definition, so the line a
frame already carries is enough to find the definition it came from. Importing a resolver makes
every trace captured in that scope carry it:

```scala
import stackResolutions.tastyStackResolution
```

A resolved `Frame` gains a `Source`: the path of the file, the chain of enclosing definitions, the
definition's own source name, and what kind of definition it is. `displayClass` and
`displayMethod` give the resolved names where they are known and fall back to the demangled ones
where they are not, so a renderer needs no branch of its own:

```scala
trace.frames.prim.let: frame =>
  frame.displayClass         // the enclosing definitions, in source terms
  frame.displayMethod        // the definition's own name
  frame.source.let(_.kind)   // Kind.Method, Kind.Lambda, Kind.Constructor, …
```

The `Kind` distinguishes a method from a lambda, a value, a constructor, an extension or a default
argument — and, separately, marks the kinds that are pure plumbing: bridges, forwarders,
initializers and specializations, which `plumbing` reports so that a renderer can dim or drop
them.

Resolution costs one file read per top-level class named in the trace, so it is opt-in rather than
automatic. A trace already in hand is resolved explicitly instead of by import, through whichever
[classloader](classpath.md) can find the compiled classes:

```scala
import classloaders.threadContextClassloader

trace.resolved
```

The capability to read those files is one this layer deliberately does not have: the default
resolver does nothing, and an implementation is supplied from a module that has a classpath. That
is what keeps stack traces working unchanged where no such capability exists — in a browser, in a
native binary, inside a WebAssembly component.

### Codepoints

A `Codepoint` is the source file and line of a call site, captured automatically wherever a method
asks for one as a given:

```scala
def note(message: Text)(using codepoint: Codepoint): Text =
  t"${codepoint.text}: $message"   // e.g. app.scala:42: message

note(t"checkpoint")
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
