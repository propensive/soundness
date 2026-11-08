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

trace.frames.head.let(_.method)   // the topmost method, demangled
trace.cause                       // an Optional[StackTrace]
```

`crop` and `drop` trim frames — the machinery below a test framework's entry point, say — so a
rendered trace shows the frames that matter.

### Demangling

Frame names are rewritten from the JVM's encoding toward Scala's: operator names become their
symbols (`$plus` becomes `+`), and the compiler's synthetic markers become compact glyphs, with a
legend, `StackTrace.legend`, available for rendering alongside:

```tabular
column  Glyph
column  JVM marker
column  Meaning
row  `λ`  `$anonfun`, `$$Lambda`  lambda
row  `Λ`  `$adapted`  adapted lambda: the bridge a lambda's method is called through
row  `α`  `$anon`  anonymous class
row  `ⲛ`  `<init>`  constructor
row  `ι`  `<clinit>`, `$init$`  initializer, of a class's statics or of a trait
row  `ℓ`  `$lzyINIT`, `$lzy`, `$lzyHandle`  lazy value: its initializer, field or handle
row  `↑`  `super$`  super accessor
row  `⇢`  `protected$`, `inline$`, `$accessor`  accessor: protected, inline or parameter
row  `≔`  `$_setter_$`  trait setter
row  `⊢`  `$extension`  extension method
row  `∂`  `$direct`  direct method, the erased form of a context function
row  `δ`  `$default`  default argument
row  `⊡`  `$retainedBody`  retained inline body
row  `⊙`  `$outer`  outer reference
row  `⁅⁆`  `$package`  package file
row  `Ξ`  trailing `$`  object
row  `Σ`  `$mc…$sp`  specialized method, with its signature
```

A number after a `$` becomes a subscript, so `$anonfun$3` reads `λ₃`; a `$uXXXX` escape becomes
the character it encodes; and the `$$` joining a private member to the class it was expanded into
becomes one separator. The markers are the ones the Scala compiler's `NameKinds` defines, held in
`StackTrace.Marker.all`, from which the legend is derived. The result is a trace that reads as
the program was written, not as it was compiled.

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
trace.frames.head.let: frame =>
  frame.displayClass         // the enclosing definitions, in source terms
  frame.displayMethod        // the definition's own name
  frame.source.let(_.kind)   // Kind.Method, Kind.Lambda, Kind.Constructor, …
```

The `Kind` distinguishes the kinds of definition, and `plumbing` reports those that are pure
plumbing the compiler synthesized, so that a renderer can dim or drop them:

```tabular
column  Kind
column  Plumbing
row  `Method`  no
row  `Lambda`  no
row  `Value`  no
row  `Class`  no
row  `Constructor`  no
row  `Extension`  no
row  `Default`  no
row  `Bridge`  yes
row  `Forwarder`  yes
row  `Initializer`  yes
row  `Specialized`  yes
row  `Synthetic`  yes
```

Resolution costs one file read per top-level class named in the trace, so it is opt-in rather than
automatic. A trace already in hand is resolved explicitly instead of by import, through whichever
[classloader](classpath.md) can find the compiled classes:

```scala
import classloaders.threadContextClassloader
import workingDirectories.javaBaseWorkingDirectory

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

```scala fuel
compile
##
fqcn"com.example.Main"     // a valid Fqcn
fqcn"com.example.class"    // does not compile: a Java keyword
```

Anywhere a class name travels — a manifest's `Main-Class`, a reflective lookup — an `Fqcn` carries
the guarantee that it could at least name a class.
