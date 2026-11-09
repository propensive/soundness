## Stack Traces

### About

A JVM stack trace describes compiled classes, not the Scala which produced them. A lambda
appears as `$anonfun`, an extension method gains `$extension`, and an operator is spelled
`$plus`.

A `StackTrace` captures a trace as an [immutable] value, with its frames, positions and chain
of causes. It then *demangles* the trace, so the names a developer reads are closer to the names
they wrote.

Two small related tools sit beside it. A `Codepoint` is the source position of a call site,
captured automatically. An `Fqcn` is a validated fully-qualified class name.

### On stack traces

The stack trace of a `Throwable` is mutable, printable state. The standard idiom writes it to a
stream. Inspecting it in a program means picking through `StackTraceElement`s, whose names are
the compiler's and not the programmer's.

[Errors](errors.md) here are values. Their traces should be values too: comparable,
transformable, and rendered where and how the program chooses. A trace which can be trimmed,
resolved and rendered as a value follows [immutability](../philosophy/immutability.md) into
diagnostics.

Everything comes from the `soundness` package, and the second import lets the samples print
their results:

```scala
import soundness.*
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Capturing a trace

Any `Throwable` converts to a `StackTrace`. The value holds the class of the exception, its
message, its frames, and its cause as another `StackTrace`:

```scala
val exception = Exception("boom", Exception("the cause"))
val trace = exception.stackTrace
```

```scala fuel
run
##
Out.println(trace.className)
Out.println(trace.message.text)
Out.println(trace.cause.let(_.message.text).inspect)
```

Each frame names its class and method, already demangled. The frames of a trace captured here
belong to the machinery which runs these samples, so they are not shown. `crop` and `drop` trim
frames, such as the machinery below the entry point of a test framework, so a rendered trace
shows only the frames which matter.

### Demangling

The names of frames are rewritten from the JVM's encoding toward Scala's. `StackTrace.rewrite`
is the function which does it, and it can be tried on any compiled name. An operator becomes its
symbol:

```scala fuel
run
##
Out.println(StackTrace.rewrite("$plus$plus"))
```

The compiler's synthetic markers become compact glyphs. A number after a `$` becomes a
subscript, and the `$$` which joins a private member to the class it was expanded into becomes
one separator:

```scala fuel
run
##
Out.println(StackTrace.rewrite("com.example.Main$"))
Out.println(StackTrace.rewrite("$anonfun$3", method = true))
Out.println(StackTrace.rewrite("greeting$extension", method = true))
Out.println(StackTrace.rewrite("com.example.Main$$anon$1"))
```

A `$uXXXX` escape becomes the character it encodes. These are the glyphs:

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

The markers are the ones which the Scala compiler's `NameKinds` defines. They are held in
`StackTrace.Marker.all`, and `StackTrace.legend` is derived from them, for rendering beside a
trace:

```scala fuel
run
##
Out.println(StackTrace.legend.at(t"λ").inspect)
Out.println(StackTrace.legend.at(t"⊢").inspect)
```

The result is a trace which reads as the program was written, and not as it was compiled.

Whether an error captures a trace at all is the `Diagnostics` choice which
[errors](errors.md) describes. A trace suits debugging, and is omitted where errors are
expected and handled.

### Resolving frames to their source

Demangling can only tidy a compiled name. `$anonfun$3` becomes `λ₃`, which says that a lambda
ran, but not *which* lambda. The names which most need explaining are exactly the ones which the
compiler mints after its output is pickled: anonymous functions, initializers, bridges,
specializations. No rewriting recovers them, because the information is not in the name.

Position does what the name cannot. The compiler records the extent of every definition, so the
line which a frame already carries is enough to find the definition it came from. Importing a
resolver makes every trace captured in its scope carry that information:

```scala
import stackResolutions.tastyStackResolution
```

A resolved `Frame` gains a `Source`: the path of the file, the chain of enclosing definitions,
the definition's own name in the source, and the kind of definition it is. `displayClass` and
`displayMethod` give the resolved names where they are known, and fall back to the demangled
names where they are not, so a renderer needs no branch of its own:

```scala
trace.frames.head.let: frame =>
  frame.displayClass
  frame.displayMethod
  frame.source.let(_.kind)
```

The `Kind` of a definition says what it is. `plumbing` says which kinds are pure plumbing which
the compiler synthesized, so a renderer can dim or drop them:

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

Resolution costs one file read for each top-level class named in the trace, so it is chosen
rather than automatic. A trace already in hand is resolved explicitly, through whichever
[classloader](classpath.md#choosing-a-classloader) can find the compiled classes:

```scala
import classloaders.threadContextClassloader
import workingDirectories.javaBaseWorkingDirectory

val resolved = trace.resolved
```

The [capability] to read those files is one which this layer deliberately lacks. The default
resolver does nothing, and an implementation comes from a module which has a classpath. That is
what keeps stack traces working unchanged where no such capability exists: in a browser, in a
native binary, or inside a WebAssembly component.

### Codepoints

A `Codepoint` is the source file and line of a call site. It is captured automatically wherever
a method asks for one as a [given]:

```scala
def note(message: Text)(using codepoint: Codepoint): Text =
  t"${codepoint.text}: $message"
```

A call such as `note(t"checkpoint")` gives a text such as `app.scala:42: checkpoint`, with the
position of the call and not of the definition. This is how [logs](logging.md),
[tests](testing.md) and [caches](caching.md) know where they were called from, and no caller
passes a location by hand.

### Class names

An `Fqcn` is a fully-qualified class name which satisfies the JVM's rules: every segment is
valid, and none is a keyword. The `fqcn"…"` [interpolator] checks it at [compiletime]:

```scala
val main = fqcn"com.example.Main"
```

A Java keyword is not a valid segment:

```scala fuel
compile
errors show
##
fqcn"com.example.class"
```

At runtime, a class name is checked by parsing it. Wherever a class name travels, such as the
`Main-Class` of a manifest or a reflective lookup, an `Fqcn` carries the guarantee that it could
at least name a class.

### Related

- [Errors](errors.md) describes the error values whose traces these are, and the `Diagnostics`
  choice.
- [Classpath](classpath.md) describes the classloaders which resolve a frame to its source.
- [Debugging](debugging.md) renders traces and values for inspection.
- [Logging](logging.md) and [testing](testing.md) use codepoints to say where a message or a
  check came from.
