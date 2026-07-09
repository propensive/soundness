## Compilation

### About

The Scala compiler is invoked programmatically as a typed API: sources supplied as values, options
as typed flags valid only for the compiler versions that support them, and results — diagnostics
with their positions, progress, success or failure — as streams and values rather than console
output. This is the machinery beneath Soundness's own [staged execution](staging.md) and
compiletime [benchmarks](../standards/benchmarking.md), and it serves any tool that compiles Scala
on a user's behalf.

### On driving the compiler

The compiler's command line is stringly typed twice over: options are strings whose validity
depends on the compiler version, and diagnostics come back as formatted text to re-parse. A build
tool, a notebook, a code runner — anything that compiles programmatically — wants the inverse:
typed options checked against the version in use, and structured diagnostics.

`Scalac` provides that, invoking the compiler in-process over in-memory sources. Everything comes
from the `soundness` package:

```scala
import soundness.*
import strategies.throwUnsafely
```

### Configuring a compiler

A `Scalac` is parameterized by its language version and carries its options — each a typed value,
usable only at the versions that accept it:

```scala
val compiler = Scalac[3.8](List(scalacOptions.experimental, scalacOptions.newSyntax))
```

The options mirror the compiler's own — warnings, language features, internal flags — under
`scalacOptions`, and an option that does not exist at the chosen version is a compile error in the
tool, not a runtime complaint from the compiler.

### Compiling

Sources are a map of names to content, compiled against a [classpath](classpath.md) into an output
directory; the returned process exposes its diagnostics and progress as streams and completes with
a result:

```scala
supervise:
  val process = compiler(classpath)(Map(t"hello.scala" -> source), outputPath)

  process.notices.each: notice =>
    report(notice.importance, notice.file, notice.message)

  process.complete()   // CompileResult.Success, Failure, or Crash
```

Each `Notice` carries its importance — info, warning or error — its file and its span, so a tool
can underline the offending code rather than echo compiler output. A crash arrives as a value too,
with its [stack trace](stack-traces.md), and the process can be aborted mid-compilation.

### Other languages

`Javac` compiles Java sources through the same shape of API, and a Kotlin counterpart exists for
mixed builds — so a tool that orchestrates compilation does not change idiom per language.
