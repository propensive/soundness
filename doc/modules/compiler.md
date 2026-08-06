## Compilation

### About

The Scala compiler is invoked programmatically as a typed API: sources supplied as values, options
as typed flags valid only for the compiler versions that support them, and results — diagnostics
with their positions, progress, success or failure — as streams and values rather than console
output. This is the machinery beneath Soundness's own [staged execution](staging.md) and
compiletime [benchmarks](../standards/benchmarking.md), and it serves any tool that compiles Scala
on a user's behalf.

Compilation is only half the job. The output is then *linked* into an artifact — an executable
JAR, JavaScript, a WebAssembly component, a native binary, an Android package — and both the
intermediate representation a compilation emits and the artifact a linker produces are types, so
a mismatch between them is a compile error in the tool rather than a failure at link time.

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

### Semantic diagnostics

A compiler error message is usually a sentence with types spliced into it, and by the time it
reaches a tool those types are just words. Asked for semantic diagnostics, the compiler instead
marks each interpolated argument in band, carrying pickled TASTy for the types among them, and a
`Notice` keeps that marked-up form alongside the plain one.

The markup parses into a `SemanticMessage`: a typed tree of styled spans, placeholders and types.
Each type is unpickled from its TASTy and re-rendered through the same type-rendering machinery
used elsewhere, so a type reads the way it would in the code, abbreviated against what is
imported. A "found `A`, required `B`" message
therefore arrives as two types to render, link and compare, rather than as a string to
re-parse.

### Universes

A compilation emits one intermediate representation, and that choice decides which library
artifacts it can link against. `Universe.Classfile` is JVM classfiles, `Universe.Sjsir` is
Scala.js IR, and `Universe.Nir` is Scala Native IR. The universe is a type parameter of the
`Compilation`, so a Scala.js compilation is not interchangeable with a native one, and the
compiler flags each universe needs — `-scalajs`, or the Scala Native plugin — follow from it
rather than being supplied by hand.

### Producing artifacts

Universes and application formats are nodes of a `Toolchain`, a directed acyclic graph whose edges
are tools. Producing an artifact is path search: `produce` takes what you have, the format you
want, and an output location, and runs each tool on the path between them:

```scala
Toolchain(jarEdges()).produce
  ( Deliverable.Emission(output, classpath),
    Universe.Classfile,
    Jar,
    destination,
    List(jarOptions.name(t"app.jar")),
    List(EntryPoint(Fqcn(t"Main"))) )
```

From classfiles come an executable `Jar`, a `Dex` archive of Dalvik bytecode, a complete, signed
and aligned Android `Apk`, and an `Xeq` bundle runnable straight from a shell. From Scala.js IR
come `Js` — as an ECMAScript module, a CommonJS module or a plain script, the module system being
part of the node's identity — a `Wasm` module with JavaScript glue, a standalone `Wasi` component
at a stated generation, and an `OciImage` wrapping that component as an OCI artifact. From Scala
Native IR comes a `Binary` per target triple. Every universe additionally produces a `Library`,
packaging unlinked output for downstream assembly.

Formats several edges apart need no special handling: an `Apk` runs dexing and then packaging, and
an `OciImage` links the component and then wraps it, because those are the paths. A build that
cannot reach a format says so — as a `NoPath` naming the two formats — rather than failing part-way
through, and a setting that configures nothing on the path is rejected before any tool runs.

Edges whose tools have prerequisites come from providers that demand evidence of them, so an edge
whose tooling is absent cannot be built: `sjsEdges.wasi()` needs a probed `WasiToolchain` and a
`WitWorld`, and `nativeEdges()` probes for `clang`.

### Other languages

`Javac` compiles Java sources through the same shape of API, so a tool that orchestrates
compilation does not change idiom per language, and a mixed Scala and Java application links into
one artifact.

`Kotlinc` does the same for Kotlin, parameterized by the language version it targets, with its
options — `-Werror`, `-jvm-target`, explicit API mode, and the rest — under `kotlincOptions` and
version-checked in the same way:

```scala
supervise:
  val kotlinc = Kotlinc[2.4](List(kotlincOptions.warnings.asErrors, kotlincOptions.jvmTarget(17)))
  val process = kotlinc(classpath)(Map(t"demo/Greeting.kt" -> source), outputPath)
  process.complete()
```

The Kotlin compiler reads its sources from disk, so they are written to a scratch directory that
is removed when the compilation ends; a diagnostic still names the source as it was given, not the
file it was written to. The Kotlin standard library is never implied — like every other classpath
entry, it is supplied explicitly — and the output is classfiles, linkable as any classfile
artifact, including a `Dex` or an `Apk`.

As with the Scala compiler behind `Scalac`, the Kotlin compiler itself is a compile-only
dependency: a tool that drives it puts `kotlin-compiler-embeddable` on its own runtime classpath,
choosing the version it wants to drive, rather than inheriting one.

Calling *into* a compiled Kotlin library, meanwhile, needs no compilation at all: its declarations
are read from its classfiles, as [foreign interoperability](foreign-interop.md) describes.
