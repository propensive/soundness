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

The single-type-argument form targets the classfile universe, and `targeting` selects another
explicitly:

```scala
val forJs = Scalac[3.8](options).targeting[Universe.Sjsir]
```

Each universe beyond the JVM's has runtime prerequisites, supplied on the classpath like any
other. `Universe.Sjsir` needs `scalajs-javalib`, `scalajs-library_2.13`, `scalajs-scalalib_2.13`
and `scala3-library_sjs1`; their absence surfaces as ordinary compiler diagnostics rather than
as a distinct kind of failure. `Universe.Nir` needs `scalalib`, `scala3lib`, `javalib`,
`auxlib`, `clib`, `posixlib` and `nativelib`, and additionally a `NirPlugin` in scope — evidence
of where the Scala Native compiler plugin lives, since NIR is emitted by a plugin rather than by
a backend built into the compiler.

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
    List(EntryPoint(fqcn"com.example.Main")) )
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

Formats are values, and their identity is value equality, so wherever a parameter changes what a
user receives, it is a constructor parameter and each parameterization is a distinct node: the
module system a JavaScript host imports through, the WASI generation an artifact's ABI follows,
the target triple a binary runs on, the delivery mode of a bundle. A binary for
`Triple.Arm64MacOs` is as distinct from one for `Triple.X64Linux` as a JAR is from a JavaScript
bundle; that they share a linker does not make them interchangeable. The node set is open, too —
`Format`, `Tool` and `Edge` are ordinary public types, so registering an edge for a format
Soundness has never heard of extends the graph.

### Deliverables and paths

The value flowing along a path is a `Deliverable`, and its case says where in the graph it
belongs: `Sources` at a source node, an `Emission` — an output directory and the classpath it was
compiled against — at an intermediate representation, and a `Product`, a linked or packaged file,
at an application format.

The path itself can be asked for directly, which is how a tool reports what it is about to run:

```scala
toolchain.path(Universe.Classfile, Apk).map(_.target.id)   // List(t"dex", t"apk")
```

The path is the *unique shortest* one. Where none exists, `path` raises `NoPath`; where two are
equally short, it raises `AmbiguousPath`, resolved by producing an intermediate format explicitly
in a separate step rather than by a tie-break the caller cannot see. Assembly is checked as
strictly: `Toolchain` rejects a duplicate edge between the same pair of formats, and rejects a
cycle.

Each intermediate product on a multi-stage path is written below the destination under its
format's `id`, and the final product to the destination itself. An APK, whose path runs
`Classfile → Dex → Apk`, therefore dexes into `dex/` and packages from there — and nothing about
that sequence is written into the APK tool.

A staging rig, which must fold its own running classpath into the JARs it produces, pairs
`Bundler.applicationClasspath` — the running application's classpath, introspected from the
thread-context classloader — with its compiled output:

```scala
Toolchain(jarEdges()).produce
  ( Deliverable.Emission(out, Bundler.applicationClasspath),
    Universe.Classfile,
    Jar,
    out,
    List(jarOptions.name(t"$uuid.jar")),
    List(EntryPoint(executor)) )
```

### Settings

A tool is configured by `Setting`s, which are addressed to the formats whose production they
configure rather than to a tool directly. `produce` applies each setting, in order, to every edge
on the path whose target it applies to; a setting matching no format on the path raises
`InapplicableSetting` before any tool runs.

Addressing settings to formats is what lets one setting configure several stages. An Android API
level is a property of the application, but it governs both how D8 desugars and what the manifest
declares, so `apkOptions.minApi` applies to `Dex` and `Apk` alike, dispatching to each node's own
configuration type. The Scala.js settings — `linkerOptions.checkIr`, `sourceMaps`, `esVersion.*`
and `optimize.*` — apply to every format the Scala.js linker produces, and the native settings
cover the garbage collector (`nativeOptions.gc.*`), build mode (`mode.*`) and link-time
optimization (`lto.*`).

Entry points are ambient along the whole path in the same way, and each tool applies or ignores
them as its format demands: an executable JAR takes at most one, as its `Main-Class`; a native
binary and an APK require exactly one; a library JAR and a DEX archive ignore them entirely,
since neither records an entry point.

### WASI components

A WASI component — a `Wasi(Wasi.Version.Wasip2)`, a component-model `.wasm` whose imports and
exports are described by WIT — has two prerequisites beyond the linker, both demanded by the edge
provider, so an edge without them cannot be constructed: a `WasiToolchain`, evidence that the
native tools the link shells out to (`wasm-tools` and the scala-wasm fork of `wit-bindgen`) are
present, obtainable only through the probing constructor `WasiToolchain()`; and a `WitWorld`,
naming the directory of WIT packages and the world to link against:

```scala
given WasiToolchain = WasiToolchain()
given WitWorld = WitWorld(witDirectory, t"my-world")

val toolchain = Toolchain(sjsEdges(), List(sjsEdges.wasi()), ociEdges())
toolchain.produce(emission, Universe.Sjsir, OciImage, destination)
```

WASI generations are separate nodes because they determine an artifact's ABI: `Wasip1` is a flat,
libc-style syscall interface on core modules, `Wasip2` is the component model, and `Wasip3` adds
native asynchrony. Only `Wasip2` has an edge producing it, so asking for `Wasip3` raises `NoPath`
rather than silently producing something else.

Wrapping a component as an OCI artifact is a further edge, `Wasi(Wasip2) → OciImage`, rather than
anything the component link knows about; the path composes the two. `Js(module)` and `Wasm`
require no native tooling at all — the linker is an ordinary JVM library.

### Native binaries

The native edges shell out to `clang` and `clang++`, so `nativeEdges` probes for them once and
raises `ToolchainError` if either is missing. It takes the triples to target, defaulting to the
build host's own:

```scala
val toolchain = Toolchain(nativeEdges(Triple.Arm64MacOs, Triple.X64Linux))

toolchain.produce
  ( emission,
    Universe.Nir,
    Binary(Triple.X64Linux),
    destination,
    List(nativeOptions.mode.releaseFast, nativeOptions.gc.commix),
    List(EntryPoint(fqcn"com.example.Main")) )
```

Each triple is its own node, rendered as an LLVM target triple such as `aarch64-apple-darwin` or
`x86_64-unknown-linux-gnu`. Targets beyond the build host require a C toolchain capable of
cross-compiling to them.

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
