## Compilation

### About

The Scala compiler is invoked as a typed API. Sources are supplied as values. Options are typed
flags, valid only for the compiler versions which support them. Results, which are diagnostics
with their positions, progress, and success or failure, are [streams](streams.md) and values,
not console output.

This is the machinery beneath [staged execution](staging.md) and the compiletime
[benchmarks](../standards/benchmarking.md), and it serves any tool which compiles Scala on a
user's behalf.

Compilation is half the job. The output is then *linked* into an artifact: an executable JAR,
JavaScript, a WebAssembly component, a native binary or an Android package. The intermediate
representation which a compilation emits, and the artifact which a linker produces, are both
types. A mismatch between them is a compile error in the tool, and not a failure at link time.

### On driving the compiler

The command line of the compiler is stringly typed twice over. Options are strings whose
validity depends on the version of the compiler, and diagnostics come back as formatted text to
parse again. A build tool, a notebook or a code runner wants the inverse: typed options, checked
against the version in use, and structured diagnostics.

`Scalac` provides that. It invokes the compiler in the same process, over sources in memory. A
compilation described as a value and run by a toolchain is
[direct style](../philosophy/direct-style.md): the build is code, checked like any other code.

Everything comes from the `soundness` package:

```scala
import soundness.*

import environments.javaBaseEnvironment
import probates.awaitProbate
import strategies.throwUnsafely
import systems.javaBaseSystem
import threads.virtualThreads
import workingDirectories.javaBaseWorkingDirectory
```

The samples on this page run compilers and linkers, so they are compiled but not run.

### Configuring a compiler

A `Scalac` is parameterized by its language version, and carries its options. Each option is a
typed value, usable only at the versions which accept it:

```scala
val compiler = Scalac[3.8](List(scalacOptions.experimental, scalacOptions.newSyntax))
```

The options mirror the compiler's own, for warnings, language features and internal flags, under
`scalacOptions`. An option which does not exist at the chosen version is a compile error in the
tool, not a complaint from the compiler at [runtime].

### Compiling

Sources are a map from names to content. They are compiled against a [classpath](classpath.md)
into an output directory. The returned process exposes its diagnostics and progress as streams,
and completes with a result:

```scala
supervise:
  val process = compiler(classpath)(Map(t"hello.scala" -> source), outputPath)
  val result = process.complete()   // CompileResult.Success, Failure, or Crash

  process.notices.each: notice =>
    report(notice.importance, notice.file, notice.message)

  result
```

`complete()` gives a `CompileResult`, which is `Success`, `Failure` or `Crash`. Each `Notice`
carries its importance, which is info, warning or error, its file and its span. A tool can
underline the offending code, and need not echo the compiler's output. A crash arrives as a
value too, with its [stack trace](stack-traces.md), and the process can be aborted before the
compilation ends.

### Semantic diagnostics

A compiler error message is usually a sentence with types spliced into it. By the time it
reaches a tool, those types are just words. Asked for semantic diagnostics, the compiler instead
marks each interpolated argument in band, with pickled TASTy for the types among them, and a
`Notice` keeps that marked-up form beside the plain one.

The markup parses into a `SemanticMessage`: a typed tree of styled spans, placeholders and types.
Each type is unpickled from its TASTy and rendered again through the same machinery which
renders types elsewhere, so a type reads as it would in the code, abbreviated against what is
imported. A "found `A`, required `B`" message therefore arrives as two types to render, link and
compare, and not as a string to parse again.

### Universes

A compilation emits one intermediate representation, and that choice decides which library
artifacts it can link against. The universe is a type parameter of the `Compilation`, so a
Scala.js compilation cannot be mistaken for a native one. The compiler flags which each universe
needs follow from it, and are not supplied by hand:

```tabular
column  Universe
column  Emits
column  Compiler flag
column  Runtime libraries on the classpath
row  `Universe.Classfile`  JVM classfiles
row  `Universe.Sjsir`  Scala.js IR  `-scalajs`  `scalajs-javalib`, `scalajs-library_2.13`, `scalajs-scalalib_2.13`, `scala3-library_sjs1`
row  `Universe.Nir`  Scala Native IR  `-Xplugin:` with the `NirPlugin` jar  `scalalib`, `scala3lib`, `javalib`, `auxlib`, `clib`, `posixlib`, `nativelib`
```

The form with a single type argument targets the classfile universe. `targeting` selects another
explicitly:

```scala
val forJs = compiler.targeting[Universe.Sjsir]
```

The runtime libraries are supplied on the classpath like any other. Their absence appears as
ordinary compiler diagnostics, and not as a distinct kind of failure. `Universe.Nir` also needs
a `NirPlugin` in scope, which is evidence of where the Scala Native compiler plugin lives, since
NIR is emitted by a plugin and not by a backend built into the compiler.

### Producing artifacts

Universes and application formats are the nodes of a `Toolchain`, a
[directed acyclic graph](graphs.md) whose edges are tools. Producing an artifact is a search for
a path. `produce` takes what you have, the format you want, and an output location, and runs
each tool on the path between them:

```scala
supervise:
  Toolchain(jarEdges()).produce
    ( Deliverable.Emission(outputPath, classpath),
      Universe.Classfile,
      Artifact.Jar,
      destination,
      List(jarOptions.name(t"app.jar")),
      List(EntryPoint(fqcn"com.example.Main")) )
```

Each universe reaches its own set of application formats:

```tabular
column  From
column  Format
column  Artifact
row  any universe  `Library`  unlinked output, packaged for downstream assembly
row  `Universe.Classfile`  `Artifact.Jar`  an executable JAR
row  `Universe.Classfile`  `Dex`  an archive of Dalvik bytecode
row  `Universe.Classfile`  `Apk`  a complete, signed and aligned Android package
row  `Universe.Classfile`  `Xeq`  a bundle runnable straight from a shell
row  `Universe.Sjsir`  `Js`  an ECMAScript module, a CommonJS module or a plain script
row  `Universe.Sjsir`  `Wasm`  a WebAssembly module with JavaScript glue
row  `Universe.Sjsir`  `Wasi`  a standalone component at a stated WASI generation
row  `Universe.Sjsir`  `OciImage`  that component, wrapped as an OCI artifact
row  `Universe.Nir`  `Artifact.Binary`  a native executable for one target triple
```

The module system of a `Js` node is part of its identity, as the target triple is part of a
binary's.

A format several edges away needs no special handling. An `Apk` runs dexing and then packaging,
and an `OciImage` links the component and then wraps it, because those are the paths. A build
which cannot reach a format says so, with a `NoPath` naming the two formats, and does not fail
part of the way through. A setting which configures nothing on the path is rejected before any
tool runs.

An edge whose tool has prerequisites comes from a provider which demands evidence of them, so an
edge whose tooling is absent cannot be built. `sjsEdges.wasi()` needs a probed `WasiToolchain`
and a `Wasi.World`, and `nativeEdges()` probes for `clang`.

The identity of a format is its kind, which is a source, an intermediate representation or an
application, and its `id`. Wherever a parameter changes what a user receives, it is part of the
`id`, and each parameterization is a distinct node: the module system a JavaScript host imports
through, the WASI generation an artifact's ABI follows, the target triple a binary runs on, the
delivery mode of a bundle. A binary for `Triple.Arm64MacOs` is as distinct from one for
`Triple.X64Linux` as a JAR is from a JavaScript bundle. That they share a linker does not make
them interchangeable.

The set of nodes is open. `Artifact`, `Tool` and `Edge` are ordinary public types, so an edge
registered for a format which these libraries have never heard of extends the graph. A format
can be declared as data, by its kind and name, which is how a registry read at runtime declares
one:

```scala fuel
typecheck
##
val proto = Artifact.source(t"proto")
val descriptor = Artifact(Artifact.Kind.Ir, t"descriptor")
```

A declared format is the same node as the built-in format of its kind and name, so
`Artifact.ir(t"classfile")` reaches every edge registered for `Universe.Classfile`.

### Deliverables and paths

The value which flows along a path is a `Deliverable`, and its case says where in the graph it
belongs:

```tabular
column  Case
column  Holds
column  Belongs at
row  `Deliverable.Sources`  sources  a source node
row  `Deliverable.Emission`  an output directory and the classpath it was compiled against  an intermediate representation
row  `Deliverable.Product`  a linked or packaged file  an application format
```

The path itself can be asked for directly, which is how a tool reports what it is about to run:

```scala
val steps = Toolchain(jarEdges(), dexEdges(), apkEdges()).path(Universe.Classfile, Apk).map(_.target.id)
```

Here `steps` is `List(t"dex", t"apk")`. The path is the *unique shortest* one. Where none
exists, `path` raises `NoPath`. Where two are equally short, it raises `AmbiguousPath`, which is
resolved by producing an intermediate format explicitly in a separate step, and not by a
tie-break the caller cannot see. Assembly is checked as strictly. `Toolchain` rejects a
duplicate edge between the same pair of formats, and rejects a cycle.

Each intermediate product on a path of several stages is written below the destination, under
the `id` of its format, and the final product is written to the destination itself. An APK,
whose path runs `Classfile → Dex → Apk`, therefore dexes into `dex/` and packages from there.
Nothing about that sequence is written into the APK tool.

A staging rig must fold its own running classpath into the JARs it produces. It pairs
`Bundler.applicationClasspath`, which is the classpath of the running application, introspected
from the classloader which loaded it, with its compiled output:

```scala
def stage(out: Path on Linux, uuid: Uuid, executor: Fqcn)(using Monitor): Path on Linux =
  Toolchain(jarEdges()).produce
    ( Deliverable.Emission(out, Bundler.applicationClasspath),
      Universe.Classfile,
      Artifact.Jar,
      out,
      List(jarOptions.name(t"$uuid.jar")),
      List(EntryPoint(executor)) )
```

### Settings

A tool is configured by `Setting`s. A setting is addressed to the formats whose production it
configures, and not to a tool directly. `produce` applies each setting, in order, to every edge
on the path whose target it applies to. A setting which matches no format on the path raises
`InapplicableSetting` before any tool runs.

Addressing settings to formats lets one setting configure several stages. An Android API level
is a property of the application, but it governs both how D8 desugars and what the manifest
declares. So `apkOptions.minApi` applies to `Dex` and `Apk` alike, and dispatches to the
configuration type of each node. The other groups of settings are:

```tabular
column  Settings
column  Configure
column  Apply to
row  `linkerOptions.checkIr`, `sourceMaps`, `esVersion.*`, `optimize.*`  the Scala.js linker  every format the Scala.js linker produces
row  `nativeOptions.gc.*`  the garbage collector  native binaries
row  `nativeOptions.mode.*`  the build mode  native binaries
row  `nativeOptions.lto.*`  link-time optimization  native binaries
```

Entry points are ambient along the whole path in the same way, and each tool applies or ignores
them as its format demands:

```tabular
column  Format
column  Entry points
row  `Artifact.Jar`  at most one, recorded as its `Main-Class`
row  `Artifact.Binary`  exactly one
row  `Apk`  exactly one
row  `Js`, `Wasm`, `Wasi`  any number, each run as a module initializer
row  `Library`, `Dex`  ignored, since neither records an entry point
```

### WASI components

A WASI component is a `Wasi(Wasi.Version.Wasip2)`: a component-model `*.wasm` whose imports and
exports are described by WIT. It has two prerequisites beyond the linker, and the provider of
the edge demands both, so an edge without them cannot be constructed. A `WasiToolchain` is
evidence that the native tools which the link shells out to, `wasm-tools` and the scala-wasm
fork of `wit-bindgen`, are present, and is obtained only through the probing constructor
`WasiToolchain()`. A `Wasi.World` names the directory of WIT packages and the world to link
against:

```scala
def component(witDirectory: Path on Linux, emission: Deliverable, destination: Path on Linux)
:   Path on Linux =
  given WasiToolchain = WasiToolchain()
  given Wasi.World = Wasi.World(witDirectory, t"my-world")

  supervise:
    Toolchain(sjsEdges(), List(sjsEdges.wasi()), ociEdges())
    . produce(emission, Universe.Sjsir, OciImage, destination)
```

WASI generations are separate nodes, because they determine the ABI of an artifact:

```tabular
column  Generation
column  Interface
column  Producible
row  `Wasi.Version.Wasip1`  a flat, libc-style syscall interface on core modules  no
row  `Wasi.Version.Wasip2`  the component model  yes
row  `Wasi.Version.Wasip3`  the component model with native asynchrony  no
```

Asking for a generation with no edge which produces it raises `NoPath`, and does not silently
produce something else.

Wrapping a component as an OCI artifact is a further edge, `Wasi(Wasip2) → OciImage`, and not
something the component link knows about. The path composes the two. `Js(module)` and `Wasm`
need no native tooling at all, since the linker is an ordinary JVM library.

### Native binaries

The native edges shell out to `clang` and `clang++`. `nativeEdges` probes for them once, and
raises `Toolchain.Error` if either is missing. It takes the triples to target, and defaults to
the build host's own:

```scala
def binary(emission: Deliverable, destination: Path on Linux): Path on Linux = supervise:
  Toolchain(nativeEdges(Triple.Arm64MacOs, Triple.X64Linux)).produce
    ( emission,
      Universe.Nir,
      Artifact.Binary(Triple.X64Linux),
      destination,
      List(nativeOptions.mode.releaseFast, nativeOptions.gc.commix),
      List(EntryPoint(fqcn"com.example.Main")) )
```

Each triple is its own node, rendered as an LLVM target triple such as `aarch64-apple-darwin` or
`x86_64-unknown-linux-gnu`. A target other than the build host needs a C toolchain which can
cross-compile to it.

### Other languages

`Javac` compiles Java sources through the same shape of API. A tool which orchestrates
compilation does not change idiom from one language to another, and a mixed Scala and Java
application links into one artifact.

`Kotlinc` does the same for Kotlin. It is parameterized by the language version it targets, and
its options, such as `-Werror`, `-jvm-target` and explicit API mode, are under `kotlincOptions`
and checked against the version in the same way:

```scala
def kotlin(kotlinSource: Text, out: Path on Linux): CompileResult = supervise:
  val kotlinc = Kotlinc[2.4](List(kotlincOptions.warnings.asErrors, kotlincOptions.jvmTarget(17)))
  val process = kotlinc(classpath)(Map(t"demo/Greeting.kt" -> kotlinSource), out)
  process.complete()
```

The Kotlin compiler reads its sources from disk, so they are written to a scratch directory,
which is removed when the compilation ends. A diagnostic still names the source as it was given,
not the file it was written to. The Kotlin standard library is never implied. Like every other
classpath entry, it is supplied explicitly. The output is classfiles, linkable as any classfile
artifact, including a `Dex` or an `Apk`.

As with the Scala compiler behind `Scalac`, the Kotlin compiler itself is a compile-only
dependency. A tool which drives it puts `kotlin-compiler-embeddable` on its own runtime
classpath, and chooses the version it wants to drive rather than inheriting one.

Calling *into* a compiled Kotlin library needs no compilation at all. Its declarations are read
from its classfiles, as [foreign interoperability](foreign-interop.md) describes.

### Related

- [Staging](staging.md) compiles quoted code with a `Scalac` and runs it elsewhere.
- [Classpath](classpath.md) describes the classpaths which a compilation reads and produces.
- [Packaging](packaging.md) describes the JAR files and other archives which a toolchain writes.
- [Graphs](graphs.md) describes the `Dag` which a `Toolchain` is.
- [Foreign interoperability](foreign-interop.md) calls into compiled Kotlin and other
  languages.
- [Concurrency](concurrency.md) describes `supervise`, which a compilation runs inside.
