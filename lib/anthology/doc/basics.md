### Formats and toolchains

Anthology models a build as a directed acyclic graph. Its nodes are _formats_ and its edges are
_tools_; producing one format from another is a matter of finding the path between them and
running each edge's tool in turn.

Formats come in three tiers. A `Format.Source` is a language a compiler consumes: `Language.Scala`,
`Language.Java` or `Language.Kotlin`. A `Format.Ir` is an intermediate representation, in which
libraries compose but nothing yet runs: the three `Universe` values, `Universe.Classfile` (JVM
classfiles), `Universe.Sjsir` (Scala.js IR) and `Universe.Nir` (Scala Native IR). A
`Format.Application` is a closed product for a host to run: `Jar` (an executable JAR, bound to the
JDK), `Dex` (Dalvik bytecode), `Apk` (a complete, signed Android package), `Js(module)`
(JavaScript, bound to a JavaScript host through the `Es`, `CommonJs` or `Script` module system),
`Wasm` (a core WebAssembly module with JavaScript glue), `Wasi(version)` (a standalone WebAssembly
artifact bound to a WASI generation), `OciImage` (a WASI component packaged as a Wasm OCI
Artifact), `Binary(triple)` (a machine-code executable for one target platform) and `Xeq(delivery)`
(a command-line-runnable bundle). `Library(universe)`—a library JAR of a compilation's unlinked
output, for downstream assembly—exists for every universe.

Formats are values, and their identity is value equality, so wherever a parameter changes what a
user gets, it is a constructor parameter and each parameterization is a distinct node: the module
system a JavaScript host imports through, the WASI generation an artifact's ABI follows, the target
triple a binary runs on, the delivery mode of a bundle. A binary for `Triple.Arm64MacOs` is as
distinct from one for `Triple.X64Linux` as a JAR is from a JavaScript bundle; that they share a
linker does not make them interchangeable.

The node set is open. `Format`, `Tool` and `Edge` are ordinary public types, so registering an edge
for a format anthology has never heard of extends the graph.

### Assembling a toolchain

Each component contributes its edges through a provider function, which you compose into a
`Toolchain`:

```scala
val toolchain = Toolchain(jarEdges(), dexEdges(), apkEdges())
```

Edges whose tools have prerequisites are provided by functions that demand evidence of them, so an
edge whose tooling is absent cannot be constructed: `sjsEdges.wasi()` requires a `WasiToolchain`
and a `WitWorld`, `nativeEdges()` probes the C toolchain and raises `ToolchainError` if it is
missing, and `scalacEdges.nir(scalac)` requires a `NirPlugin`.

`Toolchain` rejects a duplicate edge between the same pair of formats, and rejects a cycle. Given a
valid one, `path` reports the tools that produce one format from another:

```scala
toolchain.path(Universe.Classfile, Apk).map(_.target.id)   // List(t"dex", t"apk")
```

The path is the unique shortest one. Where none exists, `path` raises `LinkError.Reason.NoPath`;
where two are equally short, it raises `AmbiguousPath`, resolved by producing an intermediate
format explicitly in a separate step.

### Compiling

A Scala compiler is represented by a `Scalac` value, parameterized by the compiler version and the
universe it compiles into, with options whose validity is checked against the version at compile
time:

```scala
val scalac: Scalac[3.8, Universe.Classfile] = Scalac[3.8](List(scalacOptions.experimental))
```

The single-type-argument form targets the classfile universe; `targeting` selects a universe
explicitly:

```scala
val forJs = Scalac[3.8](options).targeting[Universe.Sjsir]
```

Compiling into `Universe.Sjsir` requires the Scala.js runtime JARs (`scalajs-javalib`,
`scalajs-library_2.13`, `scalajs-scalalib_2.13` and `scala3-library_sjs1`) on the classpath; their
absence surfaces as ordinary compiler diagnostics. Compiling into `Universe.Nir` additionally
requires a `NirPlugin` in scope—evidence of the location of the Scala Native compiler plugin, since
NIR is emitted by a plugin rather than a backend built into the compiler—plus the Scala Native
runtime JARs (`scalalib`, `scala3lib`, `javalib`, `auxlib`, `clib`, `posixlib` and `nativelib`) on
the classpath.

### Producing an artifact

The value flowing along a path is a `Deliverable`: `Sources` at a source node, an `Emission`—an
output directory and the classpath it was compiled against—at an intermediate representation, and a
`Product`—a linked or packaged file—at an application format.

Every artifact is produced by the same verb, `Toolchain#produce`, which takes the input, the format
it inhabits, the format wanted, and where to write it:

```scala
val jarfile: Path on Linux =
  Toolchain(jarEdges()).produce
    ( Deliverable.Emission(out, classpath),
      Universe.Classfile,
      Jar,
      destination,
      List(jarOptions.name(t"app.jar")),
      List(EntryPoint(fqcn"com.example.Main")) )
```

Each intermediate product on a multi-stage path is written below the destination under its format's
`id`, and the final product to the destination itself. So an APK, whose path runs
`Classfile → Dex → Apk`, dexes into `dex/` and then packages from there; nothing about that
sequence is written into the APK tool.

Entry points are ambient along the whole path, and each tool applies or ignores them as its format
demands: an executable JAR takes at most one, as its `Main-Class`; a native binary and an APK
require exactly one; a library JAR and a DEX archive ignore them entirely, since neither records an
entry point.

A staging rig, which must fold its own running classpath into the JARs it produces, pairs
`Bundler.applicationClasspath`—the running application's classpath, introspected from the
thread-context classloader—with its compiled output:

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
configuration type:

```scala
Toolchain(dexEdges(), apkEdges()).produce
  ( Deliverable.Emission(out, classpath),
    Universe.Classfile,
    Apk,
    destination,
    List
      ( apkOptions.minApi(26),
        dexOptions.mode.release,
        apkOptions.packageName(t"com.example.app"),
        apkOptions.version(1, t"1.0") ),
    List(EntryPoint(fqcn"com.example.MainActivity")) )
```

The Scala.js settings—`linkerOptions.checkIr`, `sourceMaps`, `esVersion.*` and `optimize.*`—apply
to every format the Scala.js linker produces; the native settings cover the garbage collector
(`nativeOptions.gc.*`), build mode (`mode.*`) and link-time optimization (`lto.*`).

### WASI components and OCI artifacts

A WASI component—a `Wasi(Wasi.Version.Wasip2)`, a component-model `.wasm` whose imports and exports
are described by WIT—has two prerequisites beyond the linker itself, both demanded by the edge
provider, so an edge without them cannot be constructed:

 - a `WasiToolchain`, evidence that the native tools the link shells out to—`wasm-tools` and the
   scala-wasm fork of `wit-bindgen`—are present; instances exist only via the probing constructor,
   `WasiToolchain()`, which raises `ToolchainError` if a tool is missing
 - a `WitWorld`, naming the directory of WIT packages and the world to link against

```scala
given WasiToolchain = WasiToolchain()
given WitWorld = WitWorld(witDirectory, t"my-world")

val toolchain = Toolchain(sjsEdges(), List(sjsEdges.wasi()), ociEdges())
toolchain.produce(emission, Universe.Sjsir, OciImage, destination)
```

WASI generations are separate nodes because they determine an artifact's ABI: `Wasip1` is a flat,
libc-style syscall interface on core modules; `Wasip2` is the component model; `Wasip3` adds native
asynchrony. Only `Wasip2` has an edge producing it, so asking for `Wasip3` raises `NoPath`.

Wrapping a component as an OCI artifact is a further edge, `Wasi(Wasip2) → OciImage`, rather than
anything the component link knows about. The path composes the two: the component is linked exactly
as it would be on its own, and the wrapping step reads its imports and exports from the same WIT
world to write the artifact's config blob. `Js(module)` and `Wasm` require no native tooling at all;
the linker is an ordinary JVM library.

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

### Runnable bundles

An `Xeq` bundle packages an executable JAR for direct invocation from a shell, so its edges run from
`Jar` rather than from a universe. The delivery mode is part of the node's identity, since each is a
different distributable: `EmbedAll` is a polyglot installer script embedding every platform's runner
stub, `Download` a launcher that fetches the right stub on first run, and `Native` a single
self-contained binary for one platform.

```scala
Toolchain(jarEdges(), xeqEdges()).produce
  ( Deliverable.Emission(out, classpath),
    Universe.Classfile,
    Xeq(Packaging.Delivery.EmbedAll),
    destination,
    List(xeqOptions.name(t"mytool"), xeqOptions.runners.standard),
    List(EntryPoint(fqcn"com.example.Main")) )
```

`xeqOptions.runners.standard` names the published runner release; `runners.local` reads prebuilt
stubs from a directory instead. Targets default to every platform the runner source names, and
`xeqOptions.java`, `bundle.*`, `signing` and `buildId` configure the Java policy, self-upgrade
signing and upgrade ordering recorded in each stub.
