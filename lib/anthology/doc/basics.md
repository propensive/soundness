### Universes and artifacts

Anthology structures compilation around two tiers. A _universe_ is the intermediate
representation a compilation emits, and hence the ecosystem of library artifacts it can link
with: `Universe.Bytecode` (JVM classfiles), `Universe.Sjsir` (Scala.js IR) and `Universe.Nir`
(Scala Native IR). An _artifact_ is a linked end product: `Artifact.Jar` (an executable JAR,
bound to the JDK), `Artifact.Js` (a JavaScript module or script, bound to a JavaScript host),
`Artifact.Wasm` (a core WebAssembly module with JavaScript glue), `Artifact.Wasi[version]` (a
standalone WebAssembly artifact bound to a version of the WASI system interface: `0.1`, `0.2` or
`0.3`) and `Artifact.Binary` (a machine-code executable).

Each artifact is produced from exactly one universe, witnessed by a `Provenance` instance:
`Jar` from `Bytecode`; `Js`, `Wasm` and `Wasi` from `Sjsir`; `Binary` from `Nir`. The sjsir
universe defers the choice among its three artifacts until link time, so one compilation may be
linked as any of them.

### Compiling

A Scala compiler is represented by a `Scalac` value, parameterized by the compiler version and
the universe it compiles into, with options whose validity is checked against the version at
compile time:
```scala
val scalac: Scalac[3.8, Universe.Bytecode] = Scalac[3.8](List(scalacOptions.experimental))
```

The single-type-argument form targets the bytecode universe. `targeting` selects a universe
explicitly, while `producing` selects it via the artifact you ultimately want:
```scala
val forJs = Scalac[3.8](options).producing[Artifact.Js]            // Scalac[3.8, Universe.Sjsir]
val forNative = Scalac[3.8](options).targeting[Universe.Nir]
```

Compiling into `Universe.Sjsir` requires the Scala.js runtime JARs (`scalajs-javalib`,
`scalajs-library_2.13`, `scalajs-scalalib_2.13` and `scala3-library_sjs1`) on the classpath;
their absence surfaces as ordinary compiler diagnostics. Compiling into `Universe.Nir`
additionally requires a `NirPlugin` in scope—evidence of the location of the Scala Native
compiler plugin, since NIR is emitted by a plugin rather than a backend built into the
compiler—plus the Scala Native runtime JARs (`scalalib`, `scala3lib`, `javalib`, `auxlib`,
`clib`, `posixlib` and `nativelib`) on the classpath.

### Products

The result of a compilation—an output directory and the classpath it was compiled against—is
described by a `Compilation`, tagged with its universe:
```scala
val compilation: Compilation[Universe.Sjsir] = Compilation(out, classpath)
```

 - `Bundler.bundle(compilation, jarfile, main)` produces an executable JAR from a bytecode
   compilation
 - `Bundler.library(compilation, jarfile)` produces a library JAR from a compilation in _any_
   universe—classfiles, `.sjsir` or `.nir`—for downstream assembly
 - `Linker[artifact](options, entryPoints).link(compilation, out)` produces a fully-linked
   artifact, and requires a compilation from that artifact's origin universe

### Linking

A `Linker` is parameterized by the artifact it produces, with options whose validity is checked
against that artifact at compile time, mirroring `scalacOptions`:
```scala
val linker = Linker[Artifact.Js]
  ( List(linkerOptions.moduleKind.esModule, linkerOptions.optimize.fast),
    List(Linker.EntryPoint(fqcn"com.example.Main")) )

val mainJs: Path on Linux = linker.link(compilation, out)
```

The universe is inferred: linking `Artifact.Js` demands a `Compilation[Universe.Sjsir]`, and
supplying a compilation from any other universe is a compile error. Options such as
`moduleKind.*` apply only to `Artifact.Js` (the WebAssembly artifacts mandate their own module
kinds), while `checkIr`, `sourceMaps`, `esVersion.*` and `optimize.*` apply to every sjsir
artifact; a misapplied option is a compile error.

### Linking WASI artifacts

A WASI link produces an `Artifact.Wasi[0.2]`—a component-model `.wasm` whose imports and
exports are described by WIT. It has two prerequisites beyond the linker itself, both expressed
as contextual values without which the link does not compile:

 - a `WasiToolchain`, evidence that the native tools the link shells out to—`wasm-tools` and the
   scala-wasm fork of `wit-bindgen`—are present; instances exist only via the probing
   constructor, `WasiToolchain()`, which raises `ToolchainError` if a tool is missing
 - a `WitWorld`, naming the directory of WIT packages and the world to link against

```scala
given WasiToolchain = WasiToolchain()
given WitWorld = WitWorld(witDirectory, t"my-world")

Linker[Artifact.Wasi[0.2]](Nil).link(compilation, out)
```

WASI versions are part of the artifact's type because they determine its ABI: `0.1` (preview 1)
is a flat, libc-style syscall interface on core modules; `0.2` is the component model; `0.3`
adds native asynchrony. Only `0.2` is currently linkable—requesting `Artifact.Wasi[0.3]` is a
compile error until a `Linkage` for it exists.

`Artifact.Js` and `Artifact.Wasm` require no native tooling: the linker is an ordinary JVM
library.

### Linking native binaries

A Scala Native link shells out to `clang` and `clang++`, so its `Linkage` exists only via the
probing constructor `NativeLinkage()`, which verifies the C toolchain is present (raising
`ToolchainError` otherwise):
```scala
given NativeLinkage = NativeLinkage()

Linker[Artifact.Binary]
  ( List(nativeOptions.mode.releaseFast, nativeOptions.gc.commix),
    List(Linker.EntryPoint(fqcn"com.example.Main")) )
. link(compilation, out)
```

A native link requires exactly one entry point, and produces an executable binary. Native
options cover the garbage collector (`gc.*`), build mode (`mode.*`), link-time optimization
(`lto.*`) and the target platform: `nativeOptions.target(triple)` selects a `Triple` such as
`Triple.Arm64MacOs` or `Triple.X64Linux`, rendered as an LLVM target triple. The default
targets the build host; other targets require a C toolchain capable of cross-compiling to
them.
