### Compiling

A Scala compiler is represented by a `Scalac` value, parameterized by the compiler version and
the backend it targets, with a list of compiler options whose validity is checked against the
version at compile time:
```scala
val scalac: Scalac[3.8, Backend.Jvm] = Scalac[3.8](List(scalacOptions.experimental))
```

The single-type-argument form, `Scalac[3.8](options)`, targets the JVM. The same options may be
retargeted at another backend with `targeting`:
```scala
val portable = Scalac[3.8](List(scalacOptions.experimental)).targeting[Backend.Portable]
```

Five backends exist: `Backend.Jvm`, which produces classfiles; the three _portable_
backends—`Backend.Js`, `Backend.Wasm` and `Backend.Wasi`—which additionally produce
target-neutral `.sjsir` files, deferring the choice of linked representation until link time;
and `Backend.Native`, which produces `.nir` files for Scala Native. `Backend.Portable`, the
union of the three sjsir backends, is the natural choice when compiling a library whose linked
form is not yet known; a portable compilation may later be linked as _any_ of the three.
`Backend.Linked` is the union of every backend whose output is linkable.

Compiling for a portable backend requires the Scala.js runtime JARs (`scalajs-javalib`,
`scalajs-library_2.13`, `scalajs-scalalib_2.13` and `scala3-library_sjs1`) on the classpath;
their absence surfaces as ordinary compiler diagnostics. Compiling for `Backend.Native`
additionally requires a `NirPlugin` in scope—evidence of the location of the Scala Native
compiler plugin, since NIR is emitted by a plugin rather than a backend built into the
compiler—plus the Scala Native runtime JARs (`scalalib`, `scala3lib`, `javalib`, `auxlib`,
`clib`, `posixlib` and `nativelib`) on the classpath.

### Products

The result of a compilation—an output directory and the classpath it was compiled against—is
described by a `Compilation`, tagged with its backend:
```scala
val compilation: Compilation[Backend.Portable] = Compilation(out, classpath)
```

Three products can be made from a compilation, and each is only expressible for the backends it
is valid for:
 - `Bundler.bundle(compilation, jarfile, main)` produces an executable JAR, and requires a
   `Compilation[Backend.Jvm]`
 - `Bundler.library(compilation, jarfile)` produces a library JAR of `.sjsir` or `.nir` files
   for downstream assembly, and requires a linkable compilation
 - `Linker[target](options, entryPoints).link(compilation, out)` produces a fully-linked
   artifact for a concrete linkable target

### Linking

A `Linker` is parameterized by the concrete backend it links for, with options whose validity is
checked against that backend at compile time, mirroring `scalacOptions`:
```scala
val linker = Linker[Backend.Js]
  ( List(linkerOptions.moduleKind.esModule, linkerOptions.optimize.fast),
    List(Linker.EntryPoint(fqcn"com.example.Main")) )

val artifact: Path on Linux = linker.link(compilation, out)
```

`Backend.Js` produces a `main.js`; `Backend.Wasm` produces a `main.wasm` (with a JavaScript
loader alongside) for browsers and JavaScript runtimes; `Backend.Wasi` produces a standalone
WASI component, `main.wasm`, for runtimes such as `wasmtime`.

Options such as `moduleKind.*` apply only to `Backend.Js` (the WebAssembly backends mandate
their own module kinds), while `checkIr`, `sourceMaps`, `esVersion.*` and `optimize.*` apply to
every portable backend; a misapplied option is a compile error.

### Linking WASI components

A WASI component link has two prerequisites beyond the linker itself, both expressed as
contextual values without which `Linker[Backend.Wasi].link` does not compile:

 - a `WasiToolchain`, evidence that the native tools the link shells out to—`wasm-tools` and the
   scala-wasm fork of `wit-bindgen`—are present; instances exist only via the probing
   constructor, `WasiToolchain()`, which raises `ToolchainError` if a tool is missing
 - a `WitWorld`, naming the directory of WIT packages and the world to link against

```scala
given WasiToolchain = WasiToolchain()
given WitWorld = WitWorld(witDirectory, t"my-world")

Linker[Backend.Wasi](Nil).link(compilation, out)
```

`Backend.Js` and `Backend.Wasm` require no native tooling: the linker is an ordinary JVM
library.

### Linking native binaries

A Scala Native link shells out to `clang` and `clang++`, so its `Linkage` exists only via the
probing constructor `NativeLinkage()`, which verifies the C toolchain is present (raising
`ToolchainError` otherwise):
```scala
given Linkage[Backend.Native] = NativeLinkage()

Linker[Backend.Native]
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
