- all compiler options are presented is typed values; not strings
- functional dependencies between options are encoded in their types, where
  possible
- source files and output directory may be provided in any generic file type
  using [Anticipation](https://github.com/propensive/anticipation)
- uses a typesafe [Hellenism](https://github.com/propensive/hellenism/)
  classpath
- compiler invocation is typed according to the major compiler version
- options are typechecked against the compiler version
- supports compilation with [Scala.js](https://scala-js.org/)
- compilations are typed by the universe they inhabit (JVM bytecode, sjsir or NIR), and links
  by the artifact they produce (executable JAR, JavaScript, WebAssembly, WASI component or
  native binary), with each artifact's origin universe inferred
- WASI versions (0.1, 0.2, 0.3) are part of the artifact's type, since they determine its ABI
- fully-linked `.js` and `.wasm` artifacts and native binaries, or unlinked `.sjsir`/`.nir`
  library JARs for downstream assembly
- native links may select a target platform (e.g. arm64/macOS) as a typed LLVM triple
- linker options are typechecked against the link target
- a WASI component link will not typecheck until its native tooling is proven present
