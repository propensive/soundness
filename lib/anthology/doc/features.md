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
- a build is a directed acyclic graph whose nodes are formats (source languages, intermediate
  representations and application types) and whose edges are tools, extensible with new formats
  and tools of your own
- producing an artifact is path search: a multi-stage product such as an Android package or an
  OCI artifact runs each tool on the path in turn, with nothing hard-coded about the sequence
- distinctions that change what a user gets are node identity: WASI generations, JavaScript
  module systems, native target triples and bundle delivery modes
- one verb for every product: executable JARs, `.js`, `.wasm`, native binaries, Android packages,
  runnable bundles and unlinked library JARs all come from `Toolchain#produce`
- settings are addressed to the formats they configure, so one setting may configure several
  stages of a path (an Android API level governs both dexing and the manifest)
- an edge whose native tooling is absent cannot be constructed: a WASI component link demands
  proof of `wasm-tools` and `wit-bindgen`, and the native edges probe for `clang`
