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
- compilations are typed by target backend: JVM, JavaScript, WebAssembly or WASI
- fully-linked `.js` and `.wasm` artifacts, or unlinked `.sjsir` library JARs for
  downstream assembly
- linker options are typechecked against the link target
- a WASI component link will not typecheck until its native tooling is proven present
