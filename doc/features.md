- all compiler options are presented is typed values; not strings
- functional dependencies between options are encoded in their types, where
  possible
- source files and output directory may be provided in any generic file type
  using [Anticipation](https://github.com/propensive/anticipation)
- uses a typesafe [Hellenism](https://github.com/propensive/hellenism/)
  classpath
- compiler invocation is typed according to the major compiler version
- options are typechecked against the compiler version
- supports compilation with [https://scala-js.org/](Scala.js)
