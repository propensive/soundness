package scala.caps

// Proscala's `scala.caps.SharedUnscoped` (a capture-checking marker trait the fork adds in its
// supplementary `proscala-library`), compiled here to NIR so the Scala Native link can resolve it:
// the fork publishes `proscala-library` for the JVM and Scala.js but not yet for Scala Native, and
// the Native cross compiles against the JVM jar, which the linker cannot read. This is the whole
// library surface Soundness's Native code reaches; it goes when `proscala-library_native0.5_3`
// exists.
trait SharedUnscoped
