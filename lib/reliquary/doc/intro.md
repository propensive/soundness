Reliquary is an implementation of [LIRA](https://github.com/propensive/lira), the Library IR
Archive format: a single `.lira` file carries every compiled representation of a library
release — JVM classfiles and TASTy, Scala.js IR, Scala Native IR — deduplicated in one
content-addressed container, together with a human-readable TEL manifest, machine-verifiable
API-derived version metadata, and quantum-safe signatures.

Reliquary provides the language-blind core of the format: the container reader and writer, the
compatibility algebra (atoms, snapshots, lineages and release grades), buildpath validation,
derivative-JAR derivation, and the discipline interface by which language-specific canonicalizers
(such as `degustation`, the Scala TASTy discipline) plug in.
