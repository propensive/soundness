## Library Archives

### About

A published library is not one artifact but several: JVM classfiles, TASTy, Scala.js IR, Scala
Native IR, sources, documentation — conventionally shipped as a handful of JARs whose relationship
to one another is a naming convention and nothing more. A `.lira` file is one content-addressed
container holding every compiled representation of a release, deduplicated, alongside a
human-readable [TEL](tel.md) manifest, API-derived version metadata that a machine can verify, and
quantum-safe signatures.

Soundness implements the language-blind core of the format: reading and writing the container, the
compatibility algebra, the publication rules, and the interface through which language-specific
canonicalizers plug in.

### On versioning that can be checked

Version numbers are a promise made by hand. "This release is backwards-compatible" is a claim a
human writes into a number, and nothing checks it; a removed method ships as a patch release, a
build somewhere breaks, and the number is discovered to have been wrong after the fact. Meanwhile
the artifacts that constitute a release are held together by filename conventions —
`_2.13`, `_sjs1`, `-sources` — which no tool validates and every tool must know.

The LIRA format addresses both. What a release *is* becomes a hash of its API, computed from the
compiled artifacts rather than asserted about them, so the compatibility relation between two
releases is derived and not declared. And every representation of the release lives in one
container, so there is nothing for a naming convention to get wrong. Everything comes from the
`soundness` package:

```scala
import soundness.*
```

An identity computed from the API itself, rather than declared, is [correctness](../philosophy/correctness.md) applied to versioning.

### Atoms and API identity

The unit of API identity is an *atom*: one externally-visible feature of a library, reduced by a
*discipline* to a key and a hash of its meaning. What counts as an atom is the language's
business, not the format's — for Scala it is derived from TASTy — but the format fixes their two
classes:

- A **rigid** atom is monotonic within a version lineage: it may be added, never removed and never
  changed. Breaking one breaks a compiled consumer.
- A **replaceable** atom may be replaced — same key, new value — within a minor release. A
  consumer compiled against the old value is behaviourally stale but not broken.

The atoms are computed from the compiled library itself, not declared: the TASTy files of a
release are unpickled against their dependency classpath and every externally-visible feature is
reduced to its atom, so the API identity is what the compiler sees and nothing else.

A release's *snapshot* is the hash of its complete, sorted atom set: its API identity as a single
value. Because atom hashes are domain-separated by discipline, the snapshot is well-defined even
where a release carries atoms from several languages.

### Grades and lineages

The relation between two releases follows from their atoms rather than from their numbers.
`Grade.between` computes it:

- **Patch** — the atom sets are identical. The API has not changed at all.
- **Minor** — every rigid atom of the predecessor survives, and every replaceable key survives
  either unchanged or replaced. Pure extension plus permitted churn.
- **Major** — anything else, which must begin a fresh lineage.

A *lineage* is a module's verifiable history within one major series: the snapshots of its
releases, oldest first, each appearing once — patches do not append, having changed nothing.
Every compatibility question reduces to membership: a candidate release satisfies a requirement
exactly when the required snapshot appears in the candidate's lineage. That is a question about
hashes of real APIs, answerable without trusting anyone's version number.

### The container

A `.lira` file is one document with a fixed byte layout: an interpreter directive on the first
line, a TEL manifest, a separator line of exactly `##`, and then the binary blob stream. The
prefix is byte-fixed precisely so that finding the separator needs no TEL parsing — everything
after it is binary.

The blobs are content-addressed and deduplicated, so a class that appears in several sections is
stored once, and assembly is byte-deterministic: the same inputs and the same toolchain produce
the same file, bit for bit. Every hash the format defines is domain-separated —
`BLAKE3-256(domain ++ 0x00 ++ content)`, with the domain carrying the format epoch — so a hash
computed for one purpose can never collide with one computed for another.

Findings come in two kinds, and the distinction is deliberate. A violation of a validity rule is a
`Lira.Error` carrying the specification's own code for it. A warn-only finding — a decorative
version that disagrees with the computed grade, a blob nothing references — is an advisory,
reported alongside a successful operation rather than failing it.

### Publication

Publishing is stricter than building, and the rules are checked rather than trusted: a published
release carries a strictly numeric version; a tag names exactly one release of its module,
permanently, so a re-signed manifest may add tags but never drop or reassign one. A release
failing any of these is a development release and stays one.

Manifests are signed, and the signatures are quantum-safe, so a release's provenance survives the
arrival of a cryptographically relevant quantum computer rather than being retrospectively
worthless.

### Derivative artifacts

The existing world runs on JARs, and a release must be usable from an ordinary classpath.
A *derivative* is a byte-deterministic JAR built from one section of the container, whose hash the
manifest declares — so a JAR found on a classpath can be traced back to the release that produced
it, rather than merely being assumed to have come from it. The derivation profile is pinned
permanently: entries in ascending path order, no directory entries, and the rest fixed, because a
derivative that varied with the tool that built it could not be identified by hash.

### Disciplines

A discipline is the language-specific half: it reads a compiled artifact and emits its atoms.
`OpaqueDiscipline` treats content as an opaque blob with no API surface, which is right for
resources; `ResourceDiscipline` and `CapabilityDiscipline` handle the other content the format
recognizes; and a language canonicalizer — TASTy, for Scala — plugs in through the same interface.
The core deals only in atoms, so it needs to know nothing about any language.

One consequence shapes the interface: a replaceable atom's content may refer to something in
another module, and a cross-module value hash cannot be computed from one module's content alone.
So disciplines emit *names*, and the core resolves them against the dependencies' atom sets at
assembly time by exact key matching.
