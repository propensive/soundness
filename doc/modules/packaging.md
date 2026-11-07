## Packaging

### About

An application built with Soundness is a JAR, and a JAR that carries every dependency it
needs is large: a command-line tool of a few hundred lines ships tens of megabytes of
library code, most of it already published somewhere public. Burdock makes that JAR *slim*
without making it fragile, in three steps:

```tabular
column  When
column  Step
column  Does
row  at compiletime  `externalize`  records which dependencies the application was built against, as content hashes rather than coordinates
row  after the build  `repackage`  rewrites the JAR, replacing every dependency with a public home by a reference to it, and keeping anything without one
row  at runtime  the bootstrap loader  fetches the referenced dependencies, verifies each against its hash, caches it, and starts the application
```

So the first launch downloads, every later launch is instant, and a dependency that has changed
since the build cannot be loaded.

### On content hashes

Build tools identify a dependency by coordinates — a group, a name and a version — and trust
that the same coordinates resolve to the same bytes everywhere. Burdock identifies it by the
SHA-256 of the JAR the compiler actually read. That is the stronger claim: a reference in a
repackaged JAR names the exact bytes, and the bootstrap refuses anything else, whether a
registry served a different build under the same version or a download was tampered with on
the way. The hash also decouples *where* a dependency lives from *what* it is. The repackager
asks [deps.dev](https://deps.dev/) which Maven artifact has that hash, and consults the release
assets of any GitHub repositories named as hints; a dependency found in neither is inlined from
the local cache, so the completeness invariant holds: every dependency is either referenced or
present, and a hash that is neither stops the repackage with an error rather than producing a
JAR that will fail later.

### Externalizing an application

The application wraps its entry point in `externalize`. The macro leaves the body alone at
runtime; at compiletime it captures the build's classpath, hashes each dependency JAR,
hard-links it into the Burdock cache at `~/.cache/soundness/jars/<sha256>.jar` so the bytes stay
retrievable by hash on the build machine, and embeds the list of hashes as the resource
`META-INF/burdock.deps` in the compiled output.

<!-- doccheck: skip -->
```scala
import soundness.*
import stdios.javaLangSystemStdio
import termcapDefinitions.basicTermcap

@main
def myApp(): Unit = externalize:
  Out.println(t"hello, world")
```

Nothing else about the application changes: it is still built, tested and run as an ordinary
JAR, and the embedded resource is inert until the repackager reads it.

### Repackaging

The `soundness.repackage` entry point is launched against the application JAR, which it
locates by finding the `META-INF/burdock.deps` resource on its own classpath and rewrites in
place:

```sh
java -cp app.jar:burdock.jar soundness.repackage --github propensive/soundness
```

Each `--github owner/repo` hint (repeatable, or comma-separated) names a repository whose
release assets are a download source. GitHub keeps no global index by hash, so the hints are
indexed up front — a few requests per repository, honoring a `GITHUB_TOKEN` environment
variable if one is set — and consulted before deps.dev, since a repository the user named
takes precedence over the global index. In the rewritten JAR, each class meets one of three
fates:

```tabular
column  Classes of
column  Are
row  the application  kept
row  a dependency it could resolve  dropped, and referenced instead
row  a dependency it could not resolve  inlined
```

It carries exactly one extra class: `burdock.Bootstrap`, the runtime loader. The manifest
records the arrangement:

```tabular
column  Attribute
column  Holds
row  `Main-Class`  `burdock.Bootstrap`
row  `Burdock-Main`  the application's own entry point, which the bootstrap starts once the classpath is assembled
row  `Burdock-Require`  each externalized dependency as `<sha256>:<url>`, in classpath order
row  `Burdock-Verbosity`  what the bootstrap prints while it fetches: `silent`, as the repackager writes it, or `error`, `info` or `debug`
```

The command prints a summary — entries read and written, classes kept, inlined and stripped,
and each externalized requirement — and a progress bar while it resolves, drawn only on a real
terminal so that redirected output stays clean. The same work is available as a library call,
`Repackager.repackage`, which takes the input and output paths, a `Repackager.Resolver` from
hash to URL, a `Repackager.CacheReader` for the local cache, the bootstrap class bytes and a
progress callback, and returns a `Repackager.Summary` rather than printing anything; the
command-line entry point is a thin rendering of that call, and `DepsDev.mavenUrl` and
`GitHub.index` are the two resolvers it composes.

### Running a repackaged JAR

A repackaged JAR runs with `java -jar app.jar`, exactly as before. The bootstrap reads
`Burdock-Require`, downloads every requirement that is not already in `~/.cache/soundness/jars/` (in
parallel, bounded to a handful of connections per host), verifies each download's SHA-256
against the hash in the manifest, builds the classpath in manifest order and invokes
`Burdock-Main`. A checksum mismatch stops the launch: the application never starts against
bytes other than the ones it was built with.

A launcher that supervises startup — Ethereal's daemon does — can watch the download's
progress by setting the `burdock.progress` system property to a file path; the bootstrap writes
`<completed> <total> <bytes>` to it atomically as it goes, and a launcher that cannot read the
file simply falls back to its fixed deadline.

### Errors

Repackaging reports its failures with two errors, and a lookup that finds nothing with a
third:

```tabular
column  Error
column  Raised when
row  `Repackager.UserError`  the JAR was not built with Burdock, is a directory rather than a file, an argument is malformed, or a GitHub repository's releases cannot be listed
row  `Repackager.RepackageError`  the JAR cannot be rewritten: it has no manifest or no `Main-Class`, or a dependency is neither published nor cached
row  `DepsDev.Unresolved`  a deps.dev lookup finds nothing, which the repackager treats as "not published" rather than as a failure
```

Resolution is logged through `DepsEvent`, one event per hash resolved, so a run can be audited
afterwards.
