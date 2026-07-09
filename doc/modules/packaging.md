## Packaging

### About

Shipping a JVM application to someone who just wants to run it involves three distinct problems,
and Soundness addresses each. *Distribution*: a JAR becomes a self-contained executable — a native
launcher per platform, or a single polyglot installer script that runs as shell script, batch file
and PowerShell alike. *Dependencies*: a fat JAR slims to a thin launcher whose published
dependencies are fetched by URL and verified by hash on first run. *Isolation*: a library's
dependencies shade into a private namespace at compile time, so they cannot collide with anyone
else's.

### On distribution

"Install the JVM, download the JAR, run this command" loses users at every step. What a
command-line tool should ship as is one file that runs — finding or fetching a suitable JVM
itself — and what it should weigh is its own code, not megabytes of dependencies that already sit
on a public repository. And when the tool is a library, its bundled dependencies must not fight
the host application's: the oldest deployment problem on the JVM.

Everything comes from the `soundness` package:

```scala
import soundness.*
```

### Executables and installers

A packaging configuration names the application, its targets and its delivery, and `pack` produces
the artifact. *Native* delivery assembles one launcher binary for one platform; *embed-all*
produces a polyglot installer script carrying every platform's launcher and the application,
choosing the right one where it runs; *download* keeps the script small, fetching the platform's
launcher on demand and verifying it by [hash](hashing.md):

```scala
val packaging = Packaging
  ( name         = t"mytool",
    targets      = List(t"linux-amd64", t"darwin-arm64"),
    delivery     = Packaging.Delivery.EmbedAll,
    dependencies = Packaging.Dependencies.FatJar(jarPath),
    output       = outputPath,
    runnerSource = runnerSource )

Packager.pack(packaging)
```

The launchers are the same ones that run [daemonized applications](daemons.md): they locate or
fetch a JVM within the configured version policy, and support signed self-upgrade.

### Thin launchers

Most of a fat JAR is dependencies that live on Maven Central. Wrapping the application's entry
point in `externalize` records, as it compiles, the hash of every dependency on its classpath:

```scala
@main
def mytool(): Unit = externalize:
  runApplication()
```

Repackaging then splits the JAR: dependencies whose hashes resolve to published artifacts are
replaced by URL-and-hash references, and only unpublished code stays inlined. The resulting thin
JAR carries a small bootstrap that, on first run, downloads each requirement, verifies its hash,
caches it, and launches — so the artifact that users download is the application, and the
dependencies arrive once, verified, from where they already live.

### Shading

A library that bundles its dependencies can relocate them at *compile* time, with a compiler
plugin, rather than rewriting bytecode afterward:

```sh
scalac -Xplugin:umbrageous.jar -P:umbrageous:com.example:shaded *.scala
```

Every package matching `com.example` compiles as `shaded.com.example`, and references follow, so
two versions of the same library coexist in one classpath without touching each other.
