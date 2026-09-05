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

A distributable described as a value in the build is [direct style](../philosophy/direct-style.md) applied to packaging.

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

<!-- doccheck: skip -->
```scala
val jarPath = t"/tmp/mytool.jar".decode[Path on Linux]
val outputPath = t"/tmp/mytool".decode[Path on Linux]
val runnerSource = Packaging.RunnerSource.Remote(Runners.baseUrl, Runners.hashes)

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

### Bundling as a toolchain format

The same packaging is reachable as a [toolchain](compiler.md) format, so an application can be
compiled and bundled in one path rather than packaged as a separate step afterwards. An `Xeq`
bundle runs from `Jar` rather than from a universe, and the delivery mode is part of the node's
identity, since each is a different distributable:

<!-- doccheck: skip -->
```scala
Toolchain(jarEdges(), xeqEdges()).produce
  ( Deliverable.Emission(out, classpath),
    Universe.Classfile,
    Xeq(Packaging.Delivery.EmbedAll),
    destination,
    List(xeqOptions.name(t"mytool"), xeqOptions.runners.standard),
    List(EntryPoint(fqcn"com.example.Main")) )
```

`xeqOptions.runners.standard` names the published runner release, verified against its committed
manifest, while `runners.local` reads prebuilt stubs from a directory instead. Targets default to
every platform the runner source names, and `xeqOptions.target` adds one explicitly.
`xeqOptions.java` sets the minimum and preferred JVM versions, `bundle.jre` and `bundle.jdk`
ship one alongside, and `signing` and `buildId` configure the self-upgrade signing and upgrade
ordering recorded in each stub.

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

Hashes are resolved against Maven Central through deps.dev. There is no such global index for
GitHub releases, but a repository's releases can be listed one at a time, so the repackager
accepts hints naming repositories whose release assets are also a download source:

```sh
java -cp app.jar soundness.repackage --github propensive/soundness --github other/library
```

Hinted repositories are consulted before deps.dev, so a hint takes precedence over the global
index. Only `.jar` assets that GitHub reports a SHA-256 digest for can be matched (assets
uploaded before GitHub computed digests are not). A `GITHUB_TOKEN` in the environment is sent
with the API requests, which lifts the unauthenticated rate limit.

### Shading

A library that bundles its dependencies can relocate them at *compile* time, with a compiler
plugin, rather than rewriting bytecode afterward:

```sh
scalac -Xplugin:umbrageous.jar -P:umbrageous:com.example:shaded *.scala
```

Every package matching `com.example` compiles as `shaded.com.example`, and references follow, so
two versions of the same library coexist in one classpath without touching each other.

### Android applications

An Android application is a further target of the same [toolchain](compiler.md) rather than a
separate one. Classfiles dex to Dalvik bytecode as a `Dex` archive, and `Apk` goes one edge
further: the dexed code, a binary `AndroidManifest.xml`, zip-aligned and signed, ready to install.
Asking for the `Apk` runs both tools, since that is the path between the two formats:

<!-- doccheck: skip -->
```scala
Toolchain(dexEdges(), apkEdges()).produce
  ( Deliverable.Emission(output, classpath),
    Universe.Classfile,
    Apk,
    destination,
    List(apkOptions.minApi(24)),
    List(EntryPoint(fqcn"com.example.MainActivity")) )
```

The manifest is built from a typed configuration and encoded as Android's binary XML, and signing
uses the APK signature scheme, so no external `aapt`, `zipalign` or `apksigner` is involved.
