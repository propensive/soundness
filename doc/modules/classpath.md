## Classpaths

### About

The JVM [classpath](https://en.wikipedia.org/wiki/Classpath) and its
[classloaders](https://en.wikipedia.org/wiki/Java_Classloader) are represented as values.

A resource bundled on the classpath is read through a reference which the compiler checks. A
classpath is an [immutable] list of its entries, which are directories, JAR files and the Java
runtime, and the list can be inspected. A block of code can be run under a chosen classloader.

### On the classpath

A program which asks for a bundled data file, such as a template, a lookup table or an icon,
asks the classpath, which searches the directories and archives the program was launched with.
The Java API for this returns `null` when a resource is absent, hands back a raw `InputStream`,
and exposes the hierarchy of classloaders as mutable state shared between threads. None of it is
typed. A missing resource is discovered only when the `null` is dereferenced.

Here a classpath resource is a [path](paths.md) like any other, and is read with the same
`read` as a file. A classpath is an immutable value whose entries can be listed. A classloader
is chosen explicitly, as a [contextual value], and not reached for implicitly. Typed entries and
classloaders give [honest signatures](../philosophy/honest-signatures.md) to a part of the JVM
which usually has none.

Everything comes from the `soundness` package. The other imports choose a classloader, a
[strategy](errors.md#choosing-a-strategy) for errors, and access to the system properties, and
let the samples print their results:

```scala
import soundness.*
import classloaders.threadContextClassloader
import strategies.throwUnsafely
import systems.javaBaseSystem
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

Most of the samples on this page depend on the machine which runs them, so they are compiled but
not run.

### Reading a resource

The `cp"…"` [interpolator] names a resource on the classpath, and checks the reference as the
code compiles. The result is a `Resource`, which is read like any other source: as raw bytes, as
text, or as a parsed value. The Scala library is on every classpath, so one of its classes makes
a resource which is always present:

```scala
val option = cp"/scala/Option.class".read[Data]
```

The resource is resolved against the classloader in scope, so the read needs no handle passed by
hand. A resource path can also be built from text at [runtime], by decoding it:

```scala
val same = t"/scala/Option.class".as[Path on Classpath]
```

The check at compiletime is against the classpath of the compiler. A reference to a resource
which is not there is a compile error:

```scala fuel
compile
errors show
##
cp"/scala/Nothing/Here.txt"
```

A classpath path is also *substantiable*. Its existence can be tested at runtime before it is
read, so a program can check for an optional resource rather than handle a failure.

### Choosing a classloader

A resource reference names a path, not a thing. Two classloaders may resolve the same path to
different resources, or one may resolve it to nothing at all. The choice cannot be implicit in
the reference, and comes from the context in which the resource is read. The `given
Classloader` in scope decides, and is chosen by import:

```scala fuel
typecheck
##
import classloaders.systemClassloader
```

The `classloaders` package holds the standard choices:

```tabular
column  Given
column  Resolves through
row  `threadContextClassloader`  the current thread's context classloader, which suits most applications
row  `systemClassloader`  the JVM's system classloader
row  `platformClassloader`  the JVM's platform classloader
row  `scalaClassloader`  the classloader that loaded the Scala library
```

### Inspecting the classpath

A `LocalClasspath` is the classpath as a list of entries. The classpath of the running program
is the value of the `java.class.path` property, which decodes to one. Reading the property needs
the `System` in scope, which was imported above:

```scala
val classpath = System.properties.java.`class`.path().as[LocalClasspath]
```

Its `entries` are typed, as one of four kinds of `Classpath.Entry`:

```tabular
column  Entry
column  Represents
column  Its `path`
column  From `classify`
row  `Directory(path)`  a directory of classes and resources  the directory  a directory, or a path that does not yet exist
row  `Jar(path)`  an archive  the archive file  a file ending with a ZIP archive's trailer
row  `Url(url)`  a remote location  `Unset`  never
row  `JavaRuntime`  the JDK's own classes  `Unset`  never
```

The entries of a *local* classpath are known not to be URLs, so a match over them need not handle
that case:

```scala
val descriptions = classpath.entries.map:
  case Classpath.Entry.Directory(path) => t"directory $path"
  case Classpath.Entry.Jar(path)       => t"archive $path"
  case Classpath.Entry.JavaRuntime     => t"the Java runtime"
```

An entry is classified by what is on disk, not by its name. `Classpath.Entry.classify(path)`
makes a nonexistent path a `Directory`, since it may become one. It makes a file which ends with
a ZIP trailer a `Jar`, which is what a launcher's executable with a shebang prefix is, though its
name does not end in `jar`. It raises an `Io.Error` for any other file.

`LocalClasspath.running` is the classpath which the running process was actually loaded from:
the entries of a `URLClassLoader`, then those of its parents, then `java.class.path`, in order
and each once. A tool which compiles or loads code against itself needs this:

```scala
import filesystemOptions.dereferenceSymlinks

val running = LocalClasspath.running()
val paths = running.entries.map(_.path)
```

### Loading services

The JVM's [service-provider](https://en.wikipedia.org/wiki/Service_provider_interface)
mechanism finds the implementations of an interface which are declared on the classpath.
`services` returns them, typed to the service:

```scala
val schemes = classpath.services[java.nio.file.spi.FileSystemProvider].map(_.getScheme)
```

### Building a classloader

A classpath builds a classloader over a chosen parent, which is the platform loader unless
another is given. The *delegation* of the loader says which side wins when it and its parent
both offer a class of the same name, and it is always spelled out. `Deferential` is the usual
order of the JVM, which defers to the parent, and so shares the parent's classes with the code
being loaded. `Preferential` prefers the entries of the classpath itself, which isolates a
plugin's versions of its libraries from its host's:

```scala
val contract = LocalClasspath(Classpath.Entry.Jar(t"/lib/contract.jar"))
val plugin = LocalClasspath(Classpath.Entry.Jar(t"/lib/plugin.jar"))
val shared = contract.classloader(Classloader.Delegation.Deferential)
val isolated = plugin.classloader(Classloader.Delegation.Preferential, parent = shared)
```

Here the plugin sees the contract it was written against through its parent, while everything
else it bundles is its own.

A resource is read from a loader directly, by its path. The result is an [optional] value which
retains nothing, so the read can sit inside a `safely` block:

```scala
val version = safely(isolated(t"META-INF/plugin.version"))
```

### Running under a classloader

A `Classloader` value runs a block with itself installed as the context classloader of the
thread, and restores the previous one afterward. This confines a change of classloader to
exactly the code which needs it, such as the loading of a plugin:

```scala
classloaders.systemClassloader.use:
  Classloader[Option[Int]]
```

`Classloader[T]` finds the classloader which loaded a type. That is how the classloader of a
plugin is reached from one of its classes.

### Related

- [Paths](paths.md) describes the path types which a classpath resource shares.
- [Streams](streams.md) describes `read`, and the forms a resource is read as.
- [Stack traces](stack-traces.md#resolving-frames-to-their-source) resolve frames through a
  classloader.
- [Compiler](compiler.md) compiles code against a classpath.
- [Packaging](packaging.md) builds the JAR files which become classpath entries.
- [Environment](environment.md) describes the system properties the classpath is read from.
