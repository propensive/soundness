## Classpaths

### About

The JVM [classpath](https://en.wikipedia.org/wiki/Classpath) and its
[classloaders](https://en.wikipedia.org/wiki/Java_Classloader) are represented as values. A
resource bundled on the classpath is read through a reference the compiler checks; a
classpath is an immutable list of its entries — directories, JAR files, and the Java
runtime — that can be inspected; and a block of code can be run under a chosen
classloader.

The classpath is how the JVM finds the classes and data files packaged with a program, but
its standard API is a matter of nullable lookups and mutable global state. Soundness gives a
resource the same reading interface as any other source, and makes the structure of the
classpath something a program can examine directly.

### On the classpath

When a program asks for a bundled data file — a template, a lookup table, an icon — it asks
the classpath, which searches the directories and archives the program was launched with.
The Java API for this returns a `null` when a resource is absent, hands back a raw
`InputStream`, and exposes the classloader hierarchy as mutable state shared across threads.
None of it is typed, and a missing resource is discovered only when the `null` is
dereferenced.

Classpath entries and classloaders as typed values, rather than strings and globals, are [honest signatures](../philosophy/honest-signatures.md) for a part of the JVM that usually has none.

Soundness treats a classpath resource as a path like any other, read with the same
polymorphic `read`, and treats a classpath as an immutable value whose entries can be
listed. A classloader is chosen explicitly, as a contextual value, rather than reached for
implicitly. Everything comes from the `soundness` package, with a classloader in scope:

```scala
import soundness.*
import classloaders.threadContextClassloader
import strategies.throwUnsafely
import systems.javaBaseSystem
```

### Reading a resource

The `cp"…"` interpolator names a resource on the classpath and checks the reference as the
code compiles. The result is a `Resource`, which reads like any other source — as raw bytes,
as text, or as a parsed value:

```scala
cp"/scala/Option.class".read[Data]
```

Because the resource is resolved against the classloader in scope, reading it needs no
handle passed by hand; the `given Classloader` supplies the context. A resource path can
equally be built from text at runtime by decoding it:

```scala
t"/scala/Option.class".as[Path on Classpath]
```

A classpath path is _substantiable_, meaning its existence can be tested before it is read,
so a program can check for an optional resource rather than handle a failure.

### Choosing a classloader

A resource reference names a path, not a thing: two classloaders may resolve the same path to
different resources, or one to nothing at all, which is why the choice cannot be implicit in the
reference and has to come from the context in which it is read. Which classloader resolves a
resource is decided by the `given Classloader` in scope, chosen by import. The thread's context
classloader suits most applications; the system, platform and Scala classloaders are the other
standard choices:

```scala
import classloaders.systemClassloader
```

### Inspecting the classpath

A `LocalClasspath` is the classpath as a list of entries. The running program's classpath is
the value of the `java.class.path` property, which decodes to one — reading the property needs
the `System` in scope, imported above:

```scala
val classpath = System.properties.java.`class`.path().as[LocalClasspath]
```

Its `entries` are typed: a `Classpath.Entry` is a `Directory`, a `Jar`, a `Url`, or the
`JavaRuntime` that supplies the JDK's own classes — and a *local* classpath's entries are
statically known to exclude URLs, so a match over them need not handle that case:

```scala
classpath.entries.map:
  case Classpath.Entry.Directory(path) => t"directory $path"
  case Classpath.Entry.Jar(path)       => t"archive $path"
  case Classpath.Entry.JavaRuntime     => t"the Java runtime"
```

### Loading services

The JVM's [service-provider](https://en.wikipedia.org/wiki/Service_provider_interface)
mechanism finds the implementations of an interface declared on the classpath. `services`
returns them, typed to the service:

```scala
classpath.services[java.nio.file.spi.FileSystemProvider].map(_.getScheme)   // Set("file", "jar", …)
```

### Building a classloader

A classpath builds a classloader over a chosen parent, which is the platform loader unless
another is given. The loader's *delegation* says which side wins when it and its parent both
offer a class of the same name, and is always spelled out: `Deferential` is the JVM's usual
order, deferring to the parent, which shares the parent's classes with the code being loaded;
`Preferential` prefers the classpath's own entries, which isolates a plugin's versions of its
libraries from its host's:

```scala
val contract = LocalClasspath(Classpath.Entry.Jar(t"/lib/contract.jar"))
val plugin = LocalClasspath(Classpath.Entry.Jar(t"/lib/plugin.jar"))
val shared = contract.classloader(Classloader.Delegation.Deferential)
val isolated = plugin.classloader(Classloader.Delegation.Preferential, parent = shared)
```

Here a plugin sees the contract it was written against through its parent, while everything
else it bundles is its own. A resource is read from a loader directly, by its path, as an
optional value that retains nothing, so the read can sit inside a `safely` block:

```scala
safely(isolated(t"META-INF/plugin.version"))
```

### Running under a classloader

A `Classloader` value runs a block with itself installed as the thread's context
classloader, restoring the previous one afterward. This scopes a change of classloader to
exactly the code that needs it — loading a plugin, say — rather than leaving it set:

```scala
classloaders.systemClassloader.use:
  Classloader[Option[Int]]   // resolved against the system classloader
```

`Classloader[T]` finds the classloader that loaded a type, which is how a plugin's own
classloader is reached from one of its classes.
