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

Soundness treats a classpath resource as a path like any other, read with the same
polymorphic `read`, and treats a classpath as an immutable value whose entries can be
listed. A classloader is chosen explicitly, as a contextual value, rather than reached for
implicitly. Everything comes from the `soundness` package, with a classloader in scope:

```scala
import soundness.*
import classloaders.threadContextClassloader
import strategies.throwUnsafely
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
t"/scala/Option.class".decode[Path on Classpath]
```

A classpath path is _substantiable_, meaning its existence can be tested before it is read,
so a program can check for an optional resource rather than handle a failure.

### Choosing a classloader

Which classloader resolves a resource is decided by the `given Classloader` in scope, chosen
by import. The thread's context classloader suits most applications; the system, platform
and Scala classloaders are the other standard choices:

```scala
import classloaders.systemClassloader
```

### Inspecting the classpath

A `LocalClasspath` is the classpath as a list of entries. The running program's classpath is
the value of the `java.class.path` property, which decodes to one:

```scala
val classpath = System.properties.java.`class`.path().decode[LocalClasspath]
```

Its `entries` are typed: a `ClasspathEntry` is a `Directory`, a `Jar`, a `Url`, or the
`JavaRuntime` that supplies the JDK's own classes:

```scala
classpath.entries.each:
  case ClasspathEntry.Directory(path) => Out.println(t"directory $path")
  case ClasspathEntry.Jar(path)       => Out.println(t"archive $path")
  case ClasspathEntry.Url(url)        => Out.println(t"url $url")
  case ClasspathEntry.JavaRuntime     => Out.println(t"the Java runtime")
```

### Loading services

The JVM's [service-provider](https://en.wikipedia.org/wiki/Service_provider_interface)
mechanism finds the implementations of an interface declared on the classpath. `services`
returns them, typed to the service:

```scala
classpath.services[TestService].map(_.name)
```

### Running under a classloader

A `Classloader` value runs a block with itself installed as the thread's context
classloader, restoring the previous one afterward. This scopes a change of classloader to
exactly the code that needs it — loading a plugin, say — rather than leaving it set:

```scala
classloader.use:
  // code here resolves classes against `classloader`
```
