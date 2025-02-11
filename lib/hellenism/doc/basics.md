### Getting a `Classpath`

A `Classpath` instance is an immutable value containing references to the sources of classes and classpath
resources. There are two variants which may be relevant:
 - `LocalClasspath`, which can include local filesystem directories and JAR files, as well as the core JVM
   classes, and,
 - `OnlineClasspath`, which can additionally include remote URLs

Both define an `entries` field, which is a `List[ClasspathEntry]`. `ClasspathEntry` is an enumeration including,
`Directory`, `Jarfile`, `Url` types, plus the singleton object, `JavaRuntime`. `Url` may only appear in an
`OnlineClasspath`. The distinction is made in the classpath's type because sometimes it is necessary to use only
locally-defined resources.

A `Classpath` can be constructed from a Java `URLClassLoader` instance, a subclass of the more general
`ClassLoader` whose sources may be inspected to build the `Classpath` instance.

#### Classpath Entries

Amongst `ClasspathEntry`s, `Directory`s, `Jarfile`s and `Url`s may be resolved to rich types representing
directories, files and URLs respectively, using typeclasses provided by
[Anticipation](https://github.com/propensive/anticipation/), just by calling their `apply()` methods.

### Classpath References

A resource, that is, some binary object accessed by a path name which is resolved on a classpath, is represented
by a `ClasspathRef`. This is an immutable value which represents the unresolved path to the resource, without
reference to particular classpath or the data it refers to.

A `ClasspathRef` can be accessed using a [Serpentine](https://github.com/propensive/serpentine/) path from the
`Classpath` object, for example:
```scala
val ref = Classpath / p"com" / p"example" / p"data.file"
```

### Getting a Classpath Resource

The classpath resource, an instance of `Resource`, can be accessed from a `ClasspathRef` just by calling its
`apply()` method, like so,
```scala
val resource: Resource = ref()
```
so long as there is a contextual `Classloader` instance in scope. This is necessary because different
classloaders may resolve the same path to different resources.

#### Reading Resources

Classpath `Resource`s are readable, using typeclasses provided by
[Turbulence](https://github.com/propensive/turbulence/). So it is possible to call `readAs[Bytes]` or
`readAs[Json]` (with [Jacinta](https://github.com/propensive/jacinta/) available) and get a useful value
directly.

#### Choosing a `Classloader`

The choice of classloader is determined by a contextual `Classloader` value. This value can be constructed from
a Java `ClassLoader` (note the capital `L` in the Java standard class, and the lower-case `l` in Hellenism's
type name), but for most purposes, one of the following imports is sufficient:

- `classloaders.threadContext`: use the current thread's context classloader
- `classloaders.system`: use the _system_ classloader
- `classloaders.platform`: use the _platform_ classloader
- `classloaders.scala`: use the classloader which was used to load the first Scala classes

The `threadContext` classloader is a reasonable default choice for most purposes.