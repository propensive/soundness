# A local classpath has no URLs

An entry of a classpath is a directory, an archive, a remote URL or the Java runtime. The
entries of a `LocalClasspath` are typed as only three of those, so code which handles a local
classpath does not handle a remote entry, and cannot be asked to.

```scala fuel
compile
frame
  label A case which cannot arise
  errors show
  code
      def describe(classpath: LocalClasspath): List[Text] = classpath.entries.map:
        case Classpath.Entry.Directory(path) => t"directory $path"
        case Classpath.Entry.Jar(path)       => t"archive $path"
        case Classpath.Entry.Url(url)        => t"remote $url"
        case Classpath.Entry.JavaRuntime     => t"the Java runtime"
frame
  label The three which can
  code
      def describe(classpath: LocalClasspath): List[Text] = classpath.entries.map:
        case Classpath.Entry.Directory(path) => t"directory $path"
        case Classpath.Entry.Jar(path)       => t"archive $path"
        case Classpath.Entry.JavaRuntime     => t"the Java runtime"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

val tempest = Classpath.Entry.Jar(t"/lib/tempest.jar")
val classpath = LocalClasspath(tempest, Classpath.Entry.JavaRuntime)

Out.println(describe(classpath).inspect)
```

[Inspecting the classpath](../../soundness/modules/classpath.md#inspecting-the-classpath) has
the table of entries, and [impossible states](../../philosophy/impossible-states.md) the
principle.
