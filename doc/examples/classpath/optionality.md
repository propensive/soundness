# A resource which may not be there

Asking a classloader for a resource by its path gives an optional value: the bytes, or
`Unset`. There is no `null` to forget, and the bytes cannot be used until absence has been
given a meaning.

```scala fuel
compile
frame
  label The length of something which may be absent
  errors show
  code
      val plugin = LocalClasspath(Classpath.Entry.Jar(t"/lib/tempest.jar"))
      val loader = plugin.classloader(Classloader.Delegation.Preferential)
      val size: Int = loader(t"META-INF/plugin.version").length
frame
  label The length, if it is present
  highlight let
  code
      val plugin = LocalClasspath(Classpath.Entry.Jar(t"/lib/tempest.jar"))
      val loader = plugin.classloader(Classloader.Delegation.Preferential)
      val size: Optional[Int] = loader(t"META-INF/plugin.version").let(_.length)
frame
  label Or nothing, where it is not
  highlight or(0)
  code
      val plugin = LocalClasspath(Classpath.Entry.Jar(t"/lib/tempest.jar"))
      val loader = plugin.classloader(Classloader.Delegation.Preferential)
      val size: Int = loader(t"META-INF/plugin.version").let(_.length).or(0)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(size.show)
```

[Building a classloader](../../soundness/modules/classpath.md#building-a-classloader) describes
the loader, and [optionality](../../philosophy/optionality.md) `let` and `or`.
