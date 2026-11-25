# A classloader, installed for one block

Some code finds its classes through the context classloader of the thread, which is mutable
state. `use` installs a classloader there for the length of a block and puts the previous one
back afterwards, so the change reaches the code which needs it and nothing else.

```scala fuel
compile
frame
  label A classloader for a plugin
  code
      val plugin = LocalClasspath(Classpath.Entry.Jar(t"/lib/tempest.jar"))
      val loader = plugin.classloader(Classloader.Delegation.Preferential)
frame
  label In force inside the block
  highlight loader.use  the context classloader, until the block ends
  code
      val plugin = LocalClasspath(Classpath.Entry.Jar(t"/lib/tempest.jar"))
      val loader = plugin.classloader(Classloader.Delegation.Preferential)

      val inside = loader.use(Classloader.threadContext.java eq loader.java)
frame
  label And gone after it
  highlight outside  the previous classloader is back
  code
      val plugin = LocalClasspath(Classpath.Entry.Jar(t"/lib/tempest.jar"))
      val loader = plugin.classloader(Classloader.Delegation.Preferential)

      val inside = loader.use(Classloader.threadContext.java eq loader.java)
      val outside = Classloader.threadContext.java eq loader.java
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(inside.inspect)
Out.println(outside.inspect)
```

[Running under a classloader](../../soundness/modules/classpath.md#running-under-a-classloader)
describes `use`, and [delimited scopes](../../philosophy/delimited-scopes.md) the pattern.
