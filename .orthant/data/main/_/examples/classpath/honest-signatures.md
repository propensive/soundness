# A resource is read through a classloader

A resource reference names a path, and two classloaders may find different things there. A
read therefore needs a `Classloader`, as a contextual value, and code which was given none
does not fall back on whichever the thread happens to carry.

```scala fuel
compile
frame
  label No classloader
  errors show
  code
      import strategies.throwUnsafely

      val bytes = cp"/scala/Option.class".read[Data]
frame
  label A classloader, chosen
  highlight classloaders.threadContextClassloader
  code
      import strategies.throwUnsafely, classloaders.threadContextClassloader

      val bytes = cp"/scala/Option.class".read[Data]
frame
  label Or asked for
  highlight Classloader
  code
      import strategies.throwUnsafely

      def bytes()(using Classloader): Data = cp"/scala/Option.class".read[Data]
##
```

[Choosing a classloader](../../soundness/modules/classpath.md#choosing-a-classloader) explains
why the choice cannot be implicit, and
[honest signatures](../../philosophy/honest-signatures.md) the general rule.
