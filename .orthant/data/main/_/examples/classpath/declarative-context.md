# The same path, through another classloader

Which classloader resolves a resource is decided by an import, for every read in its scope.
The reference and the read are the same under each.

```scala fuel
compile
alternative The thread's
  highlight threadContextClassloader  suits most applications
  code
      import classloaders.threadContextClassloader
      import strategies.throwUnsafely

      val bytes = cp"/scala/Option.class".read[Data]
alternative The system's
  highlight systemClassloader  the classpath the JVM was started with
  code
      import classloaders.systemClassloader
      import strategies.throwUnsafely

      val bytes = cp"/scala/Option.class".read[Data]
alternative Scala's own
  highlight scalaClassloader  whichever loaded the Scala library
  code
      import classloaders.scalaClassloader
      import strategies.throwUnsafely

      val bytes = cp"/scala/Option.class".read[Data]
##
```

[Choosing a classloader](../../soundness/modules/classpath.md#choosing-a-classloader) has the
full list, and [declarative context](../../philosophy/declarative-context.md) the argument for
choosing by import.
