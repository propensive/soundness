# A resource which is not on the classpath

A resource named at runtime may not exist, and reading it raises a `Classpath.Error`. The read
is the same line under every policy. What a missing resource means is decided around it.

```scala fuel
compile
alternative An absent value
  highlight safely  a missing resource is no data
  code
      import classloaders.threadContextClassloader

      def script(path: Path on Classpath): Optional[Data] = safely(path.read[Data])
alternative The caller's decision
  highlight raises  both ways the read can fail are declared
  code
      import classloaders.threadContextClassloader

      def script(path: Path on Classpath): Data raises Classpath.Error raises Truncation.Error =
        path.read[Data]
alternative An exception
  highlight throwUnsafely  a missing resource stops the program
  code
      import classloaders.threadContextClassloader, strategies.throwUnsafely

      def script(path: Path on Classpath): Data = path.read[Data]
##
```

[Reading a resource](../../soundness/modules/classpath.md#reading-a-resource) describes the
paths, and [choosing a strategy](../../soundness/modules/errors.md#choosing-a-strategy) the
policies.
