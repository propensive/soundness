# Nothing crosses without a splice

A value on the heap of this JVM means nothing in another. Inside a quoted block, a local value
can therefore be named only in a splice, where it is marshaled, and a reference which would
have dangled is a compile error.

```scala fuel
compile
frame
  label A local value, used remotely as though it were there
  errors show
  code
      import scala.quoted.*
      import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
      import classloaders.threadContextClassloader, embeddings.automaticEmbedding
      import strategies.throwUnsafely, systems.javaBaseSystem
      import temporaryDirectories.systemTemporaryDirectory

      def double(lines: Long): Long = Jvm.dispatch:
        '{ lines*2 }
frame
  label Spliced, and so carried across
  highlight ${lines}  evaluated here, marshaled, and used there
  code
      import scala.quoted.*
      import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
      import classloaders.threadContextClassloader, embeddings.automaticEmbedding
      import strategies.throwUnsafely, systems.javaBaseSystem
      import temporaryDirectories.systemTemporaryDirectory

      def double(lines: Long): Long = Jvm.dispatch:
        '{ ${lines}*2 }
##
```

The example is compiled, and is not run, since it starts another process.
[Quotes, splices and phases](../../soundness/modules/staging.md#quotes-splices-and-phases)
explains the rule, and [impossible states](../../philosophy/impossible-states.md) the aim.
