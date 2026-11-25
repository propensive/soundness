# What can cross the boundary

A spliced value travels to the other JVM as JSON, and the staging machinery asks the JSON
library how. A case class of ordinary fields has an encoding, derived from its fields. A
value with none is rejected as the block compiles, and not when it is sent.

```scala fuel
compile
frame
  label A value with no transportable form
  errors show
  code
      import scala.quoted.*
      import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
      import classloaders.threadContextClassloader, embeddings.automaticEmbedding
      import strategies.throwUnsafely, systems.javaBaseSystem
      import temporaryDirectories.systemTemporaryDirectory

      class Prompter(val cue: Text)

      def whisper(prompter: Prompter): Text = Jvm.dispatch:
        '{ ${prompter}.cue }
frame
  label A case class, which JSON can carry
  highlight case  encoded and decoded by its fields
  code
      import scala.quoted.*
      import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
      import classloaders.threadContextClassloader, embeddings.automaticEmbedding
      import strategies.throwUnsafely, systems.javaBaseSystem
      import temporaryDirectories.systemTemporaryDirectory

      case class Prompter(cue: Text)

      def whisper(prompter: Prompter): Text = Jvm.dispatch:
        '{ ${prompter}.cue }
##
```

The example is compiled, and is not run, since it starts another process.
[The boundary](../../soundness/modules/staging.md#the-boundary) describes the transports, and
[JSON](../../soundness/modules/json.md) the derivation.
