# A command runs somewhere

A subprocess starts in a directory and with an environment, and `exec` asks for both as
contextual values. Code which was given no working directory cannot start a process in
whichever one the JVM happens to have.

```scala fuel
compile
frame
  label No working directory
  errors show
  code
      import environments.javaBaseEnvironment
      import logging.silentLogging, strategies.throwUnsafely

      val scenes = sh"ls".exec[List[Text]]()
frame
  label The directory of the JVM, chosen
  highlight workingDirectories.javaBaseWorkingDirectory
  code
      import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
      import logging.silentLogging, strategies.throwUnsafely

      val scenes = sh"ls".exec[List[Text]]()
frame
  label Or a parameter of the method
  highlight WorkingDirectory
  code
      import environments.javaBaseEnvironment
      import logging.silentLogging, strategies.throwUnsafely

      def scenes()(using WorkingDirectory): List[Text] = sh"ls".exec[List[Text]]()
##
```

The example is compiled, and is not run, since its output depends on where it runs.
[Honest signatures](../../philosophy/honest-signatures.md) explains why a process's
surroundings are parameters, and [environment](../../soundness/modules/environment.md) how
they are supplied.
