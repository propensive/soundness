# A command which may not be there

A command whose binary is missing, or is not executable, cannot be run at all, and `exec` says
so in its type. A method which runs a command either handles `Exec.Error` or declares it.

```scala fuel
compile
frame
  label Nothing handles the error
  errors show
  code
      import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
      import logging.silentLogging

      def herald(): Text = sh"echo Elsinore".exec[Text]()
frame
  label The error, declared
  highlight raises..Error  the caller must deal with it
  code
      import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
      import logging.silentLogging

      def herald(): Text raises Exec.Error = sh"echo Elsinore".exec[Text]()
##
```

[Failure](../../soundness/modules/processes.md#failure) separates a command which cannot run
from one which runs and fails, and [error handling](../../philosophy/error-handling.md) has
the reasons for naming the error.
