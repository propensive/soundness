# Three answers to a missing binary

`exec` raises an `Exec.Error` when its command cannot be started, and does not decide what
follows. The code around it does, and each choice gives the method a different type.

```scala fuel
compile
alternative An absent value
  highlight safely
  code
      import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
      import logging.silentLogging

      def version(): Optional[Text] = safely(sh"prospero --version".exec[Text]())
alternative A replacement
  highlight recover
  code
      import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
      import logging.silentLogging

      def version(): Text =
        recover:
          case error: Exec.Error => t"not installed"
        . protect(sh"prospero --version".exec[Text]())
alternative The caller's decision
  highlight raises..Error
  code
      import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
      import logging.silentLogging

      def version(): Text raises Exec.Error = sh"prospero --version".exec[Text]()
##
```

[Failure](../../soundness/modules/processes.md#failure) describes the error, and
[choosing a strategy](../../soundness/modules/errors.md#choosing-a-strategy) the full set of
answers to it.
