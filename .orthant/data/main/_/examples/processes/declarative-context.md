# A subprocess inherits the environment in scope

A command does not reach for the environment of the JVM. It is started with the `Environment`
in scope, so a block which has an empty environment and one variable starts a process which
sees that variable and no other.

```scala fuel
compile
frame
  label The environment of the JVM
  highlight javaBaseEnvironment  whatever the JVM was started with
  code
      import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
      import logging.silentLogging, strategies.throwUnsafely

      def home(): Text = sh"printenv HOME".exec[Text]().trim
frame
  label One variable, and nothing else
  highlight emptyEnvironment  no variables at all
  highlight variables(home  adds one, for this block
  code
      import environments.emptyEnvironment, workingDirectories.javaBaseWorkingDirectory
      import logging.silentLogging, strategies.throwUnsafely

      def home(): Text = variables(home = t"/illyria"):
        sh"printenv HOME".exec[Text]().trim
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(home())
```

[Detaching](../../soundness/modules/processes.md#detaching) relies on the same rule, and
[environment](../../soundness/modules/environment.md#overriding-for-a-block) describes
`variables`.
