# A variable which is not set is an error

Reading a variable gives its value, and never a `null` or an empty string. Where the variable
is not set, the read raises an `Environment.Error`, and a method which reads one declares it.

```scala fuel
compile
frame
  label A method which does not say that it can fail
  errors show
  code
      def editor()(using Environment): Text = Environment.editor[Text]
frame
  label The failure, in the return type
  highlight raises..Error  the way it can fail
  code
      def editor()(using Environment): Text raises Environment.Error =
        Environment.editor[Text]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
import environments.emptyEnvironment
import strategies.throwUnsafely, errorDiagnostics.emptyDiagnostics

Out.println(capture[Environment.Error](editor()).message.show)
```

The run is against an empty environment, and prints the message of the error.
[Environment variables](../../soundness/modules/environment.md#environment-variables) has the
read, and [error handling](../../philosophy/error-handling.md) the reasons for an error in a
type.
