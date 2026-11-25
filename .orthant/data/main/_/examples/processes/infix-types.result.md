# A type of one's own, as a result

`exec` gives its result as any type which is `Computable`. A new type joins by mapping one
which already is, and the given which says so reads as a sentence: an `Int` is computable,
from the text of the output.

```scala fuel
compile
frame
  label A result which nothing knows how to compute
  errors show
  code
      import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
      import logging.silentLogging, strategies.throwUnsafely

      val words = (sh"echo 'to be or not to be'" | sh"wc -w").exec[Int]()
frame
  label How a number is read from the output
  highlight map  from the instance for text
  code
      import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
      import logging.silentLogging, strategies.throwUnsafely

      given (Int is Computable) = summon[Text is Computable].map(_.trim.as[Int])

      val words = (sh"echo 'to be or not to be'" | sh"wc -w").exec[Int]()
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(words.show)
```

[Running a command](../../soundness/modules/processes.md#running-a-command) lists the types
which are computable already, and [infix types](../../philosophy/infix-types.md) the words such
a given is written in.
