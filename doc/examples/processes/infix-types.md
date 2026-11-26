# A type of one's own, as an argument

Only a value which is known to be an argument can be substituted into a command. A type joins
in with one line, which reads as the sentence it means: a `Scene` is `Parameterizable`.

```scala fuel
compile
frame
  label A value which is not an argument
  errors show
  code
      case class Scene(act: Int, number: Int)

      val scene = Scene(3, 1)
      val command = sh"prompter --cue $scene"
frame
  label How a scene is written
  highlight Scene..Parameterizable
  code
      case class Scene(act: Int, number: Int)

      given Scene is Parameterizable = scene => t"${scene.act}.${scene.number}"

      val scene = Scene(3, 1)
      val command = sh"prompter --cue $scene"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(command.inspect)
```

[Substitution and escaping](../../soundness/modules/processes.md#substitution-and-escaping)
describes the positions a value may take, and [infix types](../../philosophy/infix-types.md)
the words such a line is written in.
