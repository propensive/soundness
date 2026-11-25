# A method which reads the environment says so

The environment is not a global. A variable is read from the `Environment` in scope, and a
method which has none cannot read one. A method which needs one asks for it in its signature,
and its caller decides which environment it sees.

```scala fuel
compile
frame
  label No environment to read from
  errors show
  code
      import strategies.throwUnsafely

      def editor(): Text = Environment.editor[Text]
frame
  label The environment, as a parameter
  highlight (using..Environment)  the dependence, in the signature
  code
      import strategies.throwUnsafely

      def editor()(using Environment): Text = Environment.editor[Text]
frame
  label An environment made for the occasion
  highlight given..Environment  a function from a name
  code
      import strategies.throwUnsafely

      def editor()(using Environment): Text = Environment.editor[Text]

      given illyria: Environment = name => if name == t"EDITOR" then t"quill" else Unset

      val choice: Text = editor()
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(choice)
```

[Overriding for a block](../../soundness/modules/environment.md#overriding-for-a-block) has
more on environments which are not the real one, and
[honest signatures](../../philosophy/honest-signatures.md) the principle.
