# A variable, read as text or as a path

`HOME` holds a path, and is read as whichever type of path is asked for. The same read gives
plain text, or a `Path on Linux` which can be descended from, and no second method parses
one into the other.

```scala fuel
compile
frame
  label As text
  highlight home[Text]  the type which is wanted
  code
      import strategies.throwUnsafely

      given globe: Environment = name => if name == t"HOME" then t"/home/viola" else Unset

      val text = Environment.home[Text]
frame
  label As a path
  highlight home[Path..Linux]  checked against the platform's rules
  code
      import strategies.throwUnsafely

      given globe: Environment = name => if name == t"HOME" then t"/home/viola" else Unset

      val text = Environment.home[Text]
      val home = Environment.home[Path on Linux]
      val letters = home / "letters"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(text)
Out.println(letters.encode)
```

[Environment variables](../../soundness/modules/environment.md#environment-variables) has
the read, and [small APIs](../../philosophy/small-apis.md) the reasons for one name.
