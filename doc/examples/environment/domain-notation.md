# A variable, named as a member

A variable is read as a member of `Environment`, in the camel case of every other name in
the program. The name is translated to the upper case and underscores of the shell, so no
variable is named in a string.

```scala fuel
compile
frame
  label One word
  highlight Environment.editor  reads EDITOR
  code
      import strategies.throwUnsafely

      given globe: Environment =
        case t"EDITOR"          => t"quill"
        case t"XDG_CONFIG_HOME" => t"/etc/illyria"
        case _                  => Unset

      val editor: Text = Environment.editor[Text]
frame
  label Three words
  highlight xdgConfigHome  reads XDG_CONFIG_HOME
  code
      import strategies.throwUnsafely

      given globe: Environment =
        case t"EDITOR"          => t"quill"
        case t"XDG_CONFIG_HOME" => t"/etc/illyria"
        case _                  => Unset

      val editor: Text = Environment.editor[Text]
      val config: Text = Environment.xdgConfigHome[Text]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(editor)
Out.println(config)
```

[Environment variables](../../soundness/modules/environment.md#environment-variables)
describes the translation, and [elegant prose](../../philosophy/elegant-prose.md) the case
for notation like this.
