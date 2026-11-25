# A variable which may not be set

A variable which is not set is an error, and never an empty string or a `null`. `safely` turns
the error into an absent value, and `or` says what absence means, so a chain of fallbacks is
one expression.

```scala fuel
compile
frame
  label The editor, or nothing
  highlight safely  an unset variable is an absent value
  code
      def editor()(using Environment): Optional[Text] =
        safely(Environment.editor[Text])
frame
  label The editor, or a default
  highlight or(t"vi")  what absence means here
  code
      def editor()(using Environment): Text =
        safely(Environment.editor[Text]).or(t"vi")
frame
  label Another variable first
  highlight visual  tried before EDITOR
  code
      def editor()(using Environment): Text =
        safely(Environment.visual[Text])
        . or(safely(Environment.editor[Text]))
        . or(t"vi")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

locally:
  given quarto: Environment = name => if name == t"EDITOR" then t"quill" else Unset
  Out.println(editor())

locally:
  import environments.emptyEnvironment
  Out.println(editor())
```

[Environment variables](../../soundness/modules/environment.md#environment-variables) has the
error, and [optionality](../../philosophy/optionality.md) explains `or`.
