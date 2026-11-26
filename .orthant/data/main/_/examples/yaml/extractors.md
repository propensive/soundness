# A YAML document as a pattern

A `y"…"` literal in a `case` matches a document of its shape, and binds each substitution to
the YAML found in its place.

```scala fuel
compile
frame
  label A mapping with one key
  highlight $name  bound as Yaml
    line 4
  code
      import strategies.throwUnsafely

      def name(player: Yaml): Text = player match
        case y"{name: $name}" => name.as[Text]
        case _                => t"anonymous"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(name(t"name: Viola".read[Yaml]))
Out.println(name(t"[Viola, Feste]".read[Yaml]))
```

[Writing YAML literally](../../soundness/modules/yaml.md#writing-yaml-literally) describes the
literal as an expression.
