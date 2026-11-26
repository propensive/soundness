# Anchors, aliases and a derived decoder

An anchor names a node of a YAML document and an alias repeats it. Aliases are resolved as the
document is read, so the decoder which the compiler derives for a case class sees an ordinary
document.

```scala fuel
compile
frame
  label A block, anchored and used twice
  highlight Map[Text..Stage]  each value decoded by the derived decoder
  code
      import strategies.throwUnsafely

      case class Stage(width: Int, traps: Int)

      val stages = t"globe: &thrust\n  width: 43\n  traps: 1\nrose: *thrust"
      val houses = stages.read[Yaml].as[Map[Text, Stage]]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(houses.inspect)
```

[Anchors and aliases](../../soundness/modules/yaml.md#anchors-and-aliases) covers the nodes
which may be anchored.
