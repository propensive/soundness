# An enumeration in a YAML mapping

An enumeration is written as a mapping with one more key, which names the case, and the
decoder derived for it reads that key first. The key's name is chosen by an import.

```scala fuel
compile
frame
  label A case, encoded
  highlight yamlByKindDiscriminable  the key is called kind
  code
      import strategies.throwUnsafely
      import discriminables.yamlByKindDiscriminable

      enum Exit:
        case Pursued(by: Text)
        case Alone

      val exit: Yaml = (Exit.Pursued(t"a bear"): Exit).in[Yaml]
frame
  label A case, decoded
  highlight as[Exit]  the key chooses the case
  code
      import strategies.throwUnsafely
      import discriminables.yamlByKindDiscriminable

      enum Exit:
        case Pursued(by: Text)
        case Alone

      val exit: Yaml = (Exit.Pursued(t"a bear"): Exit).in[Yaml]
      val alone: Exit = t"kind: Alone".read[Yaml].as[Exit]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
import formatting.blockYamlFormatting

Out.println(exit.show)
Out.println(alone.inspect)
```

[Encoding](../../soundness/modules/yaml.md#encoding) names the two imports, and
[tagged unions](../../soundness/modules/json.md#tagged-unions) describes the convention.
