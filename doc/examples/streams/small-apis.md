# One method reads a source as any type

`read` pulls a source in, and takes the type which is wanted. The same source is read as text or as
bytes, under the encoding in scope, and no method is named for a type.

```scala fuel
compile
frame
  label As text
  highlight read[Text]  the type which is wanted
  code
      import codepages.utf8Codepage, charsets.utf8Charset, strategies.throwUnsafely

      val line = t"If music be the food of love, play on"
      val text = line.read[Text]
frame
  label As bytes
  highlight read[Data]  under the encoding in scope
  code
      import codepages.utf8Codepage, charsets.utf8Charset, strategies.throwUnsafely

      val line = t"If music be the food of love, play on"
      val text = line.read[Text]
      val bytes = line.read[Data]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(text)
Out.println(bytes.length.show)
```

[Reading a source](../../soundness/modules/streams.md#reading-a-source) names the other
sources, and [small APIs](../../philosophy/small-apis.md) explains the one verb.
