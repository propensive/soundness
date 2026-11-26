# JSON read straight from an entry

An entry of an archive is a source of bytes, and the JSON library reads any source of bytes.
The content of an entry is parsed and decoded with the methods used on a file or a response,
and neither library has heard of the other.

```scala fuel
compile
frame
  label An archive which holds a JSON document
  code
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer
      import strategies.throwUnsafely

      case class Play(title: Text, acts: Int)

      val entry = Zip.Entry(t"hamlet.json".as[Path on Zip], t"""{"title": "Hamlet", "acts": 5}""")
      val folio = Zipfile(List(entry))
frame
  label The entry, read as JSON and decoded
  highlight read[Json].as[Play]  the same methods as for a file
  code
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer
      import strategies.throwUnsafely

      case class Play(title: Text, acts: Int)

      val entry = Zip.Entry(t"hamlet.json".as[Path on Zip], t"""{"title": "Hamlet", "acts": 5}""")
      val folio = Zipfile(List(entry))
      val play = folio.entry(t"hamlet.json".as[Path on Zip]).read[Json].as[Play]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(play.title)
Out.println(play.acts.show)
```

[ZIP](../../soundness/modules/archives.md#zip) says that an entry's content reads as any type,
and [JSON](../../soundness/modules/json.md) how a value is decoded.
