# An archive is a value

A ZIP archive is not an object to open, mutate and remember to close. It is a list of entries,
each a path and its content, and it becomes bytes only when bytes are asked for.

```scala fuel
compile
frame
  label An entry
  code
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer
      import strategies.throwUnsafely

      val sonnet = Zip.Entry(t"sonnets/18.txt".as[Path on Zip], t"Shall I compare thee".in[Data])
frame
  label An archive of it
  highlight Zipfile(List(sonnet))  a list of entries
  code
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer
      import strategies.throwUnsafely

      val sonnet = Zip.Entry(t"sonnets/18.txt".as[Path on Zip], t"Shall I compare thee".in[Data])
      val folio = Zipfile(List(sonnet))
frame
  label Its bytes, and the archive read back from them
  highlight Zipfile.read  parsed from bytes
  code
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer
      import strategies.throwUnsafely

      val sonnet = Zip.Entry(t"sonnets/18.txt".as[Path on Zip], t"Shall I compare thee".in[Data])
      val folio = Zipfile(List(sonnet))
      val copy = Zipfile.read(folio.read[Data])
      val line = copy.entry(t"sonnets/18.txt".as[Path on Zip]).read[Text]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(line)
```

[ZIP](../../soundness/modules/archives.md#zip) describes what an entry which was read keeps of
its original header.
