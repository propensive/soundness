# An upload, parsed as it arrives

A multipart body is parsed incrementally from the chunks it arrives in. Each part has its
name, its filename and its headers as typed values, and its body is lent from the input and is
never copied aside.

```scala fuel
compile
frame
  label A body, as a browser sends it
  code
      import codepages.utf8Codepage, strategies.throwUnsafely

      val body =
        t"--xyz\r\n" +
        t"Content-Disposition: form-data; name=\"portrait\"; filename=\"droeshout.png\"\r\n" +
        t"\r\n" +
        t"pixels\r\n" +
        t"--xyz--\r\n"
frame
  label Its parts
  highlight Multipart.parse  from a chain of chunks
  code
      import codepages.utf8Codepage, strategies.throwUnsafely

      val body =
        t"--xyz\r\n" +
        t"Content-Disposition: form-data; name=\"portrait\"; filename=\"droeshout.png\"\r\n" +
        t"\r\n" +
        t"pixels\r\n" +
        t"--xyz--\r\n"

      val upload = Multipart.parse(Chain(body.in[Data]))
      val filename = upload.at(t"portrait").let(_.filename)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(filename.inspect)
```

[Multipart](../../soundness/modules/media-types.md#multipart) says when a part's body must be
read.
