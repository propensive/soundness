# An agent's file requests, served by the filesystem

An agent asks its client to read and write files, and names them by a path sent as text.
Answering needs nothing made for the purpose: the text is decoded to a `Path`, and the file is
opened, read and written as any other.

```scala fuel
compile
frame
  label A read
  highlight as[Path..Local]  the agent's text, parsed as a path
  highlight file.read[Text]  the handler's answer
  code
      import filesystemBackends.javaBaseFilesystem, systems.javaBaseSystem
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer
      import strategies.throwUnsafely

      def files()(using Acp.Registry): Unit =
        Acp.readFile:
          Acp.path.as[Path on Local].open[File]()(file.read[Text])
frame
  label And a write
  highlight handle.write(Acp.content)
  code
      import filesystemBackends.javaBaseFilesystem, systems.javaBaseSystem
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer
      import filesystemOptions.createParents, strategies.throwUnsafely

      def files()(using Acp.Registry): Unit =
        Acp.readFile:
          Acp.path.as[Path on Local].open[File]()(file.read[Text])

        Acp.writeFile:
          Acp.path.as[Path on Local].create[File](): handle ?=>
            handle.write(Acp.content)
##
```

The example is compiled, and is not run, since the handlers serve an agent in another process.
[Filesystem and terminals](../../soundness/modules/acp.md#filesystem-and-terminals) describes
the requests, and [filesystem](../../soundness/modules/filesystem.md) the operations which
answer them.
