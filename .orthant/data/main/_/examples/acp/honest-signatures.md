# A handler is lent what its request carries

Each request an agent makes has its own payload, reached inside the handler through accessors
on `Acp`. A request to read a file carries a path, a line and a limit. It carries no content,
so `Acp.content` in a `readFile` handler does not compile.

```scala fuel
compile
frame
  label A read which reaches for the content of a write
  errors show
  code
      def files(script: Text)(using Acp.Registry): Unit =
        Acp.readFile:
          if Acp.path == t"/globe/hamlet.txt" then Acp.content else t""
frame
  label A read answers with the text
  highlight Acp.path  lent to this handler, with the line and the limit
  code
      def files(script: Text)(using Acp.Registry): Unit =
        Acp.readFile:
          if Acp.path == t"/globe/hamlet.txt" then script else t""
##
```

The example is compiled, and is not run, since the handler serves an agent in another process.
[Updates](../../soundness/modules/acp.md#updates) has the table of handlers and their
accessors, and [honest signatures](../../philosophy/honest-signatures.md) the principle.
