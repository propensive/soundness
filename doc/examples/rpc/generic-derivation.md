# A server derived from a trait

An RPC interface is a trait whose methods are marked `@rpc`. The compiler reads their names,
parameters and types, and writes the code which takes a JSON-RPC request apart, calls the
implementation, and wraps the answer.

```scala fuel
compile
frame
  label An interface
  highlight @rpc
  code
      import strategies.throwUnsafely, codepages.utf8Codepage

      case class Line(text: Text)
      case class Echo(text: Text, times: Int)

      trait Cave:
        @rpc def shout(line: Line): Echo
frame
  label An implementation
  code
      import strategies.throwUnsafely, codepages.utf8Codepage

      case class Line(text: Text)
      case class Echo(text: Text, times: Int)

      trait Cave:
        @rpc def shout(line: Line): Echo

      object Cavern extends Cave:
        def shout(line: Line): Echo = Echo(line.text, 3)
frame
  label A dispatcher, from request to response
  highlight JsonRpc.serve[Cave]
  code
      import strategies.throwUnsafely, codepages.utf8Codepage

      case class Line(text: Text)
      case class Echo(text: Text, times: Int)

      trait Cave:
        @rpc def shout(line: Line): Echo

      object Cavern extends Cave:
        def shout(line: Line): Echo = Echo(line.text, 3)

      val dispatch = JsonRpc.serve[Cave](Cavern)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
import formatting.compactJsonFormatting

val request = t"""{"jsonrpc": "2.0", "id": 1, "method": "shout",
                   "params": {"line": {"text": "Hark"}}}""".read[Json]

Out.println(dispatch(request).let(_.show).or(t"no reply"))
```

[JSON-RPC](../../soundness/modules/rpc.md#json-rpc) describes the dispatcher and the client
which is derived from the same trait.
