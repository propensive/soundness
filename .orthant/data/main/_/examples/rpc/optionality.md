# A notification has no reply

A JSON-RPC method which returns `Unit` is a notification, and nothing is sent back for one.
The dispatcher's result is therefore an `Optional[Json]`, and the code which sends the
response must say what it does when there is none.

```scala fuel
compile
frame
  label A call, and a notification
  highlight Unit  nothing to send back
  code
      import strategies.throwUnsafely, codepages.utf8Codepage
      import formatting.compactJsonFormatting

      trait Prompter:
        @rpc def line(act: Int): Text
        @rpc def curtain(act: Int): Unit

      object Prompt extends Prompter:
        def line(act: Int): Text = if act == 5 then t"The rest is silence" else t"Words"
        def curtain(act: Int): Unit = ()

      val dispatch = JsonRpc.serve[Prompter](Prompt)
frame
  label The reply, where there is one
  highlight let(_.show).or(t"no..reply")  what absence means, said once
  code
      import strategies.throwUnsafely, codepages.utf8Codepage
      import formatting.compactJsonFormatting

      trait Prompter:
        @rpc def line(act: Int): Text
        @rpc def curtain(act: Int): Unit

      object Prompt extends Prompter:
        def line(act: Int): Text = if act == 5 then t"The rest is silence" else t"Words"
        def curtain(act: Int): Unit = ()

      val dispatch = JsonRpc.serve[Prompter](Prompt)

      def reply(request: Text): Text =
        dispatch(request.read[Json]).let(_.show).or(t"no reply")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(reply(t"""{"jsonrpc": "2.0", "id": 1, "method": "line", "params": {"act": 5}}"""))
Out.println(reply(t"""{"jsonrpc": "2.0", "method": "curtain", "params": {"act": 5}}"""))
```

[JSON-RPC](../../soundness/modules/rpc.md#json-rpc) describes the dispatcher, and
[optionality](../../philosophy/optionality.md) explains `let` and `or`.
