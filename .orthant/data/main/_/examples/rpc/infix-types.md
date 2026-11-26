# Two ends which must agree

A framed connection is typed by what it reads and what it sends: `Framed from Reply to Cue`
receives replies and sends cues. The other end has the same types the other way round, and an
end which is framed the wrong way does not have the type it was declared with.

```scala fuel
compile
frame
  label Both ends framed the same way
  errors show
  code
      import strategies.throwUnsafely

      case class Cue(line: Int, text: Text)
      case class Reply(line: Int, length: Int)

      val (near, far) = Duplex.pair()

      val actor: Framed from Reply to Cue = near.framed[Reply, Cue]
      val prompter: Framed from Cue to Reply = far.framed[Reply, Cue]
frame
  label Each end the mirror of the other
  highlight Framed..Reply
  highlight framed[Cue,..Reply]  the request and the reply, as types
  code
      import strategies.throwUnsafely

      case class Cue(line: Int, text: Text)
      case class Reply(line: Int, length: Int)

      val (near, far) = Duplex.pair()

      val actor: Framed from Reply to Cue = near.framed[Reply, Cue]
      val prompter: Framed from Cue to Reply = far.framed[Cue, Reply]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

actor.send(Cue(1, t"Who's there?"))

prompter.messages.next() match
  case cue: Cue => prompter.send(Reply(cue.line, cue.text.length))
  case _        => ()

Out.println(actor.messages.next().inspect)
```

[Framing](../../soundness/modules/rpc.md#framing) describes the records on the wire, and
[infix types](../../philosophy/infix-types.md) the words `from` and `to`.
