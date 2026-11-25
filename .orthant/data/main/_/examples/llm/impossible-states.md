# The identifier of a reply is not that of a batch

A provider issues identifiers for replies and for batches, and a caller chooses them for
requests. All are text on the wire. Here each is an `Llm.Id` of what it names, so the wrong
kind cannot be handed to a method which looks something up.

```scala fuel
compile
frame
  label A batch, looked up by the identifier of a reply
  errors show
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import strategies.throwUnsafely, codepages.utf8Codepage
      import errorDiagnostics.stackTracesDiagnostics

      case class Verdict(accused: Text, guilty: Boolean)

      def resume(judge: Anthropic, reply: Llm.Reply) =
        reply.id.let(judge.batch[Verdict](_))
frame
  label A batch, looked up by its own
  highlight Llm.Id[Anthropic.Batch[?]]  only a batch's identifier will do
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import strategies.throwUnsafely, codepages.utf8Codepage
      import errorDiagnostics.stackTracesDiagnostics

      case class Verdict(accused: Text, guilty: Boolean)

      def resume(judge: Anthropic, id: Llm.Id[Anthropic.Batch[?]]) =
        judge.batch[Verdict](id)
##
```

The example is compiled, and is not run, since it would call a provider over the network.
[Batches](../../soundness/modules/llm.md#batches) describes the identifiers, and
[impossible states](../../philosophy/impossible-states.md) the idea.
