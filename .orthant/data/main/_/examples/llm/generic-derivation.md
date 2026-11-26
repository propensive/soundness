# An answer which is a case class

A model answers in text, and a program wants a value. `solicit` takes the type of the answer.
Its JSON schema is derived from the case class and sent to the model, and the document which
comes back is decoded to it, so the result has fields and not prose to be parsed.

```scala fuel
compile
frame
  label A question, answered in text
  highlight llm.ask
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import strategies.throwUnsafely, codepages.utf8Codepage
      import errorDiagnostics.stackTracesDiagnostics

      val judge = Anthropic(t"claude-sonnet-4-5", t"sk-ant-…")

      def trial(): Text = judge.session:
        llm.ask(t"Did Claudius murder the king?").text
frame
  label The same question, answered as a value
  highlight solicit[Verdict]  the schema of Verdict is what the model must satisfy
  highlight case..Double)  no codec and no schema is written for it
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import strategies.throwUnsafely, codepages.utf8Codepage
      import errorDiagnostics.stackTracesDiagnostics

      case class Verdict(accused: Text, guilty: Boolean, confidence: Double)

      val judge = Anthropic(t"claude-sonnet-4-5", t"sk-ant-…")

      def trial(): Verdict = judge.session:
        llm.solicit[Verdict](t"Did Claudius murder the king?")
##
```

The example is compiled, and is not run, since it would call a provider over the network.
[Structured answers](../../soundness/modules/llm.md#structured-answers) says how the schema
reaches each provider.
