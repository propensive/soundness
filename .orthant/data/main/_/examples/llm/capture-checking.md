# A conversation ends with its block

A session with a language model holds a connection, which is closed when the block ends. A reply
is plain data and may be returned. A function which would ask another question later still
depends on the session, so it may not.

```scala fuel
compile
feature experimental.captureChecking
frame
  label A question to be asked after the session has closed
  errors show
  highlight llm.ask(question)  would ask when the session had gone
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import strategies.throwUnsafely, codepages.utf8Codepage
      import errorDiagnostics.stackTracesDiagnostics

      val oracle = Anthropic(t"claude-sonnet-4-5", t"sk-ant-…")

      val ask = oracle.session:
        (question: Text) => llm.ask(question)
frame
  label The questions, asked within it
  highlight Llm.Reply  the reply is data, and leaves freely
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import strategies.throwUnsafely, codepages.utf8Codepage
      import errorDiagnostics.stackTracesDiagnostics

      val oracle = Anthropic(t"claude-sonnet-4-5", t"sk-ant-…")

      def prophecy(): Llm.Reply = oracle.session:
        llm.ask(t"When shall we three meet again?")
        llm.ask(t"And where?")
##
```

The example is compiled, and is not run, since it would call a provider over the network.
[A conversation](../../soundness/modules/llm.md#a-conversation) describes what a session records.
