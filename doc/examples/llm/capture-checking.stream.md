# A streamed answer is read while the connection is open

`stream` returns a response whose text arrives in fragments, pulled off the connection as they
are wanted. The iterator of fragments is lazy, and depends on a session which closes with its
block, so the compiler will not let it be the block's result. The assembled reply is data, and
may be.

```scala fuel
compile
feature experimental.captureChecking
frame
  label Fragments which would be pulled from a closed connection
  errors show
  highlight .text
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import strategies.throwUnsafely, codepages.utf8Codepage
      import errorDiagnostics.stackTracesDiagnostics

      val bard = Anthropic(t"claude-sonnet-4-5", t"sk-ant-…")

      val fragments = bard.session:
        llm.stream(t"Write a sonnet about Verona.").text
frame
  label The complete reply, assembled inside the block
  highlight .reply()
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import strategies.throwUnsafely, codepages.utf8Codepage
      import errorDiagnostics.stackTracesDiagnostics

      val bard = Anthropic(t"claude-sonnet-4-5", t"sk-ant-…")

      def sonnet(): Llm.Reply = bard.session:
        llm.stream(t"Write a sonnet about Verona.").reply()
##
```

The example is compiled, and is not run, since it would call a provider over the network.
[Streaming](../../soundness/modules/llm.md#streaming) describes a response and its fragments.
