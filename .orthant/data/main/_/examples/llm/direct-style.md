# A conversation, line by line

Each question is an HTTP request, and a streamed answer arrives in fragments over a connection
held open. None of that shapes the code: a question is a call which returns a reply, the
session remembers what was said, and a stream is an iterator to walk.

```scala fuel
compile
frame
  label A question
  highlight llm.ask  returns the reply
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import strategies.throwUnsafely, codepages.utf8Codepage
      import errorDiagnostics.stackTracesDiagnostics
      import stdios.javaLangSystemStdio, termcapDefinitions.basicTermcap

      val bard = Anthropic(t"claude-sonnet-4-5", t"sk-ant-…")

      def interview(): Llm.Reply = bard.session:
        llm.ask(t"Who is the heroine of Twelfth Night?")
frame
  label A second, which depends on the first
  highlight her  the session holds the history
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import strategies.throwUnsafely, codepages.utf8Codepage
      import errorDiagnostics.stackTracesDiagnostics
      import stdios.javaLangSystemStdio, termcapDefinitions.basicTermcap

      val bard = Anthropic(t"claude-sonnet-4-5", t"sk-ant-…")

      def interview(): Llm.Reply = bard.session:
        llm.ask(t"Who is the heroine of Twelfth Night?")
        llm.ask(t"And who is her brother?")
frame
  label A third, printed as it arrives
  highlight response.text.each  each fragment, as the connection delivers it
  highlight response.reply()  the whole reply, once the stream is spent
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import strategies.throwUnsafely, codepages.utf8Codepage
      import errorDiagnostics.stackTracesDiagnostics
      import stdios.javaLangSystemStdio, termcapDefinitions.basicTermcap

      val bard = Anthropic(t"claude-sonnet-4-5", t"sk-ant-…")

      def interview(): Llm.Reply = bard.session:
        llm.ask(t"Who is the heroine of Twelfth Night?")
        llm.ask(t"And who is her brother?")
        val response = llm.stream(t"Tell their story in a sonnet.")
        response.text.each(Out.print(_))
        response.reply()
##
```

The example is compiled, and is not run, since it would call a provider over the network.
[Streaming](../../soundness/modules/llm.md#streaming) describes a response, and
[direct style](../../philosophy/direct-style.md) where the waiting went.
