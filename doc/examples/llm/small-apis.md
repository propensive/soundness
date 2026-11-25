# One conversation, whichever provider

Each provider's API is a different JSON dialect. The vocabulary here is one: a provider is a
value, `session` opens a conversation on it, and `llm.ask` returns an `Llm.Reply`. Changing
provider changes one line.

```scala fuel
compile
alternative Anthropic
  highlight Anthropic  the Messages API
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import strategies.throwUnsafely, codepages.utf8Codepage
      import errorDiagnostics.stackTracesDiagnostics

      val oracle = Anthropic(t"claude-sonnet-4-5", t"sk-ant-…")

      def prophecy(): Llm.Reply = oracle.session:
        llm.ask(t"When shall we three meet again?")
alternative OpenAI
  highlight OpenAI  Chat Completions
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import strategies.throwUnsafely, codepages.utf8Codepage
      import errorDiagnostics.stackTracesDiagnostics

      val oracle = OpenAI(t"gpt-5", t"sk-…")

      def prophecy(): Llm.Reply = oracle.session:
        llm.ask(t"When shall we three meet again?")
alternative Gemini
  highlight Gemini
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import strategies.throwUnsafely, codepages.utf8Codepage
      import errorDiagnostics.stackTracesDiagnostics

      val oracle = Gemini(t"gemini-2.5-pro", t"AIza…")

      def prophecy(): Llm.Reply = oracle.session:
        llm.ask(t"When shall we three meet again?")
alternative A local server
  highlight OpenAI.compatible  any server which speaks the OpenAI dialect
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import strategies.throwUnsafely, codepages.utf8Codepage
      import errorDiagnostics.stackTracesDiagnostics

      val oracle = OpenAI.compatible(url"http://localhost:11434/v1", t"llama3")

      def prophecy(): Llm.Reply = oracle.session:
        llm.ask(t"When shall we three meet again?")
##
```

The example is compiled, and is not run, since it would call a provider over the network.
[Providers and models](../../soundness/modules/llm.md#providers-and-models) describes the
values, and [small APIs](../../philosophy/small-apis.md) why the vocabulary is shared.
