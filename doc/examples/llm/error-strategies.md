# What a failed request does is the caller's choice

A request to a provider can fail in a dozen ways, each an `Llm.Error` with a reason. The code
which asks is the same whatever is to be done about it. The caller picks: an absent answer, a
reply chosen by the reason, or an error declared for its own caller.

```scala fuel
compile
alternative An absent answer
  highlight safely  any Llm.Error becomes Unset
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import strategies.throwUnsafely, codepages.utf8Codepage
      import errorDiagnostics.stackTracesDiagnostics

      val oracle = Anthropic(t"claude-sonnet-4-5", t"sk-ant-…")

      def prophecy(): Optional[Text] =
        safely(oracle.session(llm.ask(t"Shall Banquo's sons be kings?")).text)
alternative A reply for each reason
  highlight error.reason  one of a fixed set
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import strategies.throwUnsafely, codepages.utf8Codepage
      import errorDiagnostics.stackTracesDiagnostics

      val oracle = Anthropic(t"claude-sonnet-4-5", t"sk-ant-…")

      def prophecy(): Text =
        recover:
          case error: Llm.Error => error.reason match
            case Llm.Error.Reason.Unauthorized => t"the oracle wants its fee"
            case Llm.Error.Reason.TooLarge     => t"ask a shorter question"
            case other                         => t"the oracle is silent"
        . protect(oracle.session(llm.ask(t"Shall Banquo's sons be kings?")).text)
alternative The caller's decision
  highlight raises..Error  declared, and left to whoever calls
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import codepages.utf8Codepage, errorDiagnostics.stackTracesDiagnostics

      val oracle = Anthropic(t"claude-sonnet-4-5", t"sk-ant-…")

      def prophecy(): Text raises Llm.Error =
        oracle.session(llm.ask(t"Shall Banquo's sons be kings?")).text
##
```

The example is compiled, and is not run, since it would call a provider over the network.
[Errors and retries](../../soundness/modules/llm.md#errors-and-retries) lists the reasons, and
[choosing a strategy](../../soundness/modules/errors.md#choosing-a-strategy) the strategies.
