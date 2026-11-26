# A question needs a conversation to be asked in

`llm` reaches the session of the block it is written in, and exists nowhere else. A method
which asks the model something therefore either opens a session or names one in its signature,
and a reader of the signature knows which.

```scala fuel
compile
frame
  label A question, with no session to ask it in
  errors show
  code
      def soothsay(question: Text): Text = llm.ask(question).text
frame
  label The session is a parameter
  highlight (using..Session)  the caller's conversation is continued
  code
      def soothsay(question: Text)(using Llm.Session): Text = llm.ask(question).text
frame
  label And is supplied by the block which calls it
  highlight seer.session:
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import strategies.throwUnsafely, codepages.utf8Codepage
      import errorDiagnostics.stackTracesDiagnostics

      def soothsay(question: Text)(using Llm.Session): Text = llm.ask(question).text

      val seer = Anthropic(t"claude-sonnet-4-5", t"sk-ant-…")

      def warning(): Text = seer.session:
        soothsay(t"What should Caesar beware?")
##
```

The example is compiled, and is not run, since it would call a provider over the network.
[A conversation](../../soundness/modules/llm.md#a-conversation) describes a session, and
[honest signatures](../../philosophy/honest-signatures.md) the principle.
