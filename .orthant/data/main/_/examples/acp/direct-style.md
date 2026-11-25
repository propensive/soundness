# A turn is one expression

A prompt is not answered at once. The agent works, streams its progress, asks for permissions,
and only then says why it stopped. `prompt` is nonetheless an ordinary method call: it returns
the stop reason when the turn is over, and the updates go to a handler meanwhile.

```scala fuel
compile
frame
  label A session, and a prompt
  highlight Acp.connection.prompt  returns when the agent has finished its turn
  code
      import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
      import threads.virtualThreads, probates.awaitProbate
      import strategies.throwUnsafely, errorDiagnostics.stackTracesDiagnostics

      def revise(): Acp.StopReason = supervise:
        Acp.connect(Acp.Endpoint(sh"claude-code-acp"))(())
        . apply:
            val session = Acp.connection.newSession(t"/globe")
            Acp.connection.prompt(session.sessionId, t"Cut the fifth act")
frame
  label What the turn came to
  highlight Acp.StopReason.Refusal  one of five reasons
  code
      import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
      import threads.virtualThreads, probates.awaitProbate
      import strategies.throwUnsafely, errorDiagnostics.stackTracesDiagnostics

      def revise(): Text = supervise:
        Acp.connect(Acp.Endpoint(sh"claude-code-acp"))(())
        . apply:
            val session = Acp.connection.newSession(t"/globe")
            Acp.connection.prompt(session.sessionId, t"Cut the fifth act") match
              case Acp.StopReason.EndTurn => t"the act is cut"
              case Acp.StopReason.Refusal => t"the agent would not"
              case other                  => t"the turn was cut short"
##
```

The example is compiled, and is not run, since it launches an agent.
[Stop reasons](../../soundness/modules/acp.md#stop-reasons) lists them, and
[direct style](../../philosophy/direct-style.md) says where the waiting went.
