# An agent which lives for one block

`Acp.connect` takes two blocks. The first registers handlers, before any agent exists. The
second is lent the live connection, and when it ends the agent's process is killed. A prompt
written in the first block has no connection to send it on.

```scala fuel
compile
frame
  label A prompt, before there is a connection
  errors show
  code
      import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
      import threads.virtualThreads, probates.awaitProbate
      import strategies.throwUnsafely, errorDiagnostics.stackTracesDiagnostics

      def revise(): Unit = supervise:
        Acp.connect(Acp.Endpoint(sh"claude-code-acp")):
          Acp.connection.prompt(t"globe", t"Cut the fifth act")

        . apply(())
frame
  label The connection is lent to the second block
  highlight apply:  the agent is running from here to the end of the block
  code
      import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
      import threads.virtualThreads, probates.awaitProbate
      import strategies.throwUnsafely, errorDiagnostics.stackTracesDiagnostics

      def revise(): Acp.StopReason = supervise:
        Acp.connect(Acp.Endpoint(sh"claude-code-acp"))(())
        . apply:
            val session = Acp.connection.newSession(t"/globe")
            Acp.connection.prompt(session.sessionId, t"Cut the fifth act")
##
```

The example is compiled, and is not run, since it launches an agent.
[Connecting](../../soundness/modules/acp.md#connecting) describes the two blocks, and
[delimited scopes](../../philosophy/delimited-scopes.md) the pattern.
