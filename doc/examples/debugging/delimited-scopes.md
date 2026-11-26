# A debuggee which lives for one block

`session` launches a program under the debug agent, connects to it, and lends the connection to
a block as `debug`. When the block ends, however it ends, the socket is closed and the process
is stopped. Outside the block there is no `debug` to call.

```scala fuel
compile
frame
  label A question asked after the session has ended
  errors show
  code
      import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
      import internetAccess.online, socketBackends.javaBaseSockets, logging.silentLogging
      import threads.virtualThreads, probates.awaitProbate
      import strategies.throwUnsafely, errorDiagnostics.stackTracesDiagnostics

      def cast(): List[ThreadId] = supervise:
        val hamlet: Debuggee = Debuggee(sh"java -cp globe.jar elsinore.Hamlet")
        hamlet.session(debug.suspend())
        debug.threads()
frame
  label The same question, inside it
  highlight hamlet.session:  launched here, and stopped where the block ends
  code
      import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
      import internetAccess.online, socketBackends.javaBaseSockets, logging.silentLogging
      import threads.virtualThreads, probates.awaitProbate
      import strategies.throwUnsafely, errorDiagnostics.stackTracesDiagnostics

      def cast(): List[ThreadId] = supervise:
        val hamlet: Debuggee = Debuggee(sh"java -cp globe.jar elsinore.Hamlet")
        hamlet.session:
          debug.suspend()
          debug.threads()
##
```

The example is compiled, and is not run, since it launches another process.
[Opening a session](../../soundness/modules/debugging.md#opening-a-session) describes the
launch and the teardown, and [delimited scopes](../../philosophy/delimited-scopes.md) the
pattern.
