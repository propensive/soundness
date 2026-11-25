# A session is a block

Connecting, checking the host key and authenticating are not steps to remember. `session`
does them, lends the connection to a block, and closes it when the block ends, so what comes
out of the block is a plain value and never a live connection.

```scala fuel
compile
frame
  label Where to connect, and how far to trust it
  code
      import systems.javaBaseSystem, errorDiagnostics.stackTracesDiagnostics
      import strategies.throwUnsafely, providers.javaBaseProvider, codepages.utf8Codepage
      import internetAccess.online, socketBackends.javaBaseSockets, logging.silentLogging

      val endpoint = Ssh.Endpoint(t"elsinore.example.com", 22, t"hamlet", Nil)
      given Ssh.HostKeys = Ssh.HostKeys.trustOnFirstUse(KnownHosts(t""))
frame
  label A session, and what it returns
  highlight session:  connected and authenticated until the block ends
  code
      import systems.javaBaseSystem, errorDiagnostics.stackTracesDiagnostics
      import strategies.throwUnsafely, providers.javaBaseProvider, codepages.utf8Codepage
      import internetAccess.online, socketBackends.javaBaseSockets, logging.silentLogging

      val endpoint = Ssh.Endpoint(t"elsinore.example.com", 22, t"hamlet", Nil)
      given Ssh.HostKeys = Ssh.HostKeys.trustOnFirstUse(KnownHosts(t""))

      def uptime(): Optional[Int] = endpoint.session: ssh ?=>
        ssh.exec(t"uptime").status
##
```

The example is compiled, and is not run, since it needs a server.
[Running commands](../../soundness/modules/ssh.md#running-commands) describes what a session
offers, and [delimited scopes](../../philosophy/delimited-scopes.md) the pattern.
