# How far to trust a host key

A session checks the server's key against the policy in scope, and the policy is one given.
The code which connects and runs a command is the same under a strict policy, a lenient one,
and one fit only for tests.

```scala fuel
compile
alternative Only keys already known
  highlight given..known  a host which is not in the file is refused
  code
      import systems.javaBaseSystem, errorDiagnostics.stackTracesDiagnostics
      import strategies.throwUnsafely, providers.javaBaseProvider, codepages.utf8Codepage
      import internetAccess.online, socketBackends.javaBaseSockets, logging.silentLogging

      val endpoint = Ssh.Endpoint(t"elsinore.example.com", 22, t"hamlet", Nil)

      def uptime(known: KnownHosts): Optional[Int] =
        given KnownHosts = known

        endpoint.session: ssh ?=>
          ssh.exec(t"uptime").status
alternative Trust on first use
  highlight trustOnFirstUse  a new host is accepted, and a changed key is not
  code
      import systems.javaBaseSystem, errorDiagnostics.stackTracesDiagnostics
      import strategies.throwUnsafely, providers.javaBaseProvider, codepages.utf8Codepage
      import internetAccess.online, socketBackends.javaBaseSockets, logging.silentLogging

      val endpoint = Ssh.Endpoint(t"elsinore.example.com", 22, t"hamlet", Nil)

      def uptime(known: KnownHosts): Optional[Int] =
        given Ssh.HostKeys = Ssh.HostKeys.trustOnFirstUse(known)

        endpoint.session: ssh ?=>
          ssh.exec(t"uptime").status
alternative Anything, for a test
  highlight insecurelyAcceptAll  says what it is
  code
      import systems.javaBaseSystem, errorDiagnostics.stackTracesDiagnostics
      import strategies.throwUnsafely, providers.javaBaseProvider, codepages.utf8Codepage
      import internetAccess.online, socketBackends.javaBaseSockets, logging.silentLogging

      val endpoint = Ssh.Endpoint(t"elsinore.example.com", 22, t"hamlet", Nil)

      def uptime(known: KnownHosts): Optional[Int] =
        given Ssh.HostKeys = Ssh.HostKeys.insecurelyAcceptAll

        endpoint.session: ssh ?=>
          ssh.exec(t"uptime").status
##
```

The example is compiled, and is not run, since it needs a server.
[The transport](../../soundness/modules/ssh.md#the-transport) says where the policy is
consulted, and [declarative context](../../philosophy/declarative-context.md) why it is not an
argument of `session`.
