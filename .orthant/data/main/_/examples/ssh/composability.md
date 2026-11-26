# A remote command which cannot be split

A command for a remote shell is usually a string, and a value pasted into it can change what
it means. `exec` also takes a command written with `sh"…"`, whose arguments are quoted one by
one, so a branch name with a space in it is still one argument on the far side.

```scala fuel
compile
frame
  label A command, as text for the remote shell
  highlight $branch  split by the remote shell at any space
  code
      import systems.javaBaseSystem, errorDiagnostics.stackTracesDiagnostics
      import strategies.throwUnsafely, providers.javaBaseProvider, codepages.utf8Codepage
      import internetAccess.online, socketBackends.javaBaseSockets, logging.silentLogging
      import charsets.utf8Charset, textSanitizers.skipSanitizer

      val endpoint = Ssh.Endpoint(t"elsinore.example.com", 22, t"hamlet", Nil)
      given Ssh.HostKeys = Ssh.HostKeys.trustOnFirstUse(KnownHosts(t""))

      def subject(branch: Text): Text = endpoint.session: ssh ?=>
        ssh.exec(t"git log -1 --format=%s $branch").stdout.read[Text]
frame
  label A command, as a list of arguments
  highlight $branch  one argument, whatever it contains
  code
      import systems.javaBaseSystem, errorDiagnostics.stackTracesDiagnostics
      import strategies.throwUnsafely, providers.javaBaseProvider, codepages.utf8Codepage
      import internetAccess.online, socketBackends.javaBaseSockets, logging.silentLogging
      import charsets.utf8Charset, textSanitizers.skipSanitizer

      val endpoint = Ssh.Endpoint(t"elsinore.example.com", 22, t"hamlet", Nil)
      given Ssh.HostKeys = Ssh.HostKeys.trustOnFirstUse(KnownHosts(t""))

      def subject(branch: Text): Text = endpoint.session: ssh ?=>
        ssh.exec(sh"git log -1 --format=%s $branch").stdout.read[Text]
##
```

The example is compiled, and is not run, since it needs a server.
[Running commands](../../soundness/modules/ssh.md#running-commands) describes `exec`, and
[processes](../../soundness/modules/processes.md#substitution-and-escaping) the escaping.
