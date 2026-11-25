# An upgrade, staged from an HTTP response

An upgrade is staged from any source of bytes. An HTTP response is one, so the release is
fetched and staged in a single expression, with no temporary file and no stream copied by
hand.

```scala fuel
compile
frame
  label A release, fetched
  code
      import environments.javaBaseEnvironment, systems.javaBaseSystem
      import errorDiagnostics.stackTracesDiagnostics, strategies.throwUnsafely
      import internetAccess.online, httpBackends.javaNetHttp

      def release() = url"https://releases.example.com/herald.signed".fetch()
frame
  label And staged
  highlight Upgrade.stage  verified by the launcher at the next invocation
  code
      import environments.javaBaseEnvironment, systems.javaBaseSystem
      import errorDiagnostics.stackTracesDiagnostics, strategies.throwUnsafely
      import internetAccess.online, httpBackends.javaNetHttp

      def upgrade(): Unit raises Upgrade.Error =
        Upgrade.stage(url"https://releases.example.com/herald.signed".fetch())
##
```

The example is compiled, and is not run, since it needs the network.
[Staging an upgrade](../../soundness/modules/daemons.md#staging-an-upgrade) describes what the
launcher then does, and [the HTTP client](../../soundness/modules/http-client.md) the request.
