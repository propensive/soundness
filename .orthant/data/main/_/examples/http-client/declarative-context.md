# The same request, sent another way

Which implementation sends a request, and whether it follows a redirect, are not arguments to
`fetch`. Each is chosen by an import, for all the requests in its scope, and the request
itself is the same line.

```scala fuel
compile
alternative The JVM's client
  highlight httpBackends.javaNetHttp
  code
      import httpBackends.javaNetHttp
      import internetAccess.online, logging.silentLogging, strategies.throwUnsafely

      val status = url"https://globe.example.com/plays".fetch().status
alternative Soundness's own codecs
  highlight httpBackends.soundnessHttp
  code
      import httpBackends.soundnessHttp, socketBackends.javaBaseSockets
      import internetAccess.online, logging.silentLogging, strategies.throwUnsafely

      val status = url"https://globe.example.com/plays".fetch().status
alternative No redirects
  highlight httpRedirections.doNotFollowRedirects
  code
      import httpBackends.javaNetHttp, httpRedirections.doNotFollowRedirects
      import internetAccess.online, logging.silentLogging, strategies.throwUnsafely

      val status = url"https://globe.example.com/plays".fetch().status
##
```

The example is compiled, and is not run, since it needs the network.
[Choosing a transport](../../soundness/modules/http-client.md#choosing-a-transport) compares
the backends, and [declarative context](../../philosophy/declarative-context.md) explains the
approach.
