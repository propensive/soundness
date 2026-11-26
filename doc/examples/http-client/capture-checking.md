# A response does not outlive its connection

An HTTP session keeps one connection open for a block, and every fetch within it travels over
that connection. A response's body is still on the wire when `fetch` returns, so the response
depends on the session, and cannot be the block's result. Its status is a value, and can.

```scala fuel
compile
feature experimental.captureChecking
frame
  label A response whose body would be read from a closed connection
  errors show
  highlight session.fetch(request)
  code
      import soundness.httpUrlSessional
      import httpBackends.javaNetHttp, socketBackends.javaBaseSockets, internetAccess.online
      import logging.silentLogging, strategies.throwUnsafely

      val request = Http.Request(Http.Get, 1.1, t"example.com".as[Host], t"/", Nil,
          Spring[Data](Http.emptyBody()))

      def page() = url"https://example.com".session: session ?=>
        session.fetch(request)
frame
  label What is wanted from it, taken inside the session
  highlight .status
  code
      import soundness.httpUrlSessional
      import httpBackends.javaNetHttp, socketBackends.javaBaseSockets, internetAccess.online
      import logging.silentLogging, strategies.throwUnsafely

      val request = Http.Request(Http.Get, 1.1, t"example.com".as[Host], t"/", Nil,
          Spring[Data](Http.emptyBody()))

      def page() = url"https://example.com".session: session ?=>
        session.fetch(request).status
##
```

The example is compiled, and is not run, since it needs the network.
[Sessions](../../soundness/modules/http-client.md#sessions) describes how requests share a
connection.
