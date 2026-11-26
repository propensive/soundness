# A malformed URL is never requested

The URL of a request is written with `url"…"`, and parsed by the compiler. A mistake in it is
found where it is written, long before there is a network to send it to.

```scala fuel
compile
frame
  label A port which is not a number
  errors show
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import strategies.throwUnsafely

      val response = url"https://globe.example.com:port/plays".fetch()
frame
  label A port which is
  highlight 8080
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import strategies.throwUnsafely

      val response = url"https://globe.example.com:8080/plays".fetch()
##
```

The example is compiled, and is not run, since it needs the network.
[A GET request](../../soundness/modules/http-client.md#a-get-request) describes `fetch`, and
[safety by construction](../../philosophy/safety-by-construction.md) the principle.
