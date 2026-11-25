# Headers are named arguments

A request header is written as a named argument to `fetch`, with the header's name in camel
case. The names are those of real headers, and each takes a value of the type which that
header carries.

```scala fuel
compile
frame
  label A request with no headers
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import strategies.throwUnsafely

      val response = url"https://globe.example.com/plays".fetch()
frame
  label One header
  highlight accept  the Accept header
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import strategies.throwUnsafely

      val response = url"https://globe.example.com/plays".fetch(accept = media"application/json")
frame
  label Two headers
  highlight userAgent  the User-Agent header
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import strategies.throwUnsafely

      val response = url"https://globe.example.com/plays".fetch
        ( accept = media"application/json", userAgent = t"Groundling" )
##
```

The example is compiled, and is not run, since it needs the network.
[Headers](../../soundness/modules/http-client.md#headers) has more.
