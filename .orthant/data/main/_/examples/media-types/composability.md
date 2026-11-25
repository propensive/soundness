# A media type as the value of a header

The HTTP client knows nothing of the registry of media types, and takes one as the value of
its `accept` parameter all the same. Text is not accepted there, so a header cannot carry a
type which was never checked.

```scala fuel
compile
frame
  label Text where a media type is wanted
  errors show
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import strategies.throwUnsafely

      def script() = url"https://example.com/hamlet".fetch(accept = t"application/json")
frame
  label A checked media type
  highlight media"application/json"  checked against the registry
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import strategies.throwUnsafely

      def script() = url"https://example.com/hamlet".fetch(accept = media"application/json")
##
```

The example is compiled, and is not run, since it needs the network.
[Headers](../../soundness/modules/http-client.md#headers) describes the typed parameters of a
request.
