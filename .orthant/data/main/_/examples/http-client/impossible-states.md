# A header takes the type it carries

A request header is a named argument to `fetch`, and its type is what that header holds. The
`Accept` header takes a media type, which is itself checked, and not any text.

```scala fuel
compile
frame
  label Text where a media type belongs
  errors show
  code
      import httpBackends.javaNetHttp, logging.silentLogging, internetAccess.online
      import strategies.throwUnsafely

      val response = url"https://example.com/".fetch(accept = t"application/json")
frame
  label A media type
  highlight media"application/json"
  code
      import httpBackends.javaNetHttp, logging.silentLogging, internetAccess.online
      import strategies.throwUnsafely

      val response = url"https://example.com/".fetch(accept = media"application/json")
##
```

[Request headers](../../soundness/modules/http-client.md) are described with the rest of a
request.
