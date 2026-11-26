# A response header has the type of its name

The headers of a response are read by name, and the name decides the type of what comes back:
a media type for `contentType`, text for `server`. A name which is not a
header has no type, and does not compile.

```scala fuel
compile
frame
  label A media type
  highlight List[MediaType]
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import strategies.throwUnsafely

      val response = url"https://globe.example.com/plays".fetch()
      val kind: List[MediaType] = response.headers.contentType
frame
  label Text
  highlight List[Text]
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import strategies.throwUnsafely

      val response = url"https://globe.example.com/plays".fetch()
      val kind: List[MediaType] = response.headers.contentType
      val software: List[Text] = response.headers.server
frame
  label A header which does not exist
  errors show
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import strategies.throwUnsafely

      val response = url"https://globe.example.com/plays".fetch()
      val kind: List[MediaType] = response.headers.contentType
      val software = response.headers.sever
##
```

The example is compiled, and is not run, since it needs the network.
[Headers](../../soundness/modules/http-client.md#headers) describes the same names on a
request.
