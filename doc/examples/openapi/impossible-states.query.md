# A required query parameter

An operation's query parameters are named arguments, and the specification says which of them
are required. The photos of a pet must be asked for with a `width`, and may be asked for with a
`height`.

```scala fuel
compile
frame
  label The width is left out
  errors show
  code
      import strategies.throwUnsafely, internetAccess.online, httpBackends.javaNetHttp
      import logging.silentLogging, construables.jsonConstruable, codepages.utf8Codepage
      import errorDiagnostics.stackTracesDiagnostics

      val api = Api(cp"/openapi/local/petstore.json")

      val photos: Json = api.pets(42).photos.get(height = 600).call[Json]()
frame
  label The width is given
  highlight width  required by the specification
  code
      import strategies.throwUnsafely, internetAccess.online, httpBackends.javaNetHttp
      import logging.silentLogging, construables.jsonConstruable, codepages.utf8Codepage
      import errorDiagnostics.stackTracesDiagnostics

      val api = Api(cp"/openapi/local/petstore.json")

      val photos: Json = api.pets(42).photos.get(width = 800).call[Json]()
##
```

The example is compiled, and is not run, since it needs the network.
[Calling](../../soundness/modules/openapi.md#calling) describes how an operation is invoked.
