# A call which reads as the request it sends

A path of the API is a chain of members, a path parameter is an argument, the method is a
member, and the query parameters are named arguments. The expression is the request, and each
part of it is checked against the specification.

```scala fuel
compile
frame
  label GET /pets
  code
      import strategies.throwUnsafely, internetAccess.online, httpBackends.javaNetHttp
      import logging.silentLogging, construables.jsonConstruable, codepages.utf8Codepage
      import errorDiagnostics.stackTracesDiagnostics

      val api = Api(cp"/openapi/local/petstore.json")

      val hounds: Json = api.pets.get.call[Json]()
frame
  label GET /pets?limit=10&tag=hound
  highlight limit..)  the query
  code
      import strategies.throwUnsafely, internetAccess.online, httpBackends.javaNetHttp
      import logging.silentLogging, construables.jsonConstruable, codepages.utf8Codepage
      import errorDiagnostics.stackTracesDiagnostics

      val api = Api(cp"/openapi/local/petstore.json")

      val hounds: Json = api.pets.get(limit = 10, tag = t"hound").call[Json]()
frame
  label A parameter the operation does not have
  errors show
  code
      import strategies.throwUnsafely, internetAccess.online, httpBackends.javaNetHttp
      import logging.silentLogging, construables.jsonConstruable, codepages.utf8Codepage
      import errorDiagnostics.stackTracesDiagnostics

      val api = Api(cp"/openapi/local/petstore.json")

      val hounds: Json = api.pets.get(limit = 10, breed = t"hound").call[Json]()
##
```

The example is compiled, and is not run, since it needs the network.
[Calling](../../soundness/modules/openapi.md#calling) has the other methods.
