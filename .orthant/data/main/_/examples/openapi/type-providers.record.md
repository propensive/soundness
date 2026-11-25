# A response with no class to mirror it

`record()` reads a JSON response as a record whose members are the properties of the response
schema, each at the type the schema declares. No case class is written, and none can fall out
of step with the specification.

```scala fuel
compile
frame
  label A record of the Pet schema
  code
      import strategies.throwUnsafely, internetAccess.online, httpBackends.javaNetHttp
      import logging.silentLogging, construables.jsonConstruable, codepages.utf8Codepage
      import errorDiagnostics.stackTracesDiagnostics

      val api = Api(cp"/openapi/local/petstore.json")

      val crab = api.pets(42).get.record()
frame
  label Its properties
  highlight crab.name  a string in the schema
  code
      import strategies.throwUnsafely, internetAccess.online, httpBackends.javaNetHttp
      import logging.silentLogging, construables.jsonConstruable, codepages.utf8Codepage
      import errorDiagnostics.stackTracesDiagnostics

      val api = Api(cp"/openapi/local/petstore.json")

      val crab = api.pets(42).get.record()
      val called: Text = crab.name
frame
  label Every pet's name
  highlight _.name
  code
      import strategies.throwUnsafely, internetAccess.online, httpBackends.javaNetHttp
      import logging.silentLogging, construables.jsonConstruable, codepages.utf8Codepage
      import errorDiagnostics.stackTracesDiagnostics

      val api = Api(cp"/openapi/local/petstore.json")

      val crab = api.pets(42).get.record()
      val called: Text = crab.name
      val names: List[Text] = api.pets.get.record().map(_.name)
##
```

The example is compiled, and is not run, since it needs the network.
[Records from a response](../../soundness/modules/openapi.md#records-from-a-response) describes
nested schemas, and `tuple()`.
