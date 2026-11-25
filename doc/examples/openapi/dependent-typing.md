# A response typed by its operation

The type a response is read as must conform to the schema which the specification declares for
that operation. A pet is an object, so it is read as a case class with its fields, and not as a
number.

```scala fuel
compile
frame
  label A pet, read as a pet
  highlight call[Pet]  conforms to the schema of the 200 response
  code
      import strategies.throwUnsafely, internetAccess.online, httpBackends.javaNetHttp
      import logging.silentLogging, construables.jsonConstruable, codepages.utf8Codepage
      import errorDiagnostics.stackTracesDiagnostics

      case class Pet(id: Int, name: Text, tag: Optional[Text] = Unset)

      val api = Api(cp"/openapi/local/petstore.json")

      val crab: Pet = api.pets(42).get.call[Pet]()
frame
  label A deletion, which answers with no body
  highlight Unit  the 204 response declares no content
  code
      import strategies.throwUnsafely, internetAccess.online, httpBackends.javaNetHttp
      import logging.silentLogging, construables.jsonConstruable, codepages.utf8Codepage
      import errorDiagnostics.stackTracesDiagnostics

      case class Pet(id: Int, name: Text, tag: Optional[Text] = Unset)

      val api = Api(cp"/openapi/local/petstore.json")

      val crab: Pet = api.pets(42).get.call[Pet]()
      val gone: Unit = api.pets(42).delete.call()
frame
  label A pet, read as a number
  errors show
  code
      import strategies.throwUnsafely, internetAccess.online, httpBackends.javaNetHttp
      import logging.silentLogging, construables.jsonConstruable, codepages.utf8Codepage
      import errorDiagnostics.stackTracesDiagnostics

      val api = Api(cp"/openapi/local/petstore.json")

      val crab: Int = api.pets(42).get.call[Int]()
##
```

The example is compiled, and is not run, since it needs the network.
[Calling](../../soundness/modules/openapi.md#calling) says which response is chosen where an
operation declares several.
