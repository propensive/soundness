# An operation which needs a credential

The specification says which security scheme each operation requires, and the client asks for
a credential of that scheme as a contextual value. An operation whose credential nobody has
provided is not called without it. It does not compile.

```scala fuel
compile
frame
  label Nobody has provided a credential
  errors show
  code
      import strategies.throwUnsafely, internetAccess.online, httpBackends.javaNetHttp
      import logging.silentLogging, construables.jsonConstruable, codepages.utf8Codepage
      import errorDiagnostics.stackTracesDiagnostics

      val petstore =
        Api(cp"/openapi/swagger/petstore3.json", base = url"https://petstore3.swagger.io")

      val stock: Json = petstore.store.inventory.get.call()
frame
  label A key for the scheme the operation names
  highlight "api_key"..Text  the scheme, and what it holds
  code
      import strategies.throwUnsafely, internetAccess.online, httpBackends.javaNetHttp
      import logging.silentLogging, construables.jsonConstruable, codepages.utf8Codepage
      import errorDiagnostics.stackTracesDiagnostics

      given apiKey: ("api_key" is Credential to Text) = Credential(t"speak-friend")

      val petstore =
        Api(cp"/openapi/swagger/petstore3.json", base = url"https://petstore3.swagger.io")

      val stock: Json = petstore.store.inventory.get.call()
##
```

The example is compiled, and is not run, since it needs the network.
[Security](../../soundness/modules/openapi.md#security) has the credential for each kind of
scheme, and [honest signatures](../../philosophy/honest-signatures.md) the idea.
