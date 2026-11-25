# The errors an operation declares

Each response which the specification declares outside the success range is an error type,
named for its status. `attempt` gives a result over exactly those which the operation declares,
so a case for any other status is a mistake the compiler finds.

```scala fuel
compile
frame
  label The declared errors of GET /store/order/{orderId}
  code
      import strategies.throwUnsafely, internetAccess.online, httpBackends.javaNetHttp
      import logging.silentLogging, construables.jsonConstruable, codepages.utf8Codepage
      import errorDiagnostics.stackTracesDiagnostics

      val petstore =
        Api(cp"/openapi/swagger/petstore3.json", base = url"https://petstore3.swagger.io")

      val order: Text = petstore.store.order(42L).get.attempt[Json]() match
        case Attempt.Success(order)              => t"found"
        case Attempt.Failure(Api.NotFound(()))   => t"no such order"
        case Attempt.Failure(Api.BadRequest(())) => t"not an order number"
        case Attempt.Failure(other)              => t"refused"
frame
  label A status which it does not declare
  errors show
  code
      import strategies.throwUnsafely, internetAccess.online, httpBackends.javaNetHttp
      import logging.silentLogging, construables.jsonConstruable, codepages.utf8Codepage
      import errorDiagnostics.stackTracesDiagnostics

      val petstore =
        Api(cp"/openapi/swagger/petstore3.json", base = url"https://petstore3.swagger.io")

      val order: Text = petstore.store.order(42L).get.attempt[Json]() match
        case Attempt.Success(order)              => t"found"
        case Attempt.Failure(Api.NotFound(()))   => t"no such order"
        case Attempt.Failure(Api.Conflict(()))   => t"already ordered"
        case Attempt.Failure(other)              => t"refused"
##
```

The example is compiled, and is not run, since it needs the network.
[Errors](../../soundness/modules/openapi.md#errors) has the type for each kind of response.
