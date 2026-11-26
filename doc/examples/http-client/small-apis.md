# One method reads every kind of body

A response is read with `receive`, which takes the type that is wanted. The type chooses how
the body is read, and there is no `bodyAsText` or `bodyAsBytes` to choose between.

```scala fuel
compile
alternative Text
  highlight receive[Text]  decoded with the charset in scope
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import charsets.utf8Charset, strategies.throwUnsafely

      val response = url"https://globe.example.com/plays".fetch()
      val plays = response.receive[Text]
alternative Bytes
  highlight receive[Data]
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import charsets.utf8Charset, strategies.throwUnsafely

      val response = url"https://globe.example.com/plays".fetch()
      val plays = response.receive[Data]
alternative JSON
  highlight receive[Json]  parsed as it is read
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import charsets.utf8Charset, strategies.throwUnsafely

      val response = url"https://globe.example.com/plays".fetch()
      val plays = response.receive[Json]
alternative Only the status
  highlight receive[Http.Status]
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import charsets.utf8Charset, strategies.throwUnsafely

      val response = url"https://globe.example.com/plays".fetch()
      val plays = response.receive[Http.Status]
##
```

The example is compiled, and is not run, since it needs the network.
[Reading the response](../../soundness/modules/http-client.md#reading-the-response) has more,
and [small APIs](../../philosophy/small-apis.md) the reasons.
