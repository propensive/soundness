# A request, read from top to bottom

Fetching a page, reading its body as JSON and decoding a value from it are three expressions,
one after another. Each can fail or wait, and none of them needs a `Future`, a `for`
comprehension or a callback.

```scala fuel
frame
  label A response
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import charsets.utf8Charset, strategies.throwUnsafely

      case class Play(title: Text, acts: Int)

      val response = url"https://example.com/plays/hamlet".fetch()
frame
  label Its body, as JSON
  highlight receive[Json]
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import charsets.utf8Charset, strategies.throwUnsafely

      case class Play(title: Text, acts: Int)

      val response = url"https://example.com/plays/hamlet".fetch()
      val json = response.receive[Json]
frame
  label A value, decoded from it
  highlight as[Play]
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import charsets.utf8Charset, strategies.throwUnsafely

      case class Play(title: Text, acts: Int)

      val response = url"https://example.com/plays/hamlet".fetch()
      val json = response.receive[Json]
      val play = json.as[Play]
##
```

The example is compiled, and is not run, since it needs the network.
[Direct style](../../philosophy/direct-style.md) says what became of the effects.
