# A JSON value is a request body

The body of a request is a typed value, and its type supplies the `Content-Type`. The HTTP
client knows nothing of JSON, and JSON nothing of HTTP. One import introduces them, and a case
class is posted as `application/json`.

```scala fuel
compile
frame
  label Text, sent as text/plain
  highlight t"Hamlet"
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import codepages.utf8Codepage, strategies.throwUnsafely

      case class Play(title: Text, acts: Int)

      val response = url"https://globe.example.com/plays".submit(Http.Post)(t"Hamlet")
frame
  label A value, sent as application/json
  highlight jsonPostable
  highlight in[Json]
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import codepages.utf8Codepage, strategies.throwUnsafely, postables.jsonPostable
      import formatting.compactJsonFormatting

      case class Play(title: Text, acts: Int)

      val response =
        url"https://globe.example.com/plays".submit(Http.Post)(Play(t"Hamlet", 5).in[Json])
##
```

The example is compiled, and is not run, since it needs the network.
[Posting a body](../../soundness/modules/http-client.md#posting-a-body) describes `submit`,
and [JSON over HTTP](../../soundness/modules/json.md#json-over-http) the other direction.
