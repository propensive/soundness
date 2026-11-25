# A response header takes the type it carries

The headers of a response are named arguments, each with the type of what that header holds.
`cacheControl` is text, and `contentType` is a media type, which is itself checked by the
compiler.

```scala fuel
compile
frame
  label Text where a media type belongs
  errors show
  code
      import codepages.utf8Codepage

      val scene = Http.Response(Http.Ok, contentType = t"text/markdown")(t"*Exit*")
frame
  label A media type
  highlight media"text/markdown"
  code
      import codepages.utf8Codepage

      val scene = Http.Response(Http.Ok, contentType = media"text/markdown")(t"*Exit*")
##
```

[Responses](../../soundness/modules/http-server.md#responses) has more on the headers and
bodies of a response.
