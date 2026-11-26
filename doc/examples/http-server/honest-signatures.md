# A method which needs the request says so

Inside a handler, `request` is the request being answered. It is a contextual value, so a
method which reads it must ask for it, and can then be called from a handler, or from a test
which has made a request by hand, with no server at all.

```scala fuel
compile
frame
  label A request, where there is none
  errors show
  code
      import codepages.utf8Codepage

      def scene(): Http.Response = request.target match
        case t"/exit" => Http.Response(Http.Ok)(t"Pursued by a bear")
        case _        => Http.Response(Http.NotFound)(t"No such scene")
frame
  label The request, asked of the caller
  highlight (using..Request)
  code
      import codepages.utf8Codepage

      def scene()(using Http.Request): Http.Response = request.target match
        case t"/exit" => Http.Response(Http.Ok)(t"Pursued by a bear")
        case _        => Http.Response(Http.NotFound)(t"No such scene")
##
```

[A server](../../soundness/modules/http-server.md#a-server) describes `request`, and
[honest signatures](../../philosophy/honest-signatures.md) the principle.
