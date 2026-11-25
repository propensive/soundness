# A request needs the network

Fetching a URL uses the network, and `fetch` says so: it takes an `Online` capability from its
context. Code which was never given one cannot make a request.

```scala fuel
compile
frame
  label No access to the network
  errors show
  code
      import httpBackends.javaNetHttp, logging.silentLogging

      def status(): Http.Response raises Connect.Error = url"https://example.com/".fetch()
frame
  label Access granted, by an import
  highlight internetAccess.online
  code
      import httpBackends.javaNetHttp, logging.silentLogging, internetAccess.online

      def status(): Http.Response raises Connect.Error = url"https://example.com/".fetch()
frame
  label Access asked of the caller
  highlight (using..Online)  the caller must have it
  code
      import httpBackends.javaNetHttp, logging.silentLogging

      def status()(using Online): Http.Response raises Connect.Error =
        url"https://example.com/".fetch()
##
```

[What a request needs in scope](../../soundness/modules/http-client.md#what-a-request-needs-in-scope)
lists the other decisions a request depends on.
