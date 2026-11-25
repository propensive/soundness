# What a failed request becomes

`fetch` and `receive` do not decide what a refused connection or a missing page means. The
method which calls them does, and the same request serves each choice.

```scala fuel
compile
alternative An absent page
  highlight safely
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import charsets.utf8Charset

      def scene(page: HttpUrl): Optional[Text] = safely(page.fetch().receive[Text])
alternative An empty page for a bad status
  highlight recover
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import charsets.utf8Charset

      def scene(page: HttpUrl): Text raises Connect.Error raises Truncation.Error =
        recover:
          case error: Http.Error => t""
        . protect(page.fetch().receive[Text])
alternative The caller's decision
  highlight raises..Truncation.Error
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import charsets.utf8Charset

      def scene(page: HttpUrl)
      :   Text raises Connect.Error raises Http.Error raises Truncation.Error =
        page.fetch().receive[Text]
##
```

The example is compiled, and is not run, since it needs the network.
[Errors](../../soundness/modules/http-client.md#errors) describes the two errors, and
[Choosing a strategy](../../soundness/modules/errors.md#choosing-a-strategy) the ways to meet
them.
