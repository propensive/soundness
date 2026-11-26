# Each step of a request declares how it fails

A connection which cannot be made is a `Connect.Error`. A response outside the success range
is an `Http.Error`, and a body which ends early is a `Truncation.Error`. A method declares
each where it can arise: the first when the request is sent, and the others only when the
body is read.

```scala fuel
compile
frame
  label Sending may fail to connect
  highlight raises..Error
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging

      def response(): Http.Response raises Connect.Error =
        url"https://globe.example.com/plays".fetch()
frame
  label Reading the body may find a bad status, or too few bytes
  highlight raises..Truncation.Error
    line 7
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import charsets.utf8Charset

      def response(): Http.Response raises Connect.Error =
        url"https://globe.example.com/plays".fetch()

      def plays(): Text raises Connect.Error raises Http.Error raises Truncation.Error =
        response().receive[Text]
##
```

The example is compiled, and is not run, since it needs the network.
[Errors](../../soundness/modules/http-client.md#errors) describes what each error carries.
