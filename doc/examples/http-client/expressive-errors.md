# Why a connection failed

A `Connect.Error` holds its reason as a value of an enumeration, and a TLS failure holds the
stage at which it happened. A program can act on each, and the compiler checks that none is
forgotten.

```scala fuel
compile
frame
  label The error, captured
  highlight capture[Connect.Error]
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import strategies.throwUnsafely, errorDiagnostics.stackTracesDiagnostics

      val error = capture[Connect.Error](url"https://elsinore.invalid/".fetch())
frame
  label Advice for each reason
  highlight Ssl(stage)  Handshake, Key, Peer or Protocol
  code
      import httpBackends.javaNetHttp, internetAccess.online, logging.silentLogging
      import strategies.throwUnsafely, errorDiagnostics.stackTracesDiagnostics

      val error = capture[Connect.Error](url"https://elsinore.invalid/".fetch())

      val advice: Text = error.reason match
        case Connect.Error.Reason.Dns        => t"check the hostname"
        case Connect.Error.Reason.Refused    => t"check the port"
        case Connect.Error.Reason.Ssl(stage) => t"check the certificate"
        case Connect.Error.Reason.Timeout    => t"try again later"
        case Connect.Error.Reason.Unknown    => t"ask the Ghost"
##
```

The example is compiled, and is not run, since it needs the network.
[Errors](../../soundness/modules/http-client.md#errors) has the table of reasons.
