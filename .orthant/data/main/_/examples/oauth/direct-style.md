# A token, obtained in a line

The client-credentials grant is a request to an authorization server and a response to parse.
It is written as one call which returns an `Authorization`, and what follows uses the token
as a plain value.

```scala fuel
compile
frame
  label The authorization server
  code
      import strategies.throwUnsafely, internetAccess.online, httpBackends.javaNetHttp
      import logging.silentLogging, errorDiagnostics.stackTracesDiagnostics

      val issuer = Issuer(url"https://issuer.example/token", t"globe", t"s3cret")
frame
  label A token for two scopes
  highlight clientCredentials
  code
      import strategies.throwUnsafely, internetAccess.online, httpBackends.javaNetHttp
      import logging.silentLogging, errorDiagnostics.stackTracesDiagnostics

      val issuer = Issuer(url"https://issuer.example/token", t"globe", t"s3cret")
      val token: Authorization = issuer.clientCredentials(t"read", t"prompt")
frame
  label What it allows
  highlight grants
  code
      import strategies.throwUnsafely, internetAccess.online, httpBackends.javaNetHttp
      import logging.silentLogging, errorDiagnostics.stackTracesDiagnostics

      val issuer = Issuer(url"https://issuer.example/token", t"globe", t"s3cret")
      val token: Authorization = issuer.clientCredentials(t"read", t"prompt")
      val mayPrompt: Boolean = token.grants(List(t"prompt"))
frame
  label And a new one, when it expires
  highlight refresh
  code
      import strategies.throwUnsafely, internetAccess.online, httpBackends.javaNetHttp
      import logging.silentLogging, errorDiagnostics.stackTracesDiagnostics

      val issuer = Issuer(url"https://issuer.example/token", t"globe", t"s3cret")
      val token: Authorization = issuer.clientCredentials(t"read", t"prompt")
      val mayPrompt: Boolean = token.grants(List(t"prompt"))
      val fresh: Authorization = issuer.refresh(token)
##
```

The example is compiled, and is not run, since it would contact an authorization server.
[Obtaining a token](../../soundness/modules/oauth.md#obtaining-a-token) has the other grants,
and [direct style](../../philosophy/direct-style.md) the idea.
