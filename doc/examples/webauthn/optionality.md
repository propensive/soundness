# A challenge is given back once

`Ceremonies` keeps a session's challenge between two requests, and `conclude` gives it back
only once. The second time there is none, so its result is an `Optional`, which cannot be
passed on as though a challenge were there.

```scala fuel
compile
frame
  label The challenge may be absent
  errors show
  code
      import errorDiagnostics.stackTracesDiagnostics, providers.javaBaseProvider

      val globe =
        WebAuthn.RelyingParty(t"globe.example", t"The Globe", List(t"https://globe.example"))
      val ceremonies = Ceremonies()

      def finish(session: Session, answer: Json, stored: WebAuthn.Credential)
      :   WebAuthn.Credential raises WebAuthn.Error =
        WebAuthn.authenticate(answer, ceremonies.conclude(session), globe, stored)
frame
  label An absent challenge is a refusal
  highlight or:  what absence means here
  code
      import errorDiagnostics.stackTracesDiagnostics, providers.javaBaseProvider

      val globe =
        WebAuthn.RelyingParty(t"globe.example", t"The Globe", List(t"https://globe.example"))
      val ceremonies = Ceremonies()

      def finish(session: Session, answer: Json, stored: WebAuthn.Credential)
      :   WebAuthn.Credential raises WebAuthn.Error =
        val sent = ceremonies.conclude(session).or:
          abort(WebAuthn.Error(WebAuthn.Error.Reason.Challenge))

        WebAuthn.authenticate(answer, sent, globe, stored)
##
```

An answer which is offered twice finds no ceremony in progress, as
[In a web application](../../soundness/modules/webauthn.md#in-a-web-application) explains, and
[optionality](../../philosophy/optionality.md) has more on `or`.
