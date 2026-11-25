# What a refused sign-in becomes

`WebAuthn.authenticate` refuses an answer by raising a `WebAuthn.Error`, and does not decide
what the refusal means. The method which calls it does, and the same check serves each choice.

```scala fuel
compile
alternative Nobody signed in
  highlight safely  a refusal is an absent credential
  code
      import errorDiagnostics.stackTracesDiagnostics, providers.javaBaseProvider

      val globe =
        WebAuthn.RelyingParty(t"globe.example", t"The Globe", List(t"https://globe.example"))

      def signIn(answer: Json, sent: WebAuthn.Challenge, stored: WebAuthn.Credential)
      :   Optional[WebAuthn.Credential] =
        safely(WebAuthn.authenticate(answer, sent, globe, stored))
alternative The caller's decision
  highlight raises..Error
  code
      import errorDiagnostics.stackTracesDiagnostics, providers.javaBaseProvider

      val globe =
        WebAuthn.RelyingParty(t"globe.example", t"The Globe", List(t"https://globe.example"))

      def signIn(answer: Json, sent: WebAuthn.Challenge, stored: WebAuthn.Credential)
      :   WebAuthn.Credential raises WebAuthn.Error =
        WebAuthn.authenticate(answer, sent, globe, stored)
alternative The reason alone
  highlight error.reason  which check failed
  code
      import errorDiagnostics.stackTracesDiagnostics, providers.javaBaseProvider

      val globe =
        WebAuthn.RelyingParty(t"globe.example", t"The Globe", List(t"https://globe.example"))

      def signIn(answer: Json, sent: WebAuthn.Challenge, stored: WebAuthn.Credential)
      :   WebAuthn.Credential | WebAuthn.Error.Reason =
        recover:
          case error: WebAuthn.Error => error.reason
        . protect(WebAuthn.authenticate(answer, sent, globe, stored))
##
```

[Authentication](../../soundness/modules/webauthn.md#authentication) has the checks, and
[choosing a strategy](../../soundness/modules/errors.md#choosing-a-strategy) every way to
handle their failure.
