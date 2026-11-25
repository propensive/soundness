# An answer may be refused

`WebAuthn.register` makes every check the standard requires of a browser's answer, and any of
them may fail. The refusal is a `WebAuthn.Error`, and a method which checks an answer either
handles it or names it in its own type.

```scala fuel
compile
frame
  label The refusal is not accounted for
  errors show
  code
      import errorDiagnostics.stackTracesDiagnostics, providers.javaBaseProvider

      val globe =
        WebAuthn.RelyingParty(t"globe.example", t"The Globe", List(t"https://globe.example"))

      def enrol(answer: Json, challenge: WebAuthn.Challenge): WebAuthn.Credential =
        WebAuthn.register(answer, challenge, globe)
frame
  label The refusal is declared
  highlight raises..Error  what a caller must handle
  code
      import errorDiagnostics.stackTracesDiagnostics, providers.javaBaseProvider

      val globe =
        WebAuthn.RelyingParty(t"globe.example", t"The Globe", List(t"https://globe.example"))

      def enrol(answer: Json, challenge: WebAuthn.Challenge)
      :   WebAuthn.Credential raises WebAuthn.Error =
        WebAuthn.register(answer, challenge, globe)
##
```

[Registration](../../soundness/modules/webauthn.md#registration) describes what is checked, and
[error handling](../../philosophy/error-handling.md) the reasons for declaring it.
