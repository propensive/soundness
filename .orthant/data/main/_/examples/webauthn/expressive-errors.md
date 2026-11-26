# A refusal which says which check failed

A `WebAuthn.Error` holds its reason as a value of an enumeration, one case for each check the
standard requires, and its message is a sentence made from it.

```scala fuel
compile
frame
  label An answer which is not one
  code
      import errorDiagnostics.stackTracesDiagnostics, strategies.throwUnsafely

      val answer: Json = j"""{"type": "public-key"}"""

      val refusal: WebAuthn.Error = capture[WebAuthn.Error](WebAuthn.identifier(answer))
frame
  label The reason, as a value
  highlight refusal.reason  a case of WebAuthn.Error.Reason
  code
      import errorDiagnostics.stackTracesDiagnostics, strategies.throwUnsafely

      val answer: Json = j"""{"type": "public-key"}"""

      val refusal: WebAuthn.Error = capture[WebAuthn.Error](WebAuthn.identifier(answer))

      val malformed: Boolean = refusal.reason match
        case WebAuthn.Error.Reason.Malformed(detail) => true
        case _                                       => false
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(refusal.message.show)
Out.println(malformed.inspect)
```

[Authentication](../../soundness/modules/webauthn.md#authentication) lists what is checked, and
[expressive errors](../../philosophy/expressive-errors.md) says why an error is more than text.
