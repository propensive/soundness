# A challenge needs a source of randomness

A challenge is 32 random bytes, and random bytes come from a provider of cryptography. Making a
challenge asks for one in its signature, so a challenge cannot be made where none was chosen.

```scala fuel
compile
frame
  label No provider is in scope
  errors show
  code
      val challenge = WebAuthn.Challenge()
frame
  label The provider of the JDK
  highlight javaBaseProvider  where the random bytes come from
  code
      import providers.javaBaseProvider

      val challenge = WebAuthn.Challenge()
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(challenge.bytes.length.show)
```

[Registration](../../soundness/modules/webauthn.md#registration) begins with a challenge, and
[honest signatures](../../philosophy/honest-signatures.md) explains why it asks.
