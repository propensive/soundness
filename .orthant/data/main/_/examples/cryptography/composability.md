# A certificate valid between two dates

A certificate is valid for a period, and the period is the one the time library defines: two
instants joined by `~`. The key comes from the same generator as every other, and the result
is exported as PEM.

```scala fuel
compile
frame
  label A subject and a key
  code
      import cloaks.heapCloak, providers.javaBaseProvider, strategies.throwUnsafely
      import signatureDigests.sha256Signature, chronometries.unixChronometry
      import errorDiagnostics.stackTracesDiagnostics

      val subject = Distinguished(commonName = t"globe.example.com")
      val key = PrivateKey.generate[Ecdsa[256]]()
frame
  label A period of validity
  highlight ~  a period between two instants
  code
      import cloaks.heapCloak, providers.javaBaseProvider, strategies.throwUnsafely
      import signatureDigests.sha256Signature, chronometries.unixChronometry
      import errorDiagnostics.stackTracesDiagnostics

      val subject = Distinguished(commonName = t"globe.example.com")
      val key = PrivateKey.generate[Ecdsa[256]]()
      val validity = Instant(1_700_000_000_000L) ~ Instant(1_900_000_000_000L)
frame
  label A self-signed certificate
  highlight Certificate.selfSigned  signed with its own key
  code
      import cloaks.heapCloak, providers.javaBaseProvider, strategies.throwUnsafely
      import signatureDigests.sha256Signature, chronometries.unixChronometry
      import errorDiagnostics.stackTracesDiagnostics

      val subject = Distinguished(commonName = t"globe.example.com")
      val key = PrivateKey.generate[Ecdsa[256]]()
      val validity = Instant(1_700_000_000_000L) ~ Instant(1_900_000_000_000L)
      val certificate = Certificate.selfSigned(subject, key, validity, BigInt(1))
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(certificate.verify(key.public).inspect)
Out.println(certificate.subject.let(_.commonName).or(t"nobody"))
```

[Certificates](../../soundness/modules/cryptography.md#certificates) issues a certificate from
an authority, and validates a chain.
