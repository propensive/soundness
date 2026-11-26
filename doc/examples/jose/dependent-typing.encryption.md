# Two choices, both made by types

Encrypting a token means choosing how its content is encrypted, and how the content's key
reaches the recipient. The first is the type argument of `Jwe`, and the second follows from
the type of the recipient's key. Both end up in the header.

```scala fuel
compile
frame
  label Encrypted to a recipient
  highlight AesGcm[256]  how the content is encrypted
  highlight Ecdh[256]  how its key is agreed
  code
      import cloaks.heapCloak, providers.javaBaseProvider, strategies.throwUnsafely
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer
      import errorDiagnostics.stackTracesDiagnostics, soundness.textDecodable

      val recipient = PrivateKey.generate[Ecdh[256]]()

      val message: Jwe = Jwe[AesGcm[256]](t"Speak the speech, I pray you", recipient.public)
frame
  label Decrypted with the same cipher
  highlight decrypt[AesGcm[256]]
  code
      import cloaks.heapCloak, providers.javaBaseProvider, strategies.throwUnsafely
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer
      import errorDiagnostics.stackTracesDiagnostics, soundness.textDecodable

      val recipient = PrivateKey.generate[Ecdh[256]]()

      val message: Jwe = Jwe[AesGcm[256]](t"Speak the speech, I pray you", recipient.public)
      val opened: Text = message.in[Text].as[Jwe].decrypt[AesGcm[256]](recipient).read[Text]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(message.header.encryption.or(t"none"))
Out.println(message.header.algorithm.or(t"none"))
Out.println(opened)
```

[Encrypting](../../soundness/modules/jose.md#encrypting) has the tables of content ciphers
and recipients' keys.
