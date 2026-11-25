# A sealed message which was tampered with

An authenticated cipher refuses to open a message whose bytes, or whose header, have changed.
That refusal is an error in the type of `unseal`, and a method which calls it does not compile
until it says what a refusal means.

```scala fuel
compile
frame
  label The refusal is not accounted for
  errors show
  code
      import providers.javaBaseProvider, errorDiagnostics.stackTracesDiagnostics

      def open(key: SymmetricKey[AesGcm[256]], nonce: Data, box: Data): Data =
        key.unseal(nonce, Data(), box)
frame
  label It is declared
  highlight raises..Crypto.Error  the caller decides
  code
      import providers.javaBaseProvider, errorDiagnostics.stackTracesDiagnostics

      def open(key: SymmetricKey[AesGcm[256]], nonce: Data, box: Data): Data raises Crypto.Error =
        key.unseal(nonce, Data(), box)
frame
  label Or a forgery is an absent message
  highlight safely  `Unset` if it was tampered with
  code
      import providers.javaBaseProvider, errorDiagnostics.stackTracesDiagnostics

      def open(key: SymmetricKey[AesGcm[256]], nonce: Data, box: Data): Optional[Data] =
        safely(key.unseal(nonce, Data(), box))
##
```

[Key agreement and authenticated encryption](../../soundness/modules/cryptography.md#key-agreement-and-authenticated-encryption)
seals and opens a message.
