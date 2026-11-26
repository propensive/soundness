# A key read as the cipher its type names

A key file says which algorithm it holds, and the type it is read as says which was expected.
The two are compared, so an Ed25519 key is never read as an RSA key, and a key which was read
has the operations of its cipher and no others.

```scala fuel
compile
frame
  label A key file, read as an Ed25519 key
  code
      import cloaks.heapCloak, providers.javaBaseProvider, strategies.throwUnsafely
      import codepages.utf8Codepage, errorDiagnostics.stackTracesDiagnostics

      def load(pem: Text) = pem.read[PrivateKey[Ed25519] in Pem]
frame
  label Signing with it
  highlight PrivateKey[Ed25519]..Pem  the cipher expected, and the form
  code
      import cloaks.heapCloak, providers.javaBaseProvider, strategies.throwUnsafely
      import codepages.utf8Codepage, errorDiagnostics.stackTracesDiagnostics

      def load(pem: Text) = pem.read[PrivateKey[Ed25519] in Pem]

      def sign(pem: Text, message: Text): Signature[Ed25519] = load(pem).sign(message)
frame
  label Its signature is not an RSA signature
  errors show
  code
      import cloaks.heapCloak, providers.javaBaseProvider, strategies.throwUnsafely
      import codepages.utf8Codepage, errorDiagnostics.stackTracesDiagnostics

      def load(pem: Text) = pem.read[PrivateKey[Ed25519] in Pem]

      def sign(pem: Text, message: Text): Signature[Rsa[2048]] = load(pem).sign(message)
##
```

[Reading keys](../../soundness/modules/cryptography.md#reading-keys) lists the forms a key
file takes, and what a mismatch reports.
