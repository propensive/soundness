# Exporting a private key says so

A public key is written out freely. A private key is exported only when the call names
`Divulgence`, so every place where secret material leaves the program can be found by its
name.

```scala fuel
compile
frame
  label A public key, as PEM
  code
      import cloaks.heapCloak, providers.javaBaseProvider

      val key = PrivateKey.generate[Ecdsa[256]]()
      val shared: Pem = key.public.pem
frame
  label A private key, in the same way
  errors show
  code
      import cloaks.heapCloak, providers.javaBaseProvider

      val key = PrivateKey.generate[Ecdsa[256]]()
      val secret: Pem = key.pem
frame
  label A private key, deliberately
  highlight Divulgence  secret material leaves here
  code
      import cloaks.heapCloak, providers.javaBaseProvider

      val key = PrivateKey.generate[Ecdsa[256]]()
      val secret: Pem = key.pem(Divulgence)
##
```

[PEM](../../soundness/modules/cryptography.md#pem) describes the armored text.
