# A key does only what its cipher can

A key is typed by its cipher, and the cipher decides which operations exist. An X25519 key
agrees on a shared secret, and cannot sign. A signature made with one curve cannot be checked
as though it were made with another.

```scala fuel
compile
frame
  label A key for agreement, asked to sign
  errors show
  code
      import cloaks.heapCloak, providers.javaBaseProvider, codepages.utf8Codepage
      import signatureDigests.sha256Signature

      val romeo = PrivateKey.generate[X25519]()
      val vow = romeo.sign(t"My bounty is as boundless as the sea")
frame
  label A key for signing
  highlight Ecdsa[256]  a cipher which can sign
  code
      import cloaks.heapCloak, providers.javaBaseProvider, codepages.utf8Codepage
      import signatureDigests.sha256Signature

      val romeo = PrivateKey.generate[Ecdsa[256]]()
      val vow = romeo.sign(t"My bounty is as boundless as the sea")
frame
  label Its signature, checked with a key of another size
  errors show
  code
      import cloaks.heapCloak, providers.javaBaseProvider, codepages.utf8Codepage
      import signatureDigests.sha256Signature

      val romeo = PrivateKey.generate[Ecdsa[256]]()
      val paris = PrivateKey.generate[Ecdsa[384]]()
      val vow = romeo.sign(t"My bounty is as boundless as the sea")
      val forged = paris.public.verify(t"My bounty is as boundless as the sea", vow)
##
```

[Weak algorithms](../../soundness/modules/cryptography.md#weak-algorithms) has the table of
what each algorithm supports.
