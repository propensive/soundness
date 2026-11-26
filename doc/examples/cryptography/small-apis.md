# One way to sign, whatever the algorithm

Elliptic curves, RSA and a lattice scheme which resists quantum computers have nothing in
common but their purpose. The key's type names the algorithm, and `sign` and `verify` are the
same for all of them.

```scala fuel
compile
alternative ECDSA
  highlight Ecdsa[256]  an elliptic curve
  code
      import cloaks.heapCloak, providers.javaBaseProvider, codepages.utf8Codepage
      import signatureDigests.sha256Signature

      val will = t"I give unto my wife my second best bed"

      val signer = PrivateKey.generate[Ecdsa[256]]()
      val signature = signer.sign(will)
      val genuine: Boolean = signer.public.verify(will, signature)
alternative RSA-PSS
  highlight RsaPss[2048]  RSA, with randomized padding
  code
      import cloaks.heapCloak, providers.javaBaseProvider, codepages.utf8Codepage
      import signatureDigests.sha256Signature

      val will = t"I give unto my wife my second best bed"

      val signer = PrivateKey.generate[RsaPss[2048]]()
      val signature = signer.sign(will)
      val genuine: Boolean = signer.public.verify(will, signature)
alternative ML-DSA
  highlight MlDsa[65]  a lattice scheme, FIPS 204
  code
      import cloaks.heapCloak, providers.javaBaseProvider, codepages.utf8Codepage
      import signatureDigests.sha256Signature

      val will = t"I give unto my wife my second best bed"

      val signer = PrivateKey.generate[MlDsa[65]]()
      val signature = signer.sign(will)
      val genuine: Boolean = signer.public.verify(will, signature)
##
```

[Signing](../../soundness/modules/cryptography.md#signing) describes each scheme.
