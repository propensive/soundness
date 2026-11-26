# A shared secret which exists for one block

Two parties who hold each other's public keys agree on a secret. It is never returned. It is
lent to a block, which hashes it into whatever the protocol needs, and it is zeroed when the
block ends.

```scala fuel
compile
frame
  label Two key pairs
  code
      import cloaks.heapCloak, providers.javaBaseProvider

      val romeo = PrivateKey.generate[X25519]()
      val juliet = PrivateKey.generate[X25519]()
frame
  label The secret, as Romeo computes it
  highlight secret..=>  lent, and then zeroed
  code
      import cloaks.heapCloak, providers.javaBaseProvider

      val romeo = PrivateKey.generate[X25519]()
      val juliet = PrivateKey.generate[X25519]()

      val his = romeo.agree(juliet.public) { secret => secret.digest[Sha2[256]] }
frame
  label And as Juliet does
  highlight juliet.agree(romeo.public)  her key, his public key
  code
      import cloaks.heapCloak, providers.javaBaseProvider

      val romeo = PrivateKey.generate[X25519]()
      val juliet = PrivateKey.generate[X25519]()

      val his = romeo.agree(juliet.public) { secret => secret.digest[Sha2[256]] }
      val hers = juliet.agree(romeo.public) { secret => secret.digest[Sha2[256]] }
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println((his == hers).inspect)
```

[Key agreement and authenticated encryption](../../soundness/modules/cryptography.md#key-agreement-and-authenticated-encryption)
uses the result with an authenticated cipher.
