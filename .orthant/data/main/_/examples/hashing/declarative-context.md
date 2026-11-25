# A broken algorithm needs a permit

MD5 is kept for compatibility, and is not safe for anything else. It does not compile until an
import permits it, so a program's acceptance of weak cryptography is one line which can be
searched for.

```scala fuel
compile
frame
  label MD5, with nothing to permit it
  errors show
  code
      import providers.javaBaseProvider, codepages.utf8Codepage

      val fingerprint = t"Hello world".digest[Md5]
frame
  label The permit
  highlight permitDisallowedCrypto  a deliberate, searchable concession
  code
      import providers.javaBaseProvider, codepages.utf8Codepage
      import cryptoPermits.permitDisallowedCrypto

      val fingerprint = t"Hello world".digest[Md5]
frame
  label A strong algorithm needs none
  highlight Sha2[256]  no permit needed
  code
      import providers.javaBaseProvider, codepages.utf8Codepage

      val fingerprint = t"Hello world".digest[Sha2[256]]
##
```

[Weak algorithms](../../soundness/modules/hashing.md#weak-algorithms) names the permit which
each algorithm needs.
