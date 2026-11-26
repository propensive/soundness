# A multihash cannot name the wrong algorithm

A multihash prefixes a digest with a code for the algorithm which made it. The code is not an
argument: it comes from the digest's type, so the prefix and the bytes cannot disagree.

```scala fuel
compile
frame
  label A digest, which knows its algorithm
  highlight Digest..Sha2[256]  the algorithm is in the type
  code
      import providers.javaBaseProvider, codepages.utf8Codepage, alphabets.hexLowerCase

      val digest: Digest in Sha2[256] = t"Hello world".digest
frame
  label A multihash of it
  highlight Multihash(digest)  the code comes from that type
  code
      import providers.javaBaseProvider, codepages.utf8Codepage, alphabets.hexLowerCase

      val digest: Digest in Sha2[256] = t"Hello world".digest
      val multihash = Multihash(digest)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(digest.serialize[Hex])
Out.println(multihash.serialize.serialize[Hex])
```

The two bytes before the digest, `12` and `20`, say that thirty-two bytes of SHA-256 follow.
[Self-describing digests](../../soundness/modules/hashing.md#self-describing-digests) has more.
