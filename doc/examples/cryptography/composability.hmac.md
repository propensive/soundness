# A message authenticated, and written in hexadecimal

An HMAC is computed over any of the hash algorithms, which are the types the hashing library
defines, and its result is written by the base encodings. Three libraries meet in one line.

```scala fuel
compile
frame
  label A message and a shared secret
  code
      import providers.javaBaseProvider, codepages.utf8Codepage, alphabets.hexLowerCase

      val order = t"Let the doors be shut upon him"
      val secret = t"wittenberg".in[Data]
frame
  label Authenticated with SHA-256
  highlight hmac[Sha2[256]](secret)  the hash is a type
  code
      import providers.javaBaseProvider, codepages.utf8Codepage, alphabets.hexLowerCase

      val order = t"Let the doors be shut upon him"
      val secret = t"wittenberg".in[Data]
      val tag = order.hmac[Sha2[256]](secret)
frame
  label And written out
  highlight serialize[Hex]  the alphabet is an import
  code
      import providers.javaBaseProvider, codepages.utf8Codepage, alphabets.hexLowerCase

      val order = t"Let the doors be shut upon him"
      val secret = t"wittenberg".in[Data]
      val tag = order.hmac[Sha2[256]](secret)
      val header: Text = tag.serialize[Hex]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(header)
```

[HMAC](../../soundness/modules/cryptography.md#hmac) describes the operation, and
[hashing](../../soundness/modules/hashing.md#hashing-a-value) the algorithms.
