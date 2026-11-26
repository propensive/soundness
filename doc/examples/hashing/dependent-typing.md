# The algorithm is in the digest's type

A digest's type records the algorithm which made it. `digest` takes the algorithm from the
type that is expected, and a digest from another algorithm is a different type.

```scala fuel
compile
frame
  label The field's type chooses SHA-256
  highlight Digest..Sha2[256]  the algorithm, in the type
  code
      import providers.javaBaseProvider, codepages.utf8Codepage

      case class Block(digest: Digest in Sha2[256], payload: Text)

      def seal(payload: Text): Block = Block(payload.digest, payload)
frame
  label A SHA-512 digest is not a SHA-256 digest
  errors show
  code
      import providers.javaBaseProvider, codepages.utf8Codepage

      case class Block(digest: Digest in Sha2[256], payload: Text)

      def seal(payload: Text): Block = Block(payload.digest[Sha2[512]], payload)
##
```

[The algorithm in the type](../../soundness/modules/hashing.md#the-algorithm-in-the-type) says
how keys and signatures follow the same rule.
