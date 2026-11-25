# A hash of what a document says

The value hash of a TEL document is taken over its content under a schema, and not over its
bytes. It is an ordinary digest, of the hashing library's type, so it is compared as any digest
is, and two files which differ only in a comment and a blank line agree.

```scala fuel
compile
frame
  label A document, hashed under its schema
  highlight valueHash(schema)  a Digest in Blake3
  code
      import strategies.throwUnsafely, codepages.utf8Codepage

      case class Player(name: Text, age: Int)

      val schema: Tels = Tels.tels[Player](t"player")
      val plain = t"name Viola\nage 20\n".read[Tel].valueHash(schema)
frame
  label The same content, laid out otherwise
  highlight #..Illyria  a comment is not content
  code
      import strategies.throwUnsafely, codepages.utf8Codepage

      case class Player(name: Text, age: Int)

      val schema: Tels = Tels.tels[Player](t"player")
      val plain = t"name Viola\nage 20\n".read[Tel].valueHash(schema)
      val noted = t"# Of Illyria\nname Viola\n\nage 20\n".read[Tel].valueHash(schema)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println((plain == noted).inspect)
```

[BinTEL](../../soundness/modules/tel.md#bintel) says what the hash is for, and
[hashing](../../soundness/modules/hashing.md) describes digests.
