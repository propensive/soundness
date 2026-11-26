# The digest of a case class

A case class is digestible because its fields are. Nothing is written for the type, and a
change to any field, however deep, changes the digest.

```scala fuel
compile
frame
  label A value of two fields
  code
      import providers.javaBaseProvider, codepages.utf8Codepage, alphabets.hexLowerCase

      case class Role(name: Text, lines: Int)

      val fool = Role(t"Feste", 318).digest[Sha2[256]]
frame
  label A value which contains others
  highlight roles:..List[Role]  a list of case classes
  code
      import providers.javaBaseProvider, codepages.utf8Codepage, alphabets.hexLowerCase

      case class Role(name: Text, lines: Int)
      case class Play(title: Text, roles: List[Role])

      val before = Play(t"Twelfth Night", List(Role(t"Feste", 318))).digest[Sha2[256]]
      val after = Play(t"Twelfth Night", List(Role(t"Feste", 319))).digest[Sha2[256]]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(before.serialize[Hex])
Out.println(after.serialize[Hex])
```

[Hashing your own types](../../soundness/modules/hashing.md#hashing-your-own-types) describes
the derivation.
