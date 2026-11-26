# A filter of case classes

An element enters a Bloom filter through its digest, and a case class is digested through its
fields. A filter of a new type therefore needs nothing written for that type.

```scala fuel
compile
frame
  label A filter of text
  code
      import providers.soundnessProvider
      import Blake3.hash

      val seen = BloomFilter[Text](100, 0.001) + t"Exit, pursued by a bear"
frame
  label A filter of a type of its own
  highlight BloomFilter[Exit]  digested through its three fields
  code
      import providers.soundnessProvider
      import Blake3.hash

      case class Exit(act: Int, scene: Int, role: Text)

      val seen = BloomFilter[Exit](100, 0.001) + Exit(3, 3, t"Antigonus")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(seen.hits(Exit(3, 3, t"Antigonus")).inspect)
Out.println(seen.hits(Exit(3, 3, t"Bear")).inspect)
```

[Hashing your own types](../../soundness/modules/hashing.md#hashing-your-own-types) explains
how a case class comes to be digestible.
