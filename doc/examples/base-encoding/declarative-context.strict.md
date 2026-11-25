# How strict a decoder is, by import

Hexadecimal is written in either case, and a decoder may accept both or insist on its own.
That is a property of the alphabet in scope: `hexLowerCase` reads either, and
`hexStrictLowerCase` reads only what it would write.

```scala fuel
compile
frame
  label A tolerant alphabet
  highlight hexLowerCase  writes lower case, reads either
  code
      val tolerant: Optional[Data] =
        import alphabets.hexLowerCase
        safely(t"CAFE".deserialize[Hex])
frame
  label A strict one
  highlight hexStrictLowerCase  reads only lower case
  code
      val tolerant: Optional[Data] =
        import alphabets.hexLowerCase
        safely(t"CAFE".deserialize[Hex])

      val strict: Optional[Data] =
        import alphabets.hexStrictLowerCase
        safely(t"CAFE".deserialize[Hex])
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(tolerant.let(_.length).inspect)
Out.println(strict.let(_.length).inspect)
```

[Choosing an alphabet](../../soundness/modules/base-encoding.md#choosing-an-alphabet) lists
what each alphabet also accepts.
