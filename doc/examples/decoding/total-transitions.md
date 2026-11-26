# A number which may not fit

Every `Byte` is an `Int`, and only some `Int`s are `Byte`s. Narrowing is therefore not a
method which truncates, but a pattern which declines: `As[Byte]` matches the values which
fit, and the code says what happens to the rest.

```scala fuel
compile
frame
  label Narrowing, with the other case stated
  highlight As[Byte](byte)  matches only a value which fits
  code
      def narrow(value: Int): Optional[Byte] = value match
        case As[Byte](byte) => byte
        case _              => Unset
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(narrow(100).inspect)
Out.println(narrow(300).inspect)
```

[Narrowing and widening](../../soundness/modules/decoding.md#narrowing-and-widening) has the
rule, and [total transitions](../../philosophy/total-transitions.md) the reason: 300 never
becomes 44.
