# Bits are not a number

A `B32` is thirty-two bits, and an `S32` is a number in thirty-two bits. The bit pattern has
shifts and masks and no arithmetic, so a shift is never written as a sum by mistake, nor a sum
as a shift.

```scala fuel
compile
frame
  label Adding two bit patterns
  errors show
  code
      val mask: B32 = 0xf0.bits
      val wider = mask + mask
frame
  label Shifting one
  highlight <<  defined for bits, as + is not
  code
      val mask: B32 = 0xf0.bits
      val wider = mask << 1
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(wider.hex)
```

The pattern is printed at its full width, leading zeros included.
[Bit manipulation](../../soundness/modules/numbers.md#bit-manipulation) lists the operations.
