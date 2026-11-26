# How the bytes are read is in the field's type

A sized type fixes more than a width. Two bytes declared as `U16` are an unsigned number, and
the same two bytes declared as `S16` are a signed one, so the layout says what the bytes mean
and no call ever has to.

```scala fuel
compile
frame
  label Sixteen bits, unsigned
  highlight U16  two bytes, from 0 to 65535
  code
      import byteOrders.bigEndianByteOrder, strategies.throwUnsafely

      case class Reading(level: U16)

      object Reading:
        given structural: (Reading is Structural) = Structural.derived

      val reading = hex"ffff".unpack[Reading](0)
frame
  label Sixteen bits, signed
  highlight S16  the same two bytes, from -32768 to 32767
  code
      import byteOrders.bigEndianByteOrder, strategies.throwUnsafely

      case class Reading(level: S16)

      object Reading:
        given structural: (Reading is Structural) = Structural.derived

      val reading = hex"ffff".unpack[Reading](0)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(reading.level.inspect)
```

[The typeclass](../../soundness/modules/binary-data.md#the-typeclass) has the table of sized
types.
