# Debugging output which nothing blocks

`show` is for a person, and is refused to a type which has not said how it should look.
`inspect` is for a programmer, and is defined for every value: it falls back through a
type's other forms until one applies.

```scala fuel
compile
frame
  label A type with no form for a reader
  errors show
  code
      case class Purse(pence: Int)

      val shown: Text = Purse(1099).show
frame
  label The same value, inspected
  highlight inspect  always defined
    occurrence 2
  code
      case class Purse(pence: Int)

      val inspected: Text = Purse(1099).inspect
frame
  label A form for a reader, which inspect borrows
  highlight Purse..Showable  borrowed by inspect
    line 3
  code
      case class Purse(pence: Int)

      given (Purse is Showable) = purse => t"£${purse.pence/100}.${purse.pence%100}"

      val inspected: Text = Purse(1099).inspect
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(inspected)
```

The brackets mark a form which was borrowed from `Showable`.
[The fallback chain](../../soundness/modules/display.md#the-fallback-chain) lists what is tried,
and in which order.
