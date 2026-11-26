# A type which says how it is shown

A `Showable` instance is a function from a value to its text, and its type reads as a
statement about the type it serves: a `Purse` is showable. Once that is given, interpolation
and `show` both use it.

```scala fuel
compile
frame
  label A purse cannot be put into text
  errors show
  code
      case class Purse(pence: Int)

      val total: Text = t"Shylock is owed ${Purse(1099)}"
frame
  label A purse which is showable
  highlight Purse..Showable  the type of the instance
    line 3
  code
      case class Purse(pence: Int)

      given (Purse is Showable) = purse => t"£${purse.pence/100}.${purse.pence%100}"

      val total: Text = t"Shylock is owed ${Purse(1099)}"
frame
  label The same instance, called by name
  highlight show  uses the same instance
  code
      case class Purse(pence: Int)

      given (Purse is Showable) = purse => t"£${purse.pence/100}.${purse.pence%100}"

      val total: Text = t"Shylock is owed ${Purse(1099)}"
      val debt: Text = Purse(1099).show
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(total)
```

[Writing a Showable](../../soundness/modules/display.md#writing-a-showable) has the details, and
[infix types](../../philosophy/infix-types.md) explains the word `is`.
