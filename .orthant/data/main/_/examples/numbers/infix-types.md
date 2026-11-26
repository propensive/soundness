# Addition, declared as a sentence

`+` is not a method of any type. It is found through the `Addable` typeclass, whose instance
names the left operand, the right operand and the result, and reads as a sentence.

```scala fuel
compile
frame
  label A type with no addition
  errors show
  code
      case class Purse(ducats: Int)

      val dowry = Purse(3000) + Purse(6000)
frame
  label A purse is addable by a purse to a purse
  highlight is..to  the operands, and then the result
    line 3
  code
      case class Purse(ducats: Int)

      given Purse is Addable by Purse to Purse =
        Addable { (left, right) => Purse(left.ducats + right.ducats) }

      val dowry = Purse(3000) + Purse(6000)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(dowry.ducats.show)
```

[Overloading the operators](../../soundness/modules/numbers.md#overloading-the-operators) has
the other typeclasses, and [infix types](../../philosophy/infix-types.md) the words they share.
