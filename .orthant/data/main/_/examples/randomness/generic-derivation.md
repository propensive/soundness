# A random value of a new type

`arbitrary` gives a random value of any type which is `Randomizable`. An enumeration picks one
of its cases and a case class generates each of its fields, so a type made of randomizable
parts needs no generator written for it.

```scala fuel
compile
frame
  label A random number
  code
      import randomization.seededRandomization

      given seed: Seed = Seed(1L)

      val card = stochastic(arbitrary[Int]())
frame
  label A random case of an enumeration
  highlight arbitrary[Suit]  one of the four cases
  code
      import randomization.seededRandomization, strategies.throwUnsafely

      given seed: Seed = Seed(1L)

      enum Suit:
        case Clubs, Diamonds, Hearts, Spades

      val card = stochastic(arbitrary[Suit]())
frame
  label A random value of a case class
  highlight arbitrary[Card]  a random suit and a random mark
  code
      import randomization.seededRandomization, strategies.throwUnsafely

      given seed: Seed = Seed(1L)

      enum Suit:
        case Clubs, Diamonds, Hearts, Spades

      case class Card(suit: Suit, marked: Boolean)

      val card = stochastic(arbitrary[Card]())
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(card.inspect)
```

[Collections and custom types](../../soundness/modules/randomness.md#collections-and-custom-types)
shows how to write an instance by hand, where each field at random is not what is wanted.
