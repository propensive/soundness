# A set of cases in one machine word

A `Set` of an enumeration's cases is an object which points to others. A `Flags` of the same
enumeration is a `Long` and nothing more, with a bit for each case, and it is still set and
tested by name.

```scala fuel
compile
frame
  label A set, on the heap
  highlight Set[Suit]  an object, and the objects it points to
  code
      enum Suit:
        case Clubs, Diamonds, Hearts, Spades

      val red: Set[Suit] = Set(Suit.Diamonds, Suit.Hearts)
      val hearts: Boolean = red.has(Suit.Hearts)
frame
  label The same set, in a word
  highlight Flags[Suit]  a `Long`
    occurrence 1
  code
      enum Suit:
        case Clubs, Diamonds, Hearts, Spades

      val diamonds: Flags[Suit] = Flags[Suit]()(Suit.Diamonds) = true
      val red: Flags[Suit] = diamonds(Suit.Hearts) = true
      val hearts: Boolean = red(Suit.Hearts)
frame
  label The word itself
  highlight bits  the same 64 bits, as a number
  code
      enum Suit:
        case Clubs, Diamonds, Hearts, Spades

      val diamonds: Flags[Suit] = Flags[Suit]()(Suit.Diamonds) = true
      val red: Flags[Suit] = diamonds(Suit.Hearts) = true
      val hearts: Boolean = red(Suit.Hearts)
      val word: B64 = red.bits
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(word.inspect)
Out.println(red.set.show)
```

Diamonds and hearts are the second and third cases, so the second and third bits are on.
[On flag sets](../../soundness/modules/flags.md#on-flag-sets) explains the representation, and
[zero cost](../../philosophy/zero-cost.md) the opaque type which makes it free.
