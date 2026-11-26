# An absent value, seen in debugging output

An `Optional[Int]` which holds five is the number five, with no wrapper, so printing it
naively could not tell it from an `Int`. `inspect` marks the difference: a present value is
in corner brackets, and an absent one is a circle.

```scala fuel
compile
frame
  label Three values which look alike
  code
      val acts: Int = 5
      val scenes: Optional[Int] = 5
      val interval: Optional[Int] = Unset
frame
  label Inspected, they are distinct
  highlight inspect  for a programmer
    line 5
  code
      val acts: Int = 5
      val scenes: Optional[Int] = 5
      val interval: Optional[Int] = Unset

      val forms: List[Text] = List(acts.inspect, scenes.inspect, interval.inspect)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(forms.show)
```

[The fallback chain](../../soundness/modules/display.md#the-fallback-chain) lists the brackets
which `inspect` uses, and [optionality](../../philosophy/optionality.md) explains why there is
no wrapper to print.
