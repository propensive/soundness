# A value between two bounds

Comparisons chain as they do in mathematics. A test against a lower and an upper bound is
written as one expression, with the value in the middle, where it is read.

```scala fuel
compile
frame
  label Two comparisons, joined
  code
      val ages = List(13.0, 21.5, 30.0, 80.0)
      val adults = ages.filter { age => age > 18.0 && age < 65.0 }
frame
  label One chained comparison
  highlight 18.0..65.0  read as it is in mathematics
    line 2
  code
      val ages = List(13.0, 21.5, 30.0, 80.0)
      val adults = ages.filter { age => 18.0 < age < 65.0 }
frame
  label With a placeholder
  highlight 18.0..65.0  the value in the middle
    line 2
  code
      val ages = List(13.0, 21.5, 30.0, 80.0)
      val adults = ages.filter(18.0 < _ < 65.0)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(adults.inspect)
```

[Comparisons](../../soundness/modules/numbers.md#comparisons) has `min` and `max` beside them.
