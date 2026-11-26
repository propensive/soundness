# Two temperatures do not add

Thirty degrees plus thirty degrees is not sixty degrees of anything. A reading on a scale with
an arbitrary zero is a `Temperature`, which is not a quantity, and only a difference in kelvins
can be added to it.

```scala fuel
compile
frame
  label A reading added to a reading
  errors show
  code
      val noon = Celsius(30) + Celsius(30)
frame
  label A reading, and a rise
  highlight 5*Kelvin  a difference, not a reading
  code
      val noon = Celsius(30) + 5*Kelvin
frame
  label The difference of two readings is a rise
  highlight noon..Celsius(30)  a quantity of kelvins
    line 2
  code
      val noon = Celsius(30) + 5*Kelvin
      val rise = noon - Celsius(30)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
import temperatureScales.celsiusScale

given decimalizer: Decimalizer = Decimalizer(3)

Out.println(noon.show)
Out.println(rise.show)
```

[Temperature](../../soundness/modules/quantities.md#temperature) describes the scales.
