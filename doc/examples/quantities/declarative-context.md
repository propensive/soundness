# One temperature, read on the scale in scope

A temperature is created on a named scale, and is the same temperature whatever it is shown
on. The scale it is displayed with is an import, and no conversion is written.

```scala fuel
compile
alternative Celsius
  highlight celsiusScale  degrees Celsius
  code
      import temperatureScales.celsiusScale

      given decimalizer: Decimalizer = Decimalizer(3)

      val fever: Text = Fahrenheit(100).show
alternative Fahrenheit
  highlight fahrenheitScale  degrees Fahrenheit
  code
      import temperatureScales.fahrenheitScale

      given decimalizer: Decimalizer = Decimalizer(3)

      val fever: Text = Fahrenheit(100).show
alternative Kelvin
  highlight kelvinScale  kelvins
  code
      import temperatureScales.kelvinScale

      given decimalizer: Decimalizer = Decimalizer(3)

      val fever: Text = Fahrenheit(100).show
##
```

[Temperature](../../soundness/modules/quantities.md#temperature) shows the readings, and
[declarative context](../../philosophy/declarative-context.md) why the scale is not an
argument.
