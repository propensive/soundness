# An axis made logarithmic by a given

How data is fitted to an axis is a `Calibration`, found by the type of the values on that
axis. The call which makes the chart does not mention it, so the same call in another scope
gives another chart.

```scala fuel
compile
frame
  label The default fit is linear
  code
      val words = Series(t"words")((10, 1.5), (100, 14.0), (1000, 160.0), (10000, 1900.0))

      val chart = words.chart(Lines())
      val transform = chart.fit.abscissa.transform
frame
  label A calibration in scope
  highlight calibrations.logarithmicCalibration  for every axis of `Int` in this scope
  code
      val words = Series(t"words")((10, 1.5), (100, 14.0), (1000, 160.0), (10000, 1900.0))

      given Int is Calibration = calibrations.logarithmicCalibration
      val chart = words.chart(Lines())
      val transform = chart.fit.abscissa.transform
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(transform.inspect)
```

[Fitting](../../soundness/modules/charts.md#fitting) lists the calibrations, and
[declarative context](../../philosophy/declarative-context.md) says why the choice is not an
argument.
