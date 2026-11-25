# A meter has no default design

A battery, a thermometer and a dial are not interchangeable: they differ in height and in what
they say. A `Meter` is therefore not drawn until a design has been imported, and the choice is
visible at the top of the file.

```scala fuel
compile
frame
  label A reading, and nothing to draw it with
  errors show
  code
      import textMetrics.uniformMetric, palettes.solarizedDarkGaugePalette
      import Tui.Meter

      val charge = gaugeLine(Meter(0.8), 12)
frame
  label As a battery
  highlight meters.batteryMeter
  code
      import textMetrics.uniformMetric, palettes.solarizedDarkGaugePalette
      import Tui.Meter
      import meters.batteryMeter

      val charge = gaugeLine(Meter(0.8), 12)
frame
  label As a thermometer
  highlight meters.thermometerMeter
  code
      import textMetrics.uniformMetric, palettes.solarizedDarkGaugePalette
      import Tui.Meter
      import meters.thermometerMeter

      val charge = gaugeLine(Meter(0.8), 12)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xtermTrueColorTermcap

Out.println(charge)
```

[Statuses](../../soundness/modules/gauges.md#statuses) explains which statuses have a default,
and why four do not.
