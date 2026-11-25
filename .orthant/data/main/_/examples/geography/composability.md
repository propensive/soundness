# One journey, on two planets

The distance between two locations is an `Angle`: the part of a great circle which lies
between them. Multiplied by a radius, which is a quantity, it is a length, in the units of the
radius.

```scala fuel
compile
frame
  label The angle between two places
  code
      val elsinore = Location(56.04.deg, 12.62.deg)
      val verona = Location(45.44.deg, 10.99.deg)

      val arc: Angle = elsinore.surfaceDistance(verona)
frame
  label A length on Earth
  highlight 6371.0*Kilo(Metre)  the radius of the Earth
  code
      val elsinore = Location(56.04.deg, 12.62.deg)
      val verona = Location(45.44.deg, 10.99.deg)

      val arc: Angle = elsinore.surfaceDistance(verona)
      val journey = arc.radians*6371.0*Kilo(Metre)
frame
  label The same arc on Mars
  highlight 3390.0*Kilo(Metre)  the radius of Mars
  code
      val elsinore = Location(56.04.deg, 12.62.deg)
      val verona = Location(45.44.deg, 10.99.deg)

      val arc: Angle = elsinore.surfaceDistance(verona)
      val journey = arc.radians*6371.0*Kilo(Metre)
      val martian = arc.radians*3390.0*Kilo(Metre)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

given decimalizer: Decimalizer = Decimalizer(3)

Out.println(journey.show)
Out.println(martian.show)
```

[Locations](../../soundness/modules/geography.md#locations) explains why the geometry is kept
apart from the sphere, and [Quantities](../../soundness/modules/quantities.md) has the units.
