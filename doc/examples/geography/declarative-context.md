# Where a bearing is measured from

A bearing is an angle from some direction. A navigator measures it clockwise from north, and
a mathematician from the axis which points east. The convention is chosen by an import, and
the code which takes the bearing does not change.

```scala fuel
compile
alternative From north
  highlight fromNorthBearing  clockwise from north
  code
      import compassBearings.fromNorthBearing

      val elsinore = Location(56.04.deg, 12.62.deg)
      val verona = Location(45.44.deg, 10.99.deg)

      val course = elsinore.bearing[Angle](verona)
alternative From east
  highlight fromEastBearing  from the axis which points east
  code
      import compassBearings.fromEastBearing

      val elsinore = Location(56.04.deg, 12.62.deg)
      val verona = Location(45.44.deg, 10.99.deg)

      val course = elsinore.bearing[Angle](verona)
alternative As a point of the compass
  highlight Compass[16]  one of sixteen winds
  code
      import compassBearings.sixteenPointCompassBearing

      val elsinore = Location(56.04.deg, 12.62.deg)
      val verona = Location(45.44.deg, 10.99.deg)

      val course = elsinore.bearing[Compass[16]](verona)
##
```

[Locations](../../soundness/modules/geography.md#locations) has the other questions a
location answers, and [declarative context](../../philosophy/declarative-context.md) the
reasons for choosing by import.
