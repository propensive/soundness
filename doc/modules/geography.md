## Geography

### About

Angles, positions on the Earth, and the compass are typed values.

An `Angle` is a real angular quantity. It is constructed in degrees or in radians, is added
and scaled with wrapping, and is normalized to a principal value. It is not a bare number
whose unit is a matter of memory.

A `Location` is a latitude and a longitude, packed into a single value. Bearings, angular
distances and [geohashes](https://en.wikipedia.org/wiki/Geohash) are computed from it, and a
bearing is shown as a point of a
[compass rose](https://en.wikipedia.org/wiki/Compass_rose) with 4, 8 or 16 points.

### On angles and positions

The oldest bug in geometric code is the unit of an angle: a `Double` which one function
treats as degrees, and another as radians, with nothing in the type to decide between them.
Angles also wrap, since 370° is 10°, and code which forgets that accumulates rotations which
compare as unequal when they should not. Positions inherit both problems twice over, once
for each coordinate.

Here the angle is a type of its own. It is constructed through its unit, and its arithmetic
wraps, so a bare number never stands for an angle. Angles and locations which are
dimensioned values, not bare doubles, rule out
[impossible states](../philosophy/impossible-states.md): a latitude cannot be added to a
distance.

Everything comes from the `soundness` package. An angle's textual form, which is degrees
with a `°`, is a named given, imported beside it, and the last import lets the samples print
their results:

```scala
import soundness.*
import soundness.angleShowable
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Angles

An angle is written through its unit, by multiplying the constant `Degree`, or with the
extension `.deg`, or with `.rad` for radians. It is read back in either unit:

```scala
val bearing = 45.0.deg
```

```scala fuel
run
##
Out.println(bearing.show)
Out.println(bearing.radians.inspect)
```

Arithmetic wraps, as angles do. Two normal forms are available: `principal`, in
`[0°, 360°)`, and `canonical`, in `[−180°, 180°)`:

```scala fuel
run
##
Out.println(Angle.degrees(375).principal.show)
Out.println(Angle.degrees(355).canonical.show)
```

[SVG](svg.md#transforms) rotates figures by these angles, and
[Colors](colors.md#adjusting) turns hues by them.

### Compass points

An angle resolves to the nearest point of a compass rose. The number of points is chosen as
a type argument:

```scala fuel
run
##
Out.println(Compass[4](Angle.degrees(315)).inspect)
Out.println(Compass[8](Angle.degrees(315)).inspect)
Out.println(Compass[16](Angle.degrees(330)).inspect)
```

The winds are ordinary enumerations, so a bearing can be matched, stored and shown like any
other value:

```tabular
column  Rose
column  Resolves to
column  Bearing given
row  `Compass[4]`  `CardinalWind`: `North`, `East`, `South`, `West`  `compassBearings.fourPointCompassBearing`
row  `Compass[8]`  `CardinalWind` or `IntercardinalWind`, adding `Northeast`, `Southeast`, `Southwest`, `Northwest`  `compassBearings.eightPointCompassBearing`
row  `Compass[16]`  any of those or a `HalfWind`, adding `NorthNortheast`, `EastNortheast` and the other six  `compassBearings.sixteenPointCompassBearing`
```

The number of points is a type argument, not a parameter at runtime, so the *type* of a
resolved bearing says which rose it came from. A `CardinalWind` cannot be passed where a
`HalfWind` of sixteen points is expected, and a match on a bearing of four points is
exhaustive over four cases, not sixteen.

A rose with a different number of points is added by implementing `Directional`, which has a
single method, from an angle to a value. A domain with sectors of its own, such as a
wind-rose with twelve points, or a radar with sixty, therefore uses the same `Compass`
machinery.

### Locations

A `Location` pairs a latitude and a longitude, constructed from angles. It answers
geographic questions: the initial bearing toward another location, on whichever compass is
asked for, the angular distance between two points along the great circle, and the geohash
of the position, to a chosen precision:

```scala
import compassBearings.eightPointCompassBearing

val here = Location(51.5.deg, 0.1.deg)
val there = Location(48.9.deg, 2.4.deg)
```

```scala fuel
run
##
Out.println(here.bearing[Compass[8]](there).inspect)
Out.println(here.surfaceDistance(there).show)
Out.println(here.geohash(6).inspect)
```

The surface distance is itself an `Angle`: the fraction of the great circle which lies
between the points. It multiplies by the radius of a planet to give a
[length](quantities.md), which keeps the geometry separate from the choice of sphere. A
calculation on Earth, and the same calculation on Mars, differ only in the radius by which
they are multiplied.

A `Location` is stored as a packed pair of fixed-point angles, not as two boxed doubles, so
a collection of positions costs one machine word each. A type which has a position, such as
a city, a station or a photograph, supplies a `Locatable` instance, and is then accepted
wherever a location is. The geographic operations therefore apply to values of the domain
directly, without their coordinates being extracted first.

Geohashes are the usual base-32 encoding, and share the property which matters. Two
locations which are close together usually share a prefix, so a query by prefix is a coarse
search by proximity. "Usually" is the honest word. The boundaries of the encoding's cells
mean that two points on either side of one share nothing, which is why a geohash prefix is a
first filter, not an answer.

### Geo URIs

A location travels in text as a `geo:` URI, of
[RFC 5870](https://datatracker.ietf.org/doc/html/rfc5870), with an optional altitude and
uncertainty. A `Geolocation` is decoded from such text, and encoded back. A malformed URI
raises a typed error which names the fault:

```scala
import strategies.throwUnsafely

val place = t"geo:51.5,0.1".as[Geolocation].location
```

```scala fuel
run
errors show
##
t"geo:north".as[Geolocation]
```

### Related

- [Quantities](quantities.md) describes the lengths which a distance is measured in.
- [Network addresses](network-addresses.md) describes the other kinds of URI.
- [Base encoding](base-encoding.md) lists the Geohash alphabet among its Base32 alphabets.
