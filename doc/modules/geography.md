## Geography

### About

Angles, positions on the Earth, and the compass are typed values. An `Angle` is a genuine angular
quantity — constructed in degrees or radians, added and scaled modularly, normalised to a principal
value — rather than a bare number whose unit is a matter of memory. A `Location` is a latitude and
longitude packed into a single value, from which bearings, angular distances and
[geohashes](https://en.wikipedia.org/wiki/Geohash) are computed, and a bearing renders as a compass
point on a 4-, 8- or 16-point [compass rose](https://en.wikipedia.org/wiki/Compass_rose).

### On angles and positions

The oldest bug in geometry code is the unit of an angle: a `Double` that one function treats as
degrees and another as radians, with nothing in the type to arbitrate. Angles also wrap — 370° is
10° — and code that forgets the modulus accumulates rotations that compare unequal when they should
not. Positions inherit both problems twice over, once per coordinate.

Soundness makes the angle a type of its own, constructed through its unit and closed under modular
arithmetic, so a bare number never stands for an angle and wrapping is built into the operations.
Everything comes from the `soundness` package:

```scala
import soundness.*
```

### Angles

An angle is written through its unit — multiplying the `Degree` constant, the `.deg` extension, or
`.rad` for radians — and reads back in either:

```scala
val bearing = 45.0.deg
bearing.show      // t"45.0°"
bearing.radians   // 0.785…
```

Arithmetic wraps as angles do, and two normal forms are available: `principal` in `[0°, 360°)` and
`canonical` in `[−180°, 180°)`:

```scala
Angle.degrees(375).principal.show   // t"15.0°"
Angle.degrees(355).canonical.show   // t"-5.0°"
```

### Compass points

An angle resolves to the nearest point of a compass rose with the number of points chosen as a type
argument — the four cardinal winds, eight with the intercardinals, or all sixteen:

```scala
Compass[8](Angle.degrees(315))   // Northwest
```

The winds are ordinary enumerations — `CardinalWind`, `IntercardinalWind`, `HalfWind` — so a bearing
can be matched, stored and shown like any other value.

The number of points is a type argument rather than a runtime parameter, so the *type* of a
resolved bearing says which rose it came from: a `CardinalWind` cannot be passed where a
sixteen-point `HalfWind` is expected, and a match on a four-point bearing is exhaustive over four
cases rather than sixteen.

A rose with a different number of points is added by implementing `Directional`, which is a single
method from an angle to a value, so a domain with its own sectors — a wind-rose with twelve points,
a radar with sixty — uses the same `Compass` machinery.

### Locations

A `Location` pairs a latitude and longitude, constructed from angles, and answers geographic
questions: the initial bearing toward another location, on whichever compass is asked for; the
angular distance between two points along the great circle; and the geohash of the position to a
chosen precision:

```scala
val here = Location(51.5.deg, 0.1.deg)
val there = Location(48.9.deg, 2.4.deg)

here.bearing[Compass[8]](there)   // a compass point
here.surfaceDistance(there)       // the angular separation
here.geohash(6)                   // a six-character geohash
```

The surface distance is itself an `Angle` — the fraction of the great circle between the points —
which multiplies by a planet's radius to give a length, keeping the geometry separate from the
choice of sphere. A calculation on Earth and the same calculation on Mars differ only in the
radius they are multiplied by.

A `Location` is stored as a packed pair of fixed-point angles rather than two boxed doubles, so a
collection of positions costs one machine word each. A type that has a position — a city, a
station, a photograph — supplies a `Locatable` instance and is then accepted wherever a location
is, so the geographic operations apply to domain values directly rather than requiring their
coordinates to be extracted first.

Geohashes are the usual base-32 encoding, and share the property that matters: two locations close
together usually share a prefix, so a prefix query is a coarse proximity search. "Usually" is the
honest word — the encoding's cell boundaries mean two points either side of one share nothing —
which is why a geohash prefix is a first filter rather than an answer.

### Geo URIs

A location travels in text as an [RFC 5870](https://datatracker.ietf.org/doc/html/rfc5870) `geo:`
URI, carrying optional altitude and uncertainty. A `Geolocation` decodes from such text — with a
typed error naming the fault when the URI is malformed — and encodes back:

```scala
import strategies.throwUnsafely

t"geo:51.5,0.1".as[Geolocation].location
```
