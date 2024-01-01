_Aviation_ provides a variety of types representing and working with different time-related concepts.

### On Time

_Time_ is a complicated concept in several ways. On a physical abstraction, we
can pinpoint an instant in time and order it as happening _before_ or _after_
another instant, provided they happen in the same place. We can consider the
difference between two instants, and measure it by comparison to reference
events that occur with a known regularly. We can relate _time_ to _distance_
using the speed of light, to [partially
order](https://en.wikipedia.org/wiki/Special_relativity) events in different
locations.

In a geological and heliocentric context, we talk about instants and durations
with reference to the rotation of the Earth around the sun, the rotation of
Earth on its axis, and the phase of the moon, as well as with arbitrary
subdivisions based on historical convention and [highly composite
numbers](https://en.wikipedia.org/wiki/Highly_composite_number), and sometimes
religious convention.

The measurements we use may have different meanings in different locations on
Earth (or conversely, the same measurement may be represented differently in
different locations), and different systems for representing the measurements
have been in use at different times in different places. The relationships
between different units of measurement are [not usually
decimal](https://en.wikipedia.org/wiki/12-hour_clock), [not always
constant](https://en.wikipedia.org/wiki/Thirty_Days_Hath_September), not always
exact subdivisions, and [may not even be known in
advance](https://en.wikipedia.org/wiki/Leap_second). The same representation
may refer to distinct events in the same place, and some representations may
not represent valid events on a handful of occasions. The system of
representation may depend on the prevailing political authority at the time,
which can depend on war.

_Aviation_ makes an ambitious effort to rationalize, systematize and encode
as much of this complexity as possible.

### Instants and Durations

We can represent a moment in time, without reference to any other macroscopic
entity (such as the Earth, and units derived from it), as an `Instant`. An
`Instant` is represented, internally, as a 64-bit integer corresponding to the
number of non-leap milliseconds since the UNIX epoch.

The difference between two `Instant`s is represented as a `Duration`, and
represented internally as a 64-bit integral number of milliseconds.

#### International Atomic Time (TAI)

In addition to `Instant`s (which do not distinguish leap seconds from the
second immediately preceding it), `TaiInstant` implements an instant in
International Atomic Time (TAI) which has no discontinuity at each leap second.

Although the concept of an "instant" and, in particular, the "duration" between
any two instants _ought_ to be unambiguous, it is not. This is due to the
existence of leap seconds, each of which increments the difference between the
numbers we use to represent the time in UNIX time (or UTC) and TAI. Since 1972
(when UTC and UNIX time already differed from TAI by ten seconds), twenty-seven
leap seconds have been added, either at the end or the middle of the year.
These were corrections, based on precise observations of the tiny deviations in
the Earth's movement which change its rate of rotation over the previous
months, to try to maintain the invariant that the sun's maximum elevation above
the Greenwich Meridian (Solar Noon) should be within one second of Clock Noon
(i.e. the time at which the clock digits read 12:00:00).


