## Time

### About

Soundness represents and manipulates time: dates on a calendar, times on a
clock, absolute instants on a physical timeline, the spans between them, the time
zones that connect human and physical time, and the rules by which events recur.
Wherever a value can be known when the code is written, Soundness checks it as
the code compiles, so a date that cannot exist, or a time zone that was never
defined, is a compile error rather than a runtime surprise.

### On time

Time looks simple and is not. A physical instant — the striking of a bell, the
arrival of a photon — can be ordered before or after another and measured against
a regular reference, and that much is unambiguous. Everything humans build on top
of it is not. Calendars divide the year into months of unequal length, weeks that
ignore months entirely, and days that are themselves divided in
[non-decimal](https://en.wikipedia.org/wiki/12-hour_clock) and
[historically negotiable](https://en.wikipedia.org/wiki/Thirty_Days_Hath_September)
ways.

The relationship between physical and civil time is worse still. The length of a
day drifts as the Earth's rotation changes, so civil time is corrected with
[leap seconds](https://en.wikipedia.org/wiki/Leap_second) that cannot be predicted
years ahead. A single wall-clock reading can name two distinct instants, or none
at all, when [daylight-saving time](https://en.wikipedia.org/wiki/Daylight_saving_time)
shifts the clocks. The same moment carries different labels in different places,
and which labels apply has depended, at times, on the outcome of wars.

Soundness systematizes as much of this as can be systematized, and it does so by
encoding the distinctions in types. Dates know which calendar they belong to;
instants know which timeline they measure; moments know their zone and how to
ground themselves against it; and the awkward cases — a 31st that some months
lack, a 29th of February that most years lack, a clock reading that falls in a
daylight-saving gap — are resolved by policies chosen in context rather than left
to chance. The sections below begin with the everyday types and build toward the
rare ones.

Every name used here comes from the `soundness` package:

```scala
import soundness.*
```

Operations that can fail — parsing text, constructing a date from runtime
integers — need an error-handling strategy in scope. These examples raise
exceptions on failure, which suits a tutorial:

```scala
import strategies.throwUnsafely
```

### Dates

A date is written as a year, an abbreviated month, and a day, joined by hyphens:

```scala
val date = 2024-Jan-15
```

This is not a string to be parsed at runtime; it is a literal the compiler
understands. An impossible date does not compile. The thirtieth of February, the
thirty-first of April, and the twenty-ninth of February in a common year are all
rejected before the program runs:

```scala
2012-Feb-30   // does not compile
2012-Apr-31   // does not compile
2011-Feb-29   // does not compile
2012-Feb-29   // compiles: 2012 is a leap year
```

Catching these mistakes at compiletime, rather than trusting a date to be valid
because it parsed, is the recurring habit that shapes the whole design.

The [Gregorian calendar](https://en.wikipedia.org/wiki/Gregorian_calendar) is the
default. Date arithmetic and field access read it from context, so most code
brings it into scope explicitly:

```scala
import calendars.gregorianCalendar
```

A date read from text uses `decode`. The `ts` interpolator does the same for a
literal known at compiletime, where it validates the digits just as the
hyphenated form does:

```scala
val parsed = t"2011-12-13".decode[Date]   // 2011-Dec-13
val literal = ts"2024-12-31"              // 2024-Dec-31
```

The fields of a date read back as a year, a month and a day:

```scala
val date = 2016-Jul-11
val fields = (date.year, date.month, date.day)   // (2016, Jul, 11)
```

A date also knows its weekday and its position in the year, and dates compare and
shift by whole days:

```scala
(2024-Jan-1).weekday          // Mon
(2024-Dec-31).yearDay         // 366
(2024-Jan-1) < (2024-Jan-2)   // true
(2024-Jan-1).addDays(31)      // 2024-Feb-1
```

A `Date` is the day-precision case of a more general `Timestamp`, distinguished by
a type parameter rather than by a separate class, so a date may be used wherever a
timestamp is expected.

### Times of day

A time of day is a `Clockface`, given as hours, minutes and seconds:

```scala
val time = Clockface(14, 30, 59)
```

For the common case of an exact minute, a time can be written as a number with its
meridiem attached. The minutes always take two digits:

```scala
2.01.am    // Clockface(2, 1, 0)
7.25.pm    // Clockface(19, 25, 0)
12.00.pm   // Clockface(12, 0, 0)  — noon
12.00.am   // Clockface(0, 0, 0)   — midnight
```

These literals are checked too. An hour above twelve, or minutes that do not fit
in two digits, will not compile:

```scala
13.00.am   // does not compile
7.88.am    // does not compile
```

### Timestamps

A `Timestamp` pairs a date with a time of day. The `on` operator joins the two,
reading naturally from clock to calendar:

```scala
val appointment = 5.25.pm on 2018-Aug-11
```

A timestamp can also be built directly, parsed from text, or written as a `ts`
literal:

```scala
val built = Timestamp(2023-May-28, Clockface(14, 30, 59))
val parsed = t"2024-01-15T14:30:59".decode[Timestamp]
val literal = ts"2023-05-28T14:30:59"
```

Its fields read back individually:

```scala
val ts = Timestamp(2024-Mar-15, Clockface(14, 30, 59))
(ts.year, ts.month, ts.day())      // (Year(2024), Mar, 15)
(ts.hour, ts.minute, ts.second)    // (14, 30, 59)
```

Subtracting one timestamp from another gives the span between them, decomposed
into days, hours, minutes and seconds:

```scala
val a = Timestamp(2024-Jan-10, Clockface(8, 0, 0))
val b = Timestamp(2024-Jan-12, Clockface(11, 30, 15))
val span = b - a
(span.days, span.hours, span.minutes, span.seconds.value)   // (2, 3, 30, 15.0)
```

### Months, years and anniversaries

Months are an enumeration, `Jan` through `Dec`. A month converts to and from its
text and its number, and the whole set is available as a list:

```scala
Month(t"Jan")    // Jan
Month(7)         // Jul
Dec.numerical    // 12
Month.all.length // 12
```

A `Year` wraps a calendar year. Years add and subtract whole counts, order against
each other, and parse from text:

```scala
Year(2024) + 5           // Year(2029)
Year(1999) < Year(2000)  // true
t"2024".decode[Year]     // Year(2024)
```

A year and a month together form a `Monthstamp`, written with `-`. Subtracting a
day yields a date; adding months and years moves it through the calendar, rolling
across year boundaries as needed:

```scala
val month = 2024 - Jan
(2024 - Feb) - 29          // 2024-Feb-29
((2024 - Nov) + 3*Month)   // 2025 - Feb
((2024 - Jun) + 1*Year)    // 2025 - Jun
```

A month and a day, with no year, form an `Anniversary` — a birthday or a holiday
that recurs every year. Applying it to a year produces that year's date:

```scala
val birthday = Mar - 14
```

The twenty-ninth of February has no counterpart in a common year, so applying such
an anniversary needs a strategy for the missing day. That strategy is a given,
selected by what is in scope rather than passed at each call: rounding down lands
on the twenty-eighth, and rounding up lands on the first of March:

```scala
given Anniversary.NonexistentLeapDay = nonexistentLeapDays.roundDownLeapDay
(Feb - 29)(Year(2023))   // 2023-Feb-28
(Feb - 29)(Year(2024))   // 2024-Feb-29  — a leap year is unaffected
```

### Durations and timespans

Soundness draws a firm line between two kinds of elapsed time, and the distinction
is deliberate. A `Duration` is a physical quantity of seconds, the same everywhere
and always. A `Timespan` is a calendar quantity — years, months, weeks, days,
hours, minutes — whose meaning depends on where it is anchored, because a month is
not a fixed number of seconds and a calendar day is not always twenty-four hours.
Keeping the two apart at the type level stops a count of months from ever being
mistaken for a count of seconds.

A duration is a quantity of seconds. Constructed from milliseconds, it reports its
length in seconds:

```scala
Duration(1000L).value   // 1.0
```

A timespan is built by multiplying a count by a unit, and several units combine
into one span. Its components read back by name:

```scala
val span = 2*Year + 3*Month + 14*Day
(span.years, span.months, span.days)   // (2, 3, 14)
```

A timespan encodes as an
[ISO 8601 duration](https://en.wikipedia.org/wiki/ISO_8601#Durations) and parses
back from one. The `dur` interpolator writes such a duration as a checked literal:

```scala
val span = Timespan(years = 1, months = 2, days = 3, hours = 4, minutes = 5,
    seconds = Quantity(6.0))

span.encode                        // P1Y2M3DT4H5M6S
t"P1Y2M3DT4H5M6S".decode[Timespan] // the same span
dur"P1Y2M3DT4H5M6S"                // checked at compiletime
```

### Arithmetic on dates

Adding a span to a date moves it forward. Days and weeks are regular — they need
no policy, because a week is always seven days:

```scala
2024-Jan-1 + 3*Day    // 2024-Jan-4
2024-Jan-1 + 2*Week   // 2024-Jan-15
```

Months and years are irregular. Adding a month to the thirty-first of January asks
for a thirty-first of February, which does not exist, so the result depends on a
policy. Soundness does not pick one silently; the policy is a given the calling
code must supply. Clamping lands on the last valid day; overflowing spills into the
next month:

```scala
import monthEnds.clampMonthEnd
2024-Jan-31 + 1*Month   // 2024-Feb-29
2024-Feb-29 + 1*Year    // 2025-Feb-28
```

```scala
import monthEnds.overflowMonthEnd
2024-Jan-31 + 1*Month   // 2024-Mar-2
```

Subtracting one date from another counts the days between them:

```scala
2018-Nov-19 - (2017-Sep-1)   // 444 days
```

Weekdays can be counted across a range, which answers questions like how many
Saturdays fall in a month:

```scala
Weekday.count(2025-Mar-1, 2025-Apr-1, Sat)   // 5
```

Working-day arithmetic skips weekends and holidays. It needs to know which days
form the week — a `Hebdomad` — and which dates are holidays, both supplied from
context:

```scala
import hebdomads.europeanHebdomad

given Holidays = Holidays(List
  ( Holiday(2025-Dec-25, t"Christmas Day"),
    Holiday(2025-Dec-26, t"Boxing Day") ))

2025-Aug-11 + WorkingDays(5)   // 2025-Aug-18, skipping the weekend
2025-Dec-24 + WorkingDays(1)   // 2025-Dec-29, skipping the holidays and weekend
```

### Instants

An instant is a moment in physical time, with no reference to any calendar or
clock. An `Instant` is measured along a timeline, and the timeline is part of its
type. The POSIX timeline counts milliseconds from the
[Unix epoch](https://en.wikipedia.org/wiki/Unix_time):

```scala
import chronometries.posix

val epoch = Instant(0L)   // 1970-01-01T00:00:00Z
```

The current instant comes from a `Clock`. A program reads the wall clock through
`now()`, the current date through `today()`, and a high-resolution elapsed-time
counter through `monotonic()`. The clock is itself a given, so supplying a fixed
one makes time deterministic, which is what tests want:

```scala
given Clock = Clock.fixed(Instant(99999L))
now()   // Instant(99999L)
```

Instants move by physical quantities and subtract to a duration:

```scala
Instant(0L) + 1*Hour            // Instant(3600000L)
Instant(3600000L) - Instant(0L) // 3600 seconds
```

Two instants bound a `Period`, a half-open interval that includes its start but
not its finish. Periods report their duration, test membership, intersect, and
unite:

```scala
val period = Instant(0L) ~ Instant(3_600_000L)
period.duration.value            // 3600.0
period.has(Instant(0L))          // true
period.has(Instant(3_600_000L))  // false — the finish is excluded
```

A period divides into equal segments, and its points can be sampled at a step. The
sampling is lazy, so a range spanning years costs nothing until its values are
demanded:

```scala
import calendars.gregorianCalendar

(Instant(0L) ~ Instant(3_600_000L)).segments(15*Minute).length   // 4
Period(2024-Jan-1, 2030-Jan-1).by(1*Day).take(3).to(List)
// List(2024-Jan-1, 2024-Jan-2, 2024-Jan-3)
```

### Time zones and moments

A time zone names a place's rules for civil time. The `tz` interpolator checks
that the name exists in the
[time-zone database](https://en.wikipedia.org/wiki/Tz_database) when the code
compiles, so a misspelled zone never reaches a running program:

```scala
val london = tz"Europe/London"
tz"NotARealZone"   // does not compile
```

A `Moment` is a timestamp anchored to a zone — a wall-clock reading somewhere in
particular. Grounding it with `instant` resolves it to physical time, and any
instant places itself into a zone with `in`:

```scala
import calendars.gregorianCalendar

val moment = Moment(2024-Jan-1, Clockface(1, 0, 0), tz"UTC")
moment.instant                        // the corresponding Instant
Instant(0L).in(tz"UTC").date          // 1970-Jan-1
```

A zone's offset changes with the season. The same instant reads as one hour later
in London in summer than in winter, because British Summer Time is in force:

```scala
import instantDecodables.iso8601InstantDecodable

val summer = t"2024-07-15T12:00:00Z".decode[Instant over Posix]
summer.in(tz"Europe/London").time.hour   // 13
```

Daylight-saving transitions make some wall-clock readings ambiguous and others
impossible, and Soundness refuses to paper over either case. When the clocks
spring forward, the skipped hour is a *gap*; a moment in the gap is resolved by a
policy, pushing forward by default or backward or failing outright on request:

```scala
val spring = Moment(2024-Mar-31, Clockface(1, 30, 0), tz"Europe/London")
spring.instant   // pushed forward to 01:30 UTC by default
```

```scala
import gapPolicies.rejectGap
Moment(2024-Mar-31, Clockface(1, 30, 0), tz"Europe/London").instant
// raises a TimeError: the local time never happens
```

When the clocks fall back, an hour repeats, and a reading in the overlap names two
instants an hour apart. The two are distinguished as the first and second
occurrence:

```scala
val clock = Clockface(1, 30, 0)
val earlier = Moment(2024-Oct-27, clock, tz"Europe/London")
val later = Moment(2024-Oct-27, clock, tz"Europe/London", Occurrence.Second)
later.instant.long - earlier.instant.long   // 3600000
```

### Calendars

Dates so far have been Gregorian, the modern civil default. Soundness supports many
other calendars, and a date literal is interpreted by whichever calendar is in
scope. The same written date can name a different day under a different calendar,
which shows up in its
[Julian day number](https://en.wikipedia.org/wiki/Julian_day):

```scala
val gregorian = { import calendars.gregorianCalendar; (1900-Mar-10).jdn }
val julian = { import calendars.julianCalendar; (1900-Mar-10).jdn }
gregorian == julian   // false
```

A calendar reads the year, month and day of a date in its own terms. Converting a
Gregorian date into the [Coptic](https://en.wikipedia.org/wiki/Coptic_calendar),
Islamic or Hebrew calendar reveals an entirely different year and month:

```scala
import calendars.copticCalendar

val date = { import calendars.gregorianCalendar; 2000-Jan-1 }
(copticCalendar.annual(date)(), copticCalendar.mensual(date), copticCalendar.diurnal(date)())
// (1716, CopticMonth.Koiak, 22)
```

Other calendars follow the same shape, including the Islamic, Ethiopian, Persian,
Hebrew, Indian national and French Republican calendars, and year-offset calendars
such as the Buddhist and Minguo. A calendar formats a date with its own month
names:

```scala
import calendars.islamicCalendar
islamicCalendar.format(Date(Year(1445), IslamicMonth.Ramadan, Day(15)))
// "15 Ramadan 1445"
```

When one calendar replaced another, the changeover left a gap of dates that never
occurred. A `Regime` models such a cutover, applying the old calendar before the
switch and the new one after, and rejecting dates that fall in the gap — at compile
time when the date is a literal:

```scala
given Regime = calendars.papalCutover
(1582-Oct-4).jdn    // last Julian day
(1582-Oct-15).jdn   // first Gregorian day — the very next day
1582-Oct-10         // does not compile: a date in the gap
```

Dates also have an ordinal form — the day's number within its year — and an
[ISO week-date](https://en.wikipedia.org/wiki/ISO_week_date) form, where a date
belongs to a week-numbered year that need not match its calendar year:

```scala
import calendars.gregorianCalendar
(WeekDate.weekYear(2000-Jan-1)(), WeekDate.weekOfYear(2000-Jan-1))   // (1999, 52)
```

### Recurrences

A recurrence describes an event that repeats at a fixed interval. In its
[ISO 8601](https://en.wikipedia.org/wiki/ISO_8601#Repeating_intervals) form it is a
start, a period, and an optional number of repetitions. Its occurrences form a lazy
sequence:

```scala
import calendars.gregorianCalendar

Recurrence(2024-Jan-1, 1*Day, 3).occurrences.to(List)
// List(2024-Jan-1, 2024-Jan-2, 2024-Jan-3)
```

A recurrence with no repetition count runs forever, so it is consumed with a
bound — every occurrence up to a date, or those falling within a window:

```scala
Recurrence(2024-Jan-1, 1*Week).until(2024-Jan-22).to(List)
// List(2024-Jan-1, 2024-Jan-8, 2024-Jan-15)

Recurrence(2024-Jan-1, 1*Week).within(Period(2024-Jan-5, 2024-Jan-20)).to(List)
// List(2024-Jan-8, 2024-Jan-15)
```

A recurrence encodes to and decodes from its ISO 8601 form, and the `rec`
interpolator writes one as a checked literal:

```scala
Recurrence(2024-Jan-1, 1*Day, 5).encode   // R5/2024-01-01/P1D
rec"R3/2024-01-01/P1M".occurrences.to(List)
// List(2024-Jan-1, 2024-Feb-1, 2024-Mar-1)
```

### Recurrence rules

The calendar pattern most software needs — "the third Monday of every month", "the
last Friday", "the fourth Thursday of November" — does not fit a fixed interval.
These are described by a recurrence rule, modeled on the
[RFC 5545](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.10) `RRULE`
from iCalendar. A rule has a start, a frequency, and optional fields that select
within each period.

The simplest rule repeats at its frequency, taking the date from its start:

```scala
import calendars.gregorianCalendar

Rrule(2024-Jan-15, Frequency.Monthly, count = 4).occurrences.to(List)
// List(2024-Jan-15, 2024-Feb-15, 2024-Mar-15, 2024-Apr-15)
```

The `byDay` field selects weekdays, optionally by ordinal. The third Monday of a
month is `WeekdayOrdinal(Mon, 3)`; the last Friday counts back from the end with
`-1`:

```scala
Rrule(2024-Jan-1, Frequency.Monthly, byDay = List(WeekdayOrdinal(Mon, 3)), count = 2)
    .occurrences.to(List)
// List(2024-Jan-15, 2024-Feb-19)

Rrule(2024-Jan-1, Frequency.Monthly, byDay = List(WeekdayOrdinal(Fri, -1)), count = 2)
    .occurrences.to(List)
// List(2024-Jan-26, 2024-Feb-23)
```

Fields combine. The fourth Thursday of November is a yearly rule narrowed to one
month and one weekday:

```scala
Rrule(2024-Jan-1, Frequency.Yearly, byMonth = List(Nov),
    byDay = List(WeekdayOrdinal(Thu, 4)), count = 2).occurrences.to(List)
// List(2024-Nov-28, 2025-Nov-27)
```

A frequency can carry an interval — every second week, every third day — and a rule
can run weekly across several weekdays:

```scala
Rrule(2024-Jan-1, Frequency.Weekly, interval = 2, count = 3).occurrences.to(List)
// List(2024-Jan-1, 2024-Jan-15, 2024-Jan-29)

Rrule(2024-Jan-1, Frequency.Weekly, byDay = List(WeekdayOrdinal(Mon),
    WeekdayOrdinal(Wed), WeekdayOrdinal(Fri)), count = 4).occurrences.to(List)
// List(2024-Jan-1, 2024-Jan-3, 2024-Jan-5, 2024-Jan-8)
```

The `bySetPos` field picks from each period's expansion by position, which
expresses "the last weekday of the month" as the last of the five weekday
candidates:

```scala
Rrule(2024-Jan-1, Frequency.Monthly, byDay = List(WeekdayOrdinal(Mon),
    WeekdayOrdinal(Tue), WeekdayOrdinal(Wed), WeekdayOrdinal(Thu),
    WeekdayOrdinal(Fri)), bySetPos = List(-1), count = 2).occurrences.to(List)
// List(2024-Jan-31, 2024-Feb-29)
```

Frequencies below a day apply to timestamps, where `byHour` and `byMinute` expand
within each day:

```scala
Rrule(Timestamp(2024-Jan-1, Clockface(0, 0, 0)), Frequency.Daily,
    byHour = List(9, 17), count = 3).occurrences.to(List)
// the 9th and 17th hours of each day
```

A rule renders as an RFC 5545 string and parses back from one:

```scala
Rrule(2024-Jan-1, Frequency.Monthly, interval = 2, count = 10,
    byDay = List(WeekdayOrdinal(Mon, 3))).encode
// FREQ=MONTHLY;INTERVAL=2;COUNT=10;BYDAY=3MO

Rrule.parse(t"FREQ=MONTHLY;BYDAY=-1FR", 2024-Jan-1).occurrences.take(2).to(List)
// List(2024-Jan-26, 2024-Feb-23)
```

Several rules and individual dates combine into a `RecurrenceSet`, which merges its
sources into one ascending stream, adds explicit dates, and removes exceptions:

```scala
val weekly = Rrule(2024-Jan-1, Frequency.Weekly, count = 4)
RecurrenceSet(include = List(weekly.occurrences), rdates = List(2024-Jan-10),
    exdates = List(2024-Jan-15)).occurrences.to(List)
// List(2024-Jan-1, 2024-Jan-8, 2024-Jan-10, 2024-Jan-22)
```

### Describing recurrences

A rule, a recurrence or a timespan describes itself in words. The description is
localized, and the language is enforced by the types: it needs a `Locale` and the
month and weekday names for that language in scope, or it will not compile:

```scala
import monthFormats.englishMonths
import weekdays.englishWeekdays
given Locale[en] = Locale(en)

Rrule(2024-Jan-1, Frequency.Yearly, byMonth = List(Nov),
    byDay = List(WeekdayOrdinal(Thu, 4))).show
// "every year on the 4th Thursday of November"
```

Another language follows from a different locale and its names:

```scala
import monthFormats.frenchMonths
import weekdays.frenchWeekdays
given Locale[fr] = Locale(fr)

Rrule(2024-Jan-1, Frequency.Monthly, byDay = List(WeekdayOrdinal(Mon, 3))).show
// "tous les mois le 3e lundi"
```

A timespan describes itself relative to now, reading forward or backward depending
on its sign:

```scala
import timespanFormats.englishRelative
given Locale[en] = Locale(en)

Timespan(minutes = 18).show    // "in 18 minutes"
Timespan(minutes = -8).show    // "8 minutes ago"
Timespan().show                // "just now"
```

### Formatting dates and times

A date or time displays through a chosen format. Importing one of the named formats
fixes the convention for the whole scope:

```scala
val date = 2025-Apr-7
```

```scala
import dateFormats.iso8601DateFormat
date.show   // 2025-04-07
```

```scala
import dateFormats.americanDateFormat
date.show   // 04/07/2025
```

Times offer the same range, from the twenty-four-hour form to the twelve-hour form:

```scala
val time = Clockface(14, 30, 59)
```

```scala
import timeFormats.militaryTimeFormat
time.show   // 1430
```

```scala
import timeFormats.civilianTimeFormat
time.show   // 02:30 PM
```

For finer control, a format is composed from independent choices — the order of the
fields, the separator between them, the width of each number, and the length of the
year — each brought in on its own. Assembling a format from orthogonal pieces,
rather than choosing from a fixed menu, lets unusual conventions be expressed
without a bespoke formatter:

```scala
import endianness.bigEndian
import dateNumerics.variableWidthDateNumerics
import dateSeparators.hyphenDateSeparator
import yearFormats.fullYears

(2025-Apr-7).show   // 2025-4-7
```

### Leap seconds and atomic time

Civil time and physical time disagree by a whole number of seconds, and the number
grows. Since 1972, [leap seconds](https://en.wikipedia.org/wiki/Leap_second) have
been inserted into [UTC](https://en.wikipedia.org/wiki/Coordinated_Universal_Time)
to keep clocks aligned with the Earth's slowing rotation. A POSIX instant ignores
them, so the elapsed seconds between two POSIX instants can be short by the leap
seconds that fell between them.

[International Atomic Time](https://en.wikipedia.org/wiki/International_Atomic_Time)
(TAI) has no such discontinuities. Converting a POSIX instant to the TAI timeline
adds the leap seconds that have accumulated by that instant — thirty-seven of them
by 2017 — and the conversion round-trips exactly:

```scala
import calendars.gregorianCalendar
import instantDecodables.iso8601InstantDecodable

val instant = t"2017-06-15T00:00:00Z".decode[Instant over Posix]
instant.over[Tai].long - instant.long    // 37000
instant.over[Tai].over[Posix] == instant // true
```

Because the timelines are distinct types, subtracting an instant on one from an
instant on the other is a compile error. A whole class of off-by-leap-seconds bugs
simply cannot be written:

```scala
Instant(0L).over[Tai] - Instant(0L)   // does not compile
```

By default, arithmetic on moments is lenient: it works in civil time and steps over
leap seconds as if they were not there. Choosing the exact mode counts them, so
adding two hours across the leap second at the end of 2016 lands one second short
of the lenient answer:

```scala
import leapModes.exact
val start = Moment(2016-Dec-31, Clockface(23, 0, 0), tz"UTC")
(start + 2*Hour).instant
// 2017-01-01T00:59:59Z — one second behind the lenient result
```

A leap second itself is the rare reading `23:59:60`. A zoned `ts` literal accepts
it, flags the moment as carrying an inserted second, and grounds it to the second
that follows:

```scala
val moment = ts"2016-12-31T23:59:60Z"
moment.leap   // Leap.Inserted
```
