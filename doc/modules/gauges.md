## Gauges

### About

A long-running program has something to say while it works: how far through it is, that it is
still alive, how fast bytes are moving, which step of a pipeline is running. Soundness renders
these as *gauges* — spinners, progress bars, meters, sparklines, counters and step indicators —
either embedded in a terminal layout or drawn on their own at the cursor.

A gauge is chosen by the *type* of the thing it displays. What it looks like, what colours it uses
and which characters it may draw with are three separate decisions, each made by one import, and
none of them disturbs the other two.

### On progress display

Progress display is usually written twice: once as the drawing, and again as the arithmetic that
decides what fits. The two are kept consistent by hand, and the result is a bar that is correct at
eighty columns and corrupt at thirty — a row that overruns, a label that pushes the bar off the
edge, or a percentage that disagrees with the bar beside it.

Separating the two removes the duplication. A design says how to draw a status at whatever width it
is given, and how narrow it can usefully go; the layout says how much room there is. Neither needs
to know what the other decided. A bar handed six cells re-quantizes to six; handed three it becomes
a percentage; handed one it becomes a single shade. Everything comes from the `soundness` package:

```scala
import soundness.*
```

### Statuses

What a gauge displays is a value of a specific type, and that type is what selects the design:

Status                | Displays                                    | Default design
----------------------|---------------------------------------------|-------------------
`Fraction`            | a proportion in `[0, 1]`, clamped           | smooth block bar
`Reckoning`           | `done` against a total that may be unknown  | `17/120`
`Standing`            | how one unit of work turned out             | `✓ ✗ ‑`
`Duration`            | time spent                                  | `2m41s`
`Countdown`           | time left, clamped at zero                  | `2m41s`
`Captioned`           | any status, with a label                    | derived
`Transfer`            | bytes moved, with rate and estimate         | *none*
`Meter`               | a reading on a bounded scale                | *none*
`Sequence[Double]`    | samples over time                           | *none*
`Sequence[Step]`      | an ordered run of steps                     | *none*

Most of these are types you already have. Only where a type has to be *distinct* — because it is
the key the design is looked up by — is a new one introduced: a proportion cannot be a plain
`Double` without claiming every `Double` in the program for progress bars. Elapsed time is an
`aviation.Duration`, samples are a `Sequence[Double]`, and a run of steps is a `Sequence[Step]`.
`Countdown` keeps its own type only so that it can sit beside an elapsed time with a different
design.

Any status may be `Optional`, and every design lifts to that automatically: `Optional[Fraction]` is
progress that may not be known yet, so a job that starts out unmeasurable and later learns its total
does not change type half way through. A design says how to draw when there is no value — a bar
sweeps, because an empty bar claims "no progress" where the truth is "not measured", and a spinner
draws its frames. Nothing has to be written twice for the optional case.

A spinner is what you import when the figure is not worth showing, or is not known; it keys on
`Fraction` like a bar does, and ignores the value. Two designs for one status conflict, as two bars
always have, so a layout wanting both takes one from a locally-scoped `given`:

```scala
val resolving =
  given spinner: (Fraction is Gaugeable) = spinners.brailleDotsSpinner
  gauge(Reading(Fraction.indeterminate))
```

The four with no default are deliberate. A default exists only where every candidate design is the
same height and says the same thing; a battery, a thermometer and a dial are not interchangeable,
block and braille sparklines differ in vertical resolution and so in what the data shows, a
checklist is one row per step where a breadcrumb is one row altogether, and whether a transfer
counts in `MB` or `MiB` is an editorial decision. Choosing silently in those cases would be
choosing how much of the caller's layout to occupy, or what their numbers mean.

### Choosing a design

With nothing imported, the defaults apply:

```scala
Out.println(gaugeLine(Fraction(0.42), 40))
Out.println(gaugeLine(Reckoning(17, 120), 7))
Out.println(e"${gaugeLine(Standing.Succeeded, 1)} built")
```

`gaugeLine` renders one frame at a width you choose. A gauge always needs a width — there is no
rendering of a bar that does not know how wide it is — so a status has no `Teletypeable` of its own.

Importing a design by name replaces it, and nothing else changes:

```scala
import bars.arrowheadBar
import spinners.moonPhaseSpinner
```

The families are `spinners`, `bars`, `meters`, `sparklines`, `counters`, `standings`,
`processions` and `timers`. There are around forty spinners, from single-cell braille and block
animations to bouncing bars and emoji moon phases; eighteen bars, from the smooth eighth-block
default through segmented pips and gradient fills to `[###---]`; six meters (battery, thermometer,
needle, bullet, column and ASCII); four sparklines; nine counters; five status markers; and five
step indicators.

### Colours and glyphs

Colour is an independent axis. A `GaugePalette` names its colours by *role* — `fill`, `track`,
`leadingEdge`, `caption`, `muted`, `success`, `warning`, `danger` — so one design renders under any
palette:

```scala
import palettes.solarizedDarkGaugePalette
```

Ten palettes ship, including a hue-free `monochromeGaugePalette` and an `ansiSixteenGaugePalette`
whose colours are the canonical values a sixteen-colour terminal actually has. With no import, an
adaptive palette picks by what the terminal reports.

The character repertoire is a third axis, and degrades the whole catalogue at once:

```scala
import gaugeGlyphs.asciiGlyphs
```

Every design has an ASCII rendering, and every design that prefers exotic glyphs declares what to
fall back to — so emoji designs become their BMP siblings where the terminal cannot show them, and
under `asciiGlyphs` everything emits seven-bit output. No design carries its meaning in colour
alone, so a gauge written to a file or read by someone who cannot distinguish red from green still
says what it means.

### In a layout

`gauge` builds a pane, taking a `Reading` — a mutable cell holding the current status. Assigning to
the cell publishes the value and repaints, so a gauge driven from a background task updates itself:

```scala
val progress = Reading(Fraction(0.0))

async:
  work.each: item =>
    process(item)
    progress() = Fraction.of(done, total)

interactive: terminal ?=>
  form(Occupancy.Inline)(stack(gauge(progress)))
```

A gauge reports its design's intrinsic size on every solve, so it takes part in the layout rather
than assuming a width: an elastic design fills what it is given, an inelastic one (a spinner, a
status glyph) is held to its own width, and a multi-row design like a checklist grows the layout to
its step count. A gauge accepts no input, so Tab skips over it.

A design that animates declares how often it wants redrawing; the form takes the shortest period
over the gauges on screen and runs one timer for all of them, and none at all when nothing is
animating.

### Standalone

Outside a layout, `whilst` shows a gauge at the cursor for the duration of a block and erases it
afterwards:

```scala
whilst(Reading(Fraction.indeterminate)):
  slowThing()
```

`gaugeLine` renders a single frame for a caller doing its own drawing — the shape to use when the
redrawing is already handled:

```scala
Out.print(e"\r${gaugeLine(Fraction.of(done, total), 40)} $done/$total${csi.el()}")
```

### Degradation

Every design renders exactly the width it is given, at every width, and says so as it narrows
rather than overrunning. A bar drops its end caps below eight cells, falls through to a percentage
at four, and to a single shade glyph at one. A composite row sheds its parts in a declared order,
so a transfer that reads

```
14.2 MiB/512 MiB · 3.1 MiB/s · 2m41s left
```

at eighty cells keeps only the figures at thirty. A caption is elided rather than squeezing the
gauge it labels, and is allowed at most half the row before it begins costing the gauge cells.

A sparkline's scale is a design choice rather than part of the data — `Sparkline.Blocks.scaled(0, 100)`
fixes it, so a steady signal does not look erratic as the bounds move under it. A sparkline narrower
than its series is *decimated* — each output cell shows the maximum of the
samples it covers — rather than truncated. A truncated sparkline would show only the oldest samples
while looking like the whole series, which is worse than showing less.

### Composing

`Captioned` labels any status, and derives its design from whatever the underlying status already
uses, so it is not a style choice and needs no import:

```scala
gauge(Reading(Captioned(Fraction.indeterminate, t"resolving dependencies")))
```

A `Sequence[Step]` renders as a checklist, a breadcrumb, a chain of beads, a numbered position or a
powerline ribbon:

```scala
val steps =
  Sequence
    ( Step(t"resolve", Standing.Succeeded),
      Step(t"compile", Standing.Running),
      Step(t"publish", Standing.Pending) )

gauge(Reading(steps))   // with `import processions.checklistProcession`
```

drawing, at three rows:

```
✓ resolve
⠋ compile
· publish
```

The running step animates, and each step's marker comes from the same `Standing` vocabulary a
one-cell status glyph uses.
