## Label placement

### About

Text on a picture is set beside the thing it names: a label by a point, a name by a town, a
percentage in a wedge. When two things are close, their labels collide, and the picture is worse
than it would be with one of them moved a little. Cartouche decides where each piece of text
goes: it takes every label with its measured size, the point it describes and the sides it may
lie on, together with everything else on the page that text must not cover, and finds a
position for each one. Charts use it for their labels; a map would use it for its place names.

### On placement

The code that builds a picture knows what each label says and what it describes, but not where
the other labels will end up, and so cannot avoid them. Cartouche resolves this by running the
code that builds the picture twice. The first run describes the labels; the engine arranges
them; the second run builds the picture with the answers. The body of `arrange` is that code,
and `position` is how it asks for a label's place:

```scala
import soundness.*

val picture = arrange:
  val first = position(60.0, 12.0, 100.0, 100.0)
  val second = position(60.0, 12.0, 100.0, 100.0)
  (first.attachment, second.attachment)
```

Two labels of the same size ask for the same point. The first is set to the east of it, as
labels are by preference, and the second, which would otherwise cover it, to the west. In the
first run of the body each `position` answers provisionally, with the label where it was asked
for, and that run's result is thrown away; the engine then solves the whole arrangement, and in
the second run each call answers with its solved position. The value of `arrange` is the value
of the body's second run, so a chart built inside `arrange` is what comes out of it.

The body is run twice, so it must be repeatable: the same `position` calls in the same order
each time, with nothing done that should not happen twice. A counter that numbers the labels
belongs inside the body, where it starts again with the second run.

### Captions and targets

A label is a *caption*: a width and height, the point it describes, and how it may be set
relative to that point. The engine sees only geometry; whoever draws the text measures it.
`position` takes the caption's parts directly, or a `Caption` built beforehand:

```scala
val caption = Caption(60.0, 12.0, 100.0, 100.0, standoff = 4.0, reach = 24.0)
val placed = arrange(position(caption))
```

The answer is a `Caption.Position`: the point to set the text at, the side of the target it lies
on, the box it occupies, a leader line if it needed one, and whether it is to be drawn at all.
The side is an `Attachment` — `East`, `West`, `North`, `South`, the four diagonals or `Center` —
and the point is the label's anchor on that side: for `East`, the middle of its left edge, so
text set there with its start at the point lies to the east of the target. A renderer turns the
attachment into its own alignment; an SVG `text` element uses `text-anchor` and
`dominant-baseline`.

`attachments` lists the sides the label may lie on, best first; the default tries east, then
west, then the diagonals, then north and south. A label that may lie on one side only is given
just that side. `standoff` is the gap between the target and the label's near edge, for a
label that sits beside a marker rather than on its centre.

### Obstacles

Labels avoid each other, and they avoid whatever the body declares with `avoid`: boxes, line
segments and discs, which cover a bar, a plotted line and a marker respectively.

```scala
val kept = arrange:
  avoid(Obstacle.Box(100.0, 90.0, 40.0, 20.0))
  avoid(Obstacle.Line(0.0, 100.0, 300.0, 100.0), Obstacle.Disc(100.0, 100.0, 3.0))
  position(60.0, 12.0, 100.0, 100.0)
```

The box lies to the east of the point, so the label is set to the west. A polyline is its
segments, each an `Obstacle.Line`; a line's `width` is its stroke, and a label may not touch it.
`canvas` declares the area labels must stay within, which is otherwise unbounded:

```scala
val inside = arrange:
  canvas(Obstacle.Box(0.0, 0.0, 120.0, 120.0))
  position(60.0, 12.0, 110.0, 100.0)
```

The label would run off the right edge, so it is set to the west.

### Displacement and leaders

When every side of the target is blocked, a label may be moved away from it, up to its `reach`,
which is zero unless given. A label moved further than a short threshold gets a *leader*, a line
from its edge back to the target, so that what it names stays clear:

```scala
val led = arrange:
  avoid(Obstacle.Box(0.0, 0.0, 200.0, 110.0))
  position(60.0, 12.0, 100.0, 100.0, attachments = List(Caption.Attachment.South), reach = 40.0)

led.leader
```

The label is set below the box and its leader runs up to the point. A leader is part of the
arrangement: later labels avoid it, as they avoid the label it belongs to, and a label's own
leader may not cross another label. It may cross a fixed obstacle, since pointing past a bar or
a marker to the target is what a leader is for.

`padding` is clearance demanded around a label beyond its own box, so that neighbouring labels
are not set touching.

### Priority and hiding

Labels are placed in order of `priority`, highest first, and otherwise in the order the body
asked for them. A label placed earlier keeps its target; those after it move. A label with a
single side and no reach is placed exactly where it was asked for, regardless of what is there,
and everything after it steers around it, which is how a chart's tick labels and legend hold
still while its point labels move.

A label that collides wherever it is put is drawn at the position where it overlaps least. A
caption whose `fallback` is `Caption.Fallback.Hide` is instead not drawn: its position reports
`visible` as false, and the body draws nothing for it. A hidden label blocks nothing.

```scala
val hidden = arrange:
  avoid(Obstacle.Box(0.0, 0.0, 300.0, 300.0))
  position(60.0, 12.0, 100.0, 100.0, fallback = Caption.Fallback.Hide)

hidden.visible
```

### Choosing an arranger

The algorithm that arranges is the `Arranger` in scope. The default is greedy: each label, in
priority order, takes the least displaced position that is clear of everything placed before it,
or the least collision if nothing is clear. It is deterministic, and its work is bounded by the
number of positions a label may take. A label placed early is never moved to make room for one
placed later, which on a dense picture leaves collisions that a different order would have
avoided. Simulated annealing starts from the greedy arrangement and then moves labels among
their positions at random, keeping a move that lowers the total collision and displacement, and
sometimes one that raises it, less often as it cools; it is chosen by importing it:

```scala
import arrangers.annealingArranger

val annealed = arrange:
  position(60.0, 12.0, 100.0, 100.0, reach = 20.0)
  position(60.0, 12.0, 102.0, 101.0, reach = 20.0)
  position(60.0, 12.0, 98.0, 99.0, reach = 20.0)
```

Its random moves are drawn from a fixed seed, so the same input arranges the same way every
time, and it stops after a set number of moves. Both arrangers are case classes whose
parameters — the step between candidate positions, how many steps out to search, the
displacement beyond which a leader is drawn, and for annealing the seed, temperature and
iterations — can be given in place of the defaults:

```scala
given Arranger = Arranger.Greedy(step = 2.0, rings = 12)
```

Any other algorithm is an `Arranger` too: a value with one method, taking the captions, the
obstacles and the canvas, and answering with a position for each caption in turn.
