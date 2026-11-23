## Label placement

### About

Text on a picture is set beside the thing it names: a label by a point, a name by a town, a
percentage in a wedge. When two things are close, their labels collide, and the picture is worse
than it would be with one of them moved a little.

Cartouche decides where each piece of text goes. It takes every label with its measured size,
the point it describes and the sides it may lie on, together with everything else on the page
which text must not cover, and finds a position for each one. [Charts](charts.md) use it for
their labels. A map would use it for its place names.

### On placement

The code which builds a picture knows what each label says and what it describes, but not where
the other labels will end up, and so it cannot avoid them. Cartouche resolves this by running
the code which builds the picture twice. The first run describes the labels. The engine arranges
them. The second run builds the picture with the answers.

The body of `arrange` is that code, and `position` is how it asks for the place of a label.
Everything comes from the `soundness` package. The second import lets the samples print their
results, and the `Decimalizer` fixes how many significant figures a coordinate is printed with:

```scala
import soundness.*
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

given decimalizer: Decimalizer = Decimalizer(5)
```

Here two labels of the same size ask for the same point:

```scala fuel
run
##
val picture = arrange:
  val first = position(60.0, 12.0, 100.0, 100.0)
  val second = position(60.0, 12.0, 100.0, 100.0)
  (first.attachment, second.attachment)

Out.println(picture.inspect)
```

The first is set to the east of the point, as labels are by preference. The second would cover
it, so it is set to the west.

In the first run of the body, each `position` answers provisionally, with the label where it was
asked for, and the result of that run is thrown away. The engine then solves the whole
arrangement, and in the second run each call answers with its solved position. The value of
`arrange` is the value of the body's second run, so a chart built inside `arrange` is what comes
out of it.

The body is run twice, so it must be repeatable: the same `position` calls, in the same order,
each time, and nothing done which should not happen twice. A counter which numbers the labels
belongs inside the body, where it starts again with the second run.

### Captions and targets

A label is a *caption*: a width and height, the point it describes, and how it may be set
relative to that point. The engine sees only geometry. Whoever draws the text measures it.
`position` takes the parts of the caption directly, or a `Caption` built beforehand:

```scala
val caption = Caption(60.0, 12.0, 100.0, 100.0, standoff = 4.0, reach = 24.0)
val placed = arrange(position(caption))
```

The answer is a `Caption.Position`: the point to set the text at, the side of the target it lies
on, the box it occupies, a leader line if it needed one, and whether it is to be drawn at all:

```scala fuel
run
##
Out.println(placed.attachment.inspect)
Out.println(placed.x.show)
Out.println(placed.y.show)
Out.println(placed.visible.inspect)
```

The side is an `Attachment`, which is `East`, `West`, `North`, `South`, one of the four
diagonals, or `Center`. The point is the anchor of the label on that side. For `East`, it is the
middle of the left edge of the label, so text set there with its start at the point lies to the
east of the target. Here the `standoff` of `4.0` puts the point four units east of the target. A
renderer turns the attachment into its own alignment. An SVG `text` element uses `text-anchor`
and `dominant-baseline`.

`attachments` lists the sides the label may lie on, best first. The default tries east, then
west, then the diagonals, then north and south. A label which may lie on one side only is given
just that side. `standoff` is the gap between the target and the near edge of the label, for a
label which sits beside a marker rather than on its center.

The rest of the parameters of a caption are described in the sections which follow. All of them
are:

```tabular
column  Parameter
column  Default
column  Meaning
row  `width`, `height`  required  the label's measured size
row  `x`, `y`  required  the target point
row  `attachments`  east, west, the diagonals, north, south  the sides it may lie on, best first
row  `standoff`  `0.0`  the gap between the target and the label's near edge
row  `reach`  `0.0`  how far it may be moved from the target
row  `padding`  `0.0`  clearance demanded beyond its own box
row  `priority`  `0`  higher is placed first
row  `fallback`  `Caption.Fallback.Overlap`  what to do when no position is clear
```

### Obstacles

Labels avoid each other, and they avoid whatever the body declares with `avoid`:

```tabular
column  Obstacle
column  Parameters
column  Covers
row  `Obstacle.Box`  `left`, `top`, `width`, `height`  a bar
row  `Obstacle.Line`  `x1`, `y1`, `x2`, `y2`, and a `width` defaulting to `1.0`  a plotted line
row  `Obstacle.Disc`  `x`, `y`, `radius`  a marker
```

```scala fuel
run
##
val kept = arrange:
  avoid(Obstacle.Box(100.0, 90.0, 40.0, 20.0))
  avoid(Obstacle.Line(0.0, 100.0, 300.0, 100.0), Obstacle.Disc(100.0, 100.0, 3.0))
  position(60.0, 12.0, 100.0, 100.0)

Out.println(kept.attachment.inspect)
```

The box lies to the east of the point, and the line runs through it, so a label set to the west
would touch the line. The label is set to the northwest instead. A polyline is its segments, each an `Obstacle.Line`. The `width` of a line is its stroke, and a label may not
touch it.

`canvas` declares the area which labels must stay within, which is otherwise unbounded. Here the
label would run off the right edge, so it is set to the west:

```scala fuel
run
##
val inside = arrange:
  canvas(Obstacle.Box(0.0, 0.0, 120.0, 120.0))
  position(60.0, 12.0, 110.0, 100.0)

Out.println(inside.attachment.inspect)
```

### Displacement and leaders

When every side of the target is blocked, a label may be moved away from it, up to its `reach`,
which is zero unless given. A label moved further than a short threshold gets a *leader*: a line
from its edge back to the target, so what it names stays clear:

```scala fuel
run
##
val led = arrange:
  avoid(Obstacle.Box(0.0, 0.0, 200.0, 110.0))
  position(60.0, 12.0, 100.0, 100.0, attachments = List(Caption.Attachment.South), reach = 40.0)

Out.println(led.attachment.inspect)
Out.println(led.leader.present.inspect)
```

The label is set below the box, and its leader runs up to the point. A leader is part of the
arrangement. Later labels avoid it, as they avoid the label it belongs to, and the leader of a
label may not cross another label. It may cross a fixed obstacle, since pointing past a bar or a
marker to the target is what a leader is for.

`padding` is clearance demanded around a label beyond its own box, so neighboring labels are
not set touching.

### Priority and hiding

Labels are placed in order of `priority`, highest first, and otherwise in the order the body
asked for them. A label placed earlier keeps its target, and those after it move. A label with a
single side and no reach is placed exactly where it was asked for, whatever is there, and
everything after it steers around it. That is how the tick labels and legend of a chart hold
still while its point labels move.

A label which collides wherever it is put is drawn at the position where it overlaps least. A
caption whose `fallback` is `Caption.Fallback.Hide` is instead not drawn. Its position reports
`visible` as false, and the body draws nothing for it. A hidden label blocks nothing:

```scala fuel
run
##
val hidden = arrange:
  avoid(Obstacle.Box(0.0, 0.0, 300.0, 300.0))
  position(60.0, 12.0, 100.0, 100.0, fallback = Caption.Fallback.Hide)

Out.println(hidden.visible.inspect)
```

### Choosing an arranger

The algorithm which arranges is the `Arranger` in scope. The default is greedy. Each label, in
priority order, takes the least displaced position which is clear of everything placed before
it, or the least collision if nothing is clear. It is deterministic, and its work is bounded by
the number of positions a label may take. A label placed early is never moved to make room for
one placed later, which on a dense picture leaves collisions which a different order would have
avoided.

Simulated annealing starts from the greedy arrangement, and then moves labels among their
positions at random. It keeps a move which lowers the total collision and displacement, and
sometimes one which raises it, less often as it cools. It is chosen by importing it:

```scala fuel
run
##
import arrangers.annealingArranger

val annealed = arrange:
  val first = position(60.0, 12.0, 100.0, 100.0, reach = 20.0)
  val second = position(60.0, 12.0, 102.0, 101.0, reach = 20.0)
  val third = position(60.0, 12.0, 98.0, 99.0, reach = 20.0)
  List(first.attachment, second.attachment, third.attachment)

Out.println(annealed.inspect)
```

Its random moves are drawn from a fixed seed, so the same input arranges the same way every
time, and it stops after a set number of moves. Both arrangers are case classes,
`Arranger.Greedy` and `Arranger.Annealing`, whose parameters can be given in place of the
defaults:

```tabular
column  Parameter
column  Default
column  Arranger
column  Meaning
row  `step`  `4.0`  both  the step between candidate positions
row  `rings`  `8`  both  how many steps out to search
row  `leaderThreshold`  `6.0`  both  the displacement beyond which a leader is drawn
row  `leaderWidth`  `1.0`  both  the stroke width of a leader
row  `seed`  `Seed(0L)`  annealing  the seed its random moves are drawn from
row  `iterations`  `4000`  annealing  the number of moves
row  `temperature`  `1.0`  annealing  the starting temperature
row  `cooling`  `0.995`  annealing  the factor the temperature is multiplied by at each move
row  `displacementWeight`  `0.05`  annealing  the cost of displacement against that of collision
```

```scala
given arranger: Arranger = Arranger.Greedy(step = 2.0, rings = 12)
```

Any other algorithm is an `Arranger` too: a value with one method, which takes the captions,
the obstacles and the canvas, and answers with a position for each caption in turn.

### Related

- [Charts](charts.md) place their labels through `arrange`.
- [SVG](svg.md) describes the `text` elements which a renderer sets at the answered positions.
- [Randomness](randomness.md) describes the `Seed` which the annealing arranger draws from.
