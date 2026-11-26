# A compass with as many points as its type says

The number of points on a compass rose is a type argument, and the type of a bearing is
computed from it. `Compass[4]` is one of four winds, and a bearing taken from a rose of eight
points is not one of them.

```scala fuel
compile
frame
  label Eight points do not fit into four
  errors show
  code
      val wind: Compass[4] = Compass[8](315.0.deg)
frame
  label A rose of four points
  highlight Compass[4](  the type argument chooses the rose
  code
      val wind: Compass[4] = Compass[4](315.0.deg)

      val arrow: Text = wind match
        case North => t"up"
        case East  => t"right"
        case South => t"down"
        case West  => t"left"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(arrow)
Out.println(Compass[8](315.0.deg).inspect)
```

The match needs four cases and no more, since `Compass[4]` is `CardinalWind` and nothing else.
[Compass points](../../soundness/modules/geography.md#compass-points) has the three roses.
