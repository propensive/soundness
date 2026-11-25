# A path drawn by its steps

The `d` attribute of an SVG path is a small program of letters and numbers. An `Outline` is
the same program as a chain of named steps, each a method, so the pen's route is read from the
code which draws it.

```scala fuel
compile
frame
  label The pen is set down
  code
      import Svg.Outline

      val flag = Outline().moveTo((0, 0))
frame
  label A mast
  highlight lineUp(8)  a relative step, straight up
  code
      import Svg.Outline

      val flag = Outline().moveTo((0, 0)).lineUp(8)
frame
  label A pennant
  highlight line(3*Delta.Right..Delta.Down)  a displacement, written as arithmetic
  code
      import Svg.Outline

      val flag = Outline().moveTo((0, 0)).lineUp(8)
        . line(3*Delta.Right + Delta.Down).line(3*Delta.Left + Delta.Down)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(flag.in[Xml].show)
```

The y-axis of SVG points down, so `lineUp(8)` is written as the vertical step `v -8.0`.
[Paths](../../soundness/modules/svg.md#paths) lists every step.
