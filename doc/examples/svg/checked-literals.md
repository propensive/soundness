# A gradient stop beyond the end

A stop of a gradient has an offset between zero and one. The offset is a bounded number, so
the compiler reads the literal, and a stop which lies beyond the end of its gradient is never
written into a drawing.

```scala fuel
compile
frame
  label A stop at 150%
  errors show
  code
      val dusk = Svg.LinearGradient
        ( Svg.Id(t"dusk"), Stop(0.0, WebColors.Red), Stop(1.5, WebColors.Blue) )
frame
  label A stop at the end
  highlight 1.0  within the bounds of an offset
  code
      val dusk = Svg.LinearGradient
        ( Svg.Id(t"dusk"), Stop(0.0, WebColors.Red), Stop(1.0, WebColors.Blue) )
##
```

[Gradients and color](../../soundness/modules/svg.md#gradients-and-color) describes the
definition, and [safety by construction](../../philosophy/safety-by-construction.md) the
principle.
